;+
;NAME
; smap_readtod
;PURPOSE
; Reads SMAP TOD from file
;USAGE
; tod = smap_readtod(filename)
;INPUTS
; filename    Name of file to read
;RETURNS
; A structure containing the tod info as produced by smap_setupmap
;OPTIONAL OUTPUTS
; success     1b if the read succeeded, 0b otherwise
; errmsg      A string error message explaining the failure, ''
;              on success
;MODIFICATION HISTORY
; Author: Alex Conley, Sep 2009
;         Mike Zemcov, Jan 2010 - added functionality to deal with 
;                                 turn arounds and masking
;-

FUNCTION smap_readtod, file, SUCCESS=success, ERRMSG=errmsg
  COMPILE_OPT IDL2
  success = 0b
  errmsg  = ''

  IF ~ FILE_TEST( file ) THEN BEGIN
     errmsg = "Can't find "+file
     RETURN,0b
  ENDIF
  
  ;;See what we have
  FITS_OPEN, file, fcb, MESSAGE=fcb_message
  FITS_CLOSE, fcb
  wmask = WHERE( STRUPCASE(fcb.extname) EQ 'MASKBITS', nmask )

  ;;/UNSIGNED to preserve BBID/OBSID
  dat = MRDFITS( file, 1, /SILENT, /UNSIGNED, STATUS=status )
  IF status NE 0 THEN BEGIN
     errmsg = "Error reading fits file: "+file
     RETURN,0b
  ENDIF

  ;;Now the real reason why this function exists -- trim the 
  ;; names of the channels!
  IF TAG_EXIST( dat, 'chan', /TOP_LEVEL ) THEN $
     dat.chan = STRTRIM(dat.chan,2)
  IF TAG_EXIST( dat, 'therm', /TOP_LEVEL ) THEN $
     dat.therm = STRTRIM(dat.therm,2)

  ;;Do mask
  IF nmask NE 0 THEN BEGIN
     mask_bitsp = MRDFITS( file, wmask[0], /SILENT, /UNSIGNED, STATUS=status )
     IF status NE 0 THEN BEGIN
        errmsg = "Error reading mask extension from fits file: "+file
        RETURN,0b
     ENDIF
     mask_bitsp.name = STRTRIM(mask_bitsp.name)
     mask_bitsp.comment = STRTRIM(mask_bitsp.comment)

     ; GM (2010-08-20): try to update this (maybe still broken)
     ;smapmasks = get_smap_mask_bits()
     ;mask_bits = struct_concat(mask_bitsp,smapmasks)
     add_default_maskbits, mask_bitsp, /ADDSMAP

     dat = CREATE_STRUCT(dat,'mask_bits',mask_bitsp)
  ENDIF

  IF TAG_EXIST( dat, 'mask', /TOP_LEVEL ) THEN BEGIN
     construct_turnaround_mask,dat,SUCCESS=success_cta
     IF success_cta NE 1 THEN BEGIN
        errmsg = "Error in construct_turnaround_mask!"
        RETURN,0b
     ENDIF
  ENDIF

  success=1b
  RETURN,dat

END
