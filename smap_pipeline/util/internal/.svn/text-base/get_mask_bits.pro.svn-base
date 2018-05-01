;+
; NAME:
;   get_mask_bits
; CATEGORY
;   Herschel SPIRE SMAP pipeline
; PURPOSE:
;   Parse a HCSS Herschel header containing Hierarchical mask bit
;   information into a map between mask bits and mask names.
; CALLING SEQUENCE:
;   mask_bit_info = get_mask_bits( header [, SUCCESS= ] )
; INPUTS:
;   header      The header containing the mask bit info.  This is
;                usually found in the primary header of data files
;                output by HIPE
;   hipemasksonly  If only the default HIPE masks are required this 
;                   bit can be set, otherwise the output structure 
;                    contains new SMAP-only masks as well.
; OUTPUTS:
;   Returns an anonymous structure with tags .name, .bits, and
;   .comment tags. .bits is a 32 bit integer.
; OPTIONAL OUTPUTS:
;   success     Returns 1 on succes, something 0 on failure.
;   errmsg      Error message if one is encountered
; KEYWORDS
;   verbose     Run in verbose mode
;   defaultonly Don't read from the header -- just return the default
; PROCEDURES CALLED
;   get_hierarch
; MODIFICATION HISTORY:
;   Author: Alex Conley, May 1, 2009
;           Mike Zemcov, August 6, 2010 - added new smap-required mask
;                                         bits.
;           Alex Conley, August 18, 2010 -- default mask bits now
;                                           added, SMAP bits now moved
;                                           to add_default_maskbits
;-

FUNCTION get_mask_bits, header, HIPEMASKSONLY=hipemasksonly,$
                        ERRMSG=errmsg, SUCCESS=success,$
                        VERBOSE=verbose, DEFAULTONLY=defaultonly

  COMPILE_OPT IDL2, STRICTARRSUBS

  success = 0b
  errmsg = ''
  IF KEYWORD_SET( hipemasksonly ) THEN addsmap = 0b ELSE addsmap = 1b

  IF KEYWORD_SET( defaultonly ) THEN BEGIN
     add_default_maskbits, retarr, ADDSMAP=addsmap
     RETURN,retarr
  ENDIF

  hrch_success = 0b
  hierarch_map = get_hierarch( header, SUCCESS=hrch_success, COUNT=nhierarch )
  IF hrch_success EQ 0 THEN BEGIN
     errmsg = "Unable to process HIERARCH keywords for mask"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF

;;Herschel mask bitflags all start with mask (i.e., maskNoisy)
  wmask = WHERE( STRLOWCASE(STRMID(hierarch_map[*,1],0,4)) EQ 'mask',nmask )
  IF nmask EQ 0 THEN BEGIN
     errmsg = "Found no mask names"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF

  masknames = hierarch_map[wmask,0]
  masktags  = hierarch_map[wmask,1]

  maskstruct = { NAME: '', BITS: 0UL, COMMENT: '' }
  retarr = REPLICATE( maskstruct, nmask ) 
  retarr.name = masktags   


  FOR i=0, nmask-1 DO BEGIN
     maskval = herschel_fxpar( header, masknames[i], COMMENT=comment )
     IF !err NE 0 THEN BEGIN
        IF !err EQ -1 THEN BEGIN
           errmsg = "Unable to find "+masknames[i]+" in hdr"
        ENDIF ELSE IF !err GT 1 THEN BEGIN
           errmsg = "Found more than one match to "+masknames[i]+$
                    " in hdr"
        ENDIF ELSE errmsg="Unknown problem getting mask names"
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDIF

     ;;The comment often start with "[] " which we would like to remove
     ;; if present
     comment = STRTRIM(comment,2)
     IF STRMID(comment,0,3) EQ "[] " THEN $
        comment = STRMID(comment,3)

     retarr[i].bits = maskval
     retarr[i].comment = comment
  ENDFOR

  nuniq = N_ELEMENTS( UNIQ(retarr.bits, SORT(retarr.bits)) )
  IF nuniq NE N_ELEMENTS(retarr) THEN BEGIN
     errmsg = "Non unique mask bits in header"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF

  ;;Add on defaults in case some are missing
  add_default_maskbits, retarr, ADDSMAP=addsmap

  retarr = retarr[ SORT(retarr.bits) ]

  success = 1b
  RETURN,retarr

END
