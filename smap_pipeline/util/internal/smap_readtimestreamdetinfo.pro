;+
;NAME
; smap_readtimestreamdetinfo
;PURPOSE
; Reads SMAP file detinfo from file -- see parse_timestream_detinfo
;USAGE
; fileinfo = smap_readtimestreamdetinfo(filename)
;INPUTS
; filename    Name of file to read
;RETURNS
; A structure containing the file info as produced by
;  parse_timestream_detinfo
;OPTIONAL OUTPUTS
; success     1b if the read succeeded, 0b otherwise
; errmsg      A string error message explaining the failure, ''
;              on success
;MODIFICATION HISTORY
; Author: Alex Conley, Oct 2009
;-

FUNCTION smap_readtimestreamdetinfo, file, SUCCESS=success, ERRMSG=errmsg
  COMPILE_OPT IDL2
  success = 0b
  errmsg  = ''

  IF ~ FILE_TEST( file ) THEN BEGIN
     errmsg = "Can't find "+file
     RETURN,0b
  ENDIF
  
  ;;/UNSIGNED to preserve BBID/OBSID
  dat = MRDFITS( file, 1, /SILENT, /UNSIGNED, STATUS=status )
  IF status NE 0 THEN BEGIN
     errmsg = "Error reading fits file: "+file
     RETURN,0b
  ENDIF

  success=1b
  RETURN,dat

END
