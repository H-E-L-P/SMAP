;+
;NAME
; smap_writetod
;PURPOSE
; To write a SMAP TOD out as a fits file
;USAGE
; smap_writetod, tod, filename [, /VERBOSE, /NO_ABORT, ERRMSG=errmsg,$
;                                 CUSTOMSTR=customstr, SUCCESS=success]
;KEYWORDS
; customstr: insert string into outfile before suffix
;MODIFICATION HISTORY
; Author: Alex Conley, Nov 16, 2009
;
; 2013-02-18: add CUSTOMSTR keyword
;-

PRO SMAP_WRITETOD,datastruct,outfile,CUSTOMSTR=customstr, VERBOSE=verbose,$
                  NO_ABORT=no_abort, $
                  ERRMSG=errmsg,SUCCESS=success
  COMPILE_OPT IDL2, HIDDEN

  success = 0b
  errmsg = ''

  abort  = ~ KEYWORD_SET(no_abort)

  ; check to see what the state of passing parameters is - if they're
  ; not defined, bail
  IF (N_ELEMENTS(datastruct) EQ 0) THEN BEGIN
     errmsg = 'No map structure passed to me, aborting!'
     IF abort THEN MESSAGE,errmsg ELSE BEGIN
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN
     ENDELSE
  ENDIF
  IF SIZE(datastruct,/TNAME) NE 'STRUCT' THEN BEGIN
     errmsg = 'Map structure is not a structure, aborting!'
     IF abort THEN MESSAGE,errmsg ELSE BEGIN
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN
     ENDELSE
  ENDIF
  IF N_ELEMENTS( outfile ) EQ 0 THEN BEGIN
     errmsg = "No output file passed to me, aborting"
     IF abort THEN MESSAGE,errmsg ELSE BEGIN
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN
     ENDELSE
  ENDIF
  IF SIZE( outfile, /TNAME ) NE 'STRING' THEN BEGIN
     errmsg = "Output file is not a string, aborting"
     IF abort THEN MESSAGE,errmsg ELSE BEGIN
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN
     ENDELSE
  ENDIF
  IF KEYWORD_SET(customstr) THEN BEGIN
     ; insert customized tag into outfile (before extension, if one exists)
     ; assume first '.' separates extension
     pos = STRPOS(outfile, '.')
     IF pos GE 0 THEN $
        outfile = STRMID(outfile, 0, pos) + customstr + STRMID(outfile, pos) $
     ELSE $
        outfile = outfile + customstr
  ENDIF

  ; tell me what you're up to
  IF KEYWORD_SET(verbose) THEN $
     MESSAGE,'Writing FITS files.',/INFORMATIONAL,LEVEL=-1

  ;;Have to remove maskbits since it's a nested structure
  IF TAG_EXIST( datastruct, 'mask_bits', /TOP_LEVEL ) THEN BEGIN
     MWRFITS, REMOVE_TAGS(datastruct,'mask_bits'), outfile, /CREATE,$
              /SILENT,STATUS=status
     IF status NE 0 THEN BEGIN
        errmsg = "Error writing primary extension of output file"
        IF abort THEN MESSAGE,errmsg ELSE BEGIN
           IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
           RETURN
        ENDELSE
     ENDIF

     FXBHMAKE, mask_hdr, N_ELEMENTS(datastruct.mask_bits), 'maskbits', 'Mask'
     MWRFITS, datastruct.mask_bits, outfile, mask_hdr, /SILENT, STATUS=status
     IF status NE 0 THEN BEGIN
        errmsg = "Error writing mask bits extension of output file"
        IF abort THEN MESSAGE,errmsg ELSE BEGIN
           IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
           RETURN
        ENDELSE
     ENDIF
  ENDIF ELSE BEGIN
     MWRFITS, datastruct, outfile, /CREATE, /SILENT, STATUS=status
     IF status NE 0 THEN BEGIN
        errmsg = "Error writing primary extension of output file"
        IF abort THEN MESSAGE,errmsg ELSE BEGIN
           IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
           RETURN
        ENDELSE
     ENDIF
  ENDELSE

  success = 1b
  RETURN

END
