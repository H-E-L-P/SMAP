;+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  function smap_write3colors.pro
;;  Aug 30, 2009
;;  Mike Zemcov
;;  This function takes in an obsid and three structures of smap data
;;   (corresponding to different spire colors) and writes them to
;;   smap-approved format fits files.  It's basically just a wrapper 
;;   for smap_write_fits.pro called three times.
;;  Inputs: mapname = Base name for output maps
;;          map250: initialized 250 micron smap map structure; may already
;;           contain data
;;          map350: initialized 350 micron smap map structure; may already
;;           contain data
;;          map500: initialized 500 micron smap map structure; may already
;;           contain data
;;  Outputs: success = 1 if maps were successfully made, 0 if not
;;  Options: silent = verbose messaging, 1=silent
;;           exname = extended file name
;;           no_abort = don't abort if file error
;;           errmsg = if error, string containing error message
;;           dir = Directory to output to instead of !SMAP_MAPS
;;           opthdr = Additional info to add to header, in the form
;;                  of a string array fits header.  Note that
;;                  everything in this header will be added, so you
;;                  don't want it to have stuff like NAXIS.
;;                  Only standard length FITS keywords are supported
;;  Changelog: v1.0, MZ, Aug 30, 2009, original version.
;;             GM, Feb 14, 2011, add NO250/NO350/NO500 keywords
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-
FUNCTION SMAP_WRITE3COLORS,mapname,map250,map350,map500,$
                           DIR=dir,EXNAME=exname,SILENT=silent,$
                           NO250=no250, NO350=no350, NO500=no500, $
                           NO_ABORT=no_abort, ERRMSG=errmsg, OPTHDR=opthdr
  COMPILE_OPT IDL2

  ; error catching stuff
  success = 0b
  errmsg = ''
  
  silent = KEYWORD_SET(silent)
  abort  = ~ KEYWORD_SET(no_abort)

  ; check to see what the state of passing parameters is - if they're
  ; not defined, bail
  IF SIZE(mapname,/TNAME) NE 'STRING' THEN BEGIN
     errmsg = "Input file name is not of string type"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,success
  ENDIF

  ; if we didn't get an exname, set it to null
  IF ~(KEYWORD_SET(exname)) THEN BEGIN
     exname = ''
  ENDIF ELSE IF KEYWORD_SET(verbose) THEN $
     MESSAGE,'EXNAME set to ' + exname,/INF

  ; check we were given a sort of sane set of inputs
  IF NOT KEYWORD_SET(no250) THEN BEGIN
      IF (N_ELEMENTS(map250) EQ 0) THEN BEGIN
          errmsg = 'No 250 micron map structure passed to me, aborting!'
          IF abort THEN MESSAGE,errmsg ELSE BEGIN
              IF ~ silent THEN MESSAGE,errmsg,/INF
              RETURN,success
          ENDELSE
      ENDIF

      IF SIZE(map250,/TNAME) NE 'STRUCT' THEN BEGIN
          errmsg = '250 micron map structure is not a structure, aborting!'
          IF abort THEN MESSAGE,errmsg ELSE BEGIN
              IF ~ silent THEN MESSAGE,errmsg,/INF
              RETURN,success
          ENDELSE
      ENDIF
  ENDIF

  IF NOT KEYWORD_SET(no350) THEN BEGIN
      IF (N_ELEMENTS(map350) EQ 0) THEN BEGIN
          errmsg = 'No 350 micron map structure passed to me, aborting!'
          IF abort THEN MESSAGE,errmsg ELSE BEGIN
              IF ~ silent THEN MESSAGE,errmsg,/INF
              RETURN,success
          ENDELSE
      ENDIF

      IF SIZE(map350,/TNAME) NE 'STRUCT' THEN BEGIN
          errmsg = '350 micron map structure is not a structure, aborting!'
          IF abort THEN MESSAGE,errmsg ELSE BEGIN
              IF ~ silent THEN MESSAGE,errmsg,/INF
              RETURN,success
          ENDELSE
      ENDIF
  ENDIF

  IF NOT KEYWORD_SET(no500) THEN BEGIN
      IF (N_ELEMENTS(map500) EQ 0) THEN BEGIN
          errmsg = 'No 500 micron map structure passed to me, aborting!'
          IF abort THEN MESSAGE,errmsg ELSE BEGIN
              IF ~ silent THEN MESSAGE,errmsg,/INF
              RETURN,success
          ENDELSE
      ENDIF

      IF SIZE(map500,/TNAME) NE 'STRUCT' THEN BEGIN
          errmsg = '500 micron map structure is not a structure, aborting!'
          IF abort THEN MESSAGE,errmsg ELSE BEGIN
              IF ~ silent THEN MESSAGE,errmsg,/INF
              RETURN,success
          ENDELSE
      ENDIF
  ENDIF

  ; tell me what you're up to
  IF ~ silent THEN MESSAGE,'Writing FITS files.',/INFORMATIONAL

  ; make the filename
  filename = mapname + exname

  ; write the 250 um map
  IF NOT KEYWORD_SET(no250) THEN BEGIN
      wsf_success = WRITE_SMAP_FITSMAP(map250,filename, DIR=dir,$
                                       SILENT=silent,ERRMSG=wsf_errmsg,$
                                       OPTHDR=opthdr)
      ; check for errors
      IF ~wsf_success THEN BEGIN
          errmsg = 'WRITE_SMAP_FITSMAP kicked up error: ' + wsf_errmsg
          IF KEYWORD_SET(verbose) THEN $
             MESSAGE,errmsg,/INF
          RETURN,success
      ENDIF
  ENDIF

  ; write the 350 um map
  IF NOT KEYWORD_SET(no350) THEN BEGIN
      wsf_success = WRITE_SMAP_FITSMAP(map350,filename, DIR=dir,$
                                       SILENT=silent,ERRMSG=wsf_errmsg,$
                                       OPTHDR=opthdr)
      ; check for errors
      IF ~wsf_success THEN BEGIN
          errmsg = 'WRITE_SMAP_FITSMAP kicked up error: ' + wsf_errmsg
          IF KEYWORD_SET(verbose) THEN $
             MESSAGE,errmsg,/INF
          RETURN,success
      ENDIF
  ENDIF

  ; write the 500 um map
  IF NOT KEYWORD_SET(no500) THEN BEGIN
      wsf_success = WRITE_SMAP_FITSMAP(map500,filename, DIR=dir,$
                                       SILENT=silent,ERRMSG=wsf_errmsg,$
                                       OPTHDR=opthdr)
      ; check for errors
      IF ~wsf_success THEN BEGIN
          errmsg = 'WRITE_SMAP_FITSMAP kicked up error: ' + wsf_errmsg
          IF KEYWORD_SET(verbose) THEN $
             MESSAGE,errmsg,/INF
          RETURN,success
      ENDIF
  ENDIF

  ; if no errors, we've written the files correctly and can leave
  success=1b
  RETURN,success

END
