;+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  function read_smap_fitsmap.pro
;;  May 12, 2009
;;  Mike Zemcov
;;  This function takes a file name root and band as input and returns a
;;    stucture of map data.
;;  Usage: map = READ_SMAP_FITSMAP(fileroot, band)
;;  Input: fileroot = the root name of the files to be read.
;;         band     = The band to read in
;;  Output: mapstruct = the structure of smap map data.
;;  Options:
;;     /STRICT     Requires the file meets the full SMAP map
;;                  definition -- so it must have exposure, mask,
;;                                error info
;;     /NO_ABORT   On error, return
;;     allow_band  Don't check whether band is as expected
;;     dir         Directory to look for map in.  If not set, looks in
;;                  !SMAP_MAPS.  Set to '.' to read from the current
;;                  dir.
;;     filenamein  Input the smap filename by hand
;;     /silent     Don't print status messages
;;     noreadmask  Don't read the mask, even if present
;;     noreadexp   Don't read the exposure map, even if present
;;     noreaderr   Don't read the exposure map, even if present
;;     lambda      Wavelength of data, if it can't be
;;                  determined from file
;
;;     success     returns 1 if this worked, 0 if it didn't
;;     errmsg      Error message if problem encountered
;;
;;  Example: map = read_smap_fitsmap( 'sample_map', 'PSW' )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-
FUNCTION READ_SMAP_FITSMAP, fileroot, band, SILENT=silent, DIR=dir,$
                            ERRMSG=errmsg, SUCCESS=success,$
                            NOREADMASK=noreadmask, NOREADEXP=noreadexp, $
                            NOREADERR=noerr, NO_ABORT=no_abort, STRICT=strict,$
                            FILENAMEIN=filenamein, LAMBDA=lambda,$
                            ALLOW_BAND=allow_band

  COMPILE_OPT IDL2
  success = 0b
  errmsg = ''

  silent = KEYWORD_SET(silent)
  abort  = ~ KEYWORD_SET( no_abort )
  
  IF N_ELEMENTS(filenamein) NE 0 THEN BEGIN

     filename=filenamein 
     
     band_used = STRUPCASE(band) ;;Allow lowercase inputs
     IF ~ KEYWORD_SET( allow_band ) THEN BEGIN
        IF ~ STREGEX( band_used, 'p[sml]w',/FOLD_CASE,/BOOLEAN) THEN BEGIN
           errmsg = 'Nonsensical band given, aborting! '+band_used
           IF abort THEN MESSAGE,errmsg ELSE BEGIN
              IF ~ silent THEN MESSAGE,errmsg,/INF
              RETURN,!VALUES.F_NAN
           ENDELSE
        ENDIF
     ENDIF
     
  ENDIF ELSE BEGIN
     
     ;; make sure we got a file name, if we didn't, bail
     IF N_ELEMENTS(fileroot) EQ 0 THEN BEGIN
        errmsg = 'No file name given, aborting!'
        IF abort THEN MESSAGE,errmsg ELSE BEGIN
           IF ~ silent THEN MESSAGE,errmsg,/INF
           RETURN,!VALUES.F_NAN
        ENDELSE
     ENDIF
     IF SIZE(fileroot,/TNAME) NE 'STRING' THEN BEGIN
        errmsg = "Fileroot is not a string, but a "+SIZE(fileroot,/TNAME)
        IF abort THEN MESSAGE,errmsg ELSE BEGIN
           IF ~ silent THEN MESSAGE,errmsg,/INF
           RETURN,!VALUES.F_NAN
        ENDELSE
     ENDIF
     IF N_ELEMENTS(band) EQ 0 THEN BEGIN
        errmsg = "No band provided"
        IF abort THEN MESSAGE,errmsg ELSE BEGIN
           IF ~ silent THEN MESSAGE,errmsg,/INF
           RETURN,!VALUES.F_NAN
        ENDELSE
     ENDIF
     IF SIZE(band,/TNAME) NE 'STRING' THEN BEGIN
        errmsg="Band is not a string, but a "+SIZE(band,/TNAME)
        IF abort THEN MESSAGE,errmsg ELSE BEGIN
           IF ~ silent THEN MESSAGE,errmsg,/INF
           RETURN,!VALUES.F_NAN
        ENDELSE
     ENDIF

     band_used = STRUPCASE(band) ;;Allow lowercase inputs
     IF ~ KEYWORD_SET( allow_band ) THEN BEGIN
        IF ~ STREGEX( band_used, 'p[sml]w',/FOLD_CASE,/BOOLEAN) THEN BEGIN
           errmsg = 'Nonsensical band given, aborting! '+band_used
           IF abort THEN MESSAGE,errmsg ELSE BEGIN
              IF ~ silent THEN MESSAGE,errmsg,/INF
              RETURN,!VALUES.F_NAN
           ENDELSE
        ENDIF
     ENDIF


     IF N_ELEMENTS(dir) EQ 0 THEN idir = addslash(!SMAP_MAPS) $
     ELSE idir = addslash(dir)
     IF ~ FILE_TEST(idir,/DIRECTORY,/READ) THEN BEGIN
        IF ~ FILE_TEST(idir) THEN $
           errmsg = "Unable to find output dir: "+idir ELSE $
              IF ~ FILE_TEST(idir,/DIRECTORY) THEN $
                 errmsg = idir+" exists but isn't directory!" ELSE $
                    errmsg = "Can't write to output dir "+idir
        IF abort THEN MESSAGE,errmsg ELSE BEGIN
           IF ~ silent THEN MESSAGE,errmsg,/INF
           RETURN,!VALUES.F_NAN
        ENDELSE
     ENDIF

     ;; construct the file name
     filename = idir + fileroot + '_' + band_used + '.fits'

  ENDELSE


  ;; tell me what you're up to
  IF ~silent THEN MESSAGE,'Reading FITS files.',/INFORMATIONAL

  IF ~ FILE_TEST( filename ) THEN BEGIN
     errmsg = filename+" not found"
     IF abort THEN MESSAGE,errmsg ELSE BEGIN
        IF ~ silent THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDELSE
  ENDIF
  IF ~ FILE_TEST( filename, /READ ) THEN BEGIN
     errmsg = "Unable to read: "+filename
     IF abort THEN MESSAGE,errmsg ELSE BEGIN
        IF ~ silent THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDELSE
  ENDIF

  ;;Figure out how many extensions we have
  ;;If we have all of them, great
  ;; If we don't, things might be slightly messy since we have to
  ;; guess which ones we actually have
  FITS_OPEN, filename, fcb, MESSAGE=errmsg
  IF errmsg NE '' THEN BEGIN
     IF abort THEN MESSAGE,errmsg ELSE BEGIN
        IF ~ silent THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDELSE
  ENDIF
  FITS_CLOSE, fcb

  IF fcb.nextend EQ 0 THEN BEGIN
     errmsg = "The structure of this fits file is not what was expected"
     IF abort THEN MESSAGE,errmsg ELSE BEGIN
        IF ~ silent THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDELSE
  ENDIF

  nomask = 0b & noexp = 0b & noerr = 0b
  IF fcb.nextend EQ 1 THEN BEGIN
     ;; We only have the primary image
     ;; Make sure it's an IMAGE
     IF fcb.xtension[1] NE 'IMAGE' THEN BEGIN
        errmsg = "First extension is not an image!"
        IF abort THEN MESSAGE,errmsg ELSE BEGIN
           IF ~ silent THEN MESSAGE,errmsg,/INF
           RETURN,!VALUES.F_NAN
        ENDELSE
     ENDIF
     nomask=1b & noexp=1b & noerr=1b
  ENDIF ELSE BEGIN
     ;; Now we rely on extension names to decide what we have
     IF KEYWORD_SET( noreaderr ) THEN noerr=1b ELSE BEGIN
        werr = WHERE( STRUPCASE(STRTRIM(fcb.extname,2)) EQ 'ERROR' AND $
                      fcb.xtension EQ 'IMAGE', nerr )
        IF nerr EQ 0 THEN noerr=1b
     ENDELSE
     IF KEYWORD_SET( noreadexp ) THEN noexp = 1b ELSE BEGIN
        wexp = WHERE( STRUPCASE(STRTRIM(fcb.extname,2)) EQ 'EXPOSURE' AND $
                      fcb.xtension EQ 'IMAGE', nexp )
        IF nexp EQ 0 THEN noexp=1b
     ENDELSE
     IF KEYWORD_SET( noreadmask ) THEN nomask = 1b ELSE BEGIN
        wmask = WHERE( STRUPCASE(STRTRIM(fcb.extname,2)) EQ 'MASK' AND $
                       fcb.xtension EQ 'IMAGE', nmask )
        IF nmask EQ 0 THEN nomask=1b
     ENDELSE
  ENDELSE
  
  IF KEYWORD_SET( strict ) THEN BEGIN
     IF (~ KEYWORD_SET( noreadexp )) AND noexp THEN $
        errmsg += " file is missing EXPOSURE information"
     IF (~ KEYWORD_SET( noreaderr )) AND noerr THEN $
        errmsg += " file is missing ERROR information"
     IF (~ KEYWORD_SET( noreadmask )) AND nomask THEN $
        errmsg += " file is missing MASK information"
     IF errmsg NE '' THEN BEGIN
        IF abort THEN MESSAGE,errmsg ELSE BEGIN
           IF ~ silent THEN MESSAGE,errmsg,/INF
           RETURN,!VALUES.F_NAN
        ENDELSE
     ENDIF
  ENDIF

  ;; read in the first map.  We use this to get the size
  read_status = 0
  image = MRDFITS(filename,1,header,/SILENT,STATUS=read_status)
  IF read_status NE 0 THEN BEGIN
     errmsg = "Error reading first extension of "+filename
     IF abort THEN MESSAGE,errmsg ELSE BEGIN
        IF ~ silent THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDELSE
  ENDIF

  szim = SIZE(image)
  IF szim[0] NE 2 THEN BEGIN
     errmsg = "Image extension of "+filename+" is not 2D"
     IF abort THEN MESSAGE,errmsg ELSE BEGIN
        IF ~ silent THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDELSE
  ENDIF
  npixx = szim[1]
  npixy = szim[2]

  ;; Figure out what type the exposure is if present
  ;;  can be integer (nhits) or double (exposure in seconds)
  IF ~ noexp THEN $
     IF fcb.bitpix[wexp[0]] LT 0 THEN exp_dbl = 1b ELSE exp_dbl = 0b

  ;; Parse the astrometry.  We do this -before- we set up the
  ;; structure because the astrometry structure has an annoying
  ;; tendency to change with astrolib revisions
  EXTAST, header, astr, ext_success
  IF ext_success EQ -1 THEN BEGIN
     errmsg="Error parsing in astrometry params from "+filename
     IF abort THEN MESSAGE,errmsg ELSE BEGIN
        IF ~ silent THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDELSE
  ENDIF

  ;; get the map structure now that we know the size
  getmap_succ = 0b
  mapstruct = get_smap_mapstruct(NPIXX=npixx, NPIXY=npixy, $
                                 BAND=band_used, NOMASK=nomask,$
                                 NOEXP=noexp, NOERR=noerr, SILENT=silent,$
                                 EXP_DBL=exp_dbl,/NO_ABORT,LAMBDA=lambda,$
                                 ASTROMETRY=astr, ERRMSG=errmsg, $
                                 SUCCESS=getmap_succ)
  IF ~ getmap_succ THEN IF abort THEN MESSAGE,errmsg ELSE BEGIN
     IF ~ silent THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDELSE

  ;;Now add the image in
  mapstruct.image = TEMPORARY(image)

  ;; Masking
  tod_excludemask=SXPAR(header, 'TODMASK', COUNT=mcount, /SILENT)
  IF mcount NE 0 THEN $
     mapstruct.tod_excludemask = ULONG(tod_excludemask)

  ;; Compute the pixel scale in arcsec
  GETROT, mapstruct.astrometry, rot, cdelt
  xpixscale = 3600.0 * ABS(cdelt[0])
  ypixscale = 3600.0 * ABS(cdelt[1])
  mapstruct.pixscale = SQRT(xpixscale*ypixscale)

  ;; get the error map extension
  read_status=0b
  IF ~ noerr THEN BEGIN
     error = MRDFITS(filename,werr[0],header,/SILENT,STATUS=read_status)
     IF read_status NE 0 THEN BEGIN
        errmsg = "Error reading second (error) extension of "+filename
        IF abort THEN MESSAGE,errmsg ELSE BEGIN
           IF ~ silent THEN MESSAGE,errmsg,/INF
           RETURN,!VALUES.F_NAN
        ENDELSE
     ENDIF
     szerr = SIZE(error)
     wbad = WHERE( szerr[0:2] NE szim[0:2], nbad )
     IF nbad NE 0 THEN BEGIN
        errmsg = "Error map not the same dimensions as image for "+filename
        IF abort THEN MESSAGE,errmsg ELSE BEGIN
           IF ~ silent THEN MESSAGE,errmsg,/INF
           RETURN,!VALUES.F_NAN
        ENDELSE
     ENDIF
     mapstruct.error = TEMPORARY(error)
  ENDIF

  ;; get the exposure map extension
  IF ~ noexp THEN BEGIN
     IF exp_dbl THEN BEGIN
        exposure = MRDFITS(filename,wexp[0],header,/SILENT,STATUS=read_status)
     ENDIF ELSE BEGIN
        exposure = MRDFITS(filename,wexp[0],header,/SILENT,STATUS=read_status,$
                           /UNSIGNED)
     ENDELSE
     IF read_status NE 0 THEN BEGIN
        errmsg = "Error reading exposure extension of "+filename
        IF abort THEN MESSAGE,errmsg ELSE BEGIN
           IF ~ silent THEN MESSAGE,errmsg,/INF
           RETURN,!VALUES.F_NAN
        ENDELSE
     ENDIF
     szexp = SIZE(exposure)
     wbad = WHERE( szexp[0:2] NE szim[0:2], nbad )
     IF nbad NE 0 THEN BEGIN
        errmsg = "Exposure map not the same dimensions as image for "+filename
        IF abort THEN MESSAGE,errmsg ELSE BEGIN
           IF ~ silent THEN MESSAGE,errmsg,/INF
           RETURN,!VALUES.F_NAN
        ENDELSE
     ENDIF
     mapstruct.exposure = TEMPORARY(exposure)
  ENDIF

  ;; get the mask map extension
  IF ~ nomask THEN BEGIN
     mask = MRDFITS(filename,wmask[0],header,/SILENT,/UNSIGNED,$
                    STATUS=read_status)
     IF read_status NE 0 THEN BEGIN
        errmsg = "Error reading mask extension of "+filename
        IF abort THEN MESSAGE,errmsg ELSE BEGIN 
           IF ~ silent THEN MESSAGE,errmsg,/INF
           RETURN,!VALUES.F_NAN
        ENDELSE
     ENDIF
     szmask = SIZE(mask)
     wbad = WHERE( szmask[0:2] NE szim[0:2], nbad )
     IF nbad NE 0 THEN BEGIN
        errmsg = "Mask map not the same dimensions as image for "+filename
        IF abort THEN MESSAGE,errmsg ELSE BEGIN
           IF ~ silent THEN MESSAGE,errmsg,/INF
           RETURN,!VALUES.F_NAN
        ENDELSE
     ENDIF
     mapstruct.mask = TEMPORARY(mask)
  ENDIF

  ;; return the structure we're after
  success = 1b
  RETURN,mapstruct

  ;; and we're out
END
