;+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  function get_smap_mapstruct.pro
;;  May 12, 2009
;;  Mike Zemcov
;;  This function defines a structure to carry around maps for smap
;;  pipeline.
;;  Optional Inputs: 
;;          npixx = number of pix in x axis
;;          npixy = number of pix in y axis
;;          band  = Which band this is (PSW/PMW/PLW)
;;          astrometry = astrolib astrometry structure
;;  Outputs: mapstruct = a structure containing map data.
;;  Options:
;;     /NO_ABORT   On error, return
;;     success     returns 1 if this worked, 0 if it didn't
;;     noerr       Don't add room for the error map
;;     noexp       Don't add room for the exposure map
;;     nomask      Don't add room for the mask map
;;     filter      Add room for filter extensions
;;     lambda      Wavelength of band in microns -- ignored unless
;;                  band is non-standard
;;     exp_dbl     Exposure map (if present) is double type, not 
;;                  integer (seconds vs. hits)
;;     /SMALL      User smaller data types (floats vs doubles) to save
;;                  space
;;  Optional outputs
;;     errmsg      Error message if problem encountered
;;
;;  If you want a non-standard WCS (i.e., not RA---TAN, DEC--TAN)
;;   you should probably pass in your astrometry structure.
;;
;; CHANGELOG:
;;   20110725/GM: add mapmaskbits information to structure [not yet implemented]
;;   20110906/GM: add filter and effpsf maps (set /FILTER)
;;   20120326/GM: convert xsize and ysize to LONG
;;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-

FUNCTION GET_SMAP_MAPSTRUCT, NPIXX=npixx, NPIXY=npixy,$
                             BAND=band, ASTROMETRY=astrometry,$
                             NOERR=noerr, NOEXP=noexp, NOMASK=nomask,$
                             FILTER=filter, SMALL=small, $
                             NO_ABORT=no_abort, SUCCESS=success, ERRMSG=errmsg,$
                             SILENT=silent, LAMBDA=lambda, EXP_DBL=exp_dbl
  
  COMPILE_OPT IDL2
  success = 0b
  errmsg = ''

  silent = KEYWORD_SET(silent)
  IF KEYWORD_SET(no_abort) THEN abort=0b ELSE abort=1b
  ; check to see if we have been given a number of pixels - 
  ; if not then set the number
  IF N_ELEMENTS(npixx) EQ 0 THEN npixx = 256
  ; make y match x if it's not been defined
  IF N_ELEMENTS(npixy) EQ 0 THEN npixy = npixx
  IF N_ELEMENTS(band) EQ 0 THEN BEGIN
     IF ~ silent THEN $
        MESSAGE,'No band selected! Using PSW default.',/INFORMATIONAL
     band_used='PSW'
  ENDIF ELSE band_used = STRUPCASE(band)

  IF npixx LE 0 THEN BEGIN
     errmsg = STRING(npixx,FORMAT='("Npixx is non-positive",I5)')
     IF abort THEN MESSAGE,errmsg ELSE BEGIN
        IF ~ silent THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDELSE
  ENDIF
  IF npixy LE 0 THEN BEGIN
     errmsg = STRING(npixy,FORMAT='("Npixy is non-positive",I5)')
     IF abort THEN MESSAGE,errmsg ELSE BEGIN
        IF ~ silent THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDELSE
  ENDIF

  ; tell us what we're up to
  IF ~ silent THEN MESSAGE,'Making default map structure.',/INFORMATIONAL

  ; make dummy maps of the correct size
  ; masks are specified to be 32 bits for Herschel (I think)
  IF KEYWORD_SET(small) THEN fltmap = FLTARR(npixx, npixy) ELSE $
     fltmap = DBLARR(npixx,npixy)
  IF ~ (KEYWORD_SET( nomask ) AND KEYWORD_SET( noexp )) THEN BEGIN
     IF KEYWORD_SET(small) THEN intmap = UINTARR(npixx, npixy) ELSE $
        intmap = ULONARR(npixx,npixy)
  ENDIF

  IF N_ELEMENTS( astrometry ) EQ 0 OR $
     SIZE(astrometry,/TNAME) NE 'STRUCT' THEN BEGIN
     ;;Set up object to hold IDL/astrolib astrometry structure
     ;;Assumes not SIN or ZPN astrometry
     ;;Note that we use the FITS convention for crpix (index 
     ;; starts at one)
     astrometry = { naxis: LONARR(2),$
                    cd: [ [2.0d0, 0.0d0],[ 0.0d0, 2.0d0 ] ] / 3600.0d0,$
                    cdelt: [1.0d0, 1.0d0],$
                    crpix: [1.0,1.0], crval: [0.0d0, 0.0d0],$
                    ctype: ['RA---TAN','DEC--TAN'],$
                    longpole: 180.0d0, latpole: 0.0d0, $
                    pv1: [0.0d0, 0.0d0, 90.0d0, 180.0d0, 90.0d0], $
                    pv2: DBLARR(2), axes: [1, 2], REVERSE: 0b,$
                    coord_sys: 'C', projection: 'TAN',$
                    known: BYTARR(1), radecsys: 'FK5',$
                    equinox: 2000.0d0,  dateobs: 'UNKNOWN',$
                    mjdobs: !VALUES.D_NAN, x0y0: DBLARR(2)}
     astrometry.naxis[0] = npixx
     astrometry.naxis[1] = npixy
  ENDIF

  GETROT, astrometry, rot, cdelt, /SILENT
  pixscale = SQRT( ABS(cdelt[0] * cdelt[1]) ) * 3600.0

;  mapmask = GET_MAPMASK_STRUCT()
;
  mapstruct = {lambda:0,$
               bands:'',$
               names:'',$
               image:fltmap,$
               astrometry: astrometry,$
               pixscale: pixscale,$
               xsize:LONG(npixx),$
               ysize:LONG(npixy),$
               has_exposure: ~ KEYWORD_SET( noexp ),$
               has_error: ~ KEYWORD_SET( noerr ),$
               has_mask: ~ KEYWORD_SET( nomask ),$
               has_filter: KEYWORD_SET( filter ),$
               tod_excludemask: 0uL}

  IF ~ KEYWORD_SET( noerr ) THEN $
     mapstruct = CREATE_STRUCT( mapstruct, 'error', fltmap )
  IF ~ KEYWORD_SET( noexp ) THEN $
     IF KEYWORD_SET( exp_dbl ) THEN $
        mapstruct = CREATE_STRUCT( mapstruct, 'exposure', fltmap ) ELSE $
        mapstruct = CREATE_STRUCT( mapstruct, 'exposure', intmap )
  IF ~ KEYWORD_SET( nomask ) THEN $
     mapstruct = CREATE_STRUCT( mapstruct, 'mask', intmap )
  IF KEYWORD_SET( filter ) THEN $
     mapstruct = CREATE_STRUCT( mapstruct, 'filter', fltmap, 'effpsf', fltmap )
     

  ; set a few known parameters manually
  CASE band_used OF
     'PSW' : BEGIN
        mapstruct.lambda=250
        mapstruct.bands='250 microns'
        mapstruct.names='PSW'
     END
     'PMW' : BEGIN
        mapstruct.lambda=350
        mapstruct.bands='350 microns'
        mapstruct.names='PMW'
     END
     'PLW' : BEGIN
        mapstruct.lambda=500
        mapstruct.bands='500 microns'
        mapstruct.names='PLW'
     END
     ELSE : BEGIN
        IF N_ELEMENTS( lambda ) EQ 0 THEN BEGIN
           errmsg = 'Nonsensical band given, must specify lambda! '+band_used
           IF abort THEN MESSAGE,errmsg ELSE BEGIN
              IF ~ silent THEN MESSAGE,errmsg,/INF
              RETURN,!VALUES.F_NAN
           ENDELSE
        ENDIF
        mapstruct.lambda = lambda
        mapstruct.bands = STRING(lambda,FORMAT='(F0.1," microns")')
        mapstruct.names = band_used
     END
  ENDCASE

  ; give the struct back
  success=1b
  RETURN,mapstruct
END
