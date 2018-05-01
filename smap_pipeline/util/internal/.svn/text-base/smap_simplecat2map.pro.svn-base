;+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  function smap_cat2map.pro
;;  Feb 2, 2010
;;  Mike Zemcov
;;  This function takes a WCS fits header and a list of x, y, and fluxed
;;   for a set of sources and populates a map with them.
;;
;;  Inputs:
;;     header      header information in the format given by 
;;                  make_astr.pro in the idl astrolib
;;     x           vector of x (=RA) coordinates of sources
;;     y           vector of y (=Dec) coordinates of sources
;;     flux        vector of fluxes of sources
;;     band        one of 'PSW', 'PMW' or 'PLW'
;;  Options:
;;     pixeldist   if true, x and y are given in pixels, 
;;                  if false, x and y are given in degrees on the sky
;;     npixx       number of pixels in x direction
;;     npixy       number of pixels in y direction
;;     verbose     controls verbose output
;;     errmsg      Error message if problem encountered
;;  Outputs:
;;     mapstruct   smap-format map structure populated according to x,y,flux 
;;  For example:
;;   mapstruct = SMAP_SIMPLECAT2MAP(astrom,ra,dec,S)
;;  will return a populated map structure
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-
FUNCTION SMAP_SIMPLECAT2MAP,header,x,y,flux,band,CONVOL=convol,$
                            PIXELDIST=pixeldist,NPIXX=npixx,NPIXY=npixy,$
                            VERBOSE=verbose,ERRMSG=errmsg,SUCCESS=success


  COMPILE_OPT IDL2
  success=0b
  errmsg = ''
  IF NOT(KEYWORD_SET(verbose)) THEN BEGIN
     verbose = 0
  ENDIF
  silent = ~verbose
  
  ;; check inputs
  IF (N_ELEMENTS(header) EQ 0) THEN BEGIN
     errmsg = 'Header info not provided!'
     GOTO,err_handler
  ENDIF
  IF (N_ELEMENTS(x) EQ 0) THEN BEGIN
     errmsg = 'x info not provided!'
     GOTO,err_handler
  ENDIF
  IF (N_ELEMENTS(x) GT 1e4) THEN BEGIN
     MESSAGE,'WARNING: chose > 1e4 points, this program will be very slow!',$
             /INFORMATIONAL
  ENDIF
  IF (N_ELEMENTS(y) EQ 0) THEN BEGIN
     errmsg = 'y info not provided!'
     GOTO,err_handler
  ENDIF
  IF (N_ELEMENTS(flux) EQ 0) THEN BEGIN
     errmsg = 'flux info not provided!'
     GOTO,err_handler
  ENDIF
  IF (N_ELEMENTS(x) NE N_ELEMENTS(y) OR $
      N_ELEMENTS(x) NE N_ELEMENTS(flux)) THEN BEGIN
     errmsg = 'x,y,flux vectors do not have equal lengths!'
     GOTO,err_handler
  ENDIF
  IF (N_ELEMENTS(band) EQ 0) THEN BEGIN
     errmsg = 'No band given!'
     GOTO,err_handler
  ENDIF
  ;; if we didn't set the distances in pixels, then we set them in degrees
  IF NOT(KEYWORD_SET(pixeldist)) THEN pixeldist = 0
  ;; check on map limits and if something's not present fiddle with it
  IF NOT(KEYWORD_SET(npixx) AND KEYWORD_SET(npixy)) THEN BEGIN
     astrolimx = (MAX(x) - MIN(x)) / header.cd[0,0]
     astrolimy = (MAX(y) - MIN(y)) / header.cd[1,1]
     IF NOT(KEYWORD_SET(npixx)) THEN npixx = astrolimx
     IF NOT(KEYWORD_SET(npixy)) THEN npixy = astrolimy
  ENDIF     

  ; if we don't have the distances in pixels, then we need to make it
  IF ~pixeldist THEN BEGIN
     x = x / header.cd[0,0] + npixx / 2.
     y = y / header.cd[1,1] + npixy / 2.
  ENDIF

  ;; get default smap map structure
  mapstruct = GET_SMAP_MAPSTRUCT(NPIXX=npixx,NPIXY=npixy,band=band,$
                                 ASTROMETRY=header,$
                                 SUCCESS=success_gss,ERRMSG=errmsg_gss,$
                                 SILENT=silent)
  IF ~success_gss THEN BEGIN
     errmsg = 'GET_SMAP_STRUCT returned error: ' + errmsg_gss
     GOTO,err_handler
  ENDIF
  
  ; ok, we have a structure, now populate it
  ; if no convol then it's just a field of points
  IF ~convol THEN BEGIN
     FOR isrc=0L,N_ELEMENTS(flux)-1L DO BEGIN
        IF (FINITE(x[isrc]) AND FINITE(y[isrc])) THEN BEGIN
           mapstruct.image[x[isrc],y[isrc]] = $
              mapstruct.image[x[isrc],y[isrc]] + $
              flux[isrc]
        ENDIF
     ENDFOR
  ENDIF ELSE BEGIN
     ;; if we want to convol this is the simplest way 
     ;; to make points not be centered on pixels.
     pixsize = 3600. * SQRT(header.cd[0,0]^2 + header.cd[1,1]^2) / SQRT(2.)

     FOR isrc=0L,N_ELEMENTS(flux)-1L DO BEGIN
        IF (FINITE(x[isrc]) AND FINITE(y[isrc])) THEN BEGIN
           psf=GET_SPIRE_BEAM(band,pixsize,npixx,npixy,$
                              x[isrc],y[isrc],/SILENT)
           mapstruct.image = mapstruct.image + flux[isrc] * psf     
        ENDIF
     ENDFOR
  ENDELSE

  !EXCEPT=2

  ; remove floating point errors
  error = check_math(MASK=32) 

  ; ok, now it's populated we have nothing more to do.
  success=1b
  RETURN,mapstruct

  err_handler:
  IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
  RETURN,!VALUES.F_NAN

END
