;+
;NAME
; convert_fits_to_smap_fitsmap
;PURPOSE
; To convert a normal FITS image to a SMAP fitsmap
;USAGE
; map = convert_fits_to_smap_fitsmap( image, head, band )
;INPUTS
; image        The image array you want to convert
; head         The header corresponding to image, which -must-
;               have astrometry info in it
; band         The band you want this output as
;RETURNS
; A smap style map
;MODIFICATION HISTORY
; Original author: Alex Conley, Feb 2010
;-

FUNCTION convert_fits_to_smap_fitsmap, image, head, band, MASK=mask,$
                                       VERBOSE=verbose,$
                                       ERRMSG=errmsg, SUCCESS=success
  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg  = ''

  IF N_ELEMENTS(image) EQ 0 THEN BEGIN
     errmsg = "You failed to pass in an image"
     GOTO, err_handler
  ENDIF

  szim = SIZE(image)
  IF szim[0] NE 2 THEN BEGIN
     errmsg = "Input image is not two dimensional"
     GOTO, err_handler
  ENDIF

  npixx = szim[1]
  npixy = szim[2]
  
  no_mask = N_ELEMENTS(mask) EQ 0
  mapstruct = get_smap_mapstruct(npixx=npixx, npixy=npixy, band=band,$
                                 NOMASK=no_mask,/NOEXP,/NOERR,/SILENT,$
                                 ERRMSG=errmsg, SUCCESS=getmap_succ)
  IF getmap_succ EQ 0 THEN GOTO,err_handler

  mapstruct.image = image

  ;;Astrometry
  EXTAST, head, astr, ext_success
  IF ext_success EQ -1 THEN BEGIN
     errmsg="Error parsing in astrometry params from header"
     GOTO,err_handler
  ENDIF

  st = mapstruct.astrometry
  STRUCT_ASSIGN, TEMPORARY(astr), st ;;Deals with potentially missing tags
  mapstruct.astrometry = st

  ;;Compute the pixel scale in arcsec
  GETROT, mapstruct.astrometry, rot, cdelt
  xpixscale = 3600.0 * ABS(cdelt[0])
  ypixscale = 3600.0 * ABS(cdelt[1])
  mapstruct.pixscale = SQRT(xpixscale*ypixscale)

  IF N_ELEMENTS(mask) NE 0 THEN $
     mapstruct.mask = mask

  success = 1b
  RETURN,mapstruct

err_handler:
  IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INFO
  RETURN,!VALUES.D_NAN

END
