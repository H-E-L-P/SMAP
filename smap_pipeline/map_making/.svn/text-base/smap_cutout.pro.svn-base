;+
;NAME
; smap_cutout
;PURPOSE
; To create a cutout of a smap fitsmap around a user specified
;location
;USAGE
; cutmap = smap_cutout(map,ra,dec,size)
;INPUTS
; map     SMAP fits map structure
; ra      RA of center of new map
; dec     DEC of center of new map
; size    Size (in arcsec) of cutout.  If a two element array, is the
;          x/y size, also in arcsec
;RETURNS
; cutmap  SMAP fits map structure of cutout region
;MODIFICATION HISTORY
; Author: Alex Conley, June 2011
;-

FUNCTION smap_cutout, map, ra, dec, sz, LAMBDA=lambda
  COMPILE_OPT IDL2

  IF N_ELEMENTS(map) EQ 0 THEN MESSAGE,"No input map"
  IF SIZE(map,/TNAME) NE 'STRUCT' THEN MESSAGE,"Was expecting SMAP fits map"
  IF MIN(sz) LE 0 THEN MESSAGE,"Size is invalid"

  ;;Figure out the clip area
  IF SIZE(ra,/TNAME) EQ 'STRING' THEN work_ra = ra_to_radeg(ra) ELSE $
     work_ra = DOUBLE(ra)
  IF SIZE(dec,/TNAME) EQ 'STRING' THEN work_dec = dec_to_decdeg(dec) ELSE $
     work_dec = DOUBLE(dec)
  AD2XY, work_ra, work_dec, map.astrometry, xcen, ycen
  xcen = ROUND(xcen)
  ycen = ROUND(ycen)

  IF xcen LT 0 OR ycen LT 0 OR xcen GT map.xsize-1 OR $
     ycen GT map.ysize-1 THEN MESSAGE,"Provided coordinates outside map"

  pix_extent1 = CEIL( 0.5 * sz[0] / map.pixscale )
  IF N_ELEMENTS(sz) GT 1 THEN $
     pix_extent2 = CEIL( 0.5 * sz[1] / map.pixscale ) ELSE $
        pix_extent2 = pix_extent1
  
  xlow = (xcen - pix_extent1) > 0
  xhigh = (xcen + pix_extent1) < (map.xsize-1)
  ylow = (ycen - pix_extent2) > 0
  yhigh = (ycen + pix_extent2) < (map.ysize-1)

  xsize = xhigh-xlow+1
  ysize = yhigh-ylow+1

  IF xsize LT 5 OR ysize LT 5 THEN MESSAGE,"Requested extent is too small"

  castr = map.astrometry
  castr.crpix[0] -= xlow
  castr.crpix[1] -= ylow

  IF map.has_exposure THEN BEGIN
     exptype = SIZE(map.exposure,/TNAME)
     IF exptype EQ 'DOUBLE' or exptype EQ 'FLOAT' THEN exp_dbl = 1b ELSE $
        exp_dbl = 0b
  ENDIF

  getmap_succ = 0b
  mapstruct = get_smap_mapstruct(NPIXX=xsize, NPIXY=ysize, $
                                 BAND=map.names, NOMASK=~map.has_mask,$
                                 ASTROMETRY=TEMPORARY(castr),$
                                 NOEXP=~map.has_exposure, $
                                 NOERR=~map.has_error, /SILENT,$
                                 ERRMSG=errmsg, SUCCESS=getmap_succ,$
                                 /NO_ABORT, EXP_DBL=exp_dbl,$
                                 LAMBDA=lambda)
  IF ~ getmap_succ THEN MESSAGE,errmsg

  mapstruct.image = map.image[xlow:xhigh,ylow:yhigh]
  IF map.has_mask THEN mapstruct.mask = map.mask[xlow:xhigh,ylow:yhigh]
  IF map.has_exposure THEN $
     mapstruct.exposure = map.exposure[xlow:xhigh,ylow:yhigh]
  IF map.has_error THEN $
     mapstruct.error = map.error[xlow:xhigh,ylow:yhigh]
  mapstruct.pixscale = map.pixscale
  
  RETURN,mapstruct
END
