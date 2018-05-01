;+
;NAME
; linear map combine
;PURPOSE
; Combine 250, 350, and 500 micron maps linearly
;USAGE
; map = linear_map_combine(k1,map250,k2,map350,k3,map500,$
;                          MASKFILES=, MASKDIR=maskdir)
;INPUTS
; k1            250 micron map coefficient
; k2            350 micron map coefficient
; k3            500 micron map coefficient
; map250        250 micron map
; map350        350 micron map
; map500        500 micron map
;OUTPUTS
; map           k1*map250 + k2*map350 + k3*map500,
;                possibly with masking applied.
;OPTIONAL INPUTS
; maskfiles     Colon seperated list of mask files.  Masks are
;                anded together.  If the mask name starts with -,
;                the negative of that mask is used.
;NOTES
; The maps need to have the same scale.  This also assumes they
; are already beam matched.
;MODIFICATION HISTORY
; Author: Alex Conley, October 2011
;-

PRO linear_map_combine_testcompat, map1, map2, arctol
  COMPILE_OPT IDL2, HIDDEN

  namecomb = map1.names + " and " + map2.names
  IF map1.xsize NE map2.xsize THEN $
     MESSAGE,namecomb+" micron mapshave different x extents"
  IF map1.ysize NE map2.ysize THEN $
     MESSAGE,namecomb+" micron mapshave different y extents"
  IF ABS( (map1.pixscale-map2.pixscale)/map1.pixscale ) GT 1d-4 THEN $
     MESSAGE,namecomb+" micron maps have incompatible pixel sizes"
  
  xtestpix = map1.xsize/2
  ytestpix = map2.ysize/2
  XY2AD, xtestpix, ytestpix, map1.astrometry, ra1, dec1
  XY2AD, xtestpix, ytestpix, map2.astrometry, ra2, dec2
  IF 3600.0*COS(!DTOR*dec1)*ABS( ra1-ra2 ) GT arctol THEN $
     MESSAGE,"Incompatible ra in "+namecomb+" micron maps"
  IF 3600.0*ABS(dec1-dec2) GT arctol THEN $
     MESSAGE,"Incompatible dec in "+namecomb+" micron maps"
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION linear_map_combine, k1, map250, k2, map350, k3, map500,$
                             MASKFILES=maskfiles, MASKDIR=maskdir

  COMPILE_OPT IDL2, STRICTARRSUBS

  arctol = 0.1 ;;difference in ra/dec in arcseconds to allow

  ;;Figure out coefficient combos, whether we need maps
  IF N_ELEMENTS(k1) EQ 0 THEN MESSAGE,"k1 not set"
  IF ~ FINITE(k1) THEN MESSAGE,"Non-finite k1"
  IF N_ELEMENTS(k2) EQ 0 THEN MESSAGE,"k2 not set"
  IF ~ FINITE(k2) THEN MESSAGE,"Non-finite k2"
  IF N_ELEMENTS(k3) EQ 0 THEN MESSAGE,"k3 not set"
  IF ~ FINITE(k3) THEN MESSAGE,"Non-finite k3"

  IF ABS(k1) GT 1e-4 THEN use_map250 = 1b ELSE use_map250 = 0b
  IF ABS(k2) GT 1e-4 THEN use_map350 = 1b ELSE use_map350 = 0b
  IF ABS(k3) GT 1e-4 THEN use_map500 = 1b ELSE use_map500 = 0b
  
  IF ~ (use_map250 OR use_map350 OR use_map500) THEN $
     MESSAGE,"Coefficients result in no output map"

  IF use_map250 THEN BEGIN
     IF N_ELEMENTS(map250) EQ 0 THEN MESSAGE,"No 250 micron map provided"
     IF SIZE(map250,/TNAME) NE 'STRUCT' THEN $
        MESSAGE,"map250 isn't expected SMAP map structure"
  ENDIF
  IF use_map350 THEN BEGIN
     IF N_ELEMENTS(map350) EQ 0 THEN MESSAGE,"No 350 micron map provided"
     IF SIZE(map350,/TNAME) NE 'STRUCT' THEN $
        MESSAGE,"map350 isn't expected SMAP map structure"
  ENDIF
  IF use_map500 THEN BEGIN
     IF N_ELEMENTS(map500) EQ 0 THEN MESSAGE,"No 500 micron map provided"
     IF SIZE(map500,/TNAME) NE 'STRUCT' THEN $
        MESSAGE,"map500 isn't expected SMAP map structure"
  ENDIF

  ;;Check maps are compatible
  IF use_map250 && use_map350 THEN $
     linear_map_combine_testcompat, map250, map350, arctol
  IF use_map250 && use_map500 THEN $
     linear_map_combine_testcompat, map250, map500, arctol
  IF use_map350 && use_map500 THEN $
     linear_map_combine_testcompat, map350, map500, arctol

  ;;map construction
  IF use_map250 THEN mapout=map250 ELSE IF use_map350 THEN $
     mapout=map350 ELSE mapout=map500
  mapout.image = 0.0
  IF use_map250 THEN mapout.image += k1*map250.image
  IF use_map350 THEN mapout.image += k2*map350.image
  IF use_map500 THEN mapout.image += k3*map500.image
  
  ;;There's no particularly good way to combine the exposure maps
  ;;The model adopted here is to keep the variance per exposure time
  ;; constant if the variance was the same for each image
  IF mapout.has_exposure THEN BEGIN
     mapout.exposure = 0.0
     ;;Do weighted combine
     IF use_map250 && ~ map250.has_exposure THEN $
        MESSAGE,"Can't construct exposure information from 250 map"
     IF use_map350 && ~ map350.has_exposure THEN $
        MESSAGE,"Can't construct exposure information from 350 map"
     IF use_map500 && ~ map500.has_exposure THEN $
        MESSAGE,"Can't construct exposure information from 500 map"
     IF use_map250 THEN mapout.exposure += k1^2*map250.exposure
     IF use_map350 THEN mapout.exposure += k2^2*map350.exposure
     IF use_map500 THEN mapout.exposure += k3^2*map500.exposure
  ENDIF

  ;;Error.  Just decide to do this based on if the first thing we
  ;; actually included had an error map.  But if one has an error
  ;; map, they had better all have one, or else the results will
  ;; be meaningless
  IF mapout.has_error THEN BEGIN
     varim = REPLICATE(0.0d0,mapout.xsize,mapout.ysize)
     IF use_map250 && ~ map250.has_error THEN $
        MESSAGE,"Can't construct error information from 250 map"
     IF use_map350 && ~ map350.has_error THEN $
        MESSAGE,"Can't construct error information from 350 map"
     IF use_map500 && ~ map500.has_error THEN $
        MESSAGE,"Can't construct error information from 500 map"
     IF use_map250 THEN varim += (k1*map250.error)^2
     IF use_map350 THEN varim += (k2*map350.error)^2
     IF use_map500 THEN varim += (k3*map500.error)^2
     mapout.error = SQRT( TEMPORARY(varim) )
  ENDIF

  IF mapout.has_mask THEN BEGIN
     IF N_ELEMENTS(maskfiles) NE 0 THEN $
        add_user_mapmask, mapout, maskfiles, /ANDPLUS, MASKDIR=maskdir
     IF use_map250 && map250.has_mask THEN $
        mapout.mask OR= map250.mask
     IF use_map350 && map350.has_mask THEN $
        mapout.mask OR= map350.mask
     IF use_map500 && map500.has_mask THEN $
        mapout.mask OR= map500.mask
  ENDIF

  mapout.names = 'LINCOMB'
  IF use_map250 THEN mapout.tod_excludemask OR= map250.tod_excludemask
  IF use_map350 THEN mapout.tod_excludemask OR= map350.tod_excludemask
  IF use_map500 THEN mapout.tod_excludemask OR= map500.tod_excludemask

  RETURN,mapout

END
