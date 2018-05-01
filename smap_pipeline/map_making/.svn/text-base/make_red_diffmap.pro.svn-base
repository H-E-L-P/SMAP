;+
;NAME
; make_red_diffmap
;PURPOSE
; Combine 250 and 500 micron maps to search for red sources
;USAGE
; map = make_red_diffmap(map1,map2 [, K=k, SCALEFAC1=scalefac1,$
;                        SCALEFAC2=scalefac2, MASKFILE1=maskfile1, $
;                        MASKFILE2=maskfile2, MIN1=min1, MIN2=min2)
;INPUTS
; map1          250 micron map
; map2          500 micron map
;OUTPUTS
; map           Subracted map: scalefac2*map2*k - sqrt(1-k^2)*scalefac1*map1,
;                possibly with masking applied
;OPTIONAL INPUTS
; k             k factor (def: 0.673)
; scalefac1     Map scaling for 250 micron map (def: 1.0)
; scalefac2     Map scaling for 500 micron map (def: 1.0)
; maskfile1     File with list of borders of polyhedral map for map 1.
;                List of ra, dec pairs, one pair per line, in degrees.
;                Only points inside the polyhedron are included.
; maskfile2     Same as maskfile1, but for map2.
; min1          Minimum value of map 1 used in diffmap
; min2          Minimum value of map 2 used in diffmap
; beamsize      FWHM of beam in arcsec.  Needed if /ADDFIRAS is used
;                 (def sqrt(2)*35.3)
;KEYWORDS
; addfiras      Add FIRAS background to each map before subtracting
;                to avoid positive sources being negative.  Depends
;                on the beam size
;NOTES
; The maps need to have the same scale.  This also assumes they
; are beam matched
;MODIFICATION HISTORY
; Author: Alex Conley, Aug 2011
;-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION make_red_diffmap, map1, map2, K=k, SCALEFAC1=scalefac1,$
                           SCALEFAC2=scalefac2, MASKFILE1=maskfile1,$
                           MASKFILE2=maskfile2, MIN1=min1, MIN2=min2,$
                           ADDFIRAS=addfiras, BEAMSIZE=beamsize

  COMPILE_OPT IDL2, STRICTARRSUBS

  arctol = 0.1 ;;difference in ra/dec in arcseconds to allow

  ;;input parse
  IF N_ELEMENTS(map1) EQ 0 THEN MESSAGE,"Missing map1"
  IF SIZE( map1, /TNAME ) NE 'STRUCT' THEN $
     MESSAGE,"map1 is not SMAP fitsmap structure"
  IF N_ELEMENTS(map2) EQ 0 THEN MESSAGE,"Missing map2"
  IF SIZE( map2, /TNAME ) NE 'STRUCT' THEN $
     MESSAGE,"map2 is not SMAP fitsmap structure"

  ;;Default k requires f_500/f_250 > 1.1
  IF N_ELEMENTS(k) EQ 0 THEN k = 1.0/SQRT(1+1.1^2)

  IF k LT 0 THEN MESSAGE,"Invalid (negative) k"
  IF k GT 1 THEN MESSAGE,"Invalid k (>1)"
  IF N_ELEMENTS(scalefac1) NE 0 THEN BEGIN
     has_scalefac1 = 1b
     IF scalefac1 LT 0 THEN MESSAGE,"Invalid (negative) scalefac for map1"
  ENDIF ELSE has_scalefac1 = 0b
  IF N_ELEMENTS(scalefac2) NE 0 THEN BEGIN
     has_scalefac2 = 1b
     IF scalefac2 LT 0 THEN MESSAGE,"Invalid (negative) scalefac for map2"
  ENDIF ELSE has_scalefac2 = 0b
  IF N_ELEMENTS(maskfile1) THEN BEGIN
     IF ~ FILE_TEST(maskfile1,/READ) THEN $
        MESSAGE,"Can't read mask file 1: "+maskfile1
     has_maskfile1 = 1b
  ENDIF ELSE has_maskfile1 = 0b
  IF N_ELEMENTS(maskfile2) THEN BEGIN
     IF ~ FILE_TEST(maskfile2,/READ) THEN $
        MESSAGE,"Can't read mask file 2: "+maskfile2
     has_maskfile2 = 1b
  ENDIF ELSE has_maskfile2 = 0b

  IF N_ELEMENTS(beamsize) EQ 0 THEN beamsize = SQRT(2)*35.3
  IF KEYWORD_SET(addfiras) && beamsize LE 0 THEN $
     MESSAGE,"Invalid (non-positive) beam size"

  ;;Check maps are compatible
  IF map1.xsize NE map2.xsize THEN MESSAGE,"Map1 and 2 have different x extents"
  IF map1.ysize NE map2.ysize THEN MESSAGE,"Map1 and 2 have different x extents"
  IF ABS( (map1.pixscale-map2.pixscale)/map1.pixscale ) GT 1d-4 THEN $
     MESSAGE,"Map1 and map2 have incompatible pixel sizes"
  xtestpix = map1.xsize/2
  ytestpix = map1.ysize/2
  XY2AD, xtestpix, ytestpix, map1.astrometry, ra1, dec1
  XY2AD, xtestpix, ytestpix, map2.astrometry, ra2, dec2
  IF 3600.0*COS(!DTOR*dec1)*ABS( ra1-ra2 ) GT arctol THEN $
     MESSAGE,"Incompatible ra in two maps"
  IF 3600.0*ABS(dec1-dec2) GT arctol THEN $
     MESSAGE,"Incompatible dec in two maps"

  ;;Min, firas bkg, and scale
  im1 = map1.image
  im2 = map2.image
  IF N_ELEMENTS( min1 ) NE 0 THEN im1 >= min1
  IF N_ELEMENTS( min2 ) NE 0 THEN im2 >= min2
  IF KEYWORD_SET( addfiras ) THEN BEGIN
     im1 += get_firas_flux( map1.names, FWHM=beamsize )
     im2 += get_firas_flux( map2.names, FWHM=beamsize )
  ENDIF
  IF has_scalefac1 THEN im1 *= scalefac1
  IF has_scalefac2 THEN im2 *= scalefac2

  mapout = map1
  mapout.image = k*TEMPORARY(im2) - SQRT(1-k^2)*TEMPORARY(im1)

  ;;Deal with the error
  IF mapout.has_error THEN BEGIN
     IF map1.has_error THEN BEGIN
        IF has_scalefac1 THEN nim1 = scalefac1*map1.error ELSE nim1 = map1.error
     ENDIF ELSE nim1 = REPLICATE(0.0,map1.xsize,map1.ysize)
     IF map2.has_error THEN BEGIN
        IF has_scalefac2 THEN nim2 = scalefac2*map2.error ELSE nim2 = map2.error
     ENDIF ELSE nim2 = REPLICATE(0.0,map2.xsize,map2.ysize)
     mapout.error = SQRT( (k*nim1)^2 + (2-k^2)*nim2^2 )
  ENDIF

  ;;Deal with masking
  ;;First, respect input masks
  IF map1.has_mask THEN BEGIN
     wbad = WHERE( map1.mask NE 0, nbad, NCOMPLEMENT=ngood )
     IF ngood EQ 0 THEN MESSAGE,"Mask from map1 eliminates all data"
     IF nbad NE 0 THEN mapout.image[wbad] = !VALUES.D_NAN
  ENDIF
  IF map2.has_mask THEN BEGIN
     wbad = WHERE( map2.mask NE 0, nbad, NCOMPLEMENT=ngood )
     IF ngood EQ 0 THEN MESSAGE,"Mask from map2 eliminates all data"
     IF nbad NE 0 THEN BEGIN
        mapout.image[wbad] = !VALUES.D_NAN
        IF mapout.has_mask THEN mapout.mask[wbad]=1
     ENDIF
  ENDIF

  ;;Now build region mask
  IF has_maskfile1 THEN BEGIN
     READCOL, maskfile1, ra, dec, /SILENT, FORMAT='(F,F)'
     IF N_ELEMENTS(ra) EQ 0 THEN MESSAGE,"Mask "+maskfile1+" had no information"
     AD2XY, ra, dec, map1.astrometry, xpoly, ypoly
     ;;This is 1 for everything -inside the polygon-
     curr_rmask = get_region_mask(map1.xsize,map1.ysize,xpoly,ypoly)

     ;;If we have both, and them together
     IF has_maskfile2 THEN BEGIN
        READCOL, maskfile2, ra, dec, /SILENT, FORMAT='(F,F)'
        IF N_ELEMENTS(ra) EQ 0 THEN MESSAGE,"Mask "+maskfile2+$
                                            " had no information"
        AD2XY, ra, dec, map2.astrometry, xpoly, ypoly
        ;;Now 1 for everything inside both masks
        curr_rmask AND= get_region_mask(map1.xsize,map1.ysize,xpoly,ypoly)
     ENDIF

     wbad = WHERE( TEMPORARY(curr_rmask) EQ 0, nbad, NCOMPLEMENT=ngood )
     IF ngood EQ 0 THEN MESSAGE,"Masking blocks all data"
     IF nbad NE 0 THEN BEGIN
        mapout.image[wbad] = !VALUES.D_NAN
        IF mapout.has_error THEN mapout.error[wbad] = !VALUES.D_NAN
        IF mapout.has_mask THEN mapout.mask[wbad] = 1
     ENDIF
  ENDIF ELSE IF has_maskfile2 THEN BEGIN
     READCOL, maskfile2, ra, dec, /SILENT, FORMAT='(F,F)'
     IF N_ELEMENTS(ra) EQ 0 THEN MESSAGE,"Mask "+maskfile2+$
                                         " had no information"
     AD2XY, ra, dec, map2.astrometry, xpoly, ypoly
     ;;Now 1 for everything inside the poly
     curr_rmask = get_region_mask(map1.xsize,map1.ysize,xpoly,ypoly)
     wbad = WHERE( TEMPORARY(curr_rmask) EQ 0, nbad, NCOMPLEMENT=ngood )
     IF ngood EQ 0 THEN MESSAGE,"Masking blocks all data"
     IF nbad NE 0 THEN BEGIN
        mapout.image[wbad] = !VALUES.D_NAN
        IF mapout.has_error THEN mapout.error[wbad] = !VALUES.D_NAN
        IF mapout.has_mask THEN mapout.mask[wbad] = 1
     ENDIF
  ENDIF
  
  mapout.bands=STRING(map2.bands,map1.bands,FORMAT='(A0,"-",A0)')
  mapout.names=STRING(map2.names,map1.names,FORMAT='(A0,"-",A0)')
  mapout.tod_excludemask OR= map2.tod_excludemask

  RETURN,mapout

END
