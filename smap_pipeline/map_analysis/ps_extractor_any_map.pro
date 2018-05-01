
FUNCTION ps_extractor_mask, map, xsize, ysize
  COMPILE_OPT IDL2, HIDDEN
  ;;The returned mask is 1 where the pixel should
  ;; be skipped, 0 where it's good

  mask = BYTARR( xsize, ysize )
     wmask = WHERE( map NE 0, nmask )
     IF nmask NE 0 THEN mask[wmask]=1b
  RETURN,mask
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION ps_extractor_any_map,map, hd, dlimit, base_name,$
			      px_scale=px_scale,$
                              CHECK_IMS=check_ims, $
                              ALG=alg, MAPMASKBITS=mapmaskbits,$
                              PRE_SMOOTH=pre_smooth, MIN_CORR=min_corr,$
                              SUCCESS=success, FWHM=fwhm, SIG=sig,$
                              BGSIZE=bgsize, BGINPUT=bginput

  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN
  success = 0b

  IF N_ELEMENTS( min_corr ) EQ 0 THEN min_corr = 0.75
  IF N_ELEMENTS( bgsize ) EQ 0 THEN bgsize = 32
  ms=size(map,/dim)
  xsize=ms[0]
  ysize=ms[1]

  ;;Do we need to compute bg here, or did user provide
  IF N_ELEMENTS(bginput) NE 0 THEN BEGIN
     szbg = SIZE(bginput)
     IF szbg[0] NE 2 THEN MESSAGE,"Input bg is not 2D"
     IF szbg[1] NE xsize THEN MESSAGE,"Input BG wrong x size"
     IF szbg[2] NE ysize THEN MESSAGE,"Input BG wrong x size"
     bg = bginput
     getbg = 0b
  ENDIF ELSE getbg = 1b

  ;;Deal with masking, including no coverage regions
  mask = 0.*ps_extractor_mask( map, xsize, ysize )
  wgood = WHERE(mask EQ 0,ngood, COMPLEMENT=wbad, NCOMPLEMENT=nbad)

  ;; GET FWHM IN PIXELS
  IF N_ELEMENTS(fwhm) EQ 0 THEN stop
  fwhm_pix=fwhm/px_scale

  ;;Get PSF
  ;;This should be adjusted to be large enough
  psf=kernal(fwhm, xsize<ysize, pixsize=px_scale)

  ;; ESTIMATE BACKGROUND
  ;; This should be adjusted for size of pixel relative to PSF
  IF getbg THEN BEGIN
     MESSAGE,'Starting Background Estimator ',/INF
     bg=smap_bgestimator(map,mask,bgsize)
  ENDIF
  bgsub=map-bg

  ;;Estimate initial sig
  sig=stddev(bg[wgood])

  ;;Detect sources
  ;;We don't have errors from find2, so use arbitrary assumptions
  MESSAGE,'Locating sources',/INF
  CASE alg OF
     1 : BEGIN
        find2,bgsub,x,y,f,sharp,round,dlimit*sig,$
              fwhm_pix,[0.2,1.0],[-1.0,1.0],/SILENT
        sigx = 0.5
        sigy = 0.5
        sigf = 0.1*f
     END
     2 : BEGIN
        IF keyword_set(noise) THEN noise_std = noise 
        IF nbad NE 0 THEN BEGIN
           ;;Get bad pixel coordinates
           bad_pixel_x = wbad mod xsize
           bad_pixel_y = wbad / xsize
        ENDIF
	;stop
        STARFINDER, map, psf, dlimit, min_corr, x, y, f,$
                   sigx, sigy, sigf, corr, BACKGROUND=bg,$
                   X_BAD=bad_pixel_x, Y_BAD=bad_pixel_y,$
                   ;NOISE_STD=inmap.error,$ 
		   PRE_SMOOTH=pre_smooth,$
                   /REL_THRESHOLD, CORREL_MAG=2 
        DELVARX, bad_pixel_x, bad_pixel_y, noise_std
     END
     ELSE : MESSAGE,"Unknown algorithm"
  ENDCASE
  nsources = N_ELEMENTS(x)
  ;;Output check images
  IF KEYWORD_SET( check_ims ) THEN BEGIN
     MESSAGE,'Creating Check Images',/INF

     ;;First, re-write input map
     ;st = write_smap_fitsmap(map,base_name+'_input_map')
     fits_write, base_name+'_input_map', map, hd
     ;IF st EQ 0 THEN MESSAGE,"ERROR writing copy of input map"

     ;;Now background stuff
     map_copy = map
     IF getbg THEN BEGIN
        map_copy = bg
;        st = write_smap_fitsmap(map_copy,base_name+'_bg_map')
	fits_write, base_name+'_bg_map', map_copy, hd
;        IF st EQ 0 THEN MESSAGE,"ERROR writing background map"
     ENDIF
     map_copy = bgsub
     ;st = write_smap_fitsmap(map_copy,base_name+'_bgsub_map')
     fits_write, base_name+'_bgsub_map', map_copy, hd
     ;IF st EQ 0 THEN MESSAGE,"ERROR writing background subtracted map"

     ;;Source marked and subtracted maps
     IF nsources NE 0 THEN BEGIN
        map_copy = check_image(bgsub,x,y,9)
        ;st = write_smap_fitsmap(map_copy,base_name+'_checkim_map')
	fits_write, base_name+'_checkim_map', map_copy, hd
        ;IF st EQ 0 THEN MESSAGE,"ERROR writing object marked map"

        map_copy = remove_stars(map ,x, y, f, psf, alg)
        ;st = write_smap_fitsmap(map_copy,base_name+'_nostars_map')
	fits_write, base_name+'_nostars_map', map_copy, hd
        ;IF st EQ 0 THEN MESSAGE,"ERROR writing object subtracted map"
     ENDIF
  ENDIF
  IF nsources EQ 0 THEN RETURN,!VALUES.F_NAN

  ;;Now wrap them up as a nice structure
  CASE alg OF 
     1 : retstr = REPLICATE( { x: !VALUES.F_NAN, y: !VALUES.F_NAN,$
                               f: !VALUES.F_NAN, dx: !VALUES.F_NAN,$
                               dy: !VALUES.F_NAN, df: !VALUES.F_NAN}, nsources )
     2 : retstr = REPLICATE( { x: !VALUES.F_NAN, y: !VALUES.F_NAN,$
                               f: !VALUES.F_NAN, dx: !VALUES.F_NAN,$
                               dy: !VALUES.F_NAN, df: !VALUES.F_NAN,$
                               corr: !VALUES.F_NAN}, nsources )
  ENDCASE

  retstr.x = TEMPORARY(x)
  retstr.y = TEMPORARY(y)
  retstr.f = TEMPORARY(f)
  retstr.dx = TEMPORARY(sigx)
  retstr.dy = TEMPORARY(sigy)
  retstr.df = TEMPORARY(sigf)
  IF alg EQ 2 THEN retstr.corr = TEMPORARY(corr)
  success = 1b
  RETURN,retstr

END
