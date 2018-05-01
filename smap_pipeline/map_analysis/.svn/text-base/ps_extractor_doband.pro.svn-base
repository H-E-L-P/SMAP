
FUNCTION ps_extractor_mask, map, mapmaskbits
  COMPILE_OPT IDL2, HIDDEN
  ;;The returned mask is 1 where the pixel should
  ;; be skipped, 0 where it's good

  mask = BYTARR( map.xsize, map.ysize )
  IF map.has_mask THEN BEGIN
     wmask = WHERE( (map.mask AND mapmaskbits) NE 0, nmask )
     IF nmask NE 0 THEN mask[wmask]=1b
  ENDIF
  IF map.has_exposure THEN BEGIN
     wmask = WHERE( map.exposure EQ 0, nmask )
     IF nmask NE 0 THEN mask[wmask] = 1b
  ENDIF
  RETURN,mask
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION ps_extractor_doband, inmap, dlimit, band, base_name,$
                              CHECK_IMS=check_ims, VERBOSE=verbose,$
                              ALG=alg, MAPMASKBITS=mapmaskbits,$
                              PRE_SMOOTH=pre_smooth, MIN_CORR=min_corr,$
                              SUCCESS=success, FWHM=fwhm, SIG=sig,$
                              BGSIZE=bgsize, BGINPUT=bginput,$
                              MASKCHECKSZ=maskchecksz

  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN
  success = 0b

  IF N_ELEMENTS(min_corr) EQ 0 THEN min_corr = 0.75
  IF N_ELEMENTS(bgsize) EQ 0 THEN bgsize = 32
  IF KEYWORD_SET(verbose) THEN silent=0b ELSE silent=1b
  IF N_ELEMENTS(maskchecksz) EQ 0 THEN maskchecksz = 0
  IF N_ELEMENTS(alg) EQ 0 THEN alg = 2 ;;starfinder by default

  ;;Do we need to compute bg here, or did user provide
  IF N_ELEMENTS(bginput) NE 0 THEN BEGIN
     szbg = SIZE(bginput)
     IF szbg[0] NE 2 THEN MESSAGE,"Input bg is not 2D"
     IF szbg[1] NE inmap.xsize THEN MESSAGE,"Input BG wrong x size"
     IF szbg[2] NE inmap.ysize THEN MESSAGE,"Input BG wrong x size"
     bg = bginput
     getbg = 0b
  ENDIF ELSE getbg = 1b

  ;;Deal with masking, including no coverage regions
  mask = ps_extractor_mask( inmap, mapmaskbits )
  wgood = WHERE(mask EQ 0,ngood, COMPLEMENT=wbad, NCOMPLEMENT=nbad)
  IF ngood EQ 0 THEN BEGIN
     MESSAGE,"All pixels masked in "+band+" -- skipping",/INF
     RETURN, !VALUES.F_NAN
  ENDIF

  ;;GET PIXEL SCALE USING SMAP ASTROMETRY STRUCTURE
  px_scale = inmap.pixscale

  ;; GET FWHM IN PIXELS
  IF N_ELEMENTS(fwhm) EQ 0 THEN fwhm = get_spire_beam_fwhm(band)
  fwhm_pix=fwhm/px_scale

  ;;Get PSF
  ;;This should be adjusted to be large enough
  psf=get_spire_beam(band,px_scale,/SILENT,FWHM=fwhm)

  ;; ESTIMATE BACKGROUND
  ;; This should be adjusted for size of pixel relative to PSF
  IF getbg THEN BEGIN
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,'Starting Background Estimator ('+band+')',/INF
     bg=smap_bgestimator(inmap.image,mask,bgsize)
  ENDIF
  bgsub=inmap.image-bg

  ;;Estimate initial sig
  sig=stddev(bg[wgood])

  ;;Detect sources
  ;;We don't have errors from find2, so use arbitrary assumptions
  IF KEYWORD_SET(verbose) THEN MESSAGE,'Locating '+band+' sources',/INF
  CASE alg OF
     1 : BEGIN
        find2,bgsub,x,y,f,sharp,round,dlimit*sig,$
              fwhm_pix,[0.2,1.0],[-1.0,1.0],/SILENT
        sigx = 0.5
        sigy = 0.5
        sigf = 0.1*f
     END
     2 : BEGIN
        IF inmap.has_error THEN noise_std = inmap.error
        IF nbad NE 0 THEN BEGIN
           ;;Get bad pixel coordinates
           bad_pixel_x = wbad mod inmap.xsize
           bad_pixel_y = wbad / inmap.xsize
        ENDIF
        STARFINDER, inmap.image, psf, dlimit, min_corr, x, y, f,$
                   sigx, sigy, sigf, corr, BACKGROUND=bg,$
                   X_BAD=bad_pixel_x, Y_BAD=bad_pixel_y,$
                   NOISE_STD=inmap.error, PRE_SMOOTH=pre_smooth,$
                   /REL_THRESHOLD, CORREL_MAG=2, SILENT=silent
        DELVARX, bad_pixel_x, bad_pixel_y, noise_std
     END
     3 : BEGIN
	find,bgsub,x,y,f,sharp,round,dlimit*sig,$
              fwhm_pix,[0.2,1.0],[-1.0,1.0],/SILENT
        sigx = 0.5
        sigy = 0.5
        sigf = 0.1*f
     END
     ELSE : MESSAGE,"Unknown algorithm"
  ENDCASE
  nsources = N_ELEMENTS(x)
  ;;Output check images
  IF KEYWORD_SET( check_ims ) THEN BEGIN
     IF KEYWORD_SET(verbose) THEN MESSAGE,'Creating Check Images',/INF

     ;;First, re-write input map
     st = write_smap_fitsmap(inmap,base_name+'_input_map')
     IF st EQ 0 THEN MESSAGE,"ERROR writing copy of input map"

     ;;Now background stuff
     map_copy = inmap
     IF getbg THEN BEGIN
        map_copy.image = bg
        st = write_smap_fitsmap(map_copy,base_name+'_bg_map')
        IF st EQ 0 THEN MESSAGE,"ERROR writing background map"
     ENDIF
     map_copy.image = bgsub
     st = write_smap_fitsmap(map_copy,base_name+'_bgsub_map')
     IF st EQ 0 THEN MESSAGE,"ERROR writing background subtracted map"

     ;;Source marked and subtracted maps
     IF nsources NE 0 THEN BEGIN
        map_copy.image = check_image(bgsub,x,y,9)
        st = write_smap_fitsmap(map_copy,base_name+'_checkim_map')
        IF st EQ 0 THEN MESSAGE,"ERROR writing object marked map"

        map_copy.image = remove_stars(inmap.image ,x, y, f, psf, alg)
        st = write_smap_fitsmap(map_copy,base_name+'_nostars_map')
        IF st EQ 0 THEN MESSAGE,"ERROR writing object subtracted map"
     ENDIF
  ENDIF
  IF nsources EQ 0 THEN RETURN,!VALUES.F_NAN

  ;;Now wrap them up as a nice structure
  CASE alg OF 
     1 : retstr = REPLICATE( { x: !VALUES.F_NAN, y: !VALUES.F_NAN,$
                               f: !VALUES.F_NAN, dx: !VALUES.F_NAN,$
                               dy: !VALUES.F_NAN, df: !VALUES.F_NAN,$
                               corr: !VALUES.F_NAN}, nsources )
     2 : retstr = REPLICATE( { x: !VALUES.F_NAN, y: !VALUES.F_NAN,$
                               f: !VALUES.F_NAN, dx: !VALUES.F_NAN,$
                               dy: !VALUES.F_NAN, df: !VALUES.F_NAN,$
                               corr: !VALUES.F_NAN}, nsources )
     3 : retstr = REPLICATE( { x: !VALUES.F_NAN, y: !VALUES.F_NAN,$
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

  ;;starfinder is very conservative about actually applying the mask --
  ;; even if the core is entirely masked, it may still accept an
  ;; object even if only the edge of the PSF is unmasked.  maskchecksz
  ;; allows a way to be more conservative by directly
  ;; looking at those pixels near the core position
  IF maskchecksz NE 0 AND nbad NE 0 THEN BEGIN
     maskhlf = maskchecksz / 2
     nobs = N_ELEMENTS(retstr)
     xmapmax = LONG(inmap.xsize)-1
     ymapmax = LONG(inmap.ysize)-1
     are_good = BYTARR(nobs)
     FOR i=0, nobs-1 DO BEGIN
        xpos = ROUND(retstr[i].x)
        ypos = ROUND(retstr[i].y)
        xmin = (xpos - maskhlf) > 0
        xmax = (xpos + maskhlf) < xmapmax
        ymin = (ypos - maskhlf) > 0
        ymax = (ypos + maskhlf) < ymapmax
        st = WHERE( mask[xmin:xmax,ymin:ymax] EQ 0, nnonmsk )
        IF nnonmsk EQ 0 THEN are_good[i] = 0b ELSE are_good[i]=1b
     ENDFOR
     wkeep = WHERE( are_good, nkeep, NCOMPLEMENT=ntoss )
     IF nkeep EQ 0 THEN BEGIN
        MESSAGE,"Direct mask check removed all remaining sources",/INF
        RETURN,!VALUES.F_NAN
     ENDIF
     IF ntoss NE 0 THEN retstr = retstr[wkeep]
  ENDIF
  
  success = 1b
  RETURN,retstr

END
