;+
;NAME
; matched_filter_red
;PURPOSE
; Apply a matched filter to a set of SPIRE maps, resulting in all
; three maps matching the PLW matched filter.
;USAGE
; matched_filter_red, map250, map350, map500, conf [, INST=, NPAD=, K1=, K2=,
;                     FILT250=, FILT350=, FILT500=, BEAM=, WHITE_VAR=,
;                     /VERBOSE]
;INPUTS
; map250/350/500   SPIRE map structures containing maps in each
;                   band.  They all must have the same pixel scale.
;                   Modified on output.  All except for map500
;                   can be missing (undefined)
; conf             Confusion RMS noise level per beam (same units as maps)
;OPTIONAL INPUTS
; inst             Instrument noise level per beam in combined map 
;                   (same units as maps).  Otherwise computed from error map.
; npad             Amount to pad each map by in pix (def: 100)
; k1, k2           Map combination coefficients -- used for
;                   estimating instrument noise
;OPTIONAL OUTPUTS
; filt250/350/500  Structures containing enough information to reapply
;                   the filter.  This is helpful when simulating 
;                   including additional sources in the map.
; beam             Real space beam
; white_var        Variance of white noise used in filtering
;KEYWORDS
; verbose          Print informational messages as it runs
; fixnpad          Force the exact padding to be npad.  Otherwise,
;                   the code attempts to find some amount of padding
;                   larger than npad that makes the FFTs fast (see fft_npad)
;MODIFICATION HISTORY
;   Alex Conley, modified from matched_filter, April 2013
;-

;; Gets the beam for normalization purposes.  The edge is zero padded,
;; and the core is sampled at higher resolution.  The beam is peak normalized
FUNCTION matched_filter_red_beam, nx, ny, npad_x, npad_y, fwhm, pixsize

  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN

  fwhm_pix = fwhm / pixsize
  totx = nx + npad_x
  toty = ny + npad_y
  
  coresz = 4 * fwhm_pix + 1

  IF coresz GT totx OR coresz GT toty THEN BEGIN
     ;; Just get the whole thing oversampled
     beam = GET_SPIRE_BEAM('PSW', pixsize, totx, toty, totx/2, toty/2, $
                          FWHM=fwhm, /SILENT, OVERSAMP=5)
  END ELSE BEGIN
     ;; Basic, non-oversampled beam
     beam = PSF_GAUSSIAN(NPIXEL=[totx, toty], FWHM=fwhm_pix, $
                         CENTROID=[totx/2 ,toty/2], NDIMEN=2, /DOUBLE)
     ;; High res core
     minx = totx/2 - coresz/2
     maxx = minx + coresz - 1
     miny = toty/2 - coresz/2
     maxy = miny + coresz - 1
     beam[minx:maxx,miny:maxy] = GET_SPIRE_BEAM('PSW', pixsize, coresz, $
                                                coresz, FWHM=fwhm, /SILENT, $
                                                OVERSAMP=5)
  ENDELSE

  ;; Zero edge, normalize
  matched_filter_red_zeroedge, beam, npad_x, npad_y
  RETURN, beam / MAX(beam)
END

;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO matched_filter_red_internal, map, padded_weightmap, sm_wtmap, $
                                 filter_fft, npad_x, npad_y

  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN

  nx = map.xsize
  ny = map.ysize

  ;; Padding.  We get the total amount of padding to be npad_x,
  ;; npad_y, but have to be careful because they may be odd
  did_pad = 0b
  IF npad_x GT 0 OR npad_y GT 0 THEN BEGIN
     npad_x_b = npad_x / 2
     npad_y_b = npad_y / 2
     working_map = DBLARR(nx + npad_x, ny + npad_y)
     working_map[npad_x_b:npad_x_b+nx-1, npad_y_b:npad_y_b+ny-1] = map.image
     did_pad = 1b
  ENDIF ELSE working_map = map.image

  ;; Apply filter
  sm_map = REAL_PART(FFT(FFT(working_map * padded_weightmap, 1, /DOUBLE) *$
                         filter_fft, -1, /DOUBLE)) / sm_wtmap

  ;; Unwrap results
  IF did_pad THEN BEGIN
     map.image = sm_map[npad_x_b:npad_x_b+nx-1, npad_y_b:npad_y_b+ny-1]
     IF map.has_error THEN $
        map.error = SQRT(1.0 / sm_wtmap[npad_x_b:npad_x_b+nx-1, $
                                        npad_y_b:npad_y_b+ny-1])
  ENDIF ELSE BEGIN
     map.image = sm_map
     IF map.has_error THEN map.error = SQRT(1.0 / sm_wtmap)
  ENDELSE
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO matched_filter_red_zeroedge, image, npad_x, npad_y
  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN

  sz = SIZE(image)
  IF sz[0] NE 2 THEN MESSAGE, "Image must be 2D"
  nx = sz[1] - npad_x
  ny = sz[2] - npad_y

  IF npad_x GT 0 THEN BEGIN
     npad_x_b = npad_x / 2
     image[0:npad_x_b - 1, *] = 0.0
     image[npad_x_b + nx - 1:*, *] = 0.0
  ENDIF
  IF npad_y GT 0 THEN BEGIN
     npad_y_b = npad_y / 2
     image[*, 0:npad_y_b - 1] = 0.0
     image[*, npad_y_b + ny - 1:*] = 0.0
  ENDIF
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO matched_filter_red, map250, map350, map500, conf, K1=k1, K2=k2,$
                        INST=inst, NPAD=npad, VERBOSE=verbose, $
                        FILT250=filt250, FILT350=filt350, FILT500=filt500, $
                        BEAM=beam, FIXNPAD=fixnpad, WHITE_VAR=white_var

  COMPILE_OPT IDL2, STRICTARRSUBS

  IF N_ELEMENTS(k1) EQ 0 THEN k1 = - SQRT(1.0 - 0.92^2)
  IF N_ELEMENTS(k2) EQ 0 THEN k2 = 0.0
  IF ABS(k1^2 + k2^2) GT 1.0 THEN MESSAGE,"Invalid coefficients"
  k3 = SQRT(1.0 - k1^2 - k2^2)
  
  IF N_ELEMENTS(npad) EQ 0 THEN npad = 100
  IF N_ELEMENTS(conf) EQ 0 THEN MESSAGE, "Failed to provide confusion noise"
  IF conf LT 0.0 THEN MESSAGE, "Invalid (negative) confusion noise"

  ;; in arcsec, same as in smap_redsource_smooth
  fwhm_nom = [17.6, 23.9, 35.2]

  has_250 = N_ELEMENTS(map250) NE 0
  has_350 = N_ELEMENTS(map350) NE 0
  has_500 = N_ELEMENTS(map500) NE 0
  IF ~ has_500 THEN MESSAGE, "Need at least the 500um map to be defined"

  ;;Make sure maps have same pixel size
  pixscale = map500.pixscale
  IF (has_250 && (pixscale - map250.pixscale) / pixscale GT 1e-3) THEN $
     MESSAGE,"Pixel size of 250 micron map doesn't match 500 micron map"
  IF (has_350 && (pixscale - map350.pixscale) / pixscale GT 1e-3) THEN $
     MESSAGE,"Pixel size of 350 micron map doesn't match 500 micron map"
  fwhm_nom_pix = fwhm_nom / pixscale

  ;;Dimensions must match as well
  nx = map500.xsize
  ny = map500.ysize
  IF has_250 && nx NE map250.xsize OR ny NE map250.ysize THEN $
     MESSAGE,"Dimensions of 250 micron map don't match 500 micron map"
  IF has_350 && nx NE map350.xsize OR ny NE map350.ysize THEN $
     MESSAGE,"Dimensions of 350 micron map don't match 500 micron map"

  ;; Form noise map
  IF N_ELEMENTS(inst) NE 0 THEN BEGIN
     inst_noise = FLOAT(inst)
     IF inst_noise LT 0 THEN $
        MESSAGE,"Invalid (negative) instrument noise value"

     varmap = REPLICATE(inst_noise^2, nx, ny)
  ENDIF ELSE BEGIN
     ;;Must have uncertainty maps
     IF has_250 && ~ map250.has_error THEN MESSAGE,"Need error map at 250um"
     IF has_350 && ~ map350.has_error THEN MESSAGE,"Need error map at 350um"
     IF ~ map500.has_error THEN MESSAGE,"Need error map at 500um"
  
     varmap = k1^2 * map250.error^2 + k2^2 * map350.error^2 + $
              k3^2 * map500.error^2
  ENDELSE
  
  ;;Apply mask, build estimate of depth and psf weight mask
  IF ((has_250 && (map250.has_mask || map250.has_exposure)) || $
      (has_350 && (map350.has_mask || map350.has_exposure)) || $
      map500.has_exposure || map500.has_mask) THEN BEGIN
     badpix = BYTARR(nx, ny)
     IF has_250 THEN BEGIN
        IF map250.has_mask THEN BEGIN
           wbad = WHERE(map250.mask NE 0, nw)
           IF nw NE 0 THEN badpix[TEMPORARY(wbad)] = 1b
        ENDIF
        IF map250.has_exposure THEN BEGIN
           wbad = WHERE(map250.exposure EQ 0., nw)
           IF nw NE 0 THEN badpix[TEMPORARY(wbad)] = 1b
        ENDIF
     ENDIF
     IF has_350 THEN BEGIN
        IF map350.has_mask THEN BEGIN
           wbad = WHERE(map350.mask NE 0, nw)
           IF nw NE 0 THEN badpix[TEMPORARY(wbad)] = 1b
        ENDIF
        IF map350.has_exposure THEN BEGIN
           wbad = WHERE(map350.exposure EQ 0., nw)
           IF nw NE 0 THEN badpix[TEMPORARY(wbad)] = 1b
        ENDIF
     ENDIF
     IF map500.has_mask THEN BEGIN
        wbad = WHERE(map500.mask NE 0, nw)
        IF nw NE 0 THEN badpix[TEMPORARY(wbad)] = 1b
     ENDIF
     IF map500.has_exposure THEN BEGIN
        wbad = WHERE(map500.exposure EQ 0., nw)
        IF nw NE 0 THEN badpix[TEMPORARY(wbad)] = 1b
     ENDIF

     ;;Sanitize
     wbad = WHERE(TEMPORARY(badpix) NE 0, nbad, $
                  COMPLEMENT=wgood, NCOMPLEMENT=ngood)
     IF ngood EQ 0 THEN MESSAGE, "No good pixels remaining"
     IF nbad EQ 0 THEN BEGIN
        ;; Easy, but perhaps not realistic
        szx = fix(nx * 0.25) 
        szy = fix(ny * 0.25)
        weightmap = 1.0 / TEMPORARY(varmap)
        white_invvar = MEDIAN(weightmap[nx/2-szx:nx/2+szx, ny/2-szy:ny/2+szy])
        psf_wtmap = DBLARR(nx, ny, /NOZERO)
        psf_wtmap[*] = white_invvar
     ENDIF ELSE BEGIN
        ;; Set the pixels in the image to zero
        IF has_250 THEN map250.image[wbad] = 0.0
        IF has_350 THEN map350.image[wbad] = 0.0
        map500.image[wbad] = 0.0

        ;; Set the variance map for bad pixels to an extreme value
        mnwt = 1e-3 / MAX(varmap[TEMPORARY(wgood)])
        weightmap = 1.0 / TEMPORARY(varmap)
        weightmap[wbad] = mnwt
        
        ;; Find variance estimate excluding bad values from center
        szx = fix(nx * 0.25) 
        szy = fix(ny * 0.25)
        wtcen = weightmap[nx/2-szx:nx/2+szx, ny/2-szy:ny/2+szy]
        wgoodcen = WHERE(wtcen GT 2 * mnwt, ngoodcen, NCOMPLEMENT=nbadcen)
        IF ngoodcen EQ 0 THEN MESSAGE, "Can't find central depth"
        IF nbadcen NE 0 THEN wtcen = wtcen[TEMPORARY(wgoodcen)]
        white_invvar = MEDIAN(TEMPORARY(wtcen))

        ;; The PSF weightmap is the same, but with the central part
        ;; where we put the test beam set to white_invvar to minimize
        ;; sampling issues
        psf_wtmap = weightmap
        delta = ROUND(2 * 35.2 / map500.pixscale) ;; 500um beam size
        psf_wtmap[nx/2-delta:nx/2+delta,ny/2-delta:ny/2+delta] = white_invvar
     ENDELSE
  ENDIF ELSE BEGIN
     ;; Have to assume everything is good; same as nbad=0 above
     nbad = 0
     szx = fix(nx * 0.25) 
     szy = fix(ny * 0.25)
     weightmap = 1.0 / TEMPORARY(varmap)
     white_invvar = MEDIAN(weightmap[nx/2-szx:nx/2+szx, ny/2-szy:ny/2+szy])
     psf_wtmap = DBLARR(nx, ny, /NOZERO)
     psf_wtmap[*] = white_invvar
  ENDELSE

  ret250 = ARG_PRESENT(filt250)
  ret350 = ARG_PRESENT(filt350)

  ;; Figure out actual amount of padding
  IF KEYWORD_SET(fixnpad) THEN BEGIN
     npad_x = npad ;; can be zero
     npad_y = npad
  ENDIF ELSE BEGIN
     ;; Try to pad to a nicer size
     npad_x = fft_npad(nx + npad) + npad
     npad_y = fft_npad(ny + npad) + npad
     IF npad_x GT 1000 THEN $
        MESSAGE, STRING(npad_x, FORMAT='("Large amount of x padding: ",I0)'),$
                 /INF
     IF npad_y GT 1000 THEN $
        MESSAGE, STRING(npad_y, FORMAT='("Large amount of y padding: ",I0)'),$
                 /INF
  ENDELSE

  ;; We need to construct the noise power spectrum.  This is based on
  ;; the 500 um band, so we will need the 500um PSF first.
  ;; We will supply some padding in the process
  fft_x = LONG(nx + npad_x) ;; could be zero!
  fft_y = LONG(ny + npad_y)

  IF KEYWORD_SET(verbose) THEN MESSAGE,"Setting up 500um filter", /INF

  ;; We build up information in stuctures (filt250/350/500)
  ;; so that we can optionally return it for simulation work
  filt500 = {nx: nx, ny: ny, fft_x: fft_x, fft_y: fft_y, $
             npad_x: npad_x, npad_y: npad_y,$
             padded_weightmap: DBLARR(fft_x, fft_y),$
             filt_fft: DCOMPLEXARR(fft_x, fft_y),$
             sm_wtmap: DBLARR(fft_x, fft_y)}
  
  ;; We will be noise weighting.  Pad weight arrays out to the right size
  IF npad_x EQ 0 AND npad_y EQ 0 THEN BEGIN
     filt500.padded_weightmap = TEMPORARY(weightmap)
     padded_psf_weightmap = TEMPORARY(psf_wtmap)
  ENDIF ELSE BEGIN
     ;; Having zero weights actually causes ugliness, so set a minimum
     ;; value
     minweight = 0.1 * MIN(weightmap)
     filt500.padded_weightmap = minweight
     padded_psf_weightmap = filt500.padded_weightmap
     px = npad_x / 2
     py = npad_y / 2
     filt500.padded_weightmap[px:px + nx - 1, py:py + ny - 1] = $
        TEMPORARY(weightmap)
     padded_psf_weightmap[px:px + nx - 1, py:py + ny - 1] = TEMPORARY(psf_wtmap)
  ENDELSE
  
  ;; We need two bits -- the instrument noise, and the confusion noise
  ;; Estimate the instrument noise.  We will use the median of the
  ;; central quarter of the map
  ;; This then has to be converted to Fourier space -- which is related to
  ;; the real-space noise in a pixel, and the size of the map
  white_var = 1.0 / white_invvar
  p_noise = fft_x * fft_y * white_var ;; fourier space noise, white

  ;; The confusion noise power spectrum is estimated to have the same shape
  ;; as the PSF (hence ignoring clustering), but normalized to the 
  ;; specified confusion noise.  We can't use a centered beam with
  ;; the appropriate padding here because of the way the filters are
  ;; set up here.  So we will build an approximate one and then
  ;; correct.
  d = DIST(fft_x, fft_y)
  psf500 = exp(-4.0d0 * ALOG(2) * d^2 / fwhm_nom_pix[2]^2)
  psf500 /= MAX(psf500) ;; peak normalized
  IF conf GT 0.0 THEN BEGIN
     scale_confusion = conf / STDEV(psf500)
     psf500_fft = FFT(TEMPORARY(psf500), 1, /DOUBLE)
     p_noise += scale_confusion^2 * ABS(psf500_fft)^2.0
  ENDIF ELSE $
     psf500_fft = FFT(TEMPORARY(psf500), 1, /DOUBLE) 
     
  ;; Now that we have the noise power spectrum, we can proceed
  ;; to build the matched filter in each band.

  ;;Start with 500, since that's the simplest
  filt500.filt_fft = psf500_fft / p_noise  ;; The case Ed considered
  rfilt500 = REAL_PART(FFT(filt500.filt_fft, -1, /DOUBLE)) ;;Real space filter 
  filt500.filt_fft = CONJ(filt500.filt_fft)
  
  ;; We need to deal with normalization effects.  This is hard to work out
  ;; analytically, so we will do it empirically by convolving the
  ;; beam with the filter and making sure we recover the same
  ;; normalization.  Already padded out above to the right size
  filt2_fft = CONJ(FFT(TEMPORARY(rfilt500)^2, 1, /DOUBLE))
  psf500 = matched_filter_red_beam(nx, ny, npad_x, npad_y, $
                                   fwhm_nom[2], pixscale)
  padded_psf_weightmap_fft =  FFT(padded_psf_weightmap, 1, /DOUBLE)
  psf_sm_wtmap = REAL_PART(FFT(padded_psf_weightmap_fft * filt2_fft, -1))
  fpsf500 = REAL_PART(FFT(FFT(TEMPORARY(psf500) * padded_psf_weightmap, $
                              1, /DOUBLE) * $
                          filt500.filt_fft, -1, /DOUBLE)) / $
            psf_sm_wtmap
  ;; The scaling is a little tricky because we want the noise
  ;; properties to come out right.  Also set up the real weight map FFT
  scale = MAX(fpsf500)
  filt500.filt_fft *= scale
  weightmap_fft = FFT(filt500.padded_weightmap, 1, /DOUBLE)
  filt500.sm_wtmap = scale^2 * $
                     REAL_PART(FFT(weightmap_fft * TEMPORARY(filt2_fft), -1))
  ;; Return beam
  IF ARG_PRESENT(beam) THEN BEGIN
     IF npad_x NE 0 OR npad_y NE 0 THEN $
        beam = fpsf500[npad_x/2:npad_x/2+nx-1, npad_y/2:npad_y/2+ny-1] / scale $
     ELSE beam = fpsf500 / scale
  ENDIF
  DELVARX, fpsf500

  ;;Apply to 500 micron map.  
  IF KEYWORD_SET(verbose) THEN MESSAGE, "Smoothing 500um map", /INF

  matched_filter_red_internal, map500, filt500.padded_weightmap,$
                               filt500.sm_wtmap, filt500.filt_fft,$
                               filt500.npad_x, filt500.npad_y

  ;; Now 250; basically the same drill, trying to reuse as much
  ;; as possible
  IF has_250 THEN BEGIN
     IF KEYWORD_SET(verbose) THEN MESSAGE,"Smoothing 250um map", /INF
     filt250 = {nx: nx, ny: ny, fft_x: fft_x, fft_y: fft_y, $
                npad_x: npad_x, npad_y: npad_y,$
                padded_weightmap: filt500.padded_weightmap,$
                filt_fft: DCOMPLEXARR(fft_x, fft_y),$
                sm_wtmap: DBLARR(fft_x, fft_y)}
     psf250 = exp(-4.0d0 * ALOG(2) * d^2 / fwhm_nom_pix[0]^2)
     psf250 /= MAX(psf250)
     psf250_fft = FFT(TEMPORARY(psf250), 1, /DOUBLE)
     filt250.filt_fft = psf500_fft^2 / (psf250_fft * p_noise)
     rfilt250 = REAL_PART(FFT(filt250.filt_fft, -1, /DOUBLE))
     filt250.filt_fft = CONJ(filt250.filt_fft)
     filt2_fft = CONJ(FFT(TEMPORARY(rfilt250)^2, 1, /DOUBLE))
     psf_sm_wtmap = REAL_PART(FFT(padded_psf_weightmap_fft * filt2_fft, -1))
     psf250 = matched_filter_red_beam(nx, ny, npad_x, npad_y, $
                                      fwhm_nom[0], pixscale)
     fpsf250 = REAL_PART(FFT(FFT(TEMPORARY(psf250) * padded_psf_weightmap, $
                                 1, /DOUBLE) * $
                             filt250.filt_fft, -1, /DOUBLE)) / $
               psf_sm_wtmap
     scale = MAX(TEMPORARY(fpsf250))
     filt250.filt_fft *= scale
     filt250.sm_wtmap = scale^2 *$
                        REAL_PART(FFT(weightmap_fft * TEMPORARY(filt2_fft), -1))
     matched_filter_red_internal, map250, filt250.padded_weightmap, $
                                  filt250.sm_wtmap, filt250.filt_fft,$
                                  filt250.npad_x, filt250.npad_y
     IF ~ ret250 THEN DELVARX, filt250
  ENDIF

  ;; Now 350
  IF has_350 THEN BEGIN
     IF KEYWORD_SET(verbose) THEN MESSAGE,"Smoothing 350um map", /INF
     filt350 = {nx: nx, ny: ny, fft_x: fft_x, fft_y: fft_y, $
                npad_x: npad_x, npad_y: npad_y,$
                padded_weightmap: filt500.padded_weightmap,$
                filt_fft: DCOMPLEXARR(fft_x, fft_y),$
                sm_wtmap: DBLARR(fft_x, fft_y)}
     psf350 = exp(-4.0d0 * ALOG(2) * d^2 / fwhm_nom_pix[1]^2)
     psf350 /= MAX(psf350)
     psf350_fft = FFT(TEMPORARY(psf350), 1, /DOUBLE)
     filt350.filt_fft = psf500_fft^2 / (psf350_fft * p_noise)
     rfilt350 = REAL_PART(FFT(filt350.filt_fft, -1, /DOUBLE))
     filt350.filt_fft = CONJ(filt350.filt_fft)
     filt2_fft = CONJ(FFT(TEMPORARY(rfilt350)^2, 1, /DOUBLE))
     psf_sm_wtmap = REAL_PART(FFT(padded_psf_weightmap_fft * filt2_fft, -1))
     psf350 = matched_filter_red_beam(nx, ny, npad_x, npad_y, $
                                      fwhm_nom[1], pixscale)
     fpsf350 = REAL_PART(FFT(FFT(TEMPORARY(psf350) * padded_psf_weightmap, 1, $
                                 /DOUBLE) * $
                             filt350.filt_fft, -1, /DOUBLE)) / $
               psf_sm_wtmap
     scale = MAX(TEMPORARY(fpsf350))
     filt350.filt_fft *= scale
     filt350.sm_wtmap = scale^2 *$
                        REAL_PART(FFT(weightmap_fft * TEMPORARY(filt2_fft), -1))
     matched_filter_red_internal, map350, filt350.padded_weightmap, $
                                  filt350.sm_wtmap, filt350.filt_fft,$
                                  filt350.npad_x, filt350.npad_y
     IF ~ ret350 THEN DELVARX, filt350
  ENDIF ELSE DELVARX, filt350

END
