;------------------------------------------------------------------------------
;+
;NAME
; hipass_matched
;PURPOSE
;  This is a combined matched and (apodized) high-pass filter.
;
; The matched filter is as described in Appendix A in Chapin et
; al. (2011) MNRAS 411 505.
;USAGE
;  hipass_matched, map, fwhm, conf, sig, filtscale
;INPUTS
;   map      = SMAP map structure to filter.  Modified on output
;   fwhm     = FWHM of Gaussian beam in arcsec
;   conf     = confusion RMS noise level in a pixel (same units as map)
;   sigi     = instrument noise value in a pixel for filter (same
;               units as map).  Note that this does not need to be the
;               actual noise, but is just the value used to construct
;               the filter
;   filtscale = High-pass filter scale in arcsec
;OPTIONAL INPUTS
;   qfactor  = High pass filter apodization (def: 0.2)
;   minexp   = Minimum exposure to include.
;   maskbit  = Mask bit to set for excluded pixels (def: 1uL)
;   mask     = Mask to use.
;   noiseval = Input instrument noise estimate.  If provided, ntrials
;               simulated noise maps are created and filtered to
;               produce noiseest.  Can be an array.
;   ntrials  = Number of trials to measure post-filtering instrument noise
;OPTIONAL OUTPUTS
;   noiseest = Estimated noise after filtering
;   noisescat = Mean absolute deviation of noiseest values
;MODIFICATION HISTORY
; Alex Conley, June 2014
;------------------------------------------------------------------------------

FUNCTION hipass_matched_build_psf, nx, ny, fwhm_pix
  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN
  d = DIST(nx, ny)
  psf = exp(-d^2d / (2d * (fwhm_pix / SQRT(8.0*ALOG(2)))^2d))
  psf /= MAX(psf)
  RETURN, psf
END

;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION hipass_matched_build_matched, nx, ny, psf, psf_fft, conf, sigi
  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN

  ;; The white noise level in Fourier space is related to the real-space
  ;; noise in a pixel, and the size of the map
  nsamp = double(nx)*double(ny)
  p_w = nsamp * sigi^2d               ; power spectrum level

  ;; Confusion noise power spectrum is estimated to have the same shape
  ;; as the PSF, but normalized to the specified confusion noise
  p_beam = abs(psf_fft)^2d
  scale_confusion = conf / stdev(psf)
  p_c = scale_confusion^2d * p_beam

  ;; the matched filter is the psf divided by the noise power spectrum in
  ;; Fourier space.

  p_noise = p_w + p_c                    ;; total noise power spectrum
  filt_fft = CONJ(psf_fft / p_noise)     ;; Fourier space matched filter

  ;; Determine normalization empirically by applying to normalized PSF
  ;;  and requiring that peak be preserved
  filt_beam = REAL_PART(FFT(psf_fft * filt_fft, -1))
  filt_fft /= MAX(TEMPORARY(filt_beam))
  
  RETURN, filt_fft
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION hipass_matched_build_hipass, nx, ny, filtscale_pix, qfactor
  nsig = 5.0 ;; Apodization constant

  ;; Setup basic filter
  filt = DBLARR(nx, ny, /NOZERO)  ;; filter is purely real
  filt[*] = 1.0
  filt[0, 0] = 0.0 ;; Mean is always 0 of final filter

  IF filtscale_pix LE 0.0 THEN RETURN, filt  ;; just mean subs

  ;; For sqrt(i^2 + j^2) > set1dist, the filter is unity
  set1dist = SQRT(nx * ny) / filtscale_pix
  d = DIST(nx, ny)  ;; Wrapped distance
  IF qfactor EQ 0 THEN BEGIN
     ;; No apodization -- easy
     wnonunity = WHERE(d LE set1dist, nnounity)
     IF nnounity NE 0 THEN filt[wnonunity] = 0.0
  ENDIF ELSE BEGIN
     ;; More complicated case
     ;;  We will have an inner radius within which the filter
     ;;  is zero, a transition region, and then outside that it will
     ;;  be unity
     qpixfac = 1.0 - qfactor * nsig
     IF qpixfac LE 0.0 THEN set0dist = 0.0 ELSE $
        set0dist = qpixfac * set1dist
     wzero = WHERE(d LE set0dist, nzero)
     IF nzero EQ 0 THEN filt[wzero] = 0.0
     wtransition = WHERE(d GT set0dist AND d LT set1dist, ntransition)
     IF ntransition NE 0 THEN BEGIN
        sigfac = -0.5 * (qfactor * set1dist)^(-2.0)
        filt[wtransition] = EXP(sigfac * (set1dist - d[wtransition])^2)
     ENDIF
  ENDELSE
  RETURN, filt
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION hipass_matched_build_filter, nx, ny, fwhm_pix, conf, sigi, $
                                      filtscale_pix, qfactor
  COMPILE_OPT IDL2, HIDDEN
  psf = hipass_matched_build_psf(nx, ny, fwhm_pix)
  psf_fft = fft(psf, 1)

  ;; Build matched filter (normalized)
  filt_fft = hipass_matched_build_matched(nx, ny, psf, psf_fft, conf, sigi)

  ;; Add highpass
  IF filtscale_pix GT 0.0 THEN $
     filt_fft *= hipass_matched_build_hipass(nx, ny, filtscale_pix,$
                                             qfactor)
  RETURN, filt_fft
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION hipass_matched_apply_filt, image, filt_fft
  COMPILE_OPT IDL2, HIDDEN

  IF ((SIZE(image))[1] NE (SIZE(filt_fft))[1]) OR $
     ((SIZE(image))[2] NE (SIZE(filt_fft))[2]) THEN $
     MESSAGE, "Filter doesn't match image size"

  ;; Assumes all bad pixels already zeroed
  filt_im = FFT(image, 1)
  RETURN, REAL_PART(FFT(TEMPORARY(filt_im) * filt_fft, -1))
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION hipass_matched_padsize, extent
  COMPILE_OPT IDL2, HIDDEN

  ;; Don't bother padding small things
  IF extent LT 100 THEN RETURN, extent

  l2up = 2^CEIL(ALOG(extent) / ALOG(2))
  IF l2up LT extent THEN MESSAGE, "Logic error"
  IF l2up / 2 GT extent THEN newsize = l2up / 2 ELSE newsize = l2up
  IF newsize - extent GT 0.3 * extent THEN BEGIN
     ;; Try power of 3 adjustment
     trialsize = 3L * (newsize / 4)
     IF trialsize GT extent THEN newsize = trialsize
  ENDIF
  RETURN, newsize
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO hipass_matched, map, fwhm, conf, sigi, filtscale, QFACTOR=qfactor,$
                    MINEXP=minexp, MASKBIT=maskbit, NOISEVAL=noiseval,$
                    NTRIALS=ntrials, SEED=seed, NOISEEST=noiseest, $
                    NOISESCAT=noisescat, VERBOSE=verbose, MASKIM=maskim
                    

  COMPILE_OPT IDL2

  ;; Defaults and input checks
  IF SIZE(map, /TNAME) NE 'STRUCT' THEN $
     MESSAGE, "Input map not as expected"
  IF fwhm LE 0.0 THEN MESSAGE, "Invalid (non-positive) FWHM"
  IF conf LE 0.0 THEN MESSAGE, "Invalid (non-positive) confusion noise"
  IF sigi LE 0.0 THEN MESSAGE, "Invalid (non-positive) instrument noise"
  IF filtscale LT 0.0 THEN $
     MESSAGE, "Invalid (negative) highpass filter scale"
  IF N_ELEMENTS(qfactor) EQ 0 THEN qfactor=0.2
  IF qfactor LT 0.0 THEN MESSAGE, "Invalid (negative) qfactor"
  IF N_ELEMENTS(minexp) EQ 0 THEN minexp=0.0
  IF minexp LT 0.0 THEN MESSAGE, "Invalid (negative) minexp"
  IF N_ELEMENTS(maskbit) EQ 0 THEN maskbit=1uL ELSE maskbit=ULONG(maskbit)
  IF N_ELEMENTS(ntrials) EQ 0 THEN ntrials=41
  IF N_ELEMENTS(noiseval) NE 0 THEN BEGIN
     szn = SIZE(noiseval)
     IF szn[0] EQ 0 THEN BEGIN
        scalar_noise = 1b
        noise_arr = [noiseval]
     ENDIF ELSE BEGIN
        IF szn[0] NE 1 THEN MESSAGE, "Input noiseval is not 1D"
        scalar_noise = 0b
        noise_arr = noiseval
     ENDELSE
     IF MIN(noise_arr) LT 0.0 THEN MESSAGE, "Invalid (negative) noiseval"
     IF ntrials GT 0 THEN do_noisesim=1b ELSE do_noisesim=0b
  ENDIF ELSE do_noisesim = 0b

  ;; Padding
  xpadsize = hipass_matched_padsize(map.xsize)
  ypadsize = hipass_matched_padsize(map.ysize)

  ;; Get filter
  IF KEYWORD_SET(verbose) THEN $
     MESSAGE, "Building filter", /INF
  filt_fft = hipass_matched_build_filter(xpadsize, ypadsize,$
                                         fwhm / map.pixscale, $
                                         conf, sigi,$
                                         filtscale / map.pixscale, $
                                         qfactor)

  ;; There are two types of masking going on here:
  ;;   1) A potenital user specified mask (in mask)
  ;;   2) exposure/NaN masking
  ;; In both cases the affected pixels are dealt with by
  ;;  setting the pixels to zero before applying the filter,
  ;;  and then setting the mask bit in the modified map
  ;;  if present
  im = map.image ;; Note -- not yet padded!
  IF N_ELEMENTS(maskim) NE 0 THEN BEGIN
     ;; Check user mask
     szmask = SIZE(maskim)
     IF szmask[0] NE 2 THEN $
        MESSAGE, "User mask not 2D"
     IF szmask[1] NE map.xsize THEN $
        MESSAGE, "User mask x size doesn't match image"
     IF szmask[2] NE map.ysize THEN $
        MESSAGE, "User mask y size doesn't match image"
     IF ~ map.has_mask THEN $
        MESSAGE, "Input map must have mask extension if user masking is applied"
     IF map.has_exposure THEN BEGIN
        wmask = WHERE(~FINITE(im) OR map.exposure LE minexp OR $
                     maskim NE 0, nmask)
     ENDIF ELSE BEGIN
        wmask = WHERE(~FINITE(im) OR maskim NE 0, nmask)
     ENDELSE
  ENDIF ELSE BEGIN
     IF map.has_exposure THEN BEGIN
        IF ~ map.has_mask THEN $
           MESSAGE, "Input map must have mask extension if exposure time masking is applied"
        wmask = WHERE(~FINITE(im) OR map.exposure LE minexp, nmask)
     ENDIF ELSE BEGIN
        wmask = WHERE(~FINITE(im), nmask)
     ENDELSE
  ENDELSE
  IF nmask NE 0 THEN im[wmask] = 0.0

  ;; Now pad
  IF xpadsize GT map.xsize OR ypadsize GT map.ysize THEN BEGIN
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE, STRING(map.xsize, map.ysize, xpadsize, ypadsize,$
                        FORMAT='("  Padding from ",I0,"x",I0," to ",I0,"x",I0)'),$
                 /INF
     newim = FLTARR(xpadsize, ypadsize)
     newim[0:map.xsize-1, 0:map.ysize-1] = TEMPORARY(im)
     im = TEMPORARY(newim)
  ENDIF

  ;; Apply
  IF KEYWORD_SET(verbose) THEN $
     MESSAGE, "Filtering map", /INF
  filt_image = hipass_matched_apply_filt(im, filt_fft)

  ;; Depad
  map.image = filt_image[0:map.xsize-1, 0:map.ysize-1]
  
  ;; Set mask
  IF nmask NE 0 AND map.has_mask THEN map.mask[wmask] OR= maskbit

  ;; Estimate noise if requested
  IF do_noisesim THEN BEGIN
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE, "Estimating noise", /INF
     nnoise = N_ELEMENTS(noise_arr)
     noiseest = FLTARR(nnoise)
     noisescat = FLTARR(nnoise)
     vararr = DBLARR(ntrials)
     FOR i=0, nnoise-1 DO BEGIN
        IF noise_arr[i] EQ 0.0 THEN BEGIN
           noiseest[i] = 0.0
           noisescat[i] = 0.0
        ENDIF ELSE BEGIN
           ;; wmask applies to the non-padded image, so we have
           ;; to convert the indices.  Recall that 
           ;;  x = index mod xsize  y = index / xsize (integer)
           xy_wmask = ARRAY_INDICES(maskim, wmask)
           wmask_pad = xy_wmask[1,*] * xpadsize + xy_wmask[0, *]
           FOR j=0, ntrials-1 DO BEGIN
              nsim = noise_arr[i] * RANDOMN(seed, xpadsize, ypadsize)
              IF nmask NE 0 THEN nsim[wmask_pad] = 0.0
              nsim = hipass_matched_apply_filt(nsim, filt_fft)
              IF nmask NE 0 THEN nsim[wmask_pad] = !VALUES.F_NAN
              vararr[j] = VARIANCE(TEMPORARY(nsim), /NAN)
           ENDFOR
           vararr = SQRT(vararr) ;; to stdev
           noiseest[i] = MEDIAN(vararr)
           noisescat[i] = MEANABSDEV(vararr)
        ENDELSE
     ENDFOR
     IF scalar_noise THEN BEGIN
        noiseest = noiseest[0]
        noisescat = noisescat[0]
     ENDIF
        
  ENDIF

END
