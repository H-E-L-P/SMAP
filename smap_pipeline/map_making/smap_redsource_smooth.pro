
;+
;NAME
; smap_redsource_smooth
;PURPOSE
; Smooth images to the same resolution, optimized for finding
; 500 micron red sources in the absence of confusion
;USAGE
; smap_redsource_smooth, map
;REQUIRED ARGUMENTS
; map         SMAP map structure, modified on output
;OPTIONAL ARGUMENTS
; input_fwhm  FWHM (in ") of input map.  Otherwise based on band
;KEYWORDS
; bruteforce  Use the brute force convolution method instead of
;              the FFT.  Does better in the case of missing
;              data, but is slooow.
; expzero     Set all locations in the output map where there were no
;              input hits to zero.
; verbose     Output informational messages
; checkvoids  Check for large bad coverage regions and zero them.
; nomeansub   Don't mean subtract the output maps
; addfiras    Add in the FIRAS CFIRB estimate before smoothing.
; noerrorsmooth Don't do error smoothing
; noexpsmooth Don't smooth exposure map
; nopeakcorr  Don't apply Neptune map empirical scaling correction
;NOTES
; Based on Viktoria Asboth's code.  The final target resolution
; for all input maps is ~50" (sqrt(2)*PLW resolution).  Gaps are
; smoothed over.  This version doesn't use the errmap for any weighting
;MODIFICATION HISTORY
; Author: Alex Conley, Aug 5, 2011
;-

PRO smap_redsource_smooth, map, INPUT_FWHM=input_fwhm, $
                           BRUTEFORCE=bruteforce, EXPZERO=expzero,$
                           VERBOSE=verbose, CHECKVOIDS=checkvoids,$
                           NOMEANSUB=nomeansub, ADDFIRAS=addfiras,$
                           NOERRORSMOOTH=noerrorsmooth,$
                           NOEXPSMOOTH=noexpsmooth, $
                           NOPEAKCORR=nopeakcorr
  COMPILE_OPT IDL2, STRICTARRSUBS

  IF N_ELEMENTS(map) EQ 0 THEN MESSAGE,"No map provided"
  IF SIZE(map,/TNAME) NE 'STRUCT' THEN $
     MESSAGE,"Input map is not what was expected"

  ;;Updated from SPIRE obs manual May 22, 2012
  beamsizes = [17.6, 23.9, 35.2]
  targsize  = beamsizes[2]*SQRT(2)
  peakcorr  = [1.0,1.0,1.0] ;;measured off of Neptune beam

  ;;figure out FWHM of input map
  IF N_ELEMENTS(input_fwhm) EQ 0 THEN BEGIN
     CASE map.names OF
        'PSW' : fwhmidx=0
        'PMW' : fwhmidx=1
        'PLW' : fwhmidx=2
        ELSE : MESSAGE,"Unexpected band name: "+map.names
     ENDCASE
     fwhm = beamsizes[fwhmidx]
     use_peakcorr = ~ KEYWORD_SET(nopeakcorr)
  ENDIF ELSE BEGIN
     IF input_fwhm GE targsize THEN $
        MESSAGE,"Input FWHM larger than final target"
     fwhm = input_fwhm
     use_peakcorr = 0b
  ENDELSE  

  ;;Load in appropriate peakcorr
  IF use_peakcorr THEN BEGIN
     ;;Computed from Neptune beam map
     basescale = [1.2, 1.66667, 2.4] ;;what neptune map was made at
     CASE map.names OF 
        'PSW' : BEGIN
           scale = basescale[0]*[1,2,3,4,5,6,7,8,9,10,13]
           peakval = [1.072578,1.0856497,1.0999588,1.0978775,$
                      1.1040182,1.112519,1.1244379,1.2465727,1.3912608,$
                      1.2749821,1.2445967]
        END
        'PMW' : BEGIN
           scale = basescale[1]*[1,2,3,4,5,6,7,8,9,10,13]
           peakval = [1.0121815,1.0424973,1.0552532,1.0544891,1.0721968,$
                      1.0937374,1.1229117,1.1639647,1.2707735,1.2651327,$
                      1.1958083]
        END
        'PLW' : BEGIN
           scale = basescale[2]*[1,2,3,4,5,6,7]
           peakval = [1.0194082,1.0225868,1.0280938,1.0460136,1.0415816,$
                      1.0560630,1.0286558]
        END
        ELSE : MESSAGE,"Unexpected band name: "+map.names
     ENDCASE
     ;;And.. interpolate
     minscale = MIN(scale,MAX=maxscale)
     IF map.pixscale LT minscale THEN BEGIN
        IF KEYWORD_SET(verbose) THEN $
           MESSAGE,"Your pixel scale is smaller than a neptune correction has been calculated for; proceeding anyways",/INF
        peakcorr = peakval[0]
     ENDIF ELSE IF map.pixscale GT maxscale THEN BEGIN
        IF KEYWORD_SET(verbose) THEN $
           MESSAGE,"Your pixel scale is larger than a neptune correction has been calculated for; proceeding anyways",/INF
        peakcorr = peakval[N_ELEMENTS(peakval)-1]
     ENDIF ELSE BEGIN
        ;;interpolate
        peakcorr = INTERPOL( peakval, scale, map.pixscale )
     ENDELSE
     peakcorr = 1.0/peakcorr ;;this is what we apply to the normalization
  ENDIF

  ;;And how much to convolve by
  additional_convol = SQRT(targsize^2 - fwhm^2)
  IF additional_convol LT 2*map.pixscale THEN $
     MESSAGE,"Convolution too fine grained given map pixel scale"
  
  IF KEYWORD_SET(verbose) THEN BEGIN
     fmt='("Additional convolution: ",F0.3," arcsec to achieve "'+$
         ',F0.3," from: ",F0.3)'
     MESSAGE,STRING(additional_convol,targsize,fwhm,FORMAT=fmt),/INF
  ENDIF

  ;;Add firas if requested
  IF KEYWORD_SET( addfiras ) THEN $
     map.image += get_firas_flux( map.names )

  ;;Build convolution kernel
  kernel2d = GET_SPIRE_BEAM(map.names,map.pixscale,FWHM=additional_convol,$
                            /SILENT)
  IF KEYWORD_SET( bruteforce ) THEN BEGIN
     kernel1d = GET_SPIRE_BEAM(map.names,map.pixscale,FWHM=additional_convol,$
                               /SILENT,/FACTOR)
     ;;We need to also compute the normalization factor
     ;;Better to do this with the discrete beam products
     npix = N_ELEMENTS(kernel1d)
     kernelbase = GET_SPIRE_BEAM(map.names,map.pixscale,npix,FWHM=fwhm,$
                                 /SILENT,/FACTOR)
     kernelbase /= MAX(kernelbase)
  ENDIF
     

  ;;Now, do the actual smoothing.  The complication is that
  ;; we don't expect the maps to have hits everywhere, so we
  ;; have the convolution with missing data problem.  There
  ;; are two solutions:
  ;;  1) Use the brute force CONVOLVE_FACTOR method (since we are
  ;;      assuming a Gaussian kernel)
  ;;  2) Use the FFT method but set the missing pixels to zero
  ;;The first seems to be both faster and deal with missing data
  ;; better, so is recommended.
  ;;Normalization is a bit tricky here.  We want to keep the peak
  ;; pixel of a isolated point source constant (i.e., per-beam
  ;; normalization).  We therefore rely on the Gaussian assumption,
  ;; and that we think we know the beam size before.
  wnonfin = WHERE(~ FINITE( map.image ), nnonfin)
  IF KEYWORD_SET( bruteforce ) THEN BEGIN
     IF KEYWORD_SET(verbose) THEN MESSAGE,"Doing convolution",/INF
     normfac = TOTAL( kernel1d )^2 / TOTAL( kernel1d * kernelbase )^2
     IF use_peakcorr THEN normfac *= peakcorr
     map.image = normfac * CONVOLVE_FACTOR( map.image, kernel1d, /NORM, /NAN )
     IF ~ KEYWORD_SET(noexpsmooth) AND map.has_exposure THEN $
        map.exposure = normfac * CONVOLVE_FACTOR(map.exposure, kernel1d,$
                                                 /NORM,/NAN)

     ;;Deal with noise rescaling
     IF ~ KEYWORD_SET(noerrorsmooth) AND map.has_error THEN BEGIN
        t_normfac = targsize^2 / (TOTAL(kernel2d) * fwhm^2)
        varmap = map.error^2
        IF use_peakcorr THEN t_normfac *= peakcorr
        IF nnonfin NE 0 THEN varmap[wnonfin] = 0.0
        varmap = CONVOLVE_FACTOR( varmap, kernel1d^2, /NAN )
        map.error = SQRT( TEMPORARY(varmap) ) * t_normfac
     ENDIF
  ENDIF ELSE BEGIN
     ;;FFT method
     ;;Must beware of non-finite parts of the image
     normfac = targsize^2/ (TOTAL(kernel2d) * fwhm^2)
     IF use_peakcorr THEN normfac *= peakcorr
     IF nnonfin NE 0 THEN map.image[wnonfin] = 0.0
     IF KEYWORD_SET(verbose) THEN MESSAGE,"Doing convolution",/INF
     map.image = CONVOLVE( map.image, kernel2d, /CORREL ) * normfac
     IF ~ KEYWORD_SET(noexpsmooth) AND map.has_exposure THEN $
        map.exposure = CONVOLVE(map.exposure, kernel2d, /CORREL) * normfac

     IF ~ KEYWORD_SET(noerrorsmooth) AND map.has_error THEN BEGIN
        varmap = map.error^2
        IF nnonfin NE 0 THEN varmap[wnonfin] = 0.0
        varmap = CONVOLVE(varmap, kernel2d^2, /CORREL)
        map.error = SQRT(TEMPORARY(varmap)) * normfac
     ENDIF

  ENDELSE

  ;;Fix for bad coverage regions
  IF KEYWORD_SET( checkvoids ) && nnonfin NE 0 THEN BEGIN
     voidmap = REPLICATE(1.0, map.xsize, map.ysize )
     voidmap[wnonfin] = 0.0
     t_normfac = targsize^2/ (TOTAL(kernel2d) * fwhm^2)
     voidmap = CONVOLVE(voidmap, kernel2d, /CORREL) * t_normfac
     wbad = WHERE(TEMPORARY(voidmap) LT 1.0, nbad)
     IF nbad EQ 0 THEN map.image[wbad] = !VALUES.D_NAN
  ENDIF

  ;;Use exposure calculation to nuke edges if available
  IF KEYWORD_SET(expzero) && map.has_exposure THEN BEGIN
     wnoexp = WHERE( map.exposure EQ 0, nnoexp )
     IF nnoexp NE 0 THEN map.image[wnoexp] = !VALUES.D_NAN
  ENDIF

  IF ~ KEYWORD_SET( nomeansub ) THEN $
     map.image -= MEAN( map.image, /NAN )

END
