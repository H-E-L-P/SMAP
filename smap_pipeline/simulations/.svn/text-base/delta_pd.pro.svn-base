;+
;NAME
; delta_pd
;PURPOSE
; Calculate P(D) of a delta function number counts model
;USAGE
; pd = delta_pd( maxflux, nflux, n0, s0, fwhm )
;INPUTS
; maxflux      Target maximum flux for output (may not be exactly
;               realized)
; nflux        Number of fluxes desired.  Must be a power of 2
; n0           Number of objects per unit area
; s0           Flux of the delta function (all objects have this flux)
; fwhm         FWHM of the beam, in arcsec
;RETURNS
; A structure holding the P(D), with a mean of zero
;OPTIONAL INPUTS
; sigma        Sigma value of noise to add
;MODIFICATION HISTORY
; Author: Alex Conley, Jan 5, 2012
;-

FUNCTION delta_pd, maxflux, nflux, n0, s0, fwhm, SIGMA=sigma,$
                    VERBOSE=verbose
  COMPILE_OPT IDL2, STRICTARRSUBS 

  IF nflux EQ 0 THEN MESSAGE,"No fluxes"
  IF n0 LE 0 THEN MESSAGE,"Invalid number of objects"
  IF s0 LE 0 THEN MESSAGE,"Invalid s0 (non-positive)"
  IF maxflux LE 0 THEN MESSAGE,"Invalid maximum flux (non-positive)"
  IF fwhm LE 0 THEN MESSAGE,"Invalid FWHM"
  IF ( nflux AND (nflux-1) ) NE 0 THEN MESSAGE,"nflux is not a power of 2"

  ;;return structure
  pd = { fluxes: DBLARR(nflux), prob: DBLARR(nflux) }

  prefac = !PI * n0 / ( 7200.0d0*7200.0*alog(2) ) * fwhm * fwhm

  ;;detemine expected mean and var, will be used for shifting
  mn = prefac*s0
  var = 0.5*prefac*s0^2
  IF N_ELEMENTS(sigma) NE 0 && sigma GT 0.0 THEN var += sigma*sigma

  ;;Target mean after shifting
  npad = 10.0
  targ_mean = npad * SQRT(var)
  maxflux_gen = maxflux + targ_mean
  minflux_gen = 0

  ;;compute R, which is simple and analytic
  R = DBLARR(nflux)
  pd.fluxes = (maxflux_gen-minflux_gen)*DINDGEN(nflux)/(nflux-1) + $
              minflux_gen
  wvalid = WHERE( pd.fluxes GT 0.0 AND pd.fluxes LT s0, nvalid )
  IF nvalid GT 0 THEN R[wvalid] = prefac/pd.fluxes[wvalid] ELSE $
     MESSAGE,"No non-zero R values"
  
  ;;Multiply by dflux to get normalization right
  dflux = mean( pd.fluxes[1:*] - pd.fluxes[0:nflux-2] )
  R *= dflux

  fftR = FFT( R, +1, /DOUBLE )
  
  ;;Calculate p(omega) = exp( fftR(omega)-fftR(0) )
  ;; Add shift and noise as well
  p = DCOMPLEXARR( nflux, /NOZERO )
  r0 = REAL_PART(fftR[0])
  iflux = 2*!PI/(nflux*dflux)
  ncplx = nflux/2 + 1
  w1 = iflux*[DINDGEN(ncplx-1)+1,-(nflux-DINDGEN(ncplx-2)-ncplx)]
  add_shift = targ_mean - mn
  efac = fftR[1:*] - r0
  ;;Note the signs on these are not what you might expect
  ;; because of the sign we use in the two FFTs
  IF N_ELEMENTS( sigma ) NE 0 && sigma GT 0.0 THEN BEGIN
     ;;shift and noise
     efac += COMPLEX( -0.5*sigma*sigma*w1^2, add_shift*w1 )
  ENDIF ELSE efac += COMPLEX(0,add_shift*w1) ;;shift only
  p[0] = COMPLEX(1.0,0.0) ;;exp(0)
  p[1:*] = EXP( efac )

  ;;Inverse transform
  pd.prob = REAL_PART(FFT( p, -1, /DOUBLE ))
  ;;enforce positivity
  pd.prob >= 0.0
  ;;normalize
  pd.prob /= TOTAL(pd.prob)

  ;;unshift
  real_mn = TOTAL(pd.fluxes*pd.prob)
  pd.fluxes -= real_mn
 
  IF KEYWORD_SET(verbose) THEN BEGIN
     MESSAGE,STRING(targ_mean,real_mn,$
                    FORMAT='("Expected mean: ",F0.3," realized: ",F0.3)'),/INF
     real_var = TOTAL(pd.fluxes^2*pd.prob)
     MESSAGE,STRING(var,real_var,$
                    FORMAT='("Expected var: ",E0.3," realized: ",E0.3)'),/INF
  ENDIF

  RETURN,pd

END
