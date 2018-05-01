;+
;NAME
; broken_pd
;PURPOSE
; Calculate P(D) of a multiply broken linear model
;USAGE
; pd = broken_pd( fluxes, fluxknots, fluxvals )
;INPUTS
; fluxes       Fluxes you want P(D) for.
; fluxknots    Knots the model is defined at.  The number counts
;               are assumed zero outside this.  These should be in
;               ascending order.
; fluxvals     The log10 values at fluxknots
;OPTIONAL INPUTS
; sigma        Noise level (def: 0)
; theta        Beam FWHM in arcsec (def: 18.6).
; psf          PSF as image array.  You probably want to normalize
;               this to have flux 1 at the center
; pixscale     The size of the pixel in arcsec if psf is used
;NOTES
; This assumes a strictly positive beam, and will quietly give
; you nonsense if you pass it one with negative parts
;MODIFICATION HISTORY
; Author: Alex Conley, Dec 12, 2009
;-

FUNCTION broken_pd, fluxes, fluxknots, fluxvals, $
                    SIGMA=sigma, FWHM=fwhm, PSF=psf,$
                    PIXSCALE=pixscale, INTERPOL=interpol,$
                    NINTERPOL=ninterpol
  COMPILE_OPT IDL2, STRICTARRSUBS 

  nflux = N_ELEMENTS(fluxes)
  IF N_ELEMENTS(psf) EQ 0 THEN BEGIN
     R = broken_r_gauss( fluxes, fluxknots, fluxvals, THETA=fwhm )
  ENDIF ELSE BEGIN
     IF N_ELEMENTS(pixscale) EQ 0 THEN $
        MESSAGE,"Must provide pixscale if providing arbitrary PSF"
     R = broken_r( fluxes, fluxknots, fluxvals, psf, pixscale,$
                   INTERPOL=interpol, NINTERPOL=ninterpol )
  ENDELSE

  ;;Multiply by dflux
  dflux = mean( fluxes[1:*] - fluxes[0:nflux-2] )
  R *= dflux

  wpos = WHERE( fluxes GT 0.0, npos )
  IF npos EQ 0 THEN MESSAGE,"Need positive fluxes!"
  Rpos = R[wpos]

  fftR = FFT( Rpos, +1, /DOUBLE )
  
  ;;Calculate p(omega) = exp( fftR(omega)-fftR(0) )
  p = DCOMPLEXARR( N_ELEMENTS(fftR), /NOZERO )
  r0 = REAL_PART(fftR[0])
  p[0] = complex(1.0,0.0) ;;exp(0)
  IF N_ELEMENTS( fftR ) MOD 2 EQ 0 THEN BEGIN
     n = N_ELEMENTS(fftR)
     efac = EXP( REAL_PART( fftR[1:n/2-1] - r0 ) )
     p[1:n/2-1] = efac * DCOMPLEX( COS( IMAGINARY(fftR[1:n/2-1]) ),$
                                   SIN( IMAGINARY(fftR[1:n/2-1]) ) )
     p[n/2] = DCOMPLEX(EXP( REAL_PART(fftR[n/2])-r0 ), 0.0 )
     p[n/2+1:*] = CONJ( REVERSE( p[1:n/2-1] ) )
  ENDIF ELSE BEGIN
     efac = EXP( REAL_PART( fftR[1:*] - r0 ) )
     p[1:*] = efac * DCOMPLEX( COS( IMAGINARY(fftR[1:*]) ),$
                               SIN( IMAGINARY(fftR[1:*]) ) )
  ENDELSE

  ;;Inverse transform
  pd = DBLARR( N_ELEMENTS( fluxes ) )
  pd[wpos] = REAL_PART(FFT( p, -1, /DOUBLE ))

  ;;Add noise
  IF N_ELEMENTS( sigma ) NE 0 && sigma GT 0.0 THEN BEGIN
     ;;Convolve, going out to 6 sigma
     klen = CEIL(2*6.0*sigma/dflux)
     IF N_ELEMENTS( fluxes ) LT klen THEN MESSAGE,"Can't convolve"
     cpoint = FLOOR(klen/2)
     kernel = EXP(-0.5*dflux^2*(FINDGEN(klen)-cpoint)^2/(sigma^2))/$
              SQRT(2*!PI*sigma*sigma) 
     pd = CONVOL(pd, kernel, /EDGE_ZERO, /NORMALIZE)
  ENDIF

  RETURN,pd

END
