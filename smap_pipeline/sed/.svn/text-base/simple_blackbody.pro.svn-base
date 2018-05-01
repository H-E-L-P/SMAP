;+
;NAME
; simple_blackbody
;PURPOSE
; Fits a simple blackbody to user input data
;USAGE
; extrap_flux = simple_blackbody(wave, flux, extrapwave)
;INPUTS
; wave          Wavelength (in microns)
; flux          Flux (arb units)
; extrapwave    Extrapolation wavelength desired (in microns)
;KEYWORDS
; greybody      Rather than a blackbody, use a greybody fit
;OPTIONAL INPUTS
; dflux         Flux error
; beta          If using greybody, the assumed beta value
; redshift      Redshift.  This only matters if fitting a greybody (def: 2)
;OPTIONAL OUTPUTS
; values          Values of params [norm,T]
; perrors         Errors in values
;NOTES
; The greybody fit (if chosen) assumed lambda_0 = c/nu_0 = 100 microns
; rest frame.
;MODIFICATION HISTORY
; Author: Alex Conley, Sep 16, 2010
;-

FUNCTION simple_blackbody_flux, wave, norm, T, REDSHIFT=redshift,$
                                GREYBODY=greybody, BETA=beta
  COMPILE_OPT IDL2

  IF N_ELEMENTS(redshift) EQ 0 THEN opz=3.0 ELSE opz=1.0+redshift

  c = 299792458e6 ;;In microns
  h = 6.6260693e-34 ;;J/s
  k = 1.3806505e-23 ;;J/K
  hok = h / k
  nu = c / DOUBLE(wave) ;;In Hz
  Inu = nu^3 / (EXP( hok*nu / T ) - 1.0)


  ;;norm is flux at 250
  nu_norm = c / 250.0d0
  Inu /= nu_norm^3 /( EXP( hok*nu_norm / T ) - 1.0)
  Inu *= norm

  IF KEYWORD_SET( greybody ) THEN BEGIN
     nu0 = c / (opz*100.0) ;;100 microns
     tau = (nu/nu0)^beta
     Inu *= (1.0 - EXP(-tau))
  ENDIF

  RETURN,Inu
END

FUNCTION simple_blackbody_fitfun, p, WAVE=wave, FLUX=flux, GREYBODY=greybody,$
                                  REDSHIFT=redshift,DFLUX=dflux, BETA=beta
  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN

  IF N_ELEMENTS(dflux) EQ 0 THEN dflux=REPLICATE(1.0,N_ELEMENTS(flux))

  model_flux = simple_blackbody_flux(wave,p[0],p[1],GREYBODY=greybody,$
                                     REDSHIFT=redshift, BETA=beta)

  diffvec = flux -model_flux
  
  RETURN,diffvec/dflux
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION simple_blackbody, wave, flux, extrapwave, INIT_T=init_T,$
                           DFLUX=dflux,VALUES=values,GREYBODY=greybody,$
                           REDSHIFT=redshift, BETA=beta

  COMPILE_OPT IDL2, STRICTARRSUBS

  IF N_ELEMENTS(init_T) EQ 0 THEN init_T = 30
  IF N_ELEMENTS(wave) NE N_ELEMENTS(flux) THEN MESSAGE,"Flux/wave mismatch"
  IF N_ELEMENTS(beta) EQ 0 THEN beta=1.6
  IF N_ELEMENTS(dflux) NE 0 THEN BEGIN
     IF N_ELEMENTS(dflux) EQ N_ELEMENTS(flux) THEN dflux_used=dflux ELSE $
        dflux_used = REPLICATE(dflux[0],N_ELEMENTS(flux))
  ENDIF ELSE dflux_used = REPLICATE(1.0,N_ELEMENTS(flux))

  ;;Set up extra stuff for fit: limits, optional vars, etc.
  IF KEYWORD_SET(greybody) THEN gb=1b ELSE gb=0b
  extra_info = { wave: wave, flux: flux, greybody: gb, dflux: dflux_used,$
                 redshift: 2.0, beta: beta }
  IF N_ELEMENTS(redshift) NE 0 THEN extra_info.redshift = redshift
  param_info=REPLICATE( {parname:'', fixed:0, limited:[0,0], $
                         limits:[0.d0,0.d0], step: 0.0 }, 2)
  param_info.parname = ['norm','temp']
  param_info[0].limited[0] = 1
  param_info[0].limits[0]  = 0.0
  param_info[1].limited[0] = 1
  param_info[1].limits[0] = 0.0

  init_norm = MAX( flux / simple_blackbody_flux( wave, 1.0, init_T ) )

  start_values = [init_norm, init_t]
  values = MPFIT( 'simple_blackbody_fitfun', start_values, $
                  FUNCTARGS=extra_info, MAXITER=100,$
                  ERRMSG=errmsg, STATUS=status,$
                  COVAR=covar, PERROR=perror, BESTNORM=chisq,$
                  PARINFO=param_info,/QUIET )
  IF status LE 0 THEN $
     MESSAGE,"Error performing fit: " + errmsg
  IF status EQ 5 THEN $
     PRINT,"Maximum number of iterations exceeded in simple_blackbody"

  FORPRINT,wave,flux,simple_blackbody_flux(wave,values[0],values[1],$
                                           GREYBODY=greybody, $
                                           REDSHIFT=redshift, BETA=beta),$
           FORMAT='(F5.1,2X,F5.1,2X,F5.1)'
  PRINT,"Temperature/(1+z): ",values[1]

  RETURN,simple_blackbody_flux(extrapwave,values[0],values[1],$
                               GREYBODY=greybody,$
                               REDSHIFT=redshift, BETA=beta)

END
