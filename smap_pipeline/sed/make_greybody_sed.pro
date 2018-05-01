;+
;NAME
; make_greybody_sed
;USAGE
; fluxes = make_greybody_sed(wavelengths, [T=, BETA=, LAMBDA0=])
;INPUTS
; wavelengths    Desired wavelength points, in microns
;OPTIONAL INPUTS
; T              Temperature in K (def: 40)
; beta           Greybody slope parameter (def: 1.6)
; lambda0        Greybody wavelength parameter (def: 100 microns)
; f500           Normalize flux density at 500 microns to this value
; lambda_fix     Use this wavelength instead of 500 microns in normalization
;KEYWORDS
; SIMPLE         Use simplified functional form with
;                  (1-exp(-(lambda0/lambda)^beta) -> (lambda0/lambda)^beta
;RETURNS
; Greybody intensity at wavelengths in F_nu units (not F_lambda!)
; using the formula:
;   I_nu = ( 1 - exp( -(lambda0/lambda)^beta ) B_nu( T )
;NOTES
; If you want to consider redshift, then you can just consider
; the T parameter to be T/(1+z) and lambda0 to be lambda0*(1+z).
;MODIFICATION HISTORY
; Author: Alex Conley, Jun 2009
;-

FUNCTION make_greybody_sed_simplealpha, x
  COMPILE_OPT HIDDEN
  COMMON mgss_comm, ab, bt, gam
  RETURN, x - (1-EXP(-x))*(3.0 + ab + bt)
END


FUNCTION make_greybody_sed_nonsimplealpha, x
  COMPILE_OPT HIDDEN
  COMMON mgss_comm, ab, bt, gam
  bterm = gam^bt*x^bt/(EXP( (gam*X)^bt ) - 1.0)
  RETURN, x - (1-EXP(-x))*(3.0 + ab + bt*bterm)
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION make_greybody_sed, wavelengths, T=t, BETA=beta, LAMBDA0=lambda0,$
                            ALPHA=alpha, SIMPLE=simple, F500=f500,$
                            LAMBDA_FIX=lambda_fix
  COMMON mgss_comm, ab, bt, gam
  COMPILE_OPT IDL2

  c = 299792458d6 ;;In microns
  h = 6.6260693d-34 ;;J/s
  k = 1.3806505d-23 ;;J/K
  hok = h / k

  IF N_ELEMENTS(t) EQ 0 THEN t=40.0d0
  IF N_ELEMENTS(beta) EQ 0 THEN beta=1.6d0
  IF N_ELEMENTS(lambda0) EQ 0 THEN lambda0 = 100.0
  IF N_ELEMENTS( lambda_fix ) EQ 0 THEN lambda_fix = 500.0d0

  nu = c / DOUBLE(wavelengths) ;;In Hz
  nu0 = c / lambda0 ;;Also Hz

  IF KEYWORD_SET( alpha ) && FINITE( alpha ) THEN $
     has_alpha = 1b ELSE has_alpha=0b

  IF has_alpha THEN BEGIN
     ;;Must merge on nu^-alpha at high frequency
     ;; Do this by numerically finding the desired slope
     ab = alpha
     bt = beta
     IF KEYWORD_SET( simple ) THEN BEGIN
        xval = SMAP_FX_ROOT([2.8, 5.0, 15.0],$
                            'make_greybody_sed_simplealpha',/DOUB,$
                            SUCCESS=rtsucc)
        IF rtsucc EQ 0 THEN BEGIN
           fmt = '("Alpha merge failed with T: ",F0.3," beta: ",F0.3,'+$
                 '" lambda_fix: ",F0.2)'
           MESSAGE, STRING(t,beta,lambda_fix,FORMAT=fmt)
        ENDIF
     ENDIF ELSE BEGIN
        gam = T/(hok*nu0)
        xval = SMAP_FX_ROOT([2.8, 3.0, 12.0],$
                            'make_greybody_sed_nonsimplealpha',/DOUB,$
                            SUCCESS=rtsucc)
        IF rtsucc EQ 0 THEN BEGIN
           fmt = '("Alpha merge failed with T: ",F0.3," beta: ",F0.3,'+$
                 '" lambda0: ",F0.2," lambda_fix: ",F0.2)'
           MESSAGE, STRING(t,beta,lambda0,lambda_fix,FORMAT=fmt)
        ENDIF
     ENDELSE
     IF rtsucc EQ 0 THEN BEGIN
        
     ENDIF
     nucut = T*xval/hok
  ENDIF

  Inu = 2*h*nu^3 / c^2
  Inu /= EXP( hok*nu / T ) - 1.0
  
  IF KEYWORD_SET( simple ) THEN Inu *= nu^beta ELSE BEGIN
     tau = (nu/nu0)^beta
     Inu *= (1.0 - EXP(-tau))
  ENDELSE

  IF has_alpha THEN BEGIN
     whigh = WHERE( nu GT nucut, nhigh )
     Inucut = 2*h*nucut^3 / c^2
     Inucut /= EXP( hok*nucut / T ) - 1.0
     IF KEYWORD_SET( simple ) THEN Inucut *= nucut^beta ELSE BEGIN
        taucut = (nucut/nu0)^beta
        Inucut *= (1.0 - EXP(-taucut))
     ENDELSE
     nfac = Inucut * nucut^alpha
     IF nhigh NE 0 THEN $
        Inu[whigh] = Inucut*(nucut/nu[whigh])^alpha
  ENDIF

  ;;Optional normalization stuff
  IF N_ELEMENTS(f500) NE 0 THEN BEGIN
     IF ~ FINITE(f500) THEN MESSAGE,"Invalid f500"
     IF f500 LE 0 THEN MESSAGE,"Invalid f500"
     IF lambda_fix LE 0 THEN MESSAGE,"Invalid lambda_fix"
     nu500 = c / lambda_fix
     Inu500 = 2*h*nu500^3 / c^2
     Inu500 /= EXP( hok*nu500 / T ) - 1.0
     IF KEYWORD_SET( simple ) THEN Inu500 *= nu500^beta ELSE BEGIN
        tau500 = (nu500/nu0)^beta
        Inu500 *= (1.0 - EXP(-tau500))
     ENDELSE
     IF has_alpha && nu500 GT nucut THEN $
        Inu500 = Inucut*(nucut/nu500)^alpha
     Inu *= f500 / Inu500
  ENDIF

  RETURN,Inu

END
