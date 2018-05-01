;+
;NAME
; bethermin_l
;PURPOSE
; Evaluates the Bethermin 2010 luminosity function for a given
; redshift.
;USAGE
; phi = bethermin_l(z,lum)
;INPUTS
; z       Redshift (scalar)
; lum     log10 ir luminosities in solar units.  Can be an array
;RETURNS
; phi, in number of sources per comoving Mpc per logarithmic
;      luminosity (base 10)
;OPTIONAL INPUTS:
; Various parameters of the Bethermin model, such as alpha
;MODIFICATION HISTORY
; Author: Alex Conley, April 12, 2011
;-

Function bethermin_l, z, lum, ALPHA=alpha, SIGMA=sigma, LSTAR0=lstar0,$
                      PHISTAR0=phistar0, RLLZ=rllz, RPHILZ=rphilz,$
                      ZBREAK1=zbreak1, RLMZ=rmmz, RPHIMZ=rphimz,$
                      ZBREAK2=zbreak2, RLHZ=rlhz, RPHIHZ=rphihz
  COMPILE_OPT IDL2

  ;;Table 1 of Bethermin 2010
  IF N_ELEMENTS(alpha) EQ 0 THEN alpha=1.223
  IF N_ELEMENTS(sigma) EQ 0 THEN sigma=0.406
  IF N_ELEMENTS(lstar0) EQ 0 THEN lstar0=2.377d10
  IF N_ELEMENTS(phi0) EQ 0 THEN phi0=3.234e-3
  IF N_ELEMENTS(rllz) EQ 0 THEN rllz=2.931
  IF N_ELEMENTS(rphilz) EQ 0 THEN rphilz=0.774
  IF N_ELEMENTS(zbreak1) EQ 0 THEN zbreak1=0.879
  IF N_ELEMENTS(rlmz) EQ 0 THEN rlmz=4.737
  IF N_ELEMENTS(rphimz) EQ 0 THEN rphimz = -6.246
  IF N_ELEMENTS(zbreak2) EQ 0 THEN zbreak2 = 2
  IF N_ELEMENTS(rlhz) EQ 0 THEN rlhz = 0.145
  IF N_ELEMENTS(rphihz) EQ 0 THEN rphihz = -0.919

  IF N_ELEMENTS(z) NE 1 THEN MESSAGE,"z must be scalar"
  IF z LT 0 THEN MESSAGE,"Invalid z"
  nlum = N_ELEMENTS(lum)
  IF nlum EQ 0 THEN MESSAGE,"No luminosities provided"

  ;;Determine redshift multipliers for lstar, phistar calculation



  ;;Determine lstar, phistar
  IF z LT zbreak1 THEN BEGIN
     lstar   = lstar0 * (1.0 + z)^rllz
     phistar = phi0 * (1.0 + z)^rphilz
  ENDIF ELSE IF z LT zbreak2 THEN BEGIN
     lstar_mz = (1.0 + zbreak1)^(rllz-rlmz)
     phistar_mz   = (1.0 + zbreak1)^(rphilz-rphimz)
     lstar   = lstar0 * lstar_mz * (1.0 + z)^rlmz 
     phistar = phi0 * phistar_mz * (1.0 + z)^rphimz
  ENDIF ELSE BEGIN
     lstar_hz = (1.0 + zbreak2)^(rlmz-rlhz) * $
                (1.0 + zbreak1)^(rllz-rlmz)
     phistar_hz = (1.0 + zbreak2)^(rphimz-rphihz) * $
                  (1.0 + zbreak1)^(rphilz-rphimz)
     lstar   = lstar0 * lstar_hz * (1.0 + z)^rlhz
     phistar = phi0 * phistar_hz * (1.0 + z)^rphihz
  ENDELSE

  lirolstar = 10.0d0^lum / lstar
  logbit = ALOG10( 1.0 + lirolstar )

  phi = phistar * TEMPORARY(lirolstar)^(1-alpha) * $
        exp( -1.0/(2*sigma^2) * TEMPORARY(logbit)^2 )
  RETURN, phi
END
  
  
