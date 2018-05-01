;+
;NAME
; bethermin_zdist
;PURPOSE
; To compute the integral over the Bethermin phi distribution as
; a function of z -- that is, to produce dN / dz dOmega
;USAGE
; lumint = bethermin_zdist(zvals)
;OPTIONAL INPUTS
; maxlum    Maximum log ir lum (def: 13.5)
; nlum      Number of log lum steps (def: 100)
; Omega_m   Omega_m (def: 0.27)
; H0        H_0 (def: 73.2)
;NOTES
; Assumes flat, cosmological constant
;MODIFICATION HISTORY
; Author: Alex Conley, April 12, 2011
;-

FUNCTION bethermin_zdist, zvals, MINLUM=minlum, MAXLUM=maxlum, NLUM=nlum, $
                          OMEGA_M=omega_m, H0=h0
  COMPILE_OPT IDL2

  IF N_ELEMENTS(minlum) EQ 0 THEN minlum = 9.0
  IF N_ELEMENTS(maxlum) EQ 0 THEN maxlum = 13.5
  IF N_ELEMENTS(nlum) EQ 0 THEN nlum = 100
  IF N_ELEMENTS(omega_m) EQ 0 THEN omega_m = 0.27
  IF N_ELEMENTS(H0) EQ 0 THEN H0 = 73.2 ;;km/sec/Mpc

  IF min(zvals) LT 0 THEN MESSAGE,"Invalid minimum z"
  IF minlum LT 9.0 THEN $
     MESSAGE,"Warning: IAS library only goes down to loglum 9",/INF
  IF maxlum GT 13.5 THEN $
     MESSAGE,"Warning: IAS library only goes to up to loglum 13.5",/INF
  IF omega_m LE 0 THEN MESSAGE,"Invalid omega_m"
  IF nlum LE 0 THEN MESSAGE,"Invalid number of luminosities"
  IF H0 LE 0 THEN MESSAGE,"Invalid H0"

  lumvals = (maxlum-minlum)*DINDGEN(nlum)/(nlum-1.0) + minlum
  nz      = N_ELEMENTS(zvals)
  retarr  = DBLARR(nz)
  
  ;;comoving volume element multiplier (i.e., dV/dz dOmega)
  dl = LUMDIST(zvals,H0=H0,Omega_m=omega_m,Lambda0=1.0-omega_m,/SILENT) ;;Mpc
  dVdzdOmega = 299792.458/H0 * dl^2/$
               ( (1+zvals)^2 * SQRT( 1.0-omega_m + (1+zvals)^3*omega_m ) )

  FOR i=0, nz-1 DO $
     retarr[i] = TSUM(lumvals,bethermin_l(zvals[i],lumvals))*dVdzdOmega[i]

  RETURN,retarr

END
