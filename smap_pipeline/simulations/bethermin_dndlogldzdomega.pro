;+
;NAME
; bethermin_dndlogldzdomega
;PURPOSE
; To compute the Bethermin 2010 number distribution
; a function of z and log Lir -- that is, to produce dN / dlogLir dz dOmega
;USAGE
; dist = bethermin_dndlogldzdomega(nz,nlum,[ZMIN=,ZMAX=,LUMMIN=,LUMMAX=])
;INPUTS
; nz        Number of z values
; nlum      Number of log luminosities
;OPTIONAL INPUTS
; zmin      Minimum z (def: 0)
; zmax      Maximum z (def: 10)
; lummin    Minimum log luminosity (def: 9)
; lummax    Maximum log luminosity (def:13.5)
; Omega_m   Omega_m (def: 0.27)
; H0        H_0 (def: 73.2)
;RETURNS
; A structure holding the distribution
;NOTES
; Assumes flat, cosmological constant
;MODIFICATION HISTORY
; Author: Alex Conley, April 12, 2011
;-

FUNCTION bethermin_dndlogldzdomega, nz, nlum, ZMIN=zmin, ZMAX=zmax,$
                                    LUMMIN=lummin, LUMMAX=lummax,$
                                    OMEGA_M=omega_m, H0=h0
  COMPILE_OPT IDL2

  IF nz LE 0 THEN MESSAGE,"Invalid nz"
  IF nlum LE 0 THEN MESSAGE,"Invalid nlum"
  IF N_ELEMENTS(zmin) EQ 0 THEN zmin = 0.0
  IF N_ELEMENTS(zmax) EQ 0 THEN zmax = 10.0
  IF N_ELEMENTS(lummin) EQ 0 THEN lummin = 9.0
  IF N_ELEMENTS(lummax) EQ 0 THEN lummax = 13.5
  IF lummin LT 9.0 THEN $
     MESSAGE,"Warning: IAS library only goes down to loglum 9",/INF
  IF lummax GT 13.5 THEN $
     MESSAGE,"Warning: IAS library only goes to up to loglum 13.5",/INF
  IF N_ELEMENTS(omega_m) EQ 0 THEN omega_m = 0.27
  IF omega_m LE 0 THEN MESSAGE,"Invalid omega_m"
  IF N_ELEMENTS(H0) EQ 0 THEN H0=73.2 ;;km/sec/Mpc
  IF H0 LE 0 THEN MESSAGE,"Invalid H0"

  zvals   = (zmax-zmin)*DINDGEN(nz)/(nz-1.0)+zmin
  lumvals = (lummax-lummin)*DINDGEN(nlum)/(nlum-1.0)+lummin

  retstruct = { omega_m: omega_m, H0: H0, zvals: zvals, lumvals: lumvals, $
                dndlogldzdomega: DBLARR(nz,nlum), zintegral: DBLARR(nlum),$
                lumintegral: DBLARR(nz), totsources: !VALUES.D_NAN }
  
  ;;comoving volume element multiplier (i.e., dV/dz dOmega)
  dl = LUMDIST(zvals,H0=H0,Omega_m=omega_m,Lambda0=1.0-omega_m,/SILENT) ;;Mpc
  dVdzdOmega = 299792.458/H0 * TEMPORARY(dl)^2/$
               ( (1+zvals)^2 * SQRT( 1.0-omega_m + (1+zvals)^3*omega_m ) )

  FOR i=0, nz-1 DO $
     retstruct.dndlogldzdomega[i,*] = $
     bethermin_l(zvals[i],lumvals)*dVdzdOmega[i]

  ;;Add marginal distributions over z and log L
  FOR i=0,nlum-1 DO $
     retstruct.zintegral[i] = TSUM(zvals,retstruct.dndlogldzdomega[*,i])
  FOR i=0,nz-1 DO $
     retstruct.lumintegral[i] = TSUM(lumvals,retstruct.dndlogldzdomega[i,*])
  retstruct.totsources = TSUM(zvals,retstruct.lumintegral)
  
  RETURN,retstruct

END
