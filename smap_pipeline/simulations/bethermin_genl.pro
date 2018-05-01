;+
;NAME
; bethermin_genl
;PURPOSE
; Draw samples from the Bethermin 2010 luminosity distribution
;USAGE
; loglum = bethermin_genl(nsources,dndlogldzdomega)
;INPUTS
; nsources        Number of sources to generate
; dndlogldzdomega Output of bethermin_dndlogldzdomega
;RETURNS
; Structure of z, Log L values
;MODIFICATION HISTORY
; Author: Alex Conley, April 12, 2011
;-

FUNCTION bethermin_genl, nsources, dndlogldzdomega, SEED=seed
  COMPILE_OPT IDL2

  IF nsources LE 0 THEN MESSAGE,"nsources invalid"
  IF SIZE(dndlogldzdomega,/TNAME) NE 'STRUCT' THEN $
     MESSAGE,"dndlogldzdomega not structure"

  nlum = N_ELEMENTS(dndlogldzdomega.lumvals)
  nz = N_ELEMENTS(dndlogldzdomega.zvals)

  IF MIN(dndlogldzdomega.zvals[1:*]-dndlogldzdomega.zvals[0:*]) LE 0.0 THEN $
     MESSAGE,"Z values not ascending"
  IF MIN(dndlogldzdomega.lumvals[1:*]-dndlogldzdomega.lumvals[0:*]) LE 0.0 $
  THEN MESSAGE,"Lum values not ascending"

  lums = REPLICATE({z: !VALUES.F_NAN, loglum: !VALUES.F_NAN},nsources)

  ;;Generate z values from cumulative z distribution
  cumz = TOTAL(dndlogldzdomega.lumintegral,/CUMULATIVE)
  cumz /= cumz[nz-1]
  lums.z = INTERPOL( dndlogldzdomega.zvals, cumz, RANDOMU(seed,nsources) )
  lums.z <= MAX(dndlogldzdomega.zvals)
  lums.z >= MIN(dndlogldzdomega.zvals)

  ;;Now, for each z we will generate a luminosity
  ;; To make this workable, we will generate the cumulative
  ;; probabilities for each tabulated z
  lcumarr = DBLARR(nz,nlum)
  FOR i=0,nz-1 DO BEGIN
     lcumarr[i,*] = TOTAL( dndlogldzdomega.dndlogldzdomega[i,*], /CUMULATIVE )
     lcumarr[i,*] /= lcumarr[i,nlum-1]
  ENDFOR

  ;;This is the very slow bit
  rvals = RANDOMU(seed,nsources)
  idxs = value_locate( dndlogldzdomega.zvals, lums.z )
  idz = 1.0/(dndlogldzdomega.zvals[1:*] - dndlogldzdomega.zvals[0:*])
  IF MIN(idxs) LT 0 OR MAX(idxs) EQ nz-1 THEN MESSAGE,"Generated invalid z"
  FOR i=0l, nsources-1 DO BEGIN
     idx = idxs[i]
     frac = (lums[i].z - dndlogldzdomega.zvals[idx]) * idz[idx]
     cuml = (1.0-frac)*lcumarr[idx,*] + frac*lcumarr[idx+1,*]
     lums[i].loglum = INTERPOL( dndlogldzdomega.lumvals, cuml, rvals[i] )
  ENDFOR
  lums.loglum <= MAX(dndlogldzdomega.lumvals)
  lums.loglum >= MIN(dndlogldzdomega.lumvals)

  RETURN,lums
END
