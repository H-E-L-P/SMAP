;+
;NAME
; broken_n
;PURPOSE
; Compute n(S) for a multiply broken power law model
;USAGE
; n = broken_n( fluxes, knotpos, knotval )
;INPUTS
; fluxes    Fluxes you want n(S) at
; knotpos   Position of knots
; knotval   log10 values of knots
;RETURNS
; The differential number counts model evaluated at fluxes.
;MODIFICATION HISTORY
; Author: Alex Conley, Feb 2010
;-

FUNCTION broken_n, fluxes, knotpos, knotval

  COMPILE_OPT IDL2
  nfluxes = N_ELEMENTS(fluxes)
  nknots  = N_ELEMENTS(knotpos)
  IF N_ELEMENTS(knotval) NE nknots THEN $
     MESSAGE,"Knotpos and knotval not the same length"
  IF nknots EQ 0 THEN MESSAGE,"No knots provided"
  IF nfluxes EQ 0 THEN MESSAGE,"No fluxes provided"

  sknots = SORT( knotpos )
  iknotpos = knotpos[sknots]
  iknotval = knotval[sknots] * ALOG(10.0) ;;Convert to natural log

  gamma = ( iknotval[0:nknots-2] - iknotval[1:nknots-1] ) / $
          ALOG( iknotpos[1:nknots-1]/iknotpos[0:nknots-2] )
  a     = EXP( iknotval[0:nknots-2] ) * iknotpos[0:nknots-2]^gamma
  retarr = DBLARR(nfluxes)
  wout = WHERE( fluxes LT iknotpos[0] OR fluxes GE iknotpos[nknots-1],$
                nout, COMPLEMENT=win, NCOMPLEMENT=nin )
  IF nout NE 0 THEN retarr[wout] = 0.0
  IF nin NE 0 THEN BEGIN
     pos = VALUE_LOCATE( iknotpos, fluxes[win] )
     retarr[win] = a[pos] * fluxes[win]^(-gamma[pos])
  ENDIF

  RETURN,retarr
END
