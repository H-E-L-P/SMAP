;+
;NAME 
; spline_cumn
;PURPOSE
; Generates cumulative probability distribution
;  for multiply-broken spline model
;USAGE
; prob = spline_cumn( fluxes, knotpos, knotval )
;INPUTS
; fluxes    Positions to generate cumulative probability at.
; knotpos   Positions of knots
; knotval   log10 values of knots
;OPTIONAL OUTPUTS
; ntot      Total number of sources per area
;MODIFICATION HISTORY
; Author: Alex Conley, Mar 2, 2011
;-

FUNCTION spline_cumn, fluxes, knotpos, knotval, NTOT=ntot
  COMPILE_OPT IDL2
  
  nfluxes = N_ELEMENTS(fluxes)
  nknots  = N_ELEMENTS(knotpos)
  IF N_ELEMENTS(knotval) NE nknots THEN $
     MESSAGE,"Knotpos and knotval not the same length"
  IF nknots EQ 0 THEN MESSAGE,"No knots provided"
  IF nfluxes EQ 0 THEN MESSAGE,"No fluxes provided"

  IF MIN(knotpos) LT 0 THEN MESSAGE,"Knot positions must be positive"
  lknotpos = ALOG(knotpos)
  spl = SPL_INIT(lknotpos,knotval,/DOUBLE)
  
  retarr = DBLARR(nfluxes)
  minknot = MIN(knotpos, MAX=maxknot )
  wnonzero = WHERE( fluxes GE minknot AND fluxes LT maxknot, nnonzero )
  IF nnonzero EQ 0 THEN MESSAGE,"No fluxes give non-zero counts"
  
  retarr[wnonzero] = 10^SPL_INTERP(lknotpos,knotval,spl,$
                                   ALOG(fluxes[wnonzero]))

  IF ARG_PRESENT(ntot) THEN $
     ntot = TSUM(fluxes,retarr)

  ;;Need bin sizes to get prob right
  dflux = [ fluxes[1:*] - fluxes[0:*], fluxes[nfluxes-1]-fluxes[nfluxes-2] ]

  retarr = TOTAL(retarr*dflux,/DOUBLE,/CUMULATIVE)
  retarr /= retarr[nfluxes-1] ;;To probability
  
  RETURN,retarr

END
