;+
;NAME 
; broken_cumn
;PURPOSE
; Generates cumulative probability distribution
;  for multiply-broken power law model
;USAGE
; prob = broken_cumn( fluxes, knotpos, knotval )
;INPUTS
; fluxes    Positions to generate cumulative probability at
; knotpos   Positions of knots
; knotval   log10 values of knots
;OPTIONAL OUTPUTS
; ntot      Total number of sources per area
;MODIFICATION HISTORY
; Author: Alex Conley, Feb 2010
;-

FUNCTION broken_cumn, fluxes, knotpos, knotval, NTOT=ntot
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
  
  ;;gamma = 1 is a special case
  ;;M is the integral of the number counts over the bins
  M     = DBLARR( nknots-1 )
  wone = WHERE( ABS( gamma-1.0 ) LT 1e-3, none, COMPLEMENT=wnotone,$
                NCOMPLEMENT=nnotone )
  IF none NE 0 THEN BEGIN
     M[wone] = a[wone] * ALOG( iknotpos[wone+1] / iknotpos[wone] )
  ENDIF
  IF nnotone NE 0 THEN BEGIN
     onemg = 1.0 - gamma[wnotone]
     M[wnotone] = a[wnotone] * ( iknotpos[wnotone+1]^onemg - $
                                 iknotpos[wnotone]^onemg ) / onemg
  ENDIF

  ;;Normalization constant
  ntot = TOTAL( M )
  intot = 1.0 / ntot

  Mcum = TOTAL( M, /CUMULATIVE )

  ;;The cumulative prob is the sum over M plus (possibly)
  ;; a tailing bit for the partial bin integral
  retarr = DBLARR( nfluxes )
  wlow = WHERE( fluxes LT iknotpos[0], nlow )
  IF nlow NE 0 THEN retarr[wlow] = 0.0
  whigh = WHERE( fluxes GE iknotpos[nknots-1], nhigh )
  IF nhigh NE 0 THEN retarr[whigh] = 1.0
  wcore = WHERE( fluxes GE iknotpos[0] AND $
                 fluxes LT iknotpos[nknots-1], ncore )
  IF ncore NE 0 THEN BEGIN
     ;; loc is such knotpos[loc] <= fluxes[i] < knotpos[loc+1]
     ;; and is -1 for fluxes[i] < knotpos[0] and 
     ;; nfluxes for fluxes[i] > knotpos[nknots-1]
     loc = VALUE_LOCATE( knotpos, fluxes[wcore] )
     FOR i=0,ncore-1 DO BEGIN
        cloc = loc[i]
        cflux = fluxes[wcore[i]]
        IF cloc GT 0 THEN sum_piece = Mcum[cloc-1] ELSE $
           sum_piece = 0.0
        IF cflux GT knotpos[cloc] THEN BEGIN
           omg = 1.0 - gamma[cloc]
           IF ABS( omg ) LT 1e-3 THEN $
              tail_piece = a[cloc] * ALOG( cflux/iknotpos[cloc] ) ELSE $
                 tail_piece = a[cloc]/omg * ( cflux^omg - iknotpos[cloc]^omg )
        ENDIF ELSE tail_piece = 0.0
        retarr[wcore[i]] = intot*(sum_piece + tail_piece)
     ENDFOR
  ENDIF
  RETURN,retarr
END
