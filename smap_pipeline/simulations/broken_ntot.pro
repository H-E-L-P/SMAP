;+
;NAME 
; broken_ntot
;PURPOSE
; Gets the number counts predicted by a multiply-broken
; law different counts model above a user specified flux
; density.
;USAGE
; n = broken_ntot( fluxval, knotpos, knotval )
;INPUTS
; fluxval   Lower limit of integral
; knotpos   Positions of knots
; knotval   log10 values of knots ( i.e., log_10 dN/dS )
;NOTES
; The number counts are assumed to be zero
; outside the highest and lowest knot
;MODIFICATION HISTORY
; Author: Alex Conley, Apr 2010
;-

FUNCTION broken_ntot, fluxval, knotpos, knotval
  COMPILE_OPT IDL2
  
  nfluxes = N_ELEMENTS(fluxes)
  nknots  = N_ELEMENTS(knotpos)
  IF N_ELEMENTS(knotval) NE nknots THEN $
     MESSAGE,"Knotpos and knotval not the same length"
  IF nknots EQ 0 THEN MESSAGE,"No knots provided"

  sknots = SORT( knotpos )
  iknotpos = knotpos[sknots]

  IF fluxval GE iknotpos[nknots-1] THEN RETURN,0.0

  iknotval = knotval[sknots] * ALOG(10.0) ;;Convert to natural log

  gamma = ( iknotval[0:nknots-2] - iknotval[1:nknots-1] ) / $
          ALOG( iknotpos[1:nknots-1]/iknotpos[0:nknots-2] )
  a     = EXP( iknotval[0:nknots-2] ) * iknotpos[0:nknots-2]^gamma

  ;;B is the integral of n(S) over the knots
  ;;gamma = 1 is a special case
  B     = DBLARR( nknots-1 )

  wone = WHERE( ABS( gamma-1.0 ) LT 1e-3, none, COMPLEMENT=wnotone,$
                NCOMPLEMENT=nnotone )
  IF none NE 0 THEN BEGIN
     B[wone] = a[wone] * ALOG( iknotpos[wone+1] / iknotpos[wone] )
  ENDIF
  IF nnotone NE 0 THEN BEGIN
     onemg = 1.0 - gamma[wnotone]
     B[wnotone] = a[wnotone] * ( iknotpos[wnotone+1]^onemg - $
                                 iknotpos[wnotone]^onemg ) / onemg
  ENDIF

  ;;Figure out where the knot is
  IF fluxval LE iknotpos[0] THEN RETURN,TOTAL(B)
  
  ;;ipos is the index of the last element less than or equal to
  ;;fluxval
  mnstep = (iknotpos[nknots-1]-iknotpos[0])/DOUBLE(nknots)
  ipos = VALUE_LOCATE(iknotpos, fluxval )
  IF ABS( iknotpos[ipos] - fluxval ) LT 1e-4*mnstep THEN $
     RETURN,TOTAL( B[ipos:*] )
  IF ipos EQ nknots-1 THEN RETURN,0.0
  onemg = 1.0-gamma[ipos]
  IF ABS( onemg ) LT 1e-3 THEN $
     RETURN,a[ipos]*ALOG( iknotpos[ipos+1]/fluxval )+TOTAL(B[ipos+1:*])

  RETURN,a[ipos]*(iknotpos[ipos+1]^onemg - fluxval^onemg)/onemg +$
         TOTAL(B[ipos+1:*])
END
