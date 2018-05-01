;+
;NAME 
; broken_meanflux
;PURPOSE
; Gets the mean flux per area for a multiply-broken
; power law differential counts model
;USAGE
; mean = broken_meanflux( knotpos, knotval )
;INPUTS
; knotpos   Positions of knots
; knotval   log10 values of knots
;KEYWORDS
; squared   Return the mean squared flux per area instead
;MODIFICATION HISTORY
; Author: Alex Conley, Feb 2010
;-

FUNCTION broken_meanflux, knotpos, knotval, MAXVAL=maxval, $
                          SQUARED=squared
  COMPILE_OPT IDL2
  
  nfluxes = N_ELEMENTS(fluxes)
  nknots  = N_ELEMENTS(knotpos)
  IF N_ELEMENTS(knotval) NE nknots THEN $
     MESSAGE,"Knotpos and knotval not the same length"
  IF nknots EQ 0 THEN MESSAGE,"No knots provided"

  IF N_ELEMENTS(maxval) NE 0 && maxval LE MIN(knotpos) $
     THEN RETURN,0.0

  sknots = SORT( knotpos )
  iknotpos = knotpos[sknots]
  iknotval = knotval[sknots] * ALOG(10.0) ;;Convert to natural log

  gamma = ( iknotval[0:nknots-2] - iknotval[1:nknots-1] ) / $
          ALOG( iknotpos[1:nknots-1]/iknotpos[0:nknots-2] )
  a     = EXP( iknotval[0:nknots-2] ) * iknotpos[0:nknots-2]^gamma

  ;;B is the integral of S n(S) over the knots
  ;;gamma = 2 is a special case
  B     = DBLARR( nknots-1 )

  IF ~ KEYWORD_SET(squared) THEN BEGIN
     wtwo = WHERE( ABS( gamma-2.0 ) LT 1e-3, ntwo, COMPLEMENT=wnottwo,$
                   NCOMPLEMENT=nnottwo )
     IF ntwo NE 0 THEN BEGIN
        B[wtwo] = a[wtwo] * ALOG( iknotpos[wtwo+1] / iknotpos[wtwo] )
     ENDIF
     IF nnottwo NE 0 THEN BEGIN
        twomg = 2.0 - gamma[wnottwo]
        B[wnottwo] = a[wnottwo] * ( iknotpos[wnottwo+1]^twomg - $
                                    iknotpos[wnottwo]^twomg ) / twomg
     ENDIF
  ENDIF ELSE BEGIN
     wthree = WHERE( ABS( gamma-3.0 ) LT 1e-3, nthree, COMPLEMENT=wnotthree,$
                   NCOMPLEMENT=nnotthree )
     IF nthree NE 0 THEN BEGIN
        B[wthree] = a[wthree] * ALOG( iknotpos[wthree+1] / iknotpos[wthree] )
     ENDIF
     IF nnotthree NE 0 THEN BEGIN
        threemg = 3.0 - gamma[wnotthree]
        B[wnotthree] = a[wnotthree] * ( iknotpos[wnotthree+1]^threemg - $
                                        iknotpos[wnotthree]^threemg ) / threemg
     ENDIF
  ENDELSE

  IF N_ELEMENTS(maxval) NE 0 THEN BEGIN
     ;;ipos is the index of the last element less than or equal to
     ;; maxval, while B[i] is the integral from i to i+1
     ipos = VALUE_LOCATE(iknotpos, maxval )
     IF ipos EQ nknots-1 THEN RETURN,TOTAL(B) ;;off high end
     IF ABS( iknotpos[ipos] - maxval ) LT iknotpos[ipos]*1d-5 THEN $
        RETURN,TOTAL( B[0:ipos-1] ) ;;Hit boundary nicely

     ;;Otherwise partial sum plus extra bit
     IF ipos GT 0 THEN BEGIN
        bval = TOTAL(B[0:ipos-1]) ;;Integral of whole chunks
     ENDIF ELSE bval = 0.0d0

     ;;Now partial chunk
     IF ~ KEYWORD_SET(squared) THEN BEGIN
        IF ( ABS(gamma[ipos]-2.0) LT 1e-3 ) THEN BEGIN
           bval += a[ipos]*ALOG(maxval/iknotpos[ipos])
        ENDIF ELSE BEGIN
           twomg = 2.0-gamma[ipos]
           bval += a[ipos] * ( maxval^twomg - iknotpos[ipos]^twomg )/twomg
        ENDELSE
     ENDIF ELSE BEGIN
        IF ( ABS(gamma[ipos]-3.0) LT 1e-3 ) THEN BEGIN
           bval += a[ipos]*ALOG(maxval/iknotpos[ipos])
        ENDIF ELSE BEGIN
           threemg = 3.0-gamma[ipos]
           bval += a[ipos] * ( maxval^threemg - iknotpos[ipos]^threemg )/threemg
        ENDELSE
     ENDELSE
     RETURN,bval
  ENDIF
  
  RETURN,TOTAL(B)
END
