;+
;NAME
; broken_r_gauss
;PURPOSE
; To get R for a multiply broken power law differential
;  number counts model for a Gaussian PSF
;USAGE
; R = getRmulti( flux, fluxknots, fluxvals, THETA=theta )
;INPUTS
; flux      Values you want R at
; fluxknots Locations of flux knots
; fluxvals  log 10 values of number counts at knots
;OPTIONAL INPUTS
; theta     FWHM of beam in arcsec, default 18.6
;MODIFICATION HISTORY
; Author: Alex Conley, Dec 2009
;-

FUNCTION broken_r_gauss, flux, fluxknots, fluxvals, THETA=theta

  COMPILE_OPT IDL2, STRICTARRSUBS

  IF N_ELEMENTS(theta) EQ 0 THEN theta = 18.6
  IF theta LE 0.0 THEN MESSAGE,"Theta must be positive!"

  prefac = !PI / ( 7200.0*7200.0*alog(2) ) * theta * theta

  nflux = N_ELEMENTS(flux)
  IF nflux EQ 0 THEN RETURN,!VALUES.D_NAN
  nknots = N_ELEMENTS(fluxknots)
  IF nknots EQ 0 THEN MESSAGE,"No flux knots provided"
  IF nknots LT 2 THEN MESSAGE,"Must have at least two flux knots!"
  IF N_ELEMENTS(fluxvals) NE nknots THEN $
     MESSAGE,"Fluxvals/knots not the same length"
  smin = MIN(fluxknots, MAX=smax)
  IF smin LE 0 THEN MESSAGE,"Only positive flux knots allowed!"
  
  sk = SORT(fluxknots)
  i_fluxknots = DOUBLE(fluxknots[sk])
  i_fluxvals = DOUBLE(fluxvals[TEMPORARY(sk)]) * ALOG(10.0) ;;Conv to nat log
  IF MIN( i_fluxknots[1:*] - i_fluxknots[0:nknots-2] ) LT 1e-3 THEN $
     MESSAGE,"Flux knots not sufficiently separated"
  
  ;;Calculate a, gamma parameters
  gamma = ( i_fluxvals[0:nknots-2] - i_fluxvals[1:nknots-1] ) / $
          ALOG( i_fluxknots[1:nknots-1]/i_fluxknots[0:nknots-2] )
  a     = EXP( i_fluxvals[0:nknots-2] ) * i_fluxknots[0:nknots-2]^gamma

  ;;Gamma=1 is a special case
  w1 = WHERE( ABS(gamma-1.0) LT 1d-3, n1, COMPLEMENT=wg, NCOMPLEMENT=ng )

  ;;Loop for each x
  rvals = DBLARR( nflux )
  lvals = ALOG( i_fluxknots[1:*]-i_fluxknots[0:nknots-2] )
  FOR i=0, nflux-1 DO BEGIN
     IF flux[i] LE 0.0 THEN BEGIN
        rvals[i] = 0.0
        CONTINUE
     ENDIF

     up_limit = flux[i] / smin
     IF up_limit GT 1.0 THEN BEGIN
        ;;Upper limit complicates things -- should be
        ;; the most common case, since mostly we are interested
        ;; in flux values above the minimum knot
        ;;We want l0 to be the index of the first fluxknot
        ;; larger than flux
        l0= VALUE_LOCATE( i_fluxknots, flux[i] )
        IF l0 EQ -1 THEN BEGIN
           ;;flux < smin, so this can't happen!
           MESSAGE,"You found a bug"
        ENDIF ELSE IF l0 EQ nknots-1 THEN BEGIN
           ;;flux > smax, this can happen, but luckily things are easy
           rvals[i] = 0.0
        ENDIF ELSE BEGIN
           l0 += 1 ;;To get K[l0] > flux (smallest value)
           cumsum = 0.0
           ap = a/flux[i]
           IF n1 GT 0 THEN BEGIN
              w12 = WHERE( w1 GE l0, n12 )
              IF n12 GT 0 THEN BEGIN
                 w12 = w1[w12]
                 cumsum += TOTAL( ap[w12] * lvals[w12] )
              ENDIF
           ENDIF
           IF ng NE 0 THEN BEGIN
              wg2 = WHERE( wg GE l0, ng2 )
              IF ng2 GT 0 THEN BEGIN
                 wg2 = wg[wg2]
                 cumsum += $
                    TOTAL( ap[wg2]*( i_fluxknots[wg2]^(1-gamma[wg2]) - $
                                     i_fluxknots[wg2+1]^(1-gamma[wg2]) ) / $
                           (gamma[wg2]-1) )
              ENDIF
           ENDIF

           ;;And add on extra bit
           IF ABS( gamma[l0-1] - 1.0 ) LT 1d-3 THEN BEGIN
              cumsum += ap[l0-1]*ALOG(i_fluxknots[l0]/flux[i])
           ENDIF ELSE BEGIN
              cumsum += a[l0-1] * flux[i]^(-gamma[l0-1]) * $
                        ( 1 - (flux[i]/i_fluxknots[l0])^(gamma[l0-1]-1) ) / $
                        ( gamma[l0-1] - 1 )
           ENDELSE

           rvals[i] = cumsum
        ENDELSE
     ENDIF ELSE BEGIN
        ;;Pure sum of terms
        cumsum = 0.0
        ap = a/flux[i]
        IF n1 NE 0 THEN $
           cumsum += TOTAL( ap[w1] * lvals[w1] )
        IF ng NE 0 THEN $
           cumsum += TOTAL( ap[wg]*( i_fluxknots[wg]^(1-gamma[wg]) - $
                                     i_fluxknots[wg+1]^(1-gamma[wg]) ) / $
                            (gamma[wg]-1) )
        rvals[i] = cumsum
     ENDELSE
  ENDFOR
  rvals *= prefac

  IF ( size(flux) )[0] EQ 0 THEN RETURN,rvals[0] ELSE RETURN,rvals

END
