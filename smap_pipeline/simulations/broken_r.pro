;+
;NAME
; broken_r
;PURPOSE
; Compute R for a multiply broken, power law differential counts
;  model given an arbitrary PSF
;USAGE
; R = broken_r( fluxes, knotpos, knotval, psf, pixsize )
;INPUTS
; fluxes    Fluxes you want R at
; knotpos   Position of knots
; knotval   log10 values of knots
; psf       PSF array.
; pixsize   Pixel size in arcsec
;KEYWORDS
; interpol  Rather than computer R explicitly, interpolate
;            using logarithmic points
;OPTIONAL ARGUMENTS
; ninterpol Number of interpolation points if /INTERPOL used
;            (def: 80)
;RETURNS
; R, the mean number density of sources observed with
; flux between x and x+dx
;NOTES
; Brute force calculation, so potentially quite slow.
;MODIFICATION HISTORY
; Author: Alex Conley, Feb 2010
;-

FUNCTION broken_r, fluxes, knotpos, knotval, psf, pixsize,$
                   INTERPOL=interpol, NINTERPOL=ninterpol

  COMPILE_OPT IDL2
  IF N_ELEMENTS(fluxes) EQ 0 THEN MESSAGE,"No fluxes"
  IF N_ELEMENTS(ninterpol) EQ 0 THEN ninterpol=80
  retarr = DBLARR( N_ELEMENTS(fluxes) )
  
  wnotzero = WHERE( psf NE 0.0, nnotzero )
  working_psf = psf[wnotzero]
  iworking_psf = 1.0/ working_psf
  
  IF KEYWORD_SET(interpol) THEN BEGIN
     wgtzero = WHERE( fluxes GT 0, ngtzero, COMPLEMENT=wltzero, $
                      NCOMPLEMENT=nltzero )
     IF ngtzero GT 0 THEN BEGIN
        minflux = MIN( fluxes[ wgtzero ], MAX=maxflux )
        ifluxes = ALOG(maxflux/minflux)*FINDGEN(ninterpol)/(ninterpol-1.0) + $
                  ALOG(minflux)
        rint = DBLARR(N_ELEMENTS(ifluxes))
        FOR i=0,N_ELEMENTS(ifluxes)-1 DO BEGIN
           rint[i] = TOTAL( broken_n( EXP(ifluxes[i])*iworking_psf, $
                                   knotpos, knotval ) *$
                         iworking_psf )
        ENDFOR
        rint = ALOG(rint) ;;Interpolate in log R, much smoother
        retarr[wgtzero] = EXP(INTERPOL( TEMPORARY(rint), ifluxes, $
                                        ALOG( fluxes[wgtzero] )))
     ENDIF
     IF nltzero NE 0 THEN retarr[wltzero] = 0.0
  ENDIF ELSE BEGIN
     wgtzero = WHERE( fluxes GT 0, ngtzero, COMPLEMENT=wltzero, $
                      NCOMPLEMENT=nltzero )
     FOR i=0, N_ELEMENTS(wgtzero)-1 DO BEGIN
        retarr[wgtzero[i]] = TOTAL( broken_n( fluxes[wgtzero[i]]*iworking_psf, $
                                              knotpos, knotval ) *$
                                    iworking_psf )
     ENDFOR
     IF nltzero NE 0 THEN retarr[wltzero] = 0.0
  ENDELSE     
  retarr *= (pixsize/3600.0)^2
  RETURN,retarr
END
