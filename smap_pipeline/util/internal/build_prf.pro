;+
;NAME
; build_prf
;PURPOSE
; To build the PRF from the PSF.
;USAGE
; prf = build_prf( psf, rebinfac )
;INPUTS
; psf          The PSF.  
; rebinfac     The amount to rebin the PSF by.  Best results are
;               obtained when this is an odd integer.
;RETURNS
; prf          The PRF, normalized using the same conventions as
;               get_spire_beam
;KEYWORDS
; verbose      Run in verbose mode
; nonorm       Don't try to maintain normalization
;OPTIONAL OUTPUTS
; xcent/ycent  The central location of the input PSF.  If the PSF is
;               of odd extent along each axis, this is assumed to be
;               the size/2 (integral division).  If not, these are
;               required.  Integral values refer to the center of the pixel.
; success      1 if it worked, 0 if it didn't
; errmsg       Error message on failure
;NOTES
; Non-integral rebinning is not implemented.
; If the total of the PSF is one, then normalization is based on
;  preserving the integral of the PSF.  If it is not, then the peak
;  value is preserved.  This will be more accurate if xcent/ycent are
;  integral.
;WHAT IS A PRF?
; The value of PRF[i,j] is the value of the PSF observed at the same location
; but with larger pixels.  This allows you to take advantage of a PSF
; that is more finely sampled than your map, and deal with positions
; measured to a fraction of a pixel.  One thing that may be confusing
; is that, while each element represents a larger pixels, neighboring
; pixels overlap and sample the same positions as the input PSF.
;MODIFICATION HISTORY
; Author: Alex Conley, May 2009
;-

FUNCTION build_prf, psf, rebinfac, VERBOSE=verbose, SUCCESS=success, $
                    ERRMSG=errmsg, NONORM=nonorm, XCENT=xcent,$
                    YCENT=ycent

  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg = ''

  IF rebinfac LE 0 THEN BEGIN
     errmsg = "Rebinfac is non-positive: "+STRING(rebinfac)
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF

  szpsf = SIZE(psf)
  IF szpsf[0] NE 2 THEN BEGIN
     errmsg = "PSF is not 2D"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF

  tol = 0.01 ;;pixels
  
  IF ABS(rebinfac-1.0) LT tol THEN BEGIN
     ;;Well, that's easy
     success = 1b
     RETURN,psf
  ENDIF

  IF rebinfac LT 1 THEN BEGIN
     errmsg = "Rebinning factor less than one. This is not supported"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF

  IF ABS( ROUND(rebinfac)-rebinfac ) GT tol THEN BEGIN
     errmsg = "Non-integral rebinning not yet supported"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF
  rebinfac_used = ROUND(rebinfac)

  nx = szpsf[1]
  ny = szpsf[2]

  IF N_ELEMENTS(xcent) EQ 0 THEN IF nx mod 2 EQ 1 THEN xcent = nx/2 ELSE BEGIN
     errmsg = "If input psf has non-odd extent along x, you must provide "+$
              "xcent"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDELSE
  IF N_ELEMENTS(ycent) EQ 0 THEN IF ny mod 2 EQ 1 THEN ycent = ny/2 ELSE BEGIN
     errmsg = "If input psf has non-odd extent along y, you must provide "+$
              "ycent"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDELSE
  IF xcent LT 0 OR xcent GT nx-1 THEN BEGIN
     errmsg = "xcent is off the edges of the input PSF"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF 
  IF ycent LT 0 OR ycent GT ny-1 THEN BEGIN
     errmsg = "ycent is off the edges of the input PSF"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF 

  ;;Odd rebinning factors are easy
  IF rebinfac_used mod 2 EQ 1 THEN BEGIN
     ;;Edges are a problem which I ignore
     prf = psf
     ;;Simple for loop implementation -- should be a more complex
     ;; way to do this with indices and rebin
     rebino2 = rebinfac_used/2
     FOR i=0,nx-1 DO BEGIN
        minprfidx_x = i-rebino2 > 0
        maxprfidx_x = i+rebino2+1 < nx-1
        FOR j=0,ny-1 DO BEGIN
           minprfidx_y = j-rebino2 > 0
           maxprfidx_y = j+rebino2+1 < ny-1
           prf[i,j] = MEAN( psf[minprfidx_x:maxprfidx_x,$
                                minprfidx_y:maxprfidx_y] )
        ENDFOR
     ENDFOR
  ENDIF ELSE BEGIN
     ;;Ok, this may be ugly because we may be grabbing fractional
     ;; parts of pixels.  The user should really try to avoid this,
     ;; but there may not be a choice
     ;;We want the number of fully contained pixels in the rebinning,
     ;; and will have to deal with the non-fully contained ones
     ;; seperately
     ;;Note that integral rebinning is all that is allowed, so we
     ;; only have to worry about half pixels
     conto2 = (rebinfac_used-1)/2
     vals = FLTARR(9) ;;Order: center, left edge, up left corner,$
                      ;; lower left corner, right edge, up right corner,
                      ;; lower right corner, top, bottom
     weights = FLTARR(9)
     prf = psf
     FOR i=0,nx-1 DO BEGIN
        minprfidx_x_contained = i - conto2 > 0
        maxprfidx_x_contained = i + conto2 + 1 < nx-1
        FOR j=0,ny-1 DO BEGIN
           minprfidx_y_contained = j - conto2 > 0
           maxprfidx_y_contained = j + conto2 + 1 < ny-1
           datarr = psf[minprfidx_x_contained:maxprfidx_x_contained,$
                        minprfidx_y_contained:maxprfidx_y_contained]
           ncontained = N_ELEMENTS(datarr)
           vals[0] = MEAN(datarr)
           weights[0] = ncontained
           ;;Now deal with uncontained bits
           IF minprfidx_x_contained GT 0 THEN BEGIN
              ;;We have the right edge of fractional pixels, possibly
              ;; the corners
              pixvals = psf[minprfidx_x_contained-1,$
                            minprfidx_y_contained:maxprfidx_y_contained]
              vals[1] = MEAN(pixvals)
              weights[1] = N_ELEMENTS(pixvals)*0.5 ;;Half contained
              IF maxprfidx_y_contained LT ny-1 THEN BEGIN
                 ;;We have upper-left corner
                 vals[2] = psf[minprfidx_x_contained-1,$
                               maxprfidx_y_contained+1]
                 weights[2] = 0.25
              ENDIF ELSE weights[3] = 0.0
              IF minprfidx_y_contained GT 0 THEN BEGIN
                 ;;We have lower-left corner
                 vals[3] = psf[minprfidx_x_contained-1,$
                               minprfidx_y_contained-1]
                 weights[3] = 0.25
              ENDIF ELSE weights[3] = 0.0
           ENDIF ELSE weights[1] = 0.0
           IF maxprfidx_x_contained LT nx-1 THEN BEGIN
              ;;We have the left edge of fractional pixels, possibly
              ;; the corners
              pixvals = psf[maxprfidx_x_contained+1,$
                            minprfidx_y_contained:maxprfidx_y_contained]
              vals[4] = MEAN(pixvals)
              weights[4] = N_ELEMENTS(pixvals)*0.5 ;;Half contained
              IF maxprfidx_y_contained LT ny-1 THEN BEGIN
                 ;;We have upper-right corner
                 vals[5] = psf[maxprfidx_x_contained+1,$
                               maxprfidx_y_contained+1]
                 weights[5] = 0.25
              ENDIF ELSE weights[5] = 0.0
              IF minprfidx_y_contained GT 0 THEN BEGIN
                 ;;We have lower-right corner
                 vals[6] = psf[maxprfidx_x_contained+1,$
                               minprfidx_y_contained-1]
                 weights[6] = 0.25
              ENDIF ELSE weights[6] = 0.0
           ENDIF ELSE weights[4] = 0.0
           
           IF maxprfidx_y_contained LT ny-1 THEN BEGIN
              ;;Top
              pixvals = psf[minprfidx_x_contained:maxprfidx_x_contained,$
                            maxprfidx_y_contained+1]
              vals[7] = MEAN(pixvals)
              weights[7] = N_ELEMENTS(pixvals)*0.5 ;;Half contained
           ENDIF ELSE weights[7] = 0.0

           IF minprfidx_y_contained GT 0 THEN BEGIN
              ;;Bottom
              pixvals = psf[minprfidx_x_contained:maxprfidx_x_contained,$
                            minprfidx_y_contained-1]
              vals[8] = MEAN(pixvals)
              weights[8] = N_ELEMENTS(pixvals)*0.5 ;;Half contained
           ENDIF ELSE weights[8] = 0.0

           weights /= TOTAL(weights)
           val = TOTAL(weights*vals)
           IF ~ FINITE(val) THEN BEGIN
              errmsg = "Non finite element encountered"
              IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
              RETURN,!VALUES.F_NAN
           ENDIF
           prf[i,j] = val
        ENDFOR
     ENDFOR
  ENDELSE

  IF ~ KEYWORD_SET( nonorm ) THEN BEGIN
     ;;Figure out the old normalization scheme
     IF ABS( TOTAL(psf) - 1.0 ) LT tol THEN BEGIN
        ;;Area normalization
        prf *= rebinfac_used^2
     ENDIF 
  ENDIF

  success = 1b
  RETURN,prf
END
