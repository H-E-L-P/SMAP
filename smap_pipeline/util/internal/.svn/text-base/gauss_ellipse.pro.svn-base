PRO GAUSS_ELLIPSE, pars, xx, yy, NPTS=npts

;+
; GAUSS_ELLIPSE, pars, xx, yy
; 
; Returns the coordinates of a FWHM contour for a 7-parameter 2-D
; Gaussian, with parameters as returned by GAUSSFIT2D. Can be used for
; overplotting best-fit Gaussian contour on image.
;
; INPUTS:
;   pars: 7-element parameter array
;
; OUTPUTS:
;   xx:   X positions of ellipse
;   yy:   Y positions of ellipse
;
; KEYWORDS:
;   NPTS: number of points on ellipse [default: 360]
;
; CREATED BY: gmarsden 2011-07-07
;-

IF NOT KEYWORD_SET(npts) THEN npts = 360

; theta parameter
tt = FINDGEN(npts + 1) / npts * 2 * !PI

; x(t) and y(t) for circle
xoft = pars[2] * COS(tt) * SQRT(2 * ALOG(2))
yoft = pars[3] * SIN(tt) * SQRT(2 * ALOG(2))

; ellipse
xx = xoft * COS(pars[6]) - yoft * SIN(pars[6]) + pars[4]
yy = xoft * SIN(pars[6]) + yoft * COS(pars[6]) + pars[5]

END
