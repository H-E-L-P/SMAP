; $Id: gaussian2d.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	GAUSSIAN2D
;
; PURPOSE:
;	Compute bivariate gaussian function, normalized to unit maximum.
;
; CATEGORY:
;	Models.
;
; CALLING SEQUENCE:
;	Result = GAUSSIAN2D(X_size, Y_size, X_center, Y_center, $
;						Sigma_x, Sigma_y, Angle)
;
; INPUTS:
;	X_size, Y_size:	First and second size of output array
;
;	X_center, Y_center:	Coordinates of center, not necessarily integer
;
;	Sigma_x, Sigma_y:	Standard deviations of gaussian along principal axes
;
; OPTIONAL INPUTS:
;	Angle:	Position angle (radians) of the x- principal axis with respect
;		to the horizontal axis of the array reference frame.
;		The default is Angle = 0., i.e. the gaussian's principal axes are
;		parallel to the array edges
;
; OUTPUTS:
;	Result:	2D floating-point array
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

FUNCTION gaussian2d, x_size, y_size, x_center, y_center, sigma_x, sigma_y, angle

	on_error, 2
	; check inputs
	s = round([x_size, y_size])
	c = [x_center, y_center]
	sigma = [sigma_x, sigma_y]
	if  n_elements(angle) eq 0  then  a = 0.  else  a = angle
	; check sigma's
	min_sigma = 1e-2
	if  min(sigma) lt min_sigma  then begin
	   gaussian = fltarr(s[0], s[1])
	   gaussian[round(c[0]), round(c[1])] = 1
	   return, gaussian
	endif
	w = sqrt(2)*sigma
	if  w[0] eq w[1]  then  a = 0.	; a circular gaussian has no position angle
	; define arrays of x- and y- centered coordinates
	x = findgen(s[0]) - c[0]  &  y = findgen(s[1]) - c[1]
	x = temporary(x) # make_array(s[1], VALUE = 1)
	y = make_array(s[0], VALUE = 1) # temporary(y)
	; compute x- gaussian
	if  a ne 0  then $
	   z = x * cos(a) + y * sin(a)  else  z = x
	gaussian = exp(-(z/w[0])^2)
	; multiply by y- gaussian
	if  a ne 0  then $
	   z = -x * sin(a) + y * cos(a)  else  z = y
	gaussian = temporary(gaussian) * exp(-(z/w[1])^2)
	return, gaussian
end
