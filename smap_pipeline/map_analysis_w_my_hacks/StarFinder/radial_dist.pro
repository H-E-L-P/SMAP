; $Id: radial_dist.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	RADIAL_DIST
;
; PURPOSE:
;	Compute a 2D array of radial distances from an origin.
;
; CATEGORY:
;	Models.
;
; CALLING SEQUENCE:
;	Result = RADIAL_DIST(X_size, Y_size, X_center, Y_center)
;
; INPUTS:
;	X_size, Y_size:	X- and y- size of output array
;
;	X_center, Y_center:	Coordinates of origin, not necessarily integer
;
; OUTPUTS:
;	Result:	2D array of radial distances from (X_center, Y_center)
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

FUNCTION radial_dist, x_size, y_size, x_center, y_center

	on_error, 2
	z = findgen(x_size) - x_center  &  aux = make_array(y_size, VALUE = 1)
	z = temporary(z) # aux
	d = z^2
	z = findgen(y_size) - y_center  &  aux = make_array(x_size, VALUE = 1)
	z = aux # temporary(z)
	d = temporary(d) + z^2
	d = sqrt(temporary(d))
	return, d
end
