; $Id: centroid.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	CENTROID
;
; PURPOSE:
;	Compute the centroid of a 2D array, defined as the "center of mass"
;	of a 2D intensity distribution.
;
; CATEGORY:
;	Mathematics.
;
; CALLING SEQUENCE:
;	Result = CENTROID(Array)
;
; INPUTS:
;	Array:	2D array
;
; OUTPUTS:
;	Result:	2-components floating point vector, containing the
;		coordinates of the centroid
;
; RESTRICTIONS:
;	Apply only to 2D arrays.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

FUNCTION centroid, array

	on_error, 2
	s = size(array, /DIM)  &  sx = s[0]  &  sy = s[1]
	x = findgen(sx)  &  x_one = make_array(sx, VALUE = 1)
	y = findgen(sy)  &  y_one = make_array(sy, VALUE = 1)
	tot_array = total(array)
	xc = total(array * (x # y_one)) / tot_array
	yc = total(array * (x_one # y)) / tot_array
	return, [xc, yc]
end
