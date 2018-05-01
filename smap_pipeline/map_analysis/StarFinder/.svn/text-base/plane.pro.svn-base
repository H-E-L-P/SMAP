; $Id: plane.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	PLANE
;
; PURPOSE:
;	Compute a 3D plane, defined as
;	P(x, y) = C + Cx * x + Cy * y
;
; CATEGORY:
;	Models.
;
; CALLING SEQUENCE:
;	Result = PLANE(C, Cx, Cy, Sx, Sy)
;
; INPUTS:
;	C, Cx, Cy:	Coefficients of the plane
;
;	Sx:	First size of the output array
;
; OPTIONAL INPUTS:
;	Sy:	Second size of the output array. The default is Sy = Sx
;
; OUTPUTS:
;	Result:	2D floating-point array, containing the values of the plane
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

FUNCTION plane, c, cx, cy, sx, sy

	on_error, 2
	if  n_params() eq 4  then  sy = sx
	p = c + cx * (findgen(sx) # (fltarr(sy) + 1))
	p = temporary(p) + cy * ((fltarr(sx) + 1) # findgen(sy))
	return, p
end
