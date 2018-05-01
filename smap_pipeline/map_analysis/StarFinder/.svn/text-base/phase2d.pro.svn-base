; $Id: phase2d.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	PHASE2D
;
; PURPOSE:
;	Compute the phase array for a 2D delta-function.
;
; CATEGORY:
;	Mathematics. Transforms.
;
; CALLING SEQUENCE:
;	Result = PHASE2D(X, Y, Sx, Sy)
;
; INPUTS:
;	X, Y:	x- and y- position of delta-function (not necessarily integer
;		coordinates)
;	Sx, Sy:	x- and y- size of phase array
;
; KEYWORD PARAMETERS:
;	DOUBLE_P:	Set this keyword to compute a double precision phase array
;
; OUTPUTS:
;	Return a 2D complex phase array.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

FUNCTION phase2d, x, y, sx, sy, DOUBLE_P = double_p

	on_error, 2
	if  keyword_set(double_p)  then  pi = !Dpi  else  pi = !pi
	arg = frequency(sx) * 2 * pi / sx
	arg_u = arg * x
	if  sy ne sx  then  arg = frequency(sy) * 2 * pi / sy
	arg_v = arg * y
	if  keyword_set(double_p)  then begin
	   uphase = dcomplex(cos(arg_u),-sin(arg_u))
	   vphase = dcomplex(cos(arg_v),-sin(arg_v))
	endif else begin
	   uphase = complex(cos(arg_u),-sin(arg_u))
	   vphase = complex(cos(arg_v),-sin(arg_v))
	endelse
	return, uphase # vphase
end
