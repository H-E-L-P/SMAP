; $Id: spline_interp.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	SPLINE_INTERP
;
; PURPOSE:
;	Given the coefficients of a 2D spline, as returned by SPLINE_COEFF,
;	evaluate the spline on a grid of points.
;
; CATEGORY:
;	Mathematics. Interpolation.
;
; CALLING SEQUENCE:
;	Result = SPLINE_INTERP(C, X_knots, Y_knots, X, Y)
;
; INPUTS:
;	C:	2D array of spline coefficients, returned by SPLINE_COEFF
;
;	X_knots, Y_knots:	Vector of spline knots, returned by SPLINE_COEFF
;
;	X, Y:	Coordinates of points where the spline function must be
;		evaluated. The following conditions must be hold:
;		MIN(X_knots) <= X <= MAX(X_knots)
;		MIN(Y_knots) <= Y <= MAX(Y_knots)
;		If some X or Y does not fulfills these conditions, it is replaced
;		by the corresponding bound on X_knots and Y_knots
;
; KEYWORD PARAMETERS:
;	DEGREE:	Degree of spline. It must be the same as that used in the call
;		to SPLINE_COEFF. The default value (strongly recommended) is
;		DEGREE = 3.
;
; OUTPUTS:
;	Result:	2D array of spline evaluations, of size Nx*Ny, where Nx and Ny
;		are the number of elements in the input vectors X and Y
;
; RESTRICTIONS:
;	Apply only to 2D data.
;
; PROCEDURE:
;	Apply the procedures described in
;	Paul Dierckx, "Curve and surface fitting with splines",
;		Clarendon Press, Oxford (1995)
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;	Adapted in IDL from the sofware FITPACK, written in FORTRAN by P.Dierckx.
;	Complete references may be found in
;	Paul Dierckx, "Curve and surface fitting with splines",
;		Clarendon Press, Oxford (1995)
;-

FUNCTION spline_interp, c, x_knots, y_knots, x, y, DEGREE = degree

	on_error, 2
	if  n_elements(degree) eq 0  then  degree = 3
	x_points = (x > min( x_knots)) < max(x_knots)
	y_points = (y > min( y_knots)) < max(y_knots)
	b_splines, x_points, x_knots, degree, bx, kx, /BOUNDS, /FULL
	b_splines, y_points, y_knots, degree, by, ky, /BOUNDS, /FULL
											; delta_t = 2.7
	return, transpose(bx) # c # by		; delta_t = 336
			; NOTE: the array implementation is faster than the
			; component-wise by a factor of 2 for an array 128*128
end
