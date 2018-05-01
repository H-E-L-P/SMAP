; $Id: find_rot_trans.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	FIND_ROT_TRANS
;
; PURPOSE:
;	Given two sets of coordinates representing the same points in different
;	reference frames, supposed to be reciprocally translated and rotated,
;	estimate the reciprocal translation and rotation by means of the
;	Newton-Gauss iterative method.
;
; CATEGORY:
;	Mathematics. Spatial transformations.
;
; CALLING SEQUENCE:
;	FIND_ROT_TRANS, X1, Y1, X2, Y2, Origin_0, Angle_0, Origin, Angle, Found
;
; INPUTS:
;	X1, Y1:	Vectors of x- and y- coordinates of points in
;		reference frame no. 1
;
;	X2, Y2:	Vectors of x- and y- coordinates of points in
;		reference frame no. 2
;
;	Origin_0:	2-components vector representing an initial guess of the
;		coordinates of the origin of reference frame 2 with respect to
;		reference frame 1
;
;	Angle_0:	Scalar value representing an initial guess of the rotation
;		angle (in radians) of the x- axis of ref. frame 2 with respect to
;		the x- axis of ref. frame 1
;
; KEYWORD PARAMETERS:
;	ORIGIN_TOL:	Absolute tolerance (presumably in pixel units) to check the
;		convergence condition of the origin in the iterative estimation.
;		The default is 0.1
;
;	ANGLE_TOL:	Relative tolerance to check the convergence of the angle.
;		The default is 0.01 (i.e. 1%)
;
;	_EXTRA:	Optional keywords for NEWTON_GAUSS (see the NEWTON_GAUSS procedure
;		in the file 'newton_gauss.pro')
;
; OUTPUTS:
;	Origin:	2-components vector, representing the x- and y- coordinates of
;		the origin of reference frame 2 in reference frame 1
;
;	Angle:	Scalar, angle (in radians) between the x- axis of reference
;		frame 2 and the x- axis of reference frame 1
;
;	Found:	Logical value, true if the Newton-Gauss optimization has converged
;
; RESTRICTIONS:
;	If the reciprocal translation and/or rotation between the two reference
;	frames are large, it is better to provide initial guesses of the
;	corresponding parameters, to ensure the convergence of the Newton-Gauss
;	algorithm.
;
; PROCEDURE:
;	Assume the following relationship between coordinates in ref. frame 2
;	and coordinates in ref. frame 1:
;	X2 = - Origin[0] + X1 * COS(Angle) + Y1 * SIN(Angle)
;	Y2 = - Origin[1] - X2 * SIN(Angle) + Y2 * COS(Angle)
;	and estimate Origin and Angle by means of the iterative Newton-Gauss
;	algorithm.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-



;;; Auxiliary procedures/functions.

FUNCTION frt_parametrize, origin, angle
	return, [origin[0], origin[1], angle]
end

PRO frt_deparametrize, p, origin, angle
	origin = p[0:1]  &  angle = p[2]
	return
end

FUNCTION frt_stack, x, y

	on_error, 2
	n = n_elements(x)  &  z = fltarr(2*n)
	sx = 2 * lindgen(n)  &  sy = sx + 1
	z[sx] = x  &  z[sy] = y
	return, z
end

FUNCTION frt_model, p, DATA = data

	on_error, 2
	frt_deparametrize, p, origin, angle
	rot_trans, data.x, data.y, origin, angle, rtx, rty
	return, frt_stack(rtx, rty)
end

FUNCTION frt_iacobi, p, DATA = data

	on_error, 2
	n = n_elements(data.x)  &  iacobi = fltarr(2*n, 3)
	one = make_array(n, /FLOAT, VALUE = 1.)  &  zero = fltarr(n)
	iacobi[*,0] = frt_stack(-one, zero)
	iacobi[*,1] = frt_stack(zero, -one)
	frt_deparametrize, p, origin, angle  &  origin = [0, 0]
	rot_trans, data.x, data.y, origin, angle, rtx, rty
	iacobi[*,2] = frt_stack(rty, -rtx)
	return, transpose(iacobi)
end

FUNCTION frt_converg, p0, p1, DATA = data

	on_error, 2
	frt_deparametrize, p0, o0, a0
	frt_deparametrize, p1, o1, a1
	return, convergence(o0, o1, data.o_tol, /ABSOLUTE) and $
			convergence(a0, a1, data.a_tol)
end

;;; The main routine.

PRO find_rot_trans, x1, y1, x2, y2, origin_0, angle_0, origin, angle, found, $
					ORIGIN_TOL = o_tol, ANGLE_TOL = a_tol, _EXTRA = extra

	on_error, 2
	if  n_elements(x1) lt 2  then $
	   message, 'at least 2 points are required'
	if  n_elements(o_tol) eq 0  then  o_tol = 0.1
	if  n_elements(a_tol) eq 0  then  a_tol = 0.01
	; define "global" data structure.
	data = {x: x1, y: y1, o_tol: o_tol, a_tol: a_tol}
	; parameters estimation
	newton_gauss, "frt_model", "frt_iacobi", "frt_converg", $
				  frt_parametrize(origin_0, angle_0), frt_stack(x2, y2), $
				  DATA = data, _EXTRA = extra, found, p
	if  found  then  frt_deparametrize, p, origin, angle
	return
end
