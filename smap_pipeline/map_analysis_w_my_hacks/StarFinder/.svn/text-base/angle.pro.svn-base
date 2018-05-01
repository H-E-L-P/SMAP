; $Id: angle.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	ANGLE
;
; PURPOSE:
;	Compute the position angles of a set of points on a plane with
;	respect to the horizontal axis of a reference frame passing through
;	a fixed origin. The angles are measured counter-clockwise in radians
;	and belong to the interval [0, 2*pi[.
;	The computations are performed in floating-point arithmethic.
;
; CATEGORY:
;	Mathematics.
;
; CALLING SEQUENCE:
;	Result = ANGLE(X0, Y0, X, Y)
;
; INPUTS:
;	X0, Y0:	Couple of scalars, representing coordinates of the origin
;
;	X, Y:	Coordinates of the points for which the position angle
;		must be computed
;
; OUTPUTS:
;	Result:	Array of position angles, with the same size as the input
;		arrays X and Y.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

FUNCTION angle, x0, y0, x, y

	on_error, 2
	a = float(x) - x  &  dx = x - float(x0[0])  &  dy = y - float(y0[0])
	w = where(dx eq 0 and dy ne 0, n)
	if  n ne 0  then  a[w] = !pi/2
	w = where(dx ne 0, n)
	if  n ne 0  then  a[w] = atan(dy[w] / dx[w])
	w = ((dx lt 0) or ((dx eq 0) and (dy lt 0))) and 1B
	a = a + w * !pi
	w = where(a lt 0, n)
	if  n ne 0  then  a[w] = a[w] + 2*!pi
	w = where(a eq 2*!pi, n)
	if  n ne 0  then  a[w] = 0
 	return, a
end
