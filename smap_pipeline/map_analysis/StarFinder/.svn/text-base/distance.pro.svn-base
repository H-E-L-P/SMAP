; $Id: distance.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	DISTANCE
;
; PURPOSE:
;	Compute the euclidean distance of a set of points on a plane from a
;	fixed origin. Distances are computed in floating-point aritmethic.
;
; CATEGORY:
;	Mathematics.
;
; CALLING SEQUENCE:
;	Result = DISTANCE(X0, Y0, X, Y)
;
; INPUTS:
;	X0, Y0:	Couple of scalars, representing the position of the origin
;
;	X, Y:	Vectors of coordinates
;
; OUTPUTS:
;	Result:	Vector of euclidean distances, with the same size as X and Y
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

FUNCTION distance, x0, y0, x, y

	on_error, 2
	return, sqrt((x - float(x0[0]))^2 + (y - float(y0[0]))^2)
end
