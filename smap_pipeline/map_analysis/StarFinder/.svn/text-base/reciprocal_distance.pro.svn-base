; $Id: reciprocal_distance.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	RECIPROCAL_DISTANCE
;
; PURPOSE:
;	Compute the euclidean distance for each couple of points
;	in a set of given points on a plane.
;
; CATEGORY:
;	Mathematics.
;
; CALLING SEQUENCE:
;	Result = RECIPROCAL_DISTANCE(X, Y)
;
; INPUTS:
;	X, Y:	Coordinates of N points
;
; OUTPUTS:
;	Result:	(N-1)*(N-1) floating-point array of reciprocal distances.
;		The elements of the array are defined as follows:
;		Result[j,*] = vector of distances between the j-th point and
;		              the others, for j = 0, ..., N - 2.
;		Result is 0 if N = 1. It is set to a negative scalar if X and Y
;		have different size.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

FUNCTION reciprocal_distance, x, y

	on_error, 2
	n = n_elements(x)
	if  n_elements(y) ne n  then  return, -1
	if  n eq 1  then  return, 0
	d = fltarr(n - 1, n - 1)  &  s = lindgen(n)
	for  i = 0L, n - 2  do begin
	   other = (shift(s, -i))[1:n-1]  &  other = shift(other, +i)
	   d[*,i] = distance(x[i], y[i], x[other], y[other])
	endfor
	return, transpose(d)
end
