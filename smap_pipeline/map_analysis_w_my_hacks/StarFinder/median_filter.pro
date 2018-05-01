; $Id: median_filter.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	MEDIAN_FILTER
;
; PURPOSE:
;	Apply median smoothing to a 2D array, using the intrinsic function
;	MEDIAN. In the inner portion of the array, the result is the same as
;	that of MEDIAN. At the array edge, perform a median smoothing with
;	reduced box size.
;
; CATEGORY:
;	Signal processing.
;
; CALLING SEQUENCE:
;	Result = MEDIAN_FILTER(Array, Box)
;
; INPUTS:
;	Array:	2D array to smooth
;
;	Box:	Box size for smoothing
;
; KEYWORD PARAMETERS:
;	EVEN:	If the EVEN keyword is set when Array contains an even number
;		of points (i.e. there is no middle number), MEDIAN returns the
;		average of the two middle numbers.
;
; OUTPUTS:
;	Result:	2D median smoothed array.
;		Return the input array if an error occurs.
;
; RESTRICTIONS:
;	Apply only to 2D arrays.
;
; PROCEDURE:
;	Apply MEDIAN on the inner part of the array. At the edge perform
;	smoothing on an asymmetric box of reduced size.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

FUNCTION median_filter, array, box, _EXTRA = extra

	catch, error
	if  error ne 0  then  return, array
	s = size52(array, /DIM)  &  sx = s[0]  &  sy = s[1]
	b = round(box) > 1
	j = lindgen(sx)  &  i = lindgen(sy)  &  w = b / 2
	lo_j = (j - w) > 0  &  up_j = (j + w) < (sx - 1)
	lo_i = (i - w) > 0  &  up_i = (i + w) < (sy - 1)
	filtered = median(array, 2*w + 1, _EXTRA = extra)
	for  i = 0, w - 1  do  for  j = 0, sx - 1  do $
	   filtered[j,i] = median(array[ lo_j[j]:up_j[j],lo_i[i]:up_i[i] ], _EXTRA = extra)
	for  i = sy - w, sy - 1  do  for  j = 0, sx - 1  do $
	   filtered[j,i] = median(array[ lo_j[j]:up_j[j],lo_i[i]:up_i[i] ], _EXTRA = extra)
	for  i = w, sy - w - 1  do  for  j = 0, w - 1  do $
	   filtered[j,i] = median(array[ lo_j[j]:up_j[j],lo_i[i]:up_i[i] ], _EXTRA = extra)
	for  i = w, sy - w - 1  do  for  j = sx - w, sx - 1  do $
	   filtered[j,i] = median(array[ lo_j[j]:up_j[j],lo_i[i]:up_i[i] ], _EXTRA = extra)
	return, filtered
end
