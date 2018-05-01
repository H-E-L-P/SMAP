; $Id: get_max.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	GET_MAX
;
; PURPOSE:
;	Find coordinates of maximum intensity pixel(s) in a 2D array.
;
; CATEGORY:
;	Array manipulation.
;
; CALLING SEQUENCE:
;	Result = GET_MAX(Array)
;
; INPUTS:
;	Array:	2D array
;
; KEYWORD PARAMETERS:
;	ALL:	If this keyword is set, all the pixels with maximum intensity
;		are returned. Otherwise only the first pixel (according to how IDL
;		access arrays in memory address order) is returned.
;
; OUTPUTS:
;	Result:	If ALL is not set, Result is a 2-components long integer vector,
;		with the column and row coordinates of the maximum intensity pixel.
;		If ALL is set, Result is a N*2 long integer array, where N (number of
;		columns) is the number of maximum intensity pixels: the first row of
;		Result will contain the x- coordinates of the pixels, the second row
;		of Result will contain the y- coordinates. It N = 1 and ALL is set,
;		Result is however a 2-components vector.
;		A negative scalar is always returned if Array has not 2 dimensions.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

FUNCTION get_max, array, ALL = all

	on_error, 2
	s = size52(array)
	if  s[0] ne 2  then  return, -1
	if  keyword_set(all)  then $
	   index = where(array ge max(array))  else  m = max(array, index)
	subs_to_coord, index, s[1], x, y
	if  n_elements(x) eq 1  then $
	   p = [x[0], y[0]]  else  p = transpose([transpose(x), transpose(y)])
	return, p
end
