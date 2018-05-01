; $Id: center_array.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	CENTER_ARRAY
;
; PURPOSE:
;	Shift a 2D array in order to put its maximum at the specified position.
;
; CATEGORY:
;	Array manipulation.
;
; CALLING SEQUENCE:
;	Result = CENTER_ARRAY(Array, X, Y)
;
; INPUTS:
;	Array:	2D array to shift
;
;	X, Y:	X- and y- final coordinates of the Array maximum
;
; KEYWORD PARAMETERS:
;	NO_EXTEND:	Set this keyword to a nonzero value to indicate that
;		the Array must be extended before being shifted. The original
;		size is restored after the shift. Use this keyword to prevent
;		circular shift effects.
;
; OUTPUTS:
;	Result:	Shifted array
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

FUNCTION center_array, image, x, y, NO_EXTEND = no_extend

	on_error, 2
	s = size52(image, /DIM)
	if  n_elements(x) * n_elements(y) eq 0  then begin
	   x = s[0]/2  &  y = s[1]/2
	endif
	m = get_max(image)  &  xs = x - m[0]  &  ys = y - m[1]
	if  xs eq 0 and ys eq 0  then  return, image
	if  keyword_set(no_extend)  then $
	   return, shift(image, xs, ys)  else $
	   return, extend_shift(image, xs, ys)
end
