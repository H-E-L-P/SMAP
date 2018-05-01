; $Id: extend_shift.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	EXTEND_SHIFT
;
; PURPOSE:
;	Shift a 2D array without edge effects.
;
; CATEGORY:
;	Array manipulation.
;
; CALLING SEQUENCE:
;	Result = EXTEND_SHIFT(Array, Xshift, Yshift)
;
; INPUTS:
;	Array:	2D array
;
;	Xshift, Yshift:	X- and Y- integer shifts
;
; OUTPUTS:
;	Result:	Shifted array
;
; RESTRICTIONS:
;	Apply only to 2D arrays.
;
; PROCEDURE:
;	Pad the array with 0s, perform the shift and restore
;	the original size.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

FUNCTION extend_shift, array, xshift, yshift

	on_error, 2
	if  xshift eq 0 and yshift eq 0  then  return, array
	s = size52(array, /DIM)  &  xs = round(xshift)  &  ys = round(yshift)
	a = extend_array(array, s[0] + abs(xs), s[1] + abs(ys), /NO_OFF)
	a = (shift(a, xs, ys))[0:s[0]-1,0:s[1]-1]
	return, a
end
