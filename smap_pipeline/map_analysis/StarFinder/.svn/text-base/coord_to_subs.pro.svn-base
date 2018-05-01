; $Id: coord_to_subs.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	COORD_TO_SUBS
;
; PURPOSE:
;	Convert pixel coordinates in a 2D array to array subscripts,
;	to access the array in memory address order.
;
; CATEGORY:
;	Array manipulation.
;
; CALLING SEQUENCE:
;	Result = COORD_TO_SUBS(X, Y, N_columns)
;
; INPUTS:
;	X, Y:	Coordinates of pixels
;
;	N_columns:	Number of columns in the 2D array
;
; OUTPUTS:
;	Result:	Long integer vector of array subscripts
;
; RESTRICTIONS:
;	Apply only to 2D arrays.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

FUNCTION coord_to_subs, x, y, n_columns

	on_error, 2
	return, round(y) * n_columns + round(x)
end