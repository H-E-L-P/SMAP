; $Id: subs_to_coord.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	SUBS_TO_COORD
;
; PURPOSE:
;	Convert array subscripts to pixel coordinates.
;
; CATEGORY:
;	Array manipulation.
;
; CALLING SEQUENCE:
;	SUBS_TO_COORD, Subs, N_columns, X, Y
;
; INPUTS:
;	Subs:	Long integer vector of array subscripts
;
;	N_columns:	Number of columns in the 2D array
;
; OUTPUTS:
;	X, Y:	Column and row coordinates of pixels subscripted by Subs
;
; RESTRICTIONS:
;	Apply only to 2D arrays.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

PRO subs_to_coord, subs, n_columns, x, y

	on_error, 2
	s = round(subs)  &  n = round(n_columns)
	x = s mod n  &  y = s / n
	return
end
