; $Id: check_border.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	CHECK_BORDER
;
; PURPOSE:
;	Given two 2D arrays, ideally superpose the positions of a reference
;	point and check if the first array is larger than the second by a
;	pre-fixed edge of pixels. Resize the arrays if the condition is
;	not fulfilled.
;
; CATEGORY:
;	Array manipulation.
;
; CALLING SEQUENCE:
;	CHECK_BORDER, Array1, Array2, Pix1, Pix2, Edge, $
;   			  Array1_out, Array2_out, Pix1_out, Pix2_out
;
; INPUTS:
;	Array1, Array2:	Input 2D arrays
;
;	Pix1, Pix2:	2-components vectors, coordinates of reference point in
;		Array1 and Array2 respectively
;
;	Edge:	When the coordinates of the reference point have been matched,
;		the size of Array1 must be equal to the size of Array2 plus an
;		additional border, whose width is specified by Edge
;
; OUTPUTS:
;	Array1_out, Array2_out:	Output arrays
;
;	Pix1_out, Pix2_out:	Positions of reference points in output arrays
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

PRO check_border, array1, array2, pix1, pix2, edge, $
   				  array1_out, array2_out, pix1_out, pix2_out

	on_error, 2
	s1 = size52(array1, /DIM)  &  s2 = size52(array2, /DIM)
	l1 = [0, 0]  &  u1 = s1 - 1  &  l2 = [0, 0]  &  u2 = s2 - 1
	; resize lower bounds
	d = pix1 - (pix2 + edge)
	l1 = l1 + (d > 0)  &  pix1_out = pix1 - (d > 0)
	l2 = l2 - (d < 0)  &  pix2_out = pix2 + (d < 0)
	; resize upper bounds
	d = (s1 - pix1) - (s2 - pix2 + edge)
	u1 = u1 - (d > 0)  &  u2 = u2 + (d < 0)
	; resize arrays
	array1_out = array1  &  array2_out = array2
	if  min(u1 - l1) ge 0 and min(u2 - l2) ge 0  then begin
	   array1_out = array1_out[l1[0]:u1[0],l1[1]:u1[1]]
	   array2_out = array2_out[l2[0]:u2[0],l2[1]:u2[1]]
	endif
    return
end
