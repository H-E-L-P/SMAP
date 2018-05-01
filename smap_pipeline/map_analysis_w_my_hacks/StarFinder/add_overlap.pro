; $Id: add_overlap.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	ADD_OVERLAP
;
; PURPOSE:
;	Find the overlap region of two 2D arrays, after ideally superposing the
;	relative positions of a reference point, and add the overlap region of
;	the second array to the overlap region of the first one.
;
; CATEGORY:
;	Array manipulation.
;
; CALLING SEQUENCE:
;	ADD_OVERLAP, Array1, Array2, R1, R2
;
; INPUTS:
;	Array1, Array2:	Input arrays
;
;	R1:	2-components vector of coordinates of reference point in Array1
;
;	R2:	2-components vector of coordinates of reference point in Array2
;
; OUTPUTS:
;	Array1:	Input Array1 + overlap region of Array2
;
; SIDE EFFECTS:
;	Array1 is overwritten.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

PRO add_overlap, array1, array2, r1, r2

	on_error, 2
	array_overlap, size52(array1, /DIM), size52(array2, /DIM), r1, r2, $
   				   lx1, ux1, ly1, uy1, lx2, ux2, ly2, uy2
	array1[lx1,ly1] = array1[lx1:ux1,ly1:uy1] + array2[lx2:ux2,ly2:uy2]
	return
end
