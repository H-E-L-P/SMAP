; $Id: array_overlap.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	ARRAY_OVERLAP
;
; PURPOSE:
;	Find bounds of overlap region of two 2D arrays, by ideally
;	matching the coordinates of a reference point.
;
; CATEGORY:
;	Array manipulation.
;
; CALLING SEQUENCE:
;	ARRAY_OVERLAP, Size1, Size2, R1, R2, $
;   			   Lx1, Ux1, Ly1, Uy1, Lx2, Ux2, Ly2, Uy2
;
; INPUTS:
;	Size1:	2-components vector, size of array 1, as returned by
;		size52(array1, /DIM)
;
;	Size2:	2-components vector, size of array 2
;
;	R1:	2-components vector, coordinates of reference pixel in array 1
;
;	R2:	2-components vector, coordinates of reference pixel in array 2
;
; OUTPUTS:
;	Lx1, Ux1, Ly1, Uy1:	Lower and Upper X- and Y- bounds of intersection
;		in array 1
;
;	Lx2, Ux2, Ly2, Uy2:	Lower and Upper X- and Y- bounds of intersection
;		in array 2
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-



; INT_BOUNDS: auxiliary procedure.

PRO int_bounds, p1, p2, size1, size2, l, u

	l = (p1 - p2) > 0 < (size1 - 1)
	u = (p1 + size2 - p2 - 1) > 0 < (size1 - 1)
	return
end


PRO array_overlap, size1, size2, r1, r2, $
   				   lx1, ux1, ly1, uy1, lx2, ux2, ly2, uy2

	on_error, 2
	int_bounds, r1[0], r2[0], size1[0], size2[0], lx1, ux1
	int_bounds, r1[1], r2[1], size1[1], size2[1], ly1, uy1
	int_bounds, r2[0], r1[0], size2[0], size1[0], lx2, ux2
	int_bounds, r2[1], r1[1], size2[1], size1[1], ly2, uy2
	return
end
