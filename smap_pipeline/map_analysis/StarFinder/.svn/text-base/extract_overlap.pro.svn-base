; $Id: extract_overlap.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	EXTRACT_OVERLAP
;
; PURPOSE:
;	Extract the overlap region of two 2D arrays, after ideally superposing
;	the positions of a reference point.
;
; CATEGORY:
;	Array manipulation.
;
; CALLING SEQUENCE:
;	EXTRACT_OVERLAP, Array1, Array2, R1, R2, Over1, Over2, $
;					 Lx1, Ux1, Ly1, Uy1, Lx2, Ux2, Ly2, Uy2
;
; INPUTS:
;	Array1, Array2:	2D input arrays
;
;	R1:	2-components vector, coordinates of reference point in array 1
;
;	R2:	2-components vector, coordinates of reference point in array 1
;
; OUTPUTS:
;	Over1, Over2:	Regions of overlap
;
; OPTIONAL OUTPUTS:
;	Lx1, Ux1, Ly1, Uy1:	Lower and Upper, X- and Y- bounds of overlap region
;		in Array1
;	Lx2, Ux2, Ly2, Uy2:	Lower and Upper, X- and Y- bounds of overlap region
;		in Array2
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

PRO extract_overlap, array1, array2, r1, r2, over1, over2, $
					 lx1, ux1, ly1, uy1, lx2, ux2, ly2, uy2

	on_error, 2
	array_overlap, size52(array1, /DIM), size52(array2, /DIM), $
				   r1, r2, lx1, ux1, ly1, uy1, lx2, ux2, ly2, uy2
	over1 = array1[lx1:ux1,ly1:uy1]  &  over2 = array2[lx2:ux2,ly2:uy2]
	return
end
