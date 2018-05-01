; $Id: sub_array.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	SUB_ARRAY
;
; PURPOSE:
;	Given a 2D array, extract a sub-array centered at a pre-fixed position
;	and of given size.
;
; CATEGORY:
;	Array manipulation.
;
; CALLING SEQUENCE:
;	Result = SUB_ARRAY(Array, Bx, By)
;
; INPUTS:
;	Array:	2D array
;
;	Bx:	X- size of sub-array to extract
;
; OPTIONAL INPUTS:
;	By:	Y- size of sub-array. The default is By = Bx
;
; KEYWORD PARAMETERS:
;	REFERENCE:	2-components vector, coordinates of pixel where the
;		sub-array must be centered. The default is the Array maximum.
;
; OUTPUTS:
;	Result:	Sub-array of size Bx*By and having the reference pixel specified
;		by the keyword R at the relative position (Bx/2, By/2).
;		If it is not possible to extract such a sub-array, the extracted
;		sub-array may not have the reference pixel at (Bx/2, By/2) or its
;		size may be different from Bx*By.
;
; OPTIONAL OUTPUTS:
;	LX, UX, LY, UY:	Output keywords containing the Lower and Upper,
;		X- and Y- bounds of the sub-array in Array
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

FUNCTION sub_array, array, bx, by, REFERENCE = r, $
					LX = lx, UX = ux, LY = ly, UY = uy

	on_error, 2
	if  n_params() eq 2  then  by = bx
	if  n_elements(r) ne 2  then  r = get_max(array)
	bsize = round([bx, by])
	array_overlap, size52(array, /DIM), bsize, $
				   round(r), bsize/2, lx, ux, ly, uy
	return, array[lx:ux,ly:uy]
end
