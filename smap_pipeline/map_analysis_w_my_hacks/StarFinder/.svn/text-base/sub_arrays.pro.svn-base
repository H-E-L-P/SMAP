; $Id: sub_arrays.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	SUB_ARRAYS
;
; PURPOSE:
;	Given a 2D array, extract a stack of sub-arrays centered at specified
;	positions and having a fixed size.
;	Sub-arrays corresponding to positions next to the array border are
;	suitably padded with 0s.
;
; CATEGORY:
;	Array manipulation.
;
; CALLING SEQUENCE:
;	SUB_ARRAYS, Array, X, Y, Boxsize, Stack, Masks
;
; INPUTS:
;	Array:	2D array
;
;	X, Y:	Coordinates of reference positions
;
;	Boxsize:	Scalar, representing the size of each square sub-array
;
; OUTPUTS:
;	Stack:	3D array of size Boxsize*Boxsize*N, where N is the number
;		of reference points. Each reference point is ensured to be at
;		the position (Boxsize/2, Boxsize/2) in the corresponding plane;
;		of the output Stack
;
;	Masks:	Cube of byte type, having the same size as the previous Stack.
;		The n-th plane in this cube is defined as follows:
;		Masks[j, i, n] = 1, if Stack[j, i, n] is an Array pixel
;		               = 0, if Stack[j, i, n] is a padding pixel
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

PRO sub_arrays, array, x, y, boxsize, stack, masks

	on_error, 2
	nframes = n_elements(x)
	stack = fltarr(boxsize, boxsize, nframes)
	masks = bytarr(boxsize, boxsize, nframes)
	for  n = 0L, nframes - 1  do begin
	   box = sub_array(array, boxsize, REF = [x[n], y[n]], $
	   				   LX = lx, UX = ux, LY = ly, UY = uy)
	   offset = [boxsize/2, boxsize/2] - [x[n] - lx, y[n] - ly]
	   box = extend_array(box, boxsize, /NO_OFF)
	   box = shift(box, offset[0], offset[1])
	   stack[*,*,n] = box
	   lo = offset  &  up = lo + [ux - lx, uy - ly]
	   masks[lo[0]:up[0],lo[1]:up[1],n] = 1B
	endfor
	return
end
