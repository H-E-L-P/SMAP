; $Id: all_max.pro, v 1.1 Sep 2001 e.d. $
;
;+
; NAME:
;	ALL_MAX
;
; PURPOSE:
;	Find relative maxima in a 2D array.
;	A given pixel is considered a relative maximum if it is brighter
;	than its 8-neighbors or 4-neighbors.
;
; CATEGORY:
;	Signal processing.
;
; CALLING SEQUENCE:
;	ALL_MAX, Array, X, Y, N
;
; INPUTS:
;	Array:	2D array to be searched
;
; KEYWORD PARAMETERS:
;	BOX:	Size of sub-regions where the local maxima are defined.
;		The default is 3, i.e. each returned peak is the relative
;		maximum in a 3x3 sub-array.
;
;	FOUR:	Set this keyword to identify relative maxima as pixels
;		brighter than their 4-neighbors. The default is to use
;		8-neighbors.
;
; OUTPUTS:
;	X, Y:	Coordinates of detected maxima
;
;	N:	Number of detected maxima
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;	1) Added BOX keyword (Emiliano Diolaiti, September 2001).
;-

PRO all_max, array, x, y, n, BOX = box, FOUR = four

	on_error, 2
	if  n_elements(box) eq 0  then  box  = 3
	siz = size52(array, /DIM)  &  sx = siz[0]  &  sy = siz[1]
	xedge = box/2  &  yedge = box/2
	ext_array = extend_array(array, sx + 2*xedge, sy + 2*yedge)
	m = make_array(sx + 2*xedge, sy + 2*yedge, /BYTE, VALUE = 1B)
	for  dx = -box/2, box/2  do  for  dy = -box/2, box/2  do begin
	   if  keyword_set(four)  then $
	      check = abs(dx) ne abs(dy)  else $	; 4-neighbors
	      check = dx ne 0 or dy ne 0		     	; 8-neighbors
	   if  check  then $
	      m = temporary(m) and ext_array gt shift(ext_array, dx, dy)
	endfor
   w = where(m[xedge:xedge+sx-1,yedge:yedge+sy-1] eq 1, n)
   if  n ne 0  then  subs_to_coord, w, sx, x, y
   if  n eq 1  then begin
	   x = x[0]  &  y = y[0]
	endif
	return
end
