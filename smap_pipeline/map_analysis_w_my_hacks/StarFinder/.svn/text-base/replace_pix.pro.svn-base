; $Id: replace_pix, v 1.0 Sep 1999 e.d. $
;
;+
; NAME:
;	REPLACE_PIX
;
; PURPOSE:
;	Given a 2D array and a set of "bad" pixels, replace each bad data
;	point with the median of the "good" data in a suitable box around.
;	The box size is adjusted in order to have a minimum number of good
;	points to compute the median.
;
; CATEGORY:
;	Signal processing.
;
; CALLING SEQUENCE:
;	Result = REPLACE_PIX(Data, X, Y)
;
; INPUTS:
;	Data:	2D data array
;
;	X, Y:	Coordinates of pixels to replace
;
; KEYWORD PARAMETERS:
;	BOXSIZE:	Initial value of the box size to compute the local median.
;
;	NGOOD_MIN:	Minimum number of good points in the box to compute the
;		local median. The default is 3.
;
;	BOX_INCR:	If the box of size BOXSIZE centered on a bad pixel does
;		not contain the minimum number of good points specified by the
;		keyword NGOOD_MIN, its size is iteratively increased by the amount
;		BOX_INCR until the condition is fulfilled. The default is 2.
;
;	MAXIT:	Maximum number of iterations to increase the box size.
;		The default is 5.
;
; OUTPUTS:
;	Result:	Array with replaced pixels
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, September 1999.
;-

FUNCTION replace_pix, data, x, y, BOXSIZE = boxsize, NGOOD_MIN = ngood_min, $
					  BOX_INCR = box_incr, MAXIT = maxit

	on_error, 2
	if  n_elements(boxsize) eq 0  then  boxsize = 3L
	if  n_elements(ngood_min) eq 0  then  ngood_min = 3
	if  n_elements(box_incr) eq 0  then  box_incr = 2
	if  n_elements(maxit) eq 0  then  maxit = 5
	data_out = data
	npix = n_elements(x)
	for  n = 0L, npix - 1  do begin
	   ; Adjust box size to have enough good points
	   incr = 0L  &  it = 0L
	   repeat begin
	      box = sub_array(data, boxsize + incr, REF = [x[n],y[n]], $
	   				      LX = lx, UX = ux, LY = ly, UY = uy)
	   	  w = where(x ge lx and x le ux and y ge ly and y le uy, nbad)
	   	  it = it + 1  &  incr = incr + box_incr
	   	  large_enough = (n_elements(box) - nbad) ge ngood_min or it eq maxit
	   endrep until  large_enough
	   ; Replace n-th bad pixel with median of good points around
	   bin = make_array(ux - lx + 1, uy - ly + 1, VALUE = 1B, /BYTE)
	   bin[x[w] - lx, y[w] - ly] = 0
	   w = where(bin eq 1, ngood)
	   if  ngood ne 0  then  data_out[x[n],y[n]] = median(box[w], /EVEN)
	endfor
	return, data_out
end
