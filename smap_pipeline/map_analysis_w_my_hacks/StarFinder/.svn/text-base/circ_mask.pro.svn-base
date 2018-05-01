; $Id: circ_mask.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	CIRC_MASK
;
; PURPOSE:
;	Apply a circular mask to a 2D array, setting to a pre-fixed value all
;	the pixels whose distance from a reference position is either greater
;	or smaller equal than a specified threshold.
;
; CATEGORY:
;	Signal processing.
;
; CALLING SEQUENCE:
;	Result = CIRC_MASK(Array, X0, Y0, R0)
;
; INPUTS:
;	Array:	2D array to mask
;
;	X0, Y0:	Coordinates of center of circular mask
;
;	R0:	Radius of circular mask in pixels
;
; KEYWORD PARAMETERS:
;	INNER:	Set this keyword to a nonzero value to mask the pixels within
;		a distance R0 from (X0, Y0): the boundary is included (i.e. masked).
;		The default is to mask the pixels outside that distance (in this
;		case the boundary is excluded, i.e. not masked).
;
;	VALUE:	Use this value to replace masked pixels. The default is VALUE = 0
;
; OUTPUTS:
;	Result:	Array with region defined by circular mask set to the value
;		defined by the keyword VALUE.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

FUNCTION circ_mask, array, x0, y0, r0, INNER = inner, VALUE = v

	on_error, 2
	siz = size52(array, /DIM)
	r = radial_dist(siz[0], siz[1], x0, y0)
	if  keyword_set(inner)  then $
	   w = where(r le r0, npix)  else  w = where(r gt r0, npix)
	if  n_elements(v) eq 0  then  v = 0
	circ = array  &  if  npix ne 0  then  circ[w] = v
	return, circ
end
