; $Id: max_search.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	MAX_SEARCH
;
; PURPOSE:
;	Given a 2D array, search for relative maxima above a fixed threshold
;	and fulfilling a set of optional conditions.
;	A given pixel is considered a relative maximum if it is brighter
;	than its 8-neighbors or 4-neighbors.
;
; CATEGORY:
;	Signal processing.
;
; CALLING SEQUENCE:
;	MAX_SEARCH, Array, Threshold, N, X, Y, I
;
; INPUTS:
;	Array:	2D array to be searched
;
;	Threshold:	lower detection threshold; it may be either a scalar or
;		a 2D array, with the same size as Array
;
; KEYWORD PARAMETERS:
;	X0, Y0:	Coordinates of reference pixel, representing an origin to
;		compute distances of detected maxima.
;
;	CHECK_DIST:	Set this keyword to a scalar value representing the maximum
;		allowed distance of an acceptable local max. from the reference
;		pixel defined by X0 and Y0. This keyword has no effect if X0 and Y0
;		are undefined.
;
;	NEAREST:	Set this keyword to select the maximum lying next to the
;		reference position defined by X0 and Y0. If there are more than
;		one maximum at the same distance, return all. This keyword has no
;		effect if X0 and Y0 are undefined.
;
;	MAXIMUM:	Set this keyword to select the brightest maximum
;		in the input array. If there are more than one maximum with the
;		same value, return all.
;
;	FOUR:	Set this keyword to identify relative maxima as pixels
;		brighter than their 4-neighbors. The default is to use
;		8-neighbors.
;
;	SORTED:	Set this keyword to have the output maxima sorted by
;		decreasing intensity
;
; OUTPUTS:
;	N:	Number of detected maxima
;
;	X, Y:	Long integer coordinates of detected maxima. If no maximum is
;		found, X and Y are set to negative scalars
;
;	I:	Intensities of detected maxima. The type of I is the same as that of
;		Array. If no maximum is found, I is set to a negative scalar
;
; RESTRICTIONS:
;	The optional conditions specified by the keywords CHECK_DIST, NEAREST,
;	MAXIMUM are checked in the same order they have been presented.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

PRO max_search, array, threshold, X0 = x0, Y0 = y0, CHECK_DIST = max_dist, $
   				NEAREST = nearest, MAXIMUM = maximum, _EXTRA = extra, $
   				SORTED = sorted, n, x, y, i

	on_error, 2
	; define search options
	check_dist = n_elements(max_dist) ne 0 and $
				 n_elements(x0) ne 0 and n_elements(y0) ne 0
	select_nearest = keyword_set(nearest) and $
					 n_elements(x0) ne 0 and n_elements(y0) ne 0
	select_maximum = keyword_set(maximum)
	; find local maxima
	all_max, array, x, y, n, _EXTRA = extra
	found = n ne 0
	if  found  then  i = array[x,y]
	; select detected maxima according to options
	if  found  then begin
	   if  check_dist or select_nearest then begin
	      d = distance(x0, y0, x, y)
	      if  check_dist  then begin
	         w = where(d le max_dist, n)  &  found = n ne 0
	         if  found  then begin
	            d = d[w]  &  x = x[w]  &  y = y[w]  &  i = i[w]
	         endif
	      endif
	      if  found and select_nearest  then begin
	         w = where(d le min(d), n)  &  found = n ne 0
	         if  found  then begin
	            d = d[w]  &  x = x[w]  &  y = y[w]  &  i = i[w]
	         endif
	      endif
	   endif
	   if  found and select_maximum  then begin
	      w = where(i ge max(i))
	      x = x[w]  &  y = y[w]  &  i = i[w]
	   endif
	endif
	if  found  then begin
	   n = n_elements(i)
	   if  size52(threshold, /N_DIM) eq 2  then $
	      w = where(i gt threshold[x,y], n)  else $
	      w = where(i gt threshold[0], n)
	   if  n ne 0  then begin
	      x = x[w]  &  y = y[w]  &  i = i[w]
	   endif
	endif
	if  n eq 0  then begin
	   x = -1  &  y = -1  &  i = -1
	endif else $
	   if  keyword_set(sorted)  then begin
	      s = reverse(sort(i))  &  x = x[s]  &  y = y[s]  &  i = i[s]
	   endif
	return
end
