; $Id: search_objects.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	SEARCH_OBJECTS
;
; PURPOSE:
;	Search objects in a given image as relative maxima above a pre-fixed
;	threshold. The image can be suitably smoothed to reduce the number
;	of spurious detections, due to noise spikes.
;
; CATEGORY:
;	Signal processing.
;
; CALLING SEQUENCE:
;	SEARCH_OBJECTS, Image, Threshold, N, X, Y, I
;
; INPUTS:
;	Image:	2D array to search
;
;	Threshold:	Scalar or array threshold for detection.
;		It must be of the same type as Image.
;
; KEYWORD PARAMETERS:
;	LOW_SURFACE:	2D array, with the same size as Image, containing the
;		background emission. If this keyword is defined, a lower detection
;		surface is defined, as the sum of the background emission and the
;		detection Threshold. In this case, Threshold may suitably represent
;		a multiple of the noise standard deviation.
;
;	PRE_SMOOTH:	Set this keyword to smooth the image before searching for
;		relative maxima.
;
;	MINIF:	Integer resampling factor, used to minify the Image with the
;		IDL function REBIN. This will smooth many noise spikes.
;		This keyword has effect only if PRE_SMOOTH is set.
;		If PRE_SMOOTH is set but MINIF is undefined, a standard smoothing
;		is applied, by convolving the input Image with the low-pass filter
;		   1  2  1
;		   2  4  2
;		   1  2  1
;		Notice that when the keyword BACKGROUND is defined, the same type of
;		smoothing is applied to the lower detection surface.
;
;	FOUR:	Set this keyword to identify relative maxima as pixels brigher
;		than their 4-neighbors. The default is to use 8-neighbors.
;
; OUTPUTS:
;	N:	Number of detected objects
;
;	X, Y:	Coordinates of objects
;
;	I:	Central intensity of detected objects in the background-removed
;		Image. If the keyword BACKGROUND is undefined, this output variable
;		represent the intensity of the objects in the input Image.
;		The detected objects are sorted in order of decreasing intensity
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

PRO search_objects, image, LOW_SURFACE = background, threshold, $
	                PRE_SMOOTH = pre_smooth, MINIF = minif, $
	                _EXTRA = extra, n, x, y, i

	on_error, 2
	ima = image
	; Define lower surface for detection
	if  n_elements(background) eq 0  then  background = 0
	low = background + threshold
	; Smooth image and lower surface
	if  keyword_set(pre_smooth)  then $
	   if  n_elements(minif) ne 0  then begin
	      ima = resample(ima, minif)
	      if  size52(low, /N_DIM) eq 2  then $
	         low = resample(low, minif)
	   endif else begin
	      mask = float([[1, 2, 1], [2, 4, 2], [1, 2, 1]])
	      mask = mask / total(mask)
	      ima = convol(ima, mask, /EDGE_TRUNCATE)
	      if  size52(low, /N_DIM) eq 2  then $
	         low = convol(low, mask, /EDGE_TRUNCATE)
	   endelse
	; Search image maxima above lower surface
	all_max, ima, x, y, n, _EXTRA = extra
	if  n eq 0  then  return
	if  size52(low, /N_DIM) eq 2  then $
	   w = where(ima[x, y] gt low[x, y], n)  else $
	   w = where(ima[x, y] gt low, n)
	if  n eq 0  then  return
	x = x[w]  &  y = y[w]
	if  keyword_set(pre_smooth)  then $
	   if  n_elements(minif) ne 0  then begin
	      x = x * round(minif)  &  y = y * round(minif)
	   endif
	i = (image - background)[x,y]
	sorted = reverse(sort(i))
	x = x[sorted]  &  y = y[sorted]  &  i = i[sorted]
	return
end
