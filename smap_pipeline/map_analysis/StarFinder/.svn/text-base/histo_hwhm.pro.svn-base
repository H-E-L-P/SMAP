; $Id: histo_hwhm.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	HISTO_HWHM
;
; PURPOSE:
;	Compute the Half Width at Half Maximum (HWHM) of an histogram.
;
; CATEGORY:
;	Mathematics. Statistics.
;
; CALLING SEQUENCE:
;	Result = HISTO_HWHM(H, Increment, Mode)
;
; INPUTS:
;	H:	Histogram
;
;	Increment:	+1 to compute the right tail HWHM, -1 to compute the left
;		tail HWHM. If Increment is different from +/-1, the value -1 is
;		used by default
;
; OUTPUTS:
;	Return the (left- or right- tail) HWHM of the histogram, expressed
;	in bin units.
;
; OPTIONAL OUTPUTS:
;	Mode:	subscript of the most populated bin in the input histogram.
;
; RESTRICTIONS:
;	The algorithm assumes there are no secondary maxima in the histogram
;	within a distance comparable to the HWHM from the most populated bin.
;	Otherwise the estimated HWHM may be overestimated.
;
; PROCEDURE:
;	Starting from the most populated bin, move leftwards (rightwards)
;	until a bin with intensity equal or smaller than half the intensity
;	of the initial bin is found. The distance (in bin units) between this
;	bin and the initial one is an estimate of the histogram HWHM.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

FUNCTION histo_hwhm, h, increment, mode

	on_error, 2
	if  abs(increment) ne 1  then  incr = -1  else  incr = increment
	s = reverse(sort(h))  &  mode = s[0]  &  threshold = 0.5 * h[mode]
	l = mode  &  l_min = 0  &  l_max = n_elements(h) - 1
	while  h[l] gt threshold and l gt l_min and l lt l_max  do  l = l + incr
	l = (l - incr) > l_min < l_max
	return, abs(mode - l)
end
