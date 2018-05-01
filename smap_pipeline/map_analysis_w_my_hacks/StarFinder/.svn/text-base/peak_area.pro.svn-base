; $Id: peak_area.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	PEAK_AREA
;
; PURPOSE:
;	Estimate the area of a peak in a given image.
;
; CATEGORY:
;	Signal processing.
;
; CALLING SEQUENCE:
;	Result = PEAK_AREA(Image)
;
; INPUTS:
;	Image:	Image containing the peak whose area has to be estimated
;
; KEYWORD PARAMETERS:
;	X, Y:	Coordinates of the peak. The default is the Image absolute maximum
;
;	ABS_THRESHOLD:	Threshold above which the area must be estimated.
;		No default value is fixed
;
;	REL_THRESHOLD:	Relative threshold with respect to the peak value,
;		used to fix the threshold above which the area must be estimated.
;		The default is REL_THRESHOLD = 0.5, i.e. the area is estimated at
;		half maximum. REL_THRESHOLD is neglected when ABS_THRESHOLD is
;		defined and is used by default when ABS_THRESHOLD is undefined
;
;	MAG:	Integer magnification factor for array magnification. It may be
;		useful to improve the accuracy of the measurement. The default is
;		MAG = 1, i.e. no magnification.
;
;	CUBIC:	Set this keyword to a nonzero value to magnify the image with
;		the cubic convolution interpolation implemented in the library
;		routine CONGRID. This keyword has effect only if MAG is defined.
;		If CUBIC is not set, the array magnification is performed with REBIN
;		(bilinear interpolation).
;
; OUTPUTS:
;	Result:	Area of the Image component connected to the peak and above the
;		fixed threshold. The type of the result is long-integer if MAG = 1,
;		float if MAG > 1.
;		Return 0 if an error occurs in the extraction of the connected
;		component.
;
; RESTRICTIONS:
;	The input Image is supposed to have been background-subtracted.
;
; PROCEDURE:
;	Magnify the Image if requested, threshold it with BINARY_ARRAY and use
;	IMAGE_CORE to identify the central connected component of the resulting
;	binary array. The area of the central connected component is an estimate
;	of the area of the peak.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

FUNCTION peak_area, image, X = x, Y = y, MAG = mag_fac, CUBIC = cubic, $
   					ABS_THRESHOLD = at, REL_THRESHOLD = rt

	on_error, 2
	if  n_elements(mag_fac) eq 0  then  mag_fac = 1
	mag = round(mag_fac) > 1
	if  n_elements(x) * n_elements(y) eq 0  then begin
	   m = get_max(image)  &  x = m[0]  &  y = m[1]
	endif
	; Image magnification
	siz = size52(image, /DIM) * mag  &  ima = image
	if  mag gt 1  then $
	   if  keyword_set(cubic)  then $
	      ima = congrid(ima, siz[0], siz[1], CUBIC = -0.5)  else $
	      ima = rebin(ima, siz[0], siz[1])
	xm = round(x) * mag  &  ym = round(y) * mag
	; Thresholding and central component extraction
	if  n_elements(at) ne 0  then  threshold = at $
	else begin
	   if  n_elements(rt) eq 0  then  rt = 0.5
	   threshold = rt * ima[xm,ym]
	endelse
	ima = float(binary_array(ima, threshold))
	ima = image_core(ima, 0.5, X = xm, Y = ym, error_flag)
	if  error_flag  then  ima = 0.
	; Compute area
	area = total(ima) / float(mag)^2
	if  mag eq 1  then  area = round(area)
	return, area
end
