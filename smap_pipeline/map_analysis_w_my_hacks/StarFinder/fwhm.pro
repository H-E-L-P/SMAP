; $Id: fwhm.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	FWHM
;
; PURPOSE:
;	Compute the FWHM (Full Width at Half Maximum) of a peak in an image.
;	The FWHM is estimated as the diameter of a circle having the same area
;	as the peak.
;
; CATEGORY:
;	Signal processing.
;
; CALLING SEQUENCE:
;	Result = FWHM(Array)
;
; INPUTS:
;	Array:	Image containing the peak to be measured
;
; KEYWORD PARAMETERS:
;	X, Y:	Coordinates of the peak.
;		The default is the Image absolute maximum
;
;	MAG:	Integer magnification factor for array magnification. It may be
;		useful to improve the accuracy of the initial measurement, based on
;		the area of the peak.
;		The default is MAG = 1, i.e. no magnification.
;
;	CUBIC:	Set this keyword to a nonzero value to magnify the image with
;		the cubic convolution interpolation implemented in the library
;		routine CONGRID. This keyword has effect only if MAG is defined.
;		If CUBIC is not set, the array magnification, when required, is
;		performed with REBIN (bilinear interpolation).
;
; OUTPUTS:
;	Result:	FWHM of the peak.
;
; RESTRICTIONS:
;	1) The input Array is supposed to have been background-subtracted and
;	centered with sub-pixel accuracy, especially if the FWHM is a few
;	pixels.
;	2) The results with the default settings are not much accurate in general.
;	The following calling sequence is recommended:
;	Result = FWHM(Array, MAG = 3, /CUBIC).
;	3) To compute the FWHM of a peak in a large array, use PEAK_FWHM.
;	For more details see the routine PEAK_FWHM in the file 'peak_fwhm.pro'.
;
; PROCEDURE:
;	Call PEAK_WIDTH to obtain an initial guess of the FWHM, based on
;	the area of the peak thresholded at half maximum.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

FUNCTION fwhm, array, X = x0, Y = y0, MAG = mag_fac, CUBIC = cubic

	on_error, 2
	; magnify array
	if  n_elements(mag_fac) eq 0  then  mag_fac = 1
	mag = round(mag_fac) > 1
	a = array  &  siz = size52(a, /DIM) * mag
	if  mag gt 1  then $
	   if  keyword_set(cubic)  then $
	      a = congrid(a, siz[0], siz[1], CUBIC = -0.5)  else $
	      a = rebin(a, siz[0], siz[1])
	; define peak position
	if  n_elements(x0) * n_elements(y0) eq 0  then begin
	   m = get_max(a)  &  x = m[0]  &  y = m[1]
	endif else begin
	   x = round(x0) * mag  &  y = round(y0) * mag
	endelse
	; estimate FWHM
	fw = peak_width(a, X = x, Y = y)
	if  mag gt 1  then  fw = float(fw) / mag
	return, fw
end
