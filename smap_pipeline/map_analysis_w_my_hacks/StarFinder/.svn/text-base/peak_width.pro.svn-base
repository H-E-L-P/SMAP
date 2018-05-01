; $Id: peak_width.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	PEAK_WIDTH
;
; PURPOSE:
;	Estimate the width of a peak in a given image.
;
; CATEGORY:
;	Signal processing.
;
; CALLING SEQUENCE:
;	Result = PEAK_WIDTH(Image)
;
; INPUTS:
;	Image:	Image containing the peak whose width must be measured
;
; KEYWORD PARAMETERS:
;	X, Y:	Coordinates of the peak. The default is the Image absolute maximum
;
;	ABS_THRESHOLD:	Threshold above which the peak must be measured.
;		No default value is fixed
;
;	REL_THRESHOLD:	Relative threshold with respect to the peak value,
;		used to fix the threshold above which the peak must be measured.
;		The default is REL_THRESHOLD = 0.5, i.e. the width is estimated at
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
;	Result:	Width of the Image component connected to the peak and above the
;		fixed threshold.
;
; RESTRICTIONS:
;	The input Image is supposed to have been background-subtracted.
;
; PROCEDURE:
;	Call PEAK_AREA to estimate the area of the peak. Estimate the width
;	as the diameter of a circle having the same section of the peak.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

FUNCTION peak_width, image, _EXTRA = extra

	on_error, 2
	return, sqrt(4 * peak_area(image, _EXTRA = extra) / !pi)
end
