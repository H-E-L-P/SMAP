; $Id: peak_fwhm.pro, v 1.0 Sep 1999 e.d. $
;
;+
; NAME:
;	PEAK_FWHM
;
; PURPOSE:
;	Compute the FWHM (Full Width at Half Maximum) of a peak in an image.
;
; CATEGORY:
;	Signal processing.
;
; CALLING SEQUENCE:
;	Result = PEAK_FWHM(Array)
;
; INPUTS:
;	Same as FWHM (see the file 'fwhm.pro')
;
; KEYWORD PARAMETERS:
;	Same as FWHM (see the file 'fwhm.pro')
;
; OUTPUTS:
;	Same as FWHM (see the file 'fwhm.pro')
;
; RESTRICTIONS:
;	Same as FWHM (see the file 'fwhm.pro')
;
; PROCEDURE:
;	Fix a guess of the FWHM. Extract a sub-array centered on the peak
;	of size equal to 3 times the guess of the FWHM. Compute again the
;	FWHM and repeat until the size of the sub-array is greater or equal
;	to 3 times the current FWHM.
;	Compute the final estimate of the FWHM applying all the detailed
;	input options supported by the routine FWHM.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, September 1999.
;-

FUNCTION peak_fwhm, array, X = x, Y = y, _EXTRA = extra

	on_error, 2
	if  n_elements(x) * n_elements(y) ne 0  then $
	   ref = round([x, y])  else  ref = get_max(array)
	fw = 3.
	repeat begin
	   siz = round(3 * fw)
	   peak = sub_array(array, siz, REF = ref)
	   fw = fwhm(peak)
	endrep until  siz ge round(3 * fw)
	return, fwhm(peak, _EXTRA = extra)
end
