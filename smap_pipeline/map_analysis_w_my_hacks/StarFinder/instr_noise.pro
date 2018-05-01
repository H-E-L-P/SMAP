; $Id: instr_noise, v 1.0 May 2000 e.d. $
;
;+
; NAME:
;	INSTR_NOISE
;
; PURPOSE:
;	Evaluate the overall standard deviation of the normally-distributed noise
;	in an image, given by the sum or the mean of many exposures.
;	Normally-distributed noise sources are read-out, dark current, thermal
;	background and sky background.
;	Most of these sources are instrumental.
;
; CATEGORY:
;	Signal processing.
;
; CALLING SEQUENCE:
;	Result = INSTR_NOISE(Ron, Dark, Therm, Sky, El_per_adu, Nexp)
;
; INPUTS:
;	Ron:	Read-out-noise standard deviation in electron (el.) units
;
;	Dark:	Dark current level in el.
;
;	Therm:	Thermal background in el.
;
;	Sky:	Sky background in el.
;
;	El_per_adu:	Conversion constant from el. to ADU
;		(No. of ADU = No. of el. / El_per_adu)
;
;	Nexp:	Number of exposures (with the same exposure time)
;
; KEYWORD PARAMETERS:
;	AVG:	Set this keyword to specify that the image is the average of
;		Nexp exposures. If AVG is not set, the routine assumes the image
;		is the sum of Nexp exposures.
;
; OUTPUTS:
;	Result:	Scalar, representing the noise standard deviation in ADU
;
; MODIFICATION HISTORY:
;	Written by: Emiliano Diolaiti, May 2000.
;-


FUNCTION instr_noise, ron, dark, therm, sky, el_per_adu, nexp, AVG = avg

	on_error, 2
	norm = 1.  &  if  keyword_set(avg)  then  norm = 1. / nexp
	return, norm / el_per_adu * sqrt(nexp * (float(ron)^2 + dark + therm + sky))
end
