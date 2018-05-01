; $Id: photon_noise, v 1.1 Aug 2000 e.d. $
;
;+
; NAME:
;	PHOTON_NOISE
;
; PURPOSE:
;	Evaluate the standard deviation of the photon noise in an image
;	given by the sum or the mean of many exposures.
;
; CATEGORY:
;	Signal processing.
;
; CALLING SEQUENCE:
;	Result = PHOTON_NOISE(Data, El_per_adu, Nexp)
;
; INPUTS:
;	Data:	Array of data affected by photon noise
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
;	Result:	Array with the same size as the input Data, representing the
;		photon noise standard deviation in ADU
;
; MODIFICATION HISTORY:
;	Written by: Emiliano Diolaiti, May 2000.
;	1) Set to 0 negative data before computing SQRT!
;	   (Emiliano Diolaiti, August 2000).
;-


FUNCTION photon_noise, data, el_per_adu, nexp, AVG = avg

	on_error, 2
	norm = 1.  &  if  keyword_set(avg)  then  norm = 1. / nexp
	return, sqrt(norm / el_per_adu * (data > 0))
end
