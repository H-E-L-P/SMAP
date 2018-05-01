; $Id: airy_pattern.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	AIRY_PATTERN
;
; PURPOSE:
;	Compute Airy pattern.
;
; CATEGORY:
;	Models.
;
; CALLING SEQUENCE:
;	Result = AIRY_PATTERN(X_size, Y_size, X_center, Y_center, Sampling_factor)
;
; INPUTS:
;	X_size, Y_size:	X- and y_ size of output array
;
;	X_center, Y_center:	Coordinates of center, not necessarily integer
;
; OPTIONAL INPUTS:
;	Sampling_factor:	Ratio of actual sampling factor to critical sampling
;		step for the optical system producing the Airy pattern.
;		The default is Sampling_factor = 1, i.e. critical sampling
;
; OUTPUTS:
;	Result:	2D array containing Airy pattern normalized to maximum = 1
;
; PROCEDURE:
;	Compute Airy pattern as defined in
;	Born, Wolf, "Principles of Optics", Pergamon Press, 2nd revised edition.
;	Suitably adjust sampling step.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

FUNCTION airy_pattern, x_size, y_size, x_center, y_center, sampling_factor

	if  n_elements(sampling_factor) eq 0  then $
	   sampling_factor = 1			; critical sampling
	scale = !pi / 2
	; define 2D array of radial distances
	r = radial_dist(x_size, y_size, x_center, y_center)
	r = temporary(r) * scale * sampling_factor
	; compute diffraction pattern
	w = where(r ne 0)
	d = r  &  d[w] = 2 * (beselj(r, 1))[w] / r[w]
	w = where(r eq 0, n)  &  if  n ne 0  then  d[w] = 1
	d = temporary(d)^2
	return, d
end