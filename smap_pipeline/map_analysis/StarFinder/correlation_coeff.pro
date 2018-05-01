; $Id: correlation_coeff.pro, v 1.0 Aug 1999 e.d. $
;+
; NAME:
;	CORRELATION_COEFF
;
; PURPOSE:
;	Compute the correlation coefficient of a pattern with a reference template.
;
; CATEGORY:
;	Image statistics.
;
; CALLING SEQUENCE:
;	Results = CORRELATION_COEFF(Pattern, Template)
;
; INPUTS:
;	Pattern:	Pattern to compare with the template

;	Template:	Reference template
;
; KEYWORD PARAMETERS:
;	EXCLUDE:	Vector of array subscripts identifying data points
;		which must be excluded from the computation of the correlation
;
; OUTPUTS:
;	Return the correlation coefficient between Pattern and Template.
;
; RESTRICTIONS:
;	The input data Pattern and Template must be equally sized arrays
;	(with any number of dimensions).
;
; PROCEDURE:
;	Compute the correlation coefficient according to the formula in
;		Gonzalez, Woods, "Digital Image Processing",
;		Addison-Wesley (1992), p. 584'
;	Optionally exclude 'bad pixels' from the computation
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

FUNCTION correlation_coeff, pattern, template, EXCLUDE = ex

	on_error, 2
	; find acceptable data points
	n = n_elements(pattern)  &  s = lindgen(n)
	if  n_elements(ex) ne 0  then begin
	   w = where(ex ge min(s) and ex le max(s), n)
	   if  n ne 0  then  e = ex[w]
	endif
	if  n_elements(e) ne 0  then begin
	   s[e] = -1  &  w = where(s ge 0, n)
	   if  n ne 0  then  s = s[w]
	endif
	if  n eq 0  then  return, 0.
	; compute correlation coefficient
	p = pattern[s]   &  p = p - mean(p)
	t = template[s]  &  t = t - mean(t)
	return, total(p*t) / sqrt(total(p*p) * total(t*t))
end
