; $Id: frequency.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	FREQUENCY
;
; PURPOSE:
;	Return a 1D vector of Fourier frequencies, according to how IDL
;	orders the components of the Fourier Transform of an array.
;
; CATEGORY:
;	Mathematics. Transforms.
;
; CALLING SEQUENCE:
;	Result = FREQUENCY(N)
;
; INPUTS:
;	N:	number of frequencies to compute
;
; OUTPUTS:
;	Return a n-elements vector of floating-point frequencies.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

FUNCTION frequency, n

	f = fltarr(n)  &  nv2 = n/2
	f[0:nv2] = findgen(nv2 + 1)
	if  n gt 2  then $
	if  n mod 2 eq 0  then $
	   f[nv2+1:n-1] = -reverse(findgen(nv2-1)) - 1  else $
	   f[nv2+1:n-1] = -reverse(findgen(nv2)) - 1
  	return, f
end
