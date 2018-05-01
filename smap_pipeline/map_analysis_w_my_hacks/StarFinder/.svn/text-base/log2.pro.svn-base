; $Id: log2.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	LOG2
;
; PURPOSE:
;	Compute the base 2 logarithm of an integer number.
;
; CATEGORY:
;	Mathematics.
;
; CALLING SEQUENCE:
;	Result = LOG2(N)
;
; INPUTS:
;	N:	Integer number
;
; OUTPUTS:
;	Result:	Base 2 logarithm of N
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

FUNCTION log2, n

	on_error, 2
	if  n/2 le 1  then  l = 1  else  l = 1 + log2(n/2)
	return, l
end


