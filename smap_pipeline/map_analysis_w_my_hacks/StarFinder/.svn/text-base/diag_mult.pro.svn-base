; $Id: diag_mult.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	DIAG_MULT
;
; PURPOSE:
;	Compute the array multiplication of a square array and a diagonal
;	array, represented by a 1D vector containing its main diagonal.
;
; CATEGORY:
;	Array manipulation.
;
; CALLING SEQUENCE:
;	Result = DIAG_MULT(A, D)
;
; INPUTS:
;	A:	Square array
;
;	D:	1D vector, representing main diagonal of diagonal array
;
; KEYWORD PARAMETERS:
;	PREMULT:	Set this keyword to a nonzero value to pre-multiply A by D.
;		The default is to post-multiply.
;
; OUTPUTS:
;	Return product array, having the same size as A.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

FUNCTION diag_mult, a, d, PREMULT = premult

	on_error, 2
	size_a = (size52(a, /DIM))[0]
	b = a  &  if  keyword_set(premult)  then  b = transpose(b)
	for  j = 0L, size_a - 1  do  b[*,j] = b[*,j] * d
	if  keyword_set(premult)  then  b = transpose(b)
	return, b
end
