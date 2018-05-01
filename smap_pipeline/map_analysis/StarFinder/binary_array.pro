; $Id: binary_array.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	BINARY_ARRAY
;
; PURPOSE:
;	Transform an array to binary by thresholding.
;
; CATEGORY:
;	Signal processing.
;
; CALLING SEQUENCE:
;	Result = BINARY_ARRAY(Array, Threshold)
;
; INPUTS:
;	Array:	Input array to be thresholded
;
;	Threshold:	Scalar value or 2D array with the same size as Array.
;
; OUTPUTS:
;	Result:	Byte array, defined as follows:
;		Result[j,i] = 1B, if Array[j,i] >= Threshold
;		            = 0B, otherwise
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

FUNCTION binary_array, array, threshold

	on_error, 2
	siz = size52(array, /DIM)  &  b = bytarr(siz[0], siz[1])
	w = where(array ge threshold, n)  &  if  n ne 0  then  b[w] = 1
	return, b
end
