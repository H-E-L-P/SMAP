;+
;NAME
; array_shuffle
;PURPOSE
; Random array reordering (with no repeats) using a Fisher-Yates
; shuffle.
;USAGE
; wreord = array_shuffle(seed,nelem)
;INPUTS
; seed      Random number generator seed
; nelem     Number of elements in array
;RETURNS
; Index to randomly re-order array of specified length.
;MODIFICATION HISTORY
; Author: Alex Conley, Apr 2009
;-

FUNCTION array_shuffle, seed, nelem

COMPILE_OPT IDL2, STRICTARRSUBS

IF nelem LE 0 THEN RETURN,-1

idxarr = INDGEN(nelem)
FOR i=nelem-1,0,-1 DO BEGIN
   k = FIX(i*RANDOMU(seed))
   temp = idxarr[i]
   idxarr[i] = idxarr[k]
   idxarr[k] = temp
ENDFOR

RETURN,idxarr

END
