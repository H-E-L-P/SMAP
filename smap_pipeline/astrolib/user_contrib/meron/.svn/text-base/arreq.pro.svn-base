Function Arreq, arr1, arr2, warn = wn, novalue = nov

;+
; NAME:
;	ARREQ
; VERSION:
;	3.3
; PURPOSE:
;	Compares arrays for equality.  The arrays qualify as equal if:
;	    1) They are of the same general type (num., char., or struct.).
;	    2) Number of dimensions is the same.
;	    3) Size of each dimension is the same.
;	    4) Respective elements are equal.
; CATEGORY:
;	Mathematical Function (general).
; CALLING SEQUENCE:
;	Result = ARREQ( ARR1, ARR2 [, keywords])
; INPUTS:
;    ARR1, ARR2
;	Arrays, type and number of dimensions arbitrary.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    /WARN
;	Switch. If set, a warning message is issued for incompatible data types.
;    /NOVALUE
;	Switch.  If set, only number of elements and structure are compared.
; OUTPUTS:
;	Returns 1 if the arrays are equal, 0 otherwise.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Uses the SIZE function and ISNUM from MIDL to obtain information about 
;	the arrays.  Compares, in order, number of dimensions, size of each 
;	dimension, data types, and (unless NOVALUE is set) individual elements.
; MODIFICATION HISTORY:
;	Created 15-JUL-1991 by Mati Meron.
;	Modified 30-AUG-1998 by Mati Meron.  Scope broadened to pointer arrays.
;-

    fsiz = size(arr1)
    ssiz = size(arr2)
    if fsiz(0) eq ssiz(0) then ndim = fsiz(0) else return, 0b
    for i = 1, ndim do if fsiz(i) ne ssiz(i) then return, 0b
    fnum = Isnum(arr1, type = ftyp)
    snum = Isnum(arr2, type = styp)
    if not ((fnum and snum) or (ftyp eq styp)) then begin
	if keyword_set(wn) then message, 'Incompatible data types!', /continue
	return, 0b
    endif

    if keyword_set(nov) then return, 1b else return, min(arr1 eq arr2)
end
