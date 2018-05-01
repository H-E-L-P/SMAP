Function Isnum, x, double = doub, complex = comp, type = typ

;+
; NAME:
;	ISNUM
; VERSION:
;	3.3
; PURPOSE:
;	Checks whether the input is a number.
; CATEGORY:
;	Programming.
; CALLING SEQUENCE:
;	Result = ISNUM(X)
; INPUTS:
;    X
;	Arbitrary, doesn't even have to exist.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    /DOUBLE
;	Switch.  If set the result is 1 only if X is DOUBLE or DCOMPLEX.
;    /COMPLEX
;	Switch.  If set the result is 1 only if X is COMPLEX or DCOMPLEX.
;    TYPE
;	Optional output.  See below.
; OUTPUTS:
;	Returns 1 if X is number, 0 otherwise.  Output type is byte.
; OPTIONAL OUTPUT PARAMETERS:
;    TYPE
;	The name of the variable to receive the numeric code of the type of X.
;	Included for convenience to save an additional call to TYPE.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Straightforward.  Using TYPE from MIDL.
; MODIFICATION HISTORY:
;	Created 15-JUN-1995 by Mati Meron.
;	Modified 5-MAY-1996 by Mati Meron.  Added keywords DOUBLE, COMPLEX and
;	TYPE.
;-

    numtyps = [1,2,3,4,5,6,9]
    typ = Type(x)
    res = (where(numtyps eq typ))(0) ge 0
    if keyword_set(doub) then res = res and (typ eq 5 or typ eq 9)
    if keyword_set(comp) then res = res and (typ eq 6 or typ eq 9)

    return, res
end
