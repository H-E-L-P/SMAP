Function Cast, x, low, high, fix = fix

;+
; NAME:
;	CAST
; VERSION:
;	3.3
; PURPOSE:
;	Generalized type casting.  Converts variables whose type code is out 
;	of the range [LOW,HIGH] into this range.
; CATEGORY:
;	Programming (type conversion).
; CALLING SEQUENCE:
;	Result = CAST( X, [LOW [,HIGH]])
; INPUTS:
;    X
;	Numerical, arbitrary, or a character representation of a number(s).
;    LOW
;	Number representing a type code, range (1:9).  If greater than 9, it is
;	set to 9.  If less then 1, or not given, it is set to 1.
; OPTIONAL INPUT PARAMETERS:
;    HIGH
;	Type code, same as LOW.  Default value is 9.  If provided and less then
;	LOW, it is set to LOW.
; KEYWORD PARAMETERS:
;    /FIX
;	Switch.  If set, the output is filtered through FPU_FIX, eliminating
;	floating underflow errors.
; OUTPUTS:
;	If the type of X is < LOW, CAST returns X converted to type LOW.
;	If the type of X is > HIGH, CAST returns X converted to type HIGH.
;	Otherwise CAST returns X.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	1)  An attempt to convert a string which is NOT a character 
;	    representation of a number into a numeric type will yield error.
;	2)  X cannot be a structure or pointer, but can be a structure element.
;	3)  The value 8 for either LOW or HIGH is not allowed (since it 
;	    corresponds to structure type).  Value of 10 and above is ignored.
; PROCEDURE:
;	Identifies the type of X, and if out of the range given by [LOW,HIGH]
;	calls the proper conversion routine using the system routine 
;	CALL_FUNCTION.  Also uses FPU_FIX and ISNUM from MIDL.
; MODIFICATION HISTORY:
;	Created 25-DEC-1991 by Mati Meron.
;	Modified 15-JUN-1995 by Mati Meron to accept the new DOUBLECOMPLEX type.
;	Modified 25-SEP-1998 by Mati Meron.  Underflow filtering added.
;-

    on_error, 1
    conv = ['nada', 'byte', 'fix', 'long', 'float', 'double', 'complex', $
	    'string', 'nonap', 'dcomplex']
    if n_elements(low)  eq 0 then ilo = 1 else ilo = 1   > fix(low)  < 9
    if n_elements(high) eq 0 then ihi = 9 else ihi = ilo > fix(high) < 9

    inum = Isnum(x, type = ityp) or ityp eq 7
    if ilo eq 8 or ihi eq 8 or not inum then message, "Can't do that!" else $
    if ityp lt ilo then res = call_function(conv(ilo),x) else $
    if ityp gt ihi then res = call_function(conv(ihi),x) else res = x

    if keyword_set(fix) then return, FPU_fix(res) else return, res
end
