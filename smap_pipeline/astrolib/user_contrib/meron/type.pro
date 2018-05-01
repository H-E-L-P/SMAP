Function Type, x

;+
; NAME:
;	TYPE
; VERSION:
;	3.3
; PURPOSE:
;	Finds the type class of a variable.
; CATEGORY:
;	Programming.
; CALLING SEQUENCE:
;	Result = TYPE(X)
; INPUTS:
;    X
;	Arbitrary, doesn't even need to be defined.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;	None.
; OUTPUTS:
;	Returns the type of X as a long integer, in the (0,9) range.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Extracts information from the SIZE function.
; MODIFICATION HISTORY:
;	Created 15-JUL-1991 by Mati Meron.
;-

    dum = size(x)
    return, dum(dum(0) + 1)
end
