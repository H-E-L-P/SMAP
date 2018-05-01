Function Univec, r

;+
; NAME:
;	UNIVEC
; VERSION:
;	3.3
; PURPOSE:
;	Normalizes a vector.
; CATEGORY:
;	Geometry, general.
; CALLING SEQUENCE:
;	Result = UNIVEC (R)
; INPUTS:
;    R
;	Vector, numeric, otherwise arbitrary.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;	None.
; OUTPUTS:
;	Returns a vector in the same direction as R, normalized to unity.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	A zero vector will cause error.
; PROCEDURE:
;	Straightforward.  Uses FPU_FIX and VNORM from MIDL.
;	Works also with complex vectors.
; MODIFICATION HISTORY:
;	Created 30-JUN-1992 by Mati Meron.
;	Added to MIDL 5-NOV-1997 by Mati Meron.
;	Modified 10-SEP-1998 by Mati Meron.  Underflow filtering added.
;-

    on_error, 1
    rnorm = Vnorm(r)
    if rnorm eq 0 then message, 'Zero vector, cannot normalize!' $
    else return, FPU_fix(r/rnorm)
end
