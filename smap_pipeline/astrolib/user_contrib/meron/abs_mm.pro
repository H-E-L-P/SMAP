Function Abs_mm, x

;+
; NAME:
;	ABS_MM
; VERSION:
;	3.3
; PURPOSE:
;	Calculates absolute values.  A temporary fix needed since the IDL ABS 
;	function fails with very large or very small complex numbers.
; CATEGORY:
;	Mathematical, general.
; CALLING SEQUENCE:
;	Result = ABS_MM (X)
; INPUTS:
;    X
;	Numerical, otherwise arbitrary.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;	None.
; OUTPUTS:
;	Returns the absolute value of the input.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Straightforward.  Calling FPU_FIX and ISNUM from MIDL.
; MODIFICATION HISTORY:
;	Created 15-MAR-1996 by Mati Meron as M_ABS.
;	Modified 30-AUG-1998 by Mati Meron.  Underflow filtering added.
;	Renamed 25-SEP-1999 by Mati Meron, to ABS_MM.
;-

    on_error, 1
    if Isnum(x, /complex, type = xtyp) then begin
	res = make_array(size = size([x]), type = xtyp/2 + 1)
	w = where (x ne 0, nw)
	if nw gt 0 then begin
	    tem = abs(double(x(w))) > abs(imaginary(x(w)))
	    res(w) = tem*abs(complex(double(x(w))/tem,imaginary(x(w))/tem))
	endif
    endif else res = abs(x)
    res = Fpu_fix(res,/no_abs)

    if (size(x))(0) eq 0 then return, res(0) else return, res
end
