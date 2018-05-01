Function Abgrad, arr, delta

;+
; NAME:
;	ABGRAD
; VERSION:
;	3.3
; PURPOSE:
;	Calculates the absolute value of the gradient of a function represented
;	as an array of values.  Works for 1-7 dimensions.
; CATEGORY:
;	Array Manipulation.
; CALLING SEQUENCE:
;	Result = ABGRAD( ARR [, DELTA])
; INPUTS:
;    ARR
;	Array, numeric, number of dimensions can be 1 through 7.
; OPTIONAL INPUT PARAMETERS:
;    DELTA
;	Size of step used to calculate the numeric derivatives.  The approx.
;	partial derivative in the i-th direction is calculated as
;	    (ARR(...,I + DELTA,...) - ARR(...,I - DELTA,...))/(2*DELTA).
;	The default value of DELTA is 1l.  If provided, it is rounded to a long
;	integer on input.
; KEYWORD PARAMETERS:
;	None.
; OUTPUTS:
;	Returns the absolute value of the gradient as an array of the same size
;	and type as ARR.  If ARR is not an array, returns 0.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	Due to current limitations of the MAKE_ARRAY system routine, 8 
;	dimensions are not allowed.
; PROCEDURE:
;	Creates an 7-dimensional array, with dummy leading dimensions, 
;	containing the original array.  Generates the differences using the
;	SHIFT system routine and strips the dummy dimensions at the end.
;	Uses the functions DEFAULT and FPU_FIX from MIDL.
; MODIFICATION HISTORY:
;	Created 15-NOV-1991 by Mati Meron.
;	Modified 30-AUG-1998 by Mati Meron.  Underflow filtering added.
;-

    ndmx = 7
    idel = abs(Default(delta,1l,/dtype))
    siz = size(arr)
    ndm = siz(0)
    if ndm eq 0 then begin
	message, 'Not an Array!', /continue
	return, 0
    endif else if ndm lt ndmx then siz = [ndmx,replicate(1,ndmx-ndm),siz(1:*)]
    res = make_array(size = siz)
    tarr = make_array(size = siz) + arr

    a = intarr(ndmx)
    for i = ndmx - ndm, ndmx - 1 do begin
	a(i) = idel
	b = - a
	if 2*idel mod siz(i+1) ne 0 then res = res + $
	abs(shift(tarr,a(0),a(1),a(2),a(3),a(4),a(5),a(6)) - $
	shift(tarr,b(0),b(1),b(2),b(3),b(4),b(5),b(6)))^2
	a(i) = 0
    endfor

    res = Fpu_fix(res)
    return, sqrt(reform(res,siz(ndmx-ndm+1:ndmx)))/(2.*idel)
end
