; $Id: scale_ls_sys.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	SCALE_LS_SYS
;
; PURPOSE:
;	Scale a linear system of normal equations in order to improve the
;	eigenvalue ratio.
;
; CATEGORY:
;	Mathematics. Linear systems.
;
; CALLING SEQUENCE:
;	SCALE_LS_SYS, A, B, A_scaled, B_scaled, Scaling
;
; INPUTS:
;	A:	Linear system matrix. It must be symmetric
;
;	B:	Right-hand side term of linear system
;
; OPTIONAL INPUTS
;	Scaling:	Vector of scaling factors released on output in a
;		previous call. Used with the keyword NOCOMP indicates that
;		the scaling factors must not be computed again: nly the
;		scaling operations must be performed
;
; KEYWORD PARAMETERS:
;	NOCOMP:	Set this keyword to a nonzero value to indicate that
;		the scaling factors must not be computed because they are
;		being passed on input as Scaling
;
; OUTPUTS:
;	A_scaled:	Scaled matrix. Its diagonal elements are equal to 1
;
;	B_scaled:	Scaled right-hand side term
;
;	Scaling:	Vector of scaling factors
;
; SIDE EFFECTS:
;	Scaling the linear system
;	Ax = b
;	involves scaling the vector of variables x.
;	After inversion of the system, the solution vector must be multiplied
;	component-wise (e.g. using the * operator) by the output vector Scaling.
;
; RESTRICTIONS:
;	The system matrix A must be symmetric and positive (semi-) definite.
;	No check on the properties of the input array A is performed.
;	In practice the procedure SCALE_LS_SYS can be safely applied to the
;	output of LS_SYS (see the file 'ls_sys.pro').
;
; PROCEDURE:
;	Compute scaling factors in order to have the diagonal elements of the
;	scaled matrix A all equal to 1. If s is the vector of scaling factors,
;	let's define S = diag{s} the diagonal matrix having these factors on
;	the main diagonal. The scaled version of the linear system
;	Ax = b  is
;	(SAS)x = Sb.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

PRO scale_ls_sys, a, b, a_scaled, b_scaled, scaling, NOCOMP = nocomp

	on_error, 2
	if  not keyword_set(nocomp)  then begin
	   ; compute scaling factors
	   n = size52(a, /DIM)  &  n = n[0]
	   d = lindgen(n)  &  scaling = make_array(n, /FLOAT, VALUE = 1.)
	   w = where(a[d,d] ne 0, n)
	   if  n ne 0  then  scaling[w] = 1 / sqrt(a[d[w],d[w]])
	endif
	; scale a and b
	a_scaled = diag_mult(diag_mult(a, scaling), scaling, /PREMULT)
	b_scaled = b * scaling
	return
end
