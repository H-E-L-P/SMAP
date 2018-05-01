; $Id: ls_sys.pro, v 1.1 Jan 2000 e.d. $
;
;+
; NAME:
;	LS_SYS
;
; PURPOSE:
;	Put the algebraic system of linear equations
;	Ax = b
;	in the least squares form
;	(A'A)x = A'b,
;	where A' is the transpose of A.
;
; CATEGORY:
;	Mathematics. Linear systems.
;
; CALLING SEQUENCE:
;	LS_SYS, A, B, A_ls, B_ls
;
; INPUTS:
;	A:	2D array of size n*m (i.e. n  columns and m rows), representing
;		the linear system matrix
;
;	B:	1D array of size m, representing a set of measurements
;
; KEYWORD PARAMETERS:
;	WEIGHTS:	1D array of size m, representing squared weights to be
;		applied to the measurements vector B. In this case the least
;		squares form of the input system is
;		(A'WA)x = A'Wb,
;		where W is the vector of squared weights
;
;	MASK:	1D array of valid subscripts, indicating the components of the
;		mesurements array B which must be masked, i.e. excluded
;
; OUTPUTS:
;	A_ls:	Same as  A'A  in PURPOSE above
;
;	B_ls:	Same as  A'b  in PURPOSE above
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;	Updates:
;	1) Fixed bug on transposition of b, when it is a scalar
;	   (Emiliano Diolaiti, January 2000)
;-

PRO ls_sys, a, b, WEIGHTS = w, MASK = pix, a_ls, b_ls

	on_error, 2
	s = size52(a, /DIM)  &  n_col = s[0]  &  n_rows = s[1]
	a_ls = a
	if  n_elements(w) eq n_rows  then $
	   a_ls = diag_mult(a_ls, w, /PRE)
	if  n_elements(pix) ne 0  then $
	if  min(pix) ge 0 and max(pix) lt n_rows  then begin
	   m = make_array(n_rows, VALUE = 1.)  &  m[pix] = 0
	   a_ls = diag_mult(a_ls, m, /PRE)
	endif
	a_ls = transpose(a_ls)
	if  n_col gt 1  then  b_ls = b # a_ls  else  b_ls = total(b * a_ls)
	a_ls = a # a_ls  &  if  n_col gt 1  then  b_ls = transpose(b_ls)
;	a_ls = a # a_ls  &  b_ls = transpose(b_ls)
	return
end
