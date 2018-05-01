; $Id: ginv.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	GINV
;
; PURPOSE:
;	Compute the Moore-Penrose generalized inverse of a rectangular matrix
;	by applying the Gram-Schmidt orthogonalization procedure to the columns
;	of the input array.
;
; CATEGORY:
;	Mathematics. Matrix inversion.
;
; CALLING SEQUENCE:
;	Results = GINV(A)
;
; INPUTS:
;	A:	2D array to be inverted (not necessarily square)
;
; OUTPUTS:
;	Return generalized inverse of input array A. If A has size m*n, its
;	generalized inverse will have size n*m.
;	If A is a double precision floating-point array, the output is in
;	double precision, otherwise it is in single precision
;
; OPTIONAL OUTPUTS:
;	Rank:	Rank of A, i.e. number of linearly indepent columns
;
; PROCEDURE:
;	Apply the recursive Gram-Schmidt orthogonalization procedure to the
;	columns of the input matrix A to compute the generalized inverse A'.
;	We briefly recall the properties of the Moore-Penrose generalized
;	inverse:
;	TRANPOSE(A'A) = A'A
;	TRANPOSE(AA') = AA'
;	AA'A = A
;	A'AA' = A'
;	The generalized inverse conicides with the ordinary inverse in the
;	case of a non-singular square matrix.
;	When used to solved a linear system of "normal equations", the
;	generalized inverse produces the so-called "minimum norm solution",
;	which is particularly meaningful in the case of singular of nearly
;	singular linear systems. For more details, see
;	B.W.Rust, W.R.Burrus, "Mathematical programming and the numerical
;		solution of linear equations",
;		American Elsevier Publishing Company, 1972
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;	Originally written in FORTRAN by S.Lorenzutta
;-




; GS_ORTH: recursive Gram-Schmidt orthogonalization of a set of n
; m-components vectors. The vectors are stored as a (m*n) array,
; to speed up the computation if virtual memory should be used.

PRO gs_orth, v, u, lin_indep, n, m, tol

	on_error, 2
	this = n - 1
	if  this gt 0  then begin
	   ; Induction case: apply the G-S orthogonalization to this vector.
	   ; Orthogonalize the first (n - 1) vectors before
	   gs_orth, v, u, lin_indep, n - 1, m, tol
	   norm = total(v[*,this] * v[*,this])
	   prod = (v[*,this] # v )[0:this-1]
	   v[*,this] = v[*,this] - (prod * lin_indep[0:this-1]) # $
	   						   transpose(v[*,0:this-1])
	   u[*,this] = u[*,this] - prod # transpose(u[*,0:this-1])
	   new_norm = total(v[*,this] * v[*,this])
	   is_lin_indep = norm ne 0
	   if  is_lin_indep  then  is_lin_indep = new_norm / norm gt tol
	   if  is_lin_indep  then begin
	      ; this vector is linearly independent
	      lin_indep[this] = 1  &  norm = 1 / sqrt(new_norm)
	   endif else begin
		  ; this vector is linearly dependent
	      prod = u[*,this] # u[*,0:this-1]
		  v[*,this] = - (prod * lin_indep[0:this-1]) # $
		  				transpose(v[*,0:this-1])
	      lin_indep[this] = 0
	      norm = 1 / sqrt(total(u[*,this] * u[*,this]))
	   endelse
	endif else begin
	   ; Base case: one vector to normalize
	   norm = sqrt(total(v[*,this] * v[*,this]))
	   if  norm ne 0  then begin
	      norm = 1 / norm  &  lin_indep[this] = 1
	   endif else  lin_indep[this] = 0
	endelse
	v[*,this] = v[*,this] * norm  &  u[*,this] = u[*,this] * norm
	return
end


FUNCTION ginv, a, rank

	on_error, 2
	s = size52(a, /DIM)  &  n = s[0]  &  m = s[1]
	type_a = size52(a, /TYPE)
	a_inv = transpose(double(a))	; transpose the set of column
						; vectors to speed up the computation if virtual
						; memory should be used
	u = dblarr(n, n)  &  diag = lindgen(n)  &  u[diag,diag] = 1
	lin_indep = lonarr(n)  &  tol = 2 * (machar(/DOUBLE)).eps
	gs_orth, a_inv, u, lin_indep, n, m, tol
	a_inv = temporary(a_inv) # transpose(u)
	if  type_a lt 5  then  a_inv = float(a_inv)
	rank = total(lin_indep)
	return, a_inv
end
