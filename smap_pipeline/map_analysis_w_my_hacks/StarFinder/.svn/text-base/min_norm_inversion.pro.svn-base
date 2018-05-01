; $Id: min_norm_inversion.pro, v 1.1 Aug 2000 e.d. $
;
;+
; NAME:
;	MIN_NORM_INVERSION
;
; PURPOSE:
;	Compute the minimum norm solution of an algebraic system
;	of linear equations
;
; CATEGORY:
;	Mathematics. Linear systems.
;
; CALLING SEQUENCE:
;	Result = MIN_NORM_INVERSION(A, B)
;
; INPUTS:
;	A:	matrix of linear system (n columns, m rows)
;
;	B:	vector of measurements (m rows)
;
; KEYWORD PARAMETERS:
;	SCALING:	If the linear system has been previously scaled (see the
;		routine SCALE_LS_SYS in the file 'scale_ls_sys.pro'), the solution
;		must be multiplied by the scaling factors. Set the keyword SCALING
;		to the vector of scaling factors to do this
;
; OUTPUTS:
;	Return n-components vector, representing the minimum norm solution
;	of the linear system, defined as A'b, where A' is the generalized
;	inverse of A computed by GINV (see the file 'ginv.pro')
;	If the first input A has just one element (i.e. A represents a scalar),
;	just divide B by A.
;
; OPTIONAL OUTPUTS:
;	INVERSE:	Use this output keyword to retrieve the generalized inverse
;		of the input matrix A
;
; RESTRICTIONS:
;	If an error occur (A is a scalar equal to 0 or the size of the input
;	arrays are not correct), return to caller
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;	Updates:
;	1) Fixed bug on output keyword INVERSE when A is a scalar
;	   (Emiliano Diolaiti, August 2000).
;-

FUNCTION min_norm_inversion, a, b, SCALING = scaling, INVERSE = inv_a

	on_error, 2
	if  n_elements(a) eq 1  then begin
	   inv_a = 1. / a   &  x = b * inv_a
	endif else begin
	   inv_a = ginv(a)  &  x = b # inv_a
	endelse
	if  n_elements(scaling) eq n_elements(x)  then  x = x * scaling
	return, x
end
