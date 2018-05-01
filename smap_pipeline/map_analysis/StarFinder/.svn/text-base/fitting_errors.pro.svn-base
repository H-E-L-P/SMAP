; $Id: fitting_errors.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	FITTING_ERRORS
;
; PURPOSE:
;	Estimate formal errors (standard deviations) on the solution
;	of an algebraic system of linear equations, given the inverse
;	of the system matrix.
;
; CATEGORY:
;	Mathematics. Linear systems.
;
; CALLING SEQUENCE:
;	Result = FITTING_ERRORS(Inverse)
;
; INPUTS:
;	Inverse:	Inverse matrix of the linear system
;
; KEYWORD PARAMETERS:
;	SCALING:	Set this keyword to the vector of scaling factors
;		used to scale the linear system before inversion
;
; OUTPUTS:
;	Return a vector of formal errors on the components of the solution
;	of the linear system.
;
; RESTRICTIONS:
;	The input array must be square, as always happens when it is the
;	inverse matrix of a linear system of "normal equations".
;	The system of normal equations should represent the mathematical
;	formulation of a weighted least squares fitting problem. It is
;	essential that the least squares error is weighted (by the inverse
;	variances on the data), otherwise the formal errors on the solution
;	have no meaning.
;
; PROCEDURE:
;	Estimate the errors as the square roots of the diagonal elements
;	of the inverse array.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

FUNCTION fitting_errors, inverse, SCALING = scaling

	on_error, 2
	n = (size52(inverse, /DIM))[0]  &  d = lindgen(n)
	e = sqrt(inverse[d,d] > 0)
	if  n_elements(scaling) eq n  then  e = e * scaling
	return, e
end