; $Id: convergence.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	CONVERGENCE
;
; PURPOSE:
;	Check convergence condition between two IDL variables.
;
; CATEGORY:
;	Mathematics.
;
; CALLING SEQUENCE:
;	Result = CONVERGENCE(Var1, Var2, Tolerance)
;
; INPUTS:
;	Var1:	First IDL variable
;
;	Var2:	Second IDL variable
;
;	Tolerance:	Tolerance for convergence check
;
; KEYWORD PARAMETERS:
;	ABSOLUTE:	Set this keyword to a nonzero value to check the absolute
;		error convergence between Var1 and Var2. The default is to check
;		the relative error convergence
;
; OUTPUTS:
;	Return the byte value 1B if the convergence condition is fulfilled
;	and 0B otherwise.
;
; RESTRICTIONS:
;	The relative error between a variable set to zero and a nonzero
;	variable is 1 (100%) by definition. In this case the convergence
;	condition will never be fulfilled, unless the relative Tolerance
;	is > 1, which is rather unlikely.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

FUNCTION convergence, var1, var2, tolerance, ABSOLUTE = absolute

	on_error, 2
	if  keyword_set(absolute)  then $
	   error = abs(var2 - var1)  else $
	   error = abs(relative_error(var1, var2))
	return, max(error) lt tolerance and 1B
end
