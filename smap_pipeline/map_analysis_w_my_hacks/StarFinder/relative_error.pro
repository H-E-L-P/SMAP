; $Id: relative_error.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	RELATIVE_ERROR
;
; PURPOSE:
;	Compute the relative difference between two IDL variables.
;
; CATEGORY:
;	Tools.
;
; CALLING SEQUENCE:
;	Result = RELATIVE_ERROR(Var1, Var2)
;
; INPUTS:
;	Var1:	First IDL variable
;
;	Var2:	Second IDL variable
;
; OUTPUTS:
;	Return the relative error of Var2 with respect to Var1.
;
; PROCEDURE:
;	Compute the quantity
;	(Var2 - Var1) / Var1
;	over the support of Var1 (set of pixels where Var1 is not 0).
;	Outside the support of Var1, i.e. where Var1 is 0, the relative
;	error is 1 (100%) by definition.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

FUNCTION relative_error, var1, var2

	error = float(var1 - var1) + 1  &  w = where(var1 ne 0, count)
	if  count ne 0  then  error[w] = (var2 - var1)[w] / var1[w]
	return, error
end
