;+
; NAME:
;
;       BESELK_FRACT
;
; PURPOSE:
;
;       This function returns the Modified Besel Function of the Second
;       Kind of order N, for any N, i.e., including fractional and
;       negative orders.
;
; CALLING SEQUENCE:
;
;	Result = BESELK_FRACT(X, N)
;
;
; INPUTS:
;
;       X - The value for which the K Bessel function is required. X
;           must be greater than 0. The result will have the same
;           dimensions as X.
;        
;       N - The Bessel function order.
;
; PROCEDURE:
;
;       This function uses the BESELI_FRACT function:
;
;       Results=(BESELI_FRACT(X,-N)-BESELI_FRACT(X,B))*!PI/2./SIN(N*!PI)
;
; MODIFICATION HISTORY:
;
;       David L. Windt, Bell Laboratories, June 1993
;       windt@bell-labs.com
;-

function beselk_fract,x,n
on_error,2
if n_params() ne 2 then message,'Usage: Result=BESELK_FRACT(X,N)'

return,(beseli_fract(x,-n)-beseli_fract(x,n))*!pi/2./sin(n*!pi)
end
