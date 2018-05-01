;+
; NAME:
;
;       BESELI_FRACT
;
; PURPOSE:
;
;       This function returns the Modified Bessel Function of the
;       First Kind of Order N, for any N, i.e., including fractional
;       and negative orders.
;
; CALLING SEQUENCE:
;
;	Result = BESELI_FRACT(X, N)
;
;
; INPUTS:
; 
;       X - The value for which the I Bessel function is required. X
;           must be greater than 0. The result will have the same
;           dimensions as X.
;        
;       N - The Bessel function order.
;
; PROCEDURE:
;
;       The series expansion
;
;       I_n(x) = SUM_(k=0->inf) [ (x/2)^(n+2k) / k! Gamma(n+k+1) ]
;
;       is used, and is terminated when the k'th term is less than .001.
;
; MODIFICATION HISTORY:
;
;       David L. Windt, Bell Laboratories, June 1993
;       windt@bell-labs.com
;-

function beseli_fract,x,n
on_error,2

if n_params() ne 2 then message,'Usage: Result=BESELI_FRACT(X,N)'

value=0.
threshhold=.001
k=0
repeat begin
    add_value=exp( (n+2*k)*alog(x/2.) - lngamma(k+1) - lngamma(n+k+3) ) $
      *(n+k+2)*(n+k+1)
    value=value+add_value
    k=k+1
endrep until max(abs(add_value)) le threshhold

return,value
end
