;+
; NAME:
;
;         SINC
;
; PURPOSE:
;
;         Function to return the value of the SINC function,
;         i.e., sin(x)/x.
;
; CALLING SEQUENCE:
;
;         Result = SINC(X)
; 
; INPUTS:
;
;         X - Input value.  Scalar or array.
;
; OUTPUTS:
;
;         Result - Value of SIN(X)/X. 
;
; PROCEDURE:
;
;         Straightforward; except Result is explicitly set to
;         one when X=0.
;
; MODIFICATION HISTORY:
;
;         David L. Windt, Bell Laboratories, May 1997
;
;         March 1999:
;         
;         Returned X values are no longer changed when X=1.
;
;         DLW (thanks to Paul Woodford.)
;         
;         windt@bell-labs.com
;
;-
function sinc,x

x_temp=x
mc=machar()
wh=where(abs(x) lt mc.eps,count)
if count ne 0 then x_temp(wh)=1.
result=sin(x_temp)/x_temp
if count ne 0 then result(wh)=1.
return,result
end

