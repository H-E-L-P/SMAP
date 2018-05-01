;+
; NAME:
;	    CHISQR
;
; PURPOSE:
;
;	    Compute the Chi Square statistic of a function and a fit
;	    to the function.  
;
; CALLING SEQUENCE:
; 
;	    Result=CHISQR(Y,SIGMA_Y,YFIT)
;	    
; INPUTS:
;	    Y - Input array.
;	    
;	    SIGMA_Y - Uncertainty in Y.
;	    
;	    YFIT - Fit to Y.
;
; PROCEDURE:
; 
;           CHISQR=TOTAL((Y-YFIT)^2/SIGMAY^2)
;
; MODIFICATION HISTORY:
; 
;    David L. Windt, Bell Labs, November 1989
;    windt@bell-labs.com
;-

function chisqr,y,sigma_y,yfit
on_error,2
x=total((y-yfit)*(y-yfit)/sigma_y/sigma_y)
return,x
end
