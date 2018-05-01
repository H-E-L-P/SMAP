;+
; NAME:
; 
;	KAISER_BESSEL
;
; PURPOSE:
; 
;	Window function for Fourier Transform filtering.  
;
; CATEGORY:
; 
;	Signal, image processing.
;
; CALLING SEQUENCE:
; 
;	Result = KAISER_BESSEL(N1) (for 1D)
;
;	Result = KAISER_BESSEL(N1,N2) (for 2D)
;
; INPUTS:
; 
;	N1 - The number of columns of the result.
;
;	N2 - The number of rows of the result.
;	
; KEYWORD PARAMETERS:
; 
;       ALPHA - The value of Pi*Alpha is half of the time-bandwidth
;               product.  Default = 3.0
;
; OUTPUTS:
; 
;   Result(i) = BESELI(!pi*alpha*sqrt(1-( (findgen(N)-N/2) / (N/2) )^2),0) / $
;	            BESELI(!pi*alpha,0)
;
; MODIFICATION HISTORY:
; 
;	David L Windt, Bell Labs, August 1996
;	May, 1997 - Added 2D option.
;	windt@bell-labs.com
;-

function kaiser_bessel,n1,n2,alpha=alpha
on_error,2
if n_elements(alpha) le 0 then alpha = 3.0
if n_params() eq 1 then $
  return, BESELI(!pi*alpha*sqrt(1-( (findgen(N1)-N1/2) / (N1/2) )^2),0) / $
  BESELI(!pi*alpha,0) $
else begin
    row=BESELI(!pi*alpha*sqrt(1-( (findgen(N1)-N1/2) / (N1/2) )^2),0) /BESELI(!pi*alpha,0)
    col=BESELI(!pi*alpha*sqrt(1-( (findgen(N2)-N2/2) / (N2/2) )^2),0) /BESELI(!pi*alpha,0)
    return, row#col
endelse

end
