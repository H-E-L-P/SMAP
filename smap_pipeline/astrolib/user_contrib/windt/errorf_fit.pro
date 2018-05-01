;+
; NAME:
; 
;	ERRORF_FIT
;	
; PURPOSE:
; 
; 	fit y=f(x) where:
; 	f(x) = a0*errorf((x-a1)/a2))+a3+x*a4
; 	
; CALLING SEQUENCE:
; 
;	YFIT = ERRORF_FIT(X,Y,A)
;	
; INPUTS:
; 
;       X - independent variable, must be a vector.
;       
;       Y - dependent variable, must have the same number of points ;
;           as x.
;		
;	A - initial values of adjustable parameters.
;	
; OUTPUTS:
; 
;	YFIT = fitted function.
;	
; MODIFICATION HISTORY:
; 
;	Adapted from GAUSSFIT
;	
;	D. L. Windt, Bell Laboratories, June 1990
;	windt@bell-labs.com
;-
;
pro errorfunct,x,a,y,pder
common errorf_fit,constant,linear

y=a(0)*errorf((x-a(1))/a(2))
index=3
if constant then begin 
    y=y+a(index)
    index=index+1
endif
if linear then begin
    y=y+a(index)*x
    index=index+1
endif
if n_params() lt 4 then return
pder=fltarr(n_elements(x),n_elements(a))
a_save=a
delta=.00001
for i=0,n_elements(a)-1 do begin
    a=a_save
    da=a(i)*delta
    a(i)=a(i)+da
    errorfunct,x,a,yplus
    a(i)=a(i)-2.*da
    errorfunct,x,a,yminus
    dyda=(yplus-yminus)/(2.*da)
    pder(*,i)=dyda
endfor
return
end

function errorf_fit,x,y,a,noconstant=noconstant,nolinear=nolinear
on_error,2		

if n_params() ne 3 then message,'Usage: Result=ERRORF_FIT(X,Y,A)'

common errorf_fit,constant,linear
constant=keyword_set(noconstant) eq 0
linear=keyword_set(nolinear) eq 0

n = n_elements(y)		; # of points.
return,curvefit(x,y,replicate(1.,n),a,sigmaa,funct='errorfunct')

end
