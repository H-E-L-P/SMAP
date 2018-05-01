;+
; NAME:
; 
;	SINCSQUARE_FIT
;	
; PURPOSE:
; 
; 	Fit y=f(x) where:
; 	F(x) = a0*( sin(a1*(x-a2))/(a1*(x-a2)) )^2 + a3
; 	Estimate the parameters a0,a1,a2,a3 and then call curvefit.
; 	
; CALLING SEQUENCE:
; 
;	YFIT = SINC_FIT(X,Y,A)
;	
; INPUTS:
; 
;	X - independent variable, must be a vector.
;	
;       Y - dependent variable, must have the same number of points as
;           x.
;		
; OUTPUTS:
; 
;	YFIT = fitted function.
;	
; OPTIONAL OUTPUT PARAMETERS:
; 
;	A = Fit coefficients. A four element vector as described above.
;
; MODIFICATION HISTORY:
; 
;	Adapted from GAUSSFIT
;	
;	D. L. Windt, Bell Laboratories, March, 1990
;	windt@bell-labs.com
;-
;

pro sincsquare,x,a,f,pder
f = a(0)*(sin(a(1)*(x-a(2)))/(a(1)*(x-a(2))))^2+a(3)
if n_params(0) le 3 then return ;need partial?
pder = fltarr(n_elements(x),4)  ;yes, make array.
pder(0,0)=(sin(a(1)*(x-a(2)))/(a(1)*(x-a(2))))^2
pder(0,1)=2*a(0)*sin( a(1)*(x-a(2)) ) / ( (a(1)^2)*(x-a(2)) ) * $
  ( cos( a(1)*(x-a(2)) )-sin( a(1)*(x-a(2)) )/( a(1)*(x-a(2)) ) )
pder(0,2)=-pder(*,1)*a(1)/(x-a(2))
pder(0,3)=1.
return
end

function sincsquare_fit,x,y,a
on_error,2		
cm=check_math(0.,1.)		; Don't print math error messages.
c=poly_fit(x,y,1,yf)		; Do straight line fit.
n = n_elements(y)		;# of points.
yd=y-yf
ymax=max(yd)			; get peak.
roi=where(yd gt .5*ymax)	; get fwhm points
i0=roi(0)
i1=roi(n_elements(roi)-1)
width=!pi/(x(i1)-x(i0))
a=[ymax,width,x(!c)*1.01,c(0)]
return,curvefit(x,y,replicate(1.,n),a,sigmaa,funct='sincsquare') ;call curvefit
end
