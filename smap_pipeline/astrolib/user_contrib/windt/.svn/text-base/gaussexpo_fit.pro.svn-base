;+
; NAME:
; 
;	GAUSSEXPO_FIT
;	
; PURPOSE:
; 
; 	Fit y=f(x) where:
; 	
; 	f(x) = a0*exp(-z^2/2)+a3*exp(-abs(x-a4)/a5)+a6 and z=(x-a1)/a2
; 		
;       a0 = height of gaussian, a1 = center of gaussian, a2 = 1/e
;       width of ; gaussian, a3 = height of exponential, a4 = center
;       of exponential, ; a5 = 1/e width of exponential,
;       a6=background.
;	
; 	Estimate the parameters a0,a1,a2,a3,a4,a5,a6 and then call curvefit.
; 	
; CALLING SEQUENCE:
; 
;	YFIT = GAUSSEXPO_FIT(X,Y,A)
;	
; INPUTS:
; 
;	X = independent variable, must be a vector.
;	
;	Y = dependent variable, must have the same number of points as x.
;		
; OUTPUTS:
; 
;	YFIT = fitted function.
;	
; OPTIONAL OUTPUT PARAMETERS:
; 
;       A = Fit coefficients. A six element vector as described above.
;
; MODIFICATION HISTORY:
; 
;	Adapted from GAUSSFIT
;	
;	D. L. Windt, Bell Laboratories, March, 1990
;	windt@bell-labs.com
;-
;

pro gaussexpo,x,a,f,pder
z = (x-a(1))/a(2)		    ;get z
ez = exp(-z^2/2.)*(abs(z) le 7.)    ;gaussian part ignore small terms
f1 = a(0)*ez
f2 = a(3)*exp(-abs(x-a(4))/a(5))    ; exponential part.
f = f1+f2+a(6)
if n_params(0) le 3 then return	    ;need partial?
pder = fltarr(n_elements(x),7)	    ;yes, make array.
pder(0,0) = ez			    ;compute partials
pder(0,1) = a(0) * ez * z/a(2)
pder(0,2) = pder(*,1) * z
pder(0,3) = f2/a(3)
pder(0,4) = f2/a(5)*(2*(x lt a(4))-1)
pder(0,5) = 1./a(5)/a(5)*abs(x-a(4))*f2
pder(*,6) = 1.
return
end

function gaussexpo_fit,x,y,a
on_error,2		
cm=check_math(0.,1.)		; Don't print math error messages.
n = n_elements(y)		;# of points.

c=poly_fit(x,y,1,yf)		; Do a straight line fit.
yd=y-yf
ymax=max(yd) & xmax=x(!c) & imax=!c	;x,y and subscript of extrema
ymin=min(yd) & xmin=x(!c) & imin=!c

if abs(ymax) gt abs(ymin) then i0=imax else i0=imin ;emiss or absorp?
i0 = i0 > 1 < (n-2)		;never take edges
dy=yd(i0)			;diff between extreme and mean
del = dy/exp(1.)		;1/e value
i=0
while ((i0+i+1) lt n) and $	;guess at 1/2 width.
	((i0-i) gt 0) and $
	(abs(yd(i0+i)) gt abs(del)) and $
	(abs(yd(i0-i)) gt abs(del)) do i=i+1
a = [yd(i0), x(i0), abs(x(i0)-x(i0+i))] ;estimates
a=[a,a,c(0)]
!c=0				;reset cursor for plotting
return,curvefit(x,y,replicate(1.,n),a,sigmaa,funct='gaussexpo') 
end
