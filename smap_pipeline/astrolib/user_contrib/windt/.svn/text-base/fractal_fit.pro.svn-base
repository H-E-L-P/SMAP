;+
; NAME:
; 
;	FRACTAL_FIT
;	
; PURPOSE:
; 
; 	Fit y=f(x) where:
; 	F(x) = a0/(x^a1) [+a2]
; 	Estimate the parameters a0,a1[,a2] and then call curvefit.
; 	
; CALLING SEQUENCE:
; 
;	YFIT = FRACTAL_FIT(X,Y,A,BACKGROUND=BACKGROUND)
;	
; INPUTS:
; 
;	X = independent variable, must be a vector and MUST BE POSITIVE!
;	
;	Y = dependent variable, must have the same number of points as x.
;	
;	BACKGROUND = set to add a background term (a2).
;	
; OUTPUTS:
; 
;	YFIT = fitted function.
;	
; OPTIONAL OUTPUT PARAMETERS:
; 
;	A = coefficients. a two [three] element vector as described above.
;	
; RESTRICTIONS:
; 
;	X must be positive.
;	
; MODIFICATION HISTORY:
; 
;	D. L. Windt, Bell Laboratories, March, 1990
;	windt@bell-labs.com
;-
;

pro	fractal,x,a,f,pder
; fractal function w/out background.
f=a(0)/(x^a(1))
if n_params(0) le 3 then return ;need partial?
pder = fltarr(n_elements(x),2)  ;yes, make array.
pder(0,0) = 1./(x^a(1))
pder(0,1) = -a(0)*alog(x)/(x^a(1))
return
end

pro	fractal_back,x,a,f,pder
; fractal function w background.
f=a(0)/(x^a(1))+a(2)
if n_params(0) le 3 then return ;need partial?
pder = fltarr(n_elements(x),3)  ;yes, make array.
pder(0,0) = 1./(x^a(1))
pder(0,1) = -a(0)*alog(x)/(x^a(1))
pder(*,2) = 1.
return
end

function fractal_fit,x,y,a
on_error,2		
cm=check_math(0.,1.)		; Don't print math error messages.
n = n_elements(y)		; # of points.
c=poly_fit(x,y,1,yf)		; Do a straight line fit.
yd=y-yf
ymax=max(yd) & xmax=x(!c) & imax=!c ;x,y and subscript of extrema
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
if keyword_set(background) then begin
    a = [yd(i0),abs(x(i0)-x(i0+i)),c(0)] ;estimates
    return,curvefit(x,y,replicate(1.,n),a,sigmaa,funct='fractal_back') 
endif else begin
;        a = [yd(i0),abs(x(i0)-x(i0+i))] ;estimates
    a=[yd(i0),1.5]
    return,curvefit(x,y,replicate(1.,n),a,sigmaa,funct='fractal') 
endelse
end

