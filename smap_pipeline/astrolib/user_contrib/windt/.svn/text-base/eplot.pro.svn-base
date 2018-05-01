;+
; NAME:
; 
;       EPLOT
;		
; PURPOSE:
;
;       Plot x vs y, with vertical error bars on y.
;
; CALLING SEQUENCE:
;
;       EPLOT,Y,SIGY
;       EPLOT,X,Y,SIGY
;       EPLOT,Y,SIGY_UP,SIGY_DOWN
;       EPLOT,X,Y,SIGY_UP,SIGY_DOWN
;
; INPUTS:
;
;       X, Y -  1-D arrays
;
;       SIGY - Uncertainty in Y, i.e. Y+/-SIGY
;
;       SIGY_UP, SIGY_DOWN - +/- uncertainties in Y, i.e.,
;                           Y +SIGY_UP -SIGY_DOWN
;
; KEYWORD PARAMETERS:
;
;       BARLINESTYLE = Linestyle for error bars.
;
;               plus all valid IDL plot keywords.  Only the COLOR,
;               THICK, NOCLIP, and T3D keywords apply to the error
;               bars.
;
; MODIFICATION HISTORY:
;
;      D. L. Windt, Bell Laboratories, November 1989
;      Replaced specific plot/oplot keywords with _EXTRA,
;      April, 1997
;
;      windt@bell-labs.com
;-
pro eplot,x,y,sigyup,sigylo,_extra=_extra,barlinestyle=barlinestyle, $
          color=color,linestyle=linestyle,thick=thick,noclip=noclip, $
          t3d=t3d

on_error,2

if n_params() lt 3 then message,'Usage: EPLOT,X,Y,SIGY'

if n_elements(color) eq 0 then color=!p.color
if n_elements(linestyle) eq 0 then linestyle=!p.linestyle
if n_elements(thick) eq 0 then thick=!p.thick
if n_elements(noclip) eq 0 then noclip=!p.noclip
if n_elements(t3d) eq 0 then t3d=!p.t3d
if n_elements(barlinestyle) eq 0 then barlinestyle=linestyle

plot,x,y,_extra=_extra, $
  color=color,linestyle=linestyle,thick=thick,noclip=noclip,t3d=t3d
psym=!p.psym
!p.psym=0
xt=fltarr(2)
yt=xt
if n_params() eq 3 then sigylo=sigyup
for i=0,n_elements(x)-1 do begin
    xt(0)=x(i)
    xt(1)=x(i)
    yt(0)=y(i)
    yt(1)=y(i)+sigyup(i)
    oplot,xt,yt, $
      color=color,linestyle=barlinestyle,thick=thick,noclip=noclip,t3d=t3d
    yt(1)=y(i)-sigylo(i)
    oplot,xt,yt, $
      color=color,linestyle=barlinestyle,thick=thick,noclip=noclip,t3d=t3d
endfor
!p.psym=psym
return
end
