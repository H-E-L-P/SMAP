;+
; NAME:
; 
;       OEPLOT
;		
; PURPOSE:
;
;       Overplot x vs y, with vertical error bars on y.
;
; CALLING SEQUENCE:
;
;       OEPLOT,Y,SIGY
;       OEPLOT,X,Y,SIGY
;       OEPLOT,Y,SIGY_UP,SIGY_DOWN
;       OEPLOT,X,Y,SIGY_UP,SIGY_DOWN
;
; INPUTS:
;
;       X, Y -  1-D arrays
;
;       SIGY - Uncertainty in y, i.e. Y+/-SIGY
;
;       SIGY_UP, SIGY_DOWN - +/- uncertainties in Y,
;                                i.e., Y +SIGY_UP -SIGY_DOWN
;
; KEYWORD PARAMETERS:
;
;       BARLINESTYLE = Linestyle for error bars.
; 
;               plus the IDL keywords color, linestyle,thick, psym,
;               symsize, noclip, and t3d.
;
; MODIFICATION HISTORY:
;
;       D. L. Windt, Bell Laboratories, November 1989
;       windt@bell-labs.com
;-
pro oeplot,x,y,sigyup,sigylo,barlinestyle=barlinestyle, $
           color=color,linestyle=linestyle,thick=thick, $
           psym=psym,symsize=symsize,noclip=noclip,t3d=t3d
on_error,2

if n_params() lt 3 then message,'Usage: EPLOT,X,Y,SIGY'

if n_elements(color) eq 0 then color=!p.color
if n_elements(linestyle) eq 0 then linestyle=!p.linestyle
if n_elements(thick) eq 0 then thick=!p.thick
if n_elements(psym) eq 0 then psym=!p.psym
if n_elements(symsize) eq 0 then symsize=!p.symsize
if n_elements(noclip) eq 0 then noclip=!p.noclip
if n_elements(t3d) eq 0 then t3d=!p.t3d
if n_elements(barlinestyle) eq 0 then barlinestyle=linestyle

oplot,x,y, $
  color=color,linestyle=linestyle,thick=thick,psym=psym,symsize=symsize, $
  noclip=noclip,t3d=t3d
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
