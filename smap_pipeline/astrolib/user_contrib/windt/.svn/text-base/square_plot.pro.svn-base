;+
; NAME:
;
;      SQUARE_PLOT
;      
; PURPOSE:
; 
;       Define !p.region so plots come out with aspect ratio of 1.
;	
; CALLING SEQUENCE:
; 
;       SQUARE_PLOT
;		
; KEYWORD PARAMETERS:
; 
;       CENTER - set to center plot in window.
;		
; MODIFICATION HISTORY:
; 
;	David L. Windt, Bell Laboratories, December 1991.
;	windt@bell-labs.com
;-
pro square_plot,center=center

on_error,2

xs=float(!d.x_size)
ys=float(!d.y_size)

if xs gt ys then begin
    xmax=ys/xs
    ymax=1.
endif else begin
    xmax=1
    ymax=xs/ys
endelse
xmin=0.
ymin=0.

if keyword_set(center) then begin
    delta=(1.-xmax)/2.
    xmin=delta
    xmax=xmax+delta
    delta=(1.-ymax)/2.
    ymin=delta	
    ymax=ymax+delta
endif

!p.region=[xmin,ymin,xmax,ymax]

return
end
