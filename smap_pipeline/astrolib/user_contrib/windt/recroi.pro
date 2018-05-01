;+
; NAME:
;
;        RECROI
;        
; PURPOSE:
;
;        Define a rectangular region of interest of an image using the
;        image display system and the cursor/mouse.
;		
; CATEGORY:
;
;        Image processing.
;        
; CALLING SEQUENCE:
; 
;	Result=RECROI(SX,SY[,XVERTS,YVERTS])
;	
; INPUTS:
; 
;	SX, SY = size of image, in pixels.
;	
; KEYWORD PARAMETERS:
; 
;       X0, Y0 - coordinate of lower left corner of image on display.
;                if omitted, (0,0) is assumed.  Screen device
;                coordinates.
;	    
;       ZOOM - zoom factor, if omitted, 1 is assumed.
;	
;       XAXIS, YAXIS - optional 1-d arrays corresponding to the x and
;                      y scales of image. Needed only if XROI and/or
;                      YROI are specified.
;	    
;       XROI, YROI - optional output vectors associated with the
;                    digitized rectangular region of interest.  XAXIS
;                    and YAXIS keyword parameters must be supplied.
;	    
; OUTPUTS:
; 
;	Result = vector of subscripts of pixels inside the region.
;	
; OPTIONAL OUTPUTS:
; 
;       XVERTS, YVERTS - optional output parameters which will contain
;                        the vertices enclosing the region.  Setting
;                        NOREGION inhibits the return of the pixel
;                        subscripts.
;	    
; COMMON BLOCKS:
; 
;	Colors is used to obtain the current color table which is modified
;       and then restored.
;	
; SIDE EFFECTS:
; 
;	For this implementation, bit 0 of each pixel is used to draw ; the
;       outline of the region.  You WILL have to change this to fit
;       the capabilities and procedures of your display.  ; The lowest
;       bit in which the write mask is enabled is changed.
;	 
; PROCEDURE:
; 
;	The write mask for the display is set so that only bit 0 may be
;       written.  Bit 0 is erased for all pixels.  The color tables
;       are loaded with odd values complemented, even values
;       unchanged.  A message is printed, assuming a mouse, indicating
;       the effect of the three buttons.  The operator marks opposite
;       corners of the rectangle.
;	
; MODIFICATION HISTORY:
; 
;	Adapted from DEFROI
;	
;	D. L. Windt, Bell Laboratories, November 1989
;	windt@bell-labs.com
;-

function recroi, sx, sy, xverts, yverts, x0=x0, y0=y0, zoom = zoom, $
                 noregion = noregion,xaxis=xaxis,yaxis=yaxis, $
                 xroi=xroi,yroi=yroi
common colors,orig_red, orig_green, orig_blue,red,green,blue

if n_params() lt 2 then message,'Usage: RECROI, SX, SY'

on_error,2                      ;Return to caller if error
nc = !d.n_colors                ;# of colors available
if nc eq 0 then message,"Device has static colors, Won't work."
if n_elements(red) le 0 then begin ;Define bw table?
red = indgen(nc) & green = red & blue = red & endif

if n_elements(x0) le 0 then x0=0
if n_elements(y0) le 0 then y0=0
if n_elements(zoom) le 0 then zoom=1

device,get_write=old_mask       ; get current write mask.

if !d.n_colors gt 256 then $
  new_mask=1L * (1 + 256 + 256L*256) $
else begin
    i=0L
    while ((old_mask and 2^i) eq 0) do i=i+1
    new_mask=2^i                ; mask we want for our roi bit.
endelse
device,set_write= new_mask      ; set new write mask.
erase                           ; erase bottom bit for all pixels.

;	Set up color tables where the odd elements are for the ROI,
;	and the evens are unchanged.

odd = where((indgen(nc) and new_mask) ne 0) ;Odd subscripts
rr=red & gg=green & bb=blue	; save color tables.
rr(odd) = 0
gg(odd) = 255 
bb(odd) = 0   

tvlct,rr,gg,bb			; load new table with odd elements diddled.
erase                           ; clear overlay
tvcrs,.5,.5,/norm		; move cursor into window.

get_1: print,'Digitize the first corner (use left button):'
tvrdc,xx,yy,1,/dev		; get point with wait.
xx=(xx-x0)/zoom			; convert to image coords.
yy=(yy-y0)/zoom
if (xx ge 0) and (xx lt sx) and (yy ge 0) and (yy lt sy) then begin
    xprev=xx			; save the points if they're in range.
    yprev=yy
    x_1=xx			
    y_1=yy
endif else goto,get_1

wait,.25			; wait so next point will be different.
!err=0				; reset !err.

get_2: print,'Digitize the second corner (use left button):'
repeat begin
    tvrdc,xx,yy,0,/dev          ; get x,y, no wait, device coords.
    xx = (xx - x0) / zoom	; convert to image coords.
    yy = (yy - y0) / zoom
    if (xx ge 0) and (xx lt sx) and (yy ge 0) and (yy lt sy) $
      and ((xx ne xprev) or (yy ne yprev)) then begin ; if in range...
                                ; erase last box.
        plots,[x_1,xprev,xprev,x_1,x_1]*zoom+x0, $
          [y_1,y_1,yprev,yprev,y_1]*zoom+y0,/dev,col=0,/noclip
                                ; draw new box...
        plots,[x_1,xx,xx,x_1,x_1]*zoom+x0, $
          [y_1,y_1,yy,yy,y_1]*zoom+y0,/dev,col=new_mask,/noclip
        xprev = xx
        yprev = yy
    endif
endrep until !err eq 1

if xx eq x_1 or yy eq y_1 then goto,get_2 ; make sure there 2 distinct points.

erase
temp=x_1                        ; make sure x_1 < xx..
x_1=x_1<xx
xx=temp>xx
temp=y_1                        ; make sure y_1 < yy..
y_1=y_1<yy
yy=temp>yy

xverts=[x_1,xx,xx,x_1]          ; save the result.
yverts=[y_1,y_1,yy,yy]

if !order ne 0 then yverts = sy-1-yverts ; invert y?
device,set_write=old_mask       ; re-enable writing to all bit planes.
empty                           ; sun view mask handling is flakey.
tvlct,red,green,blue            ; Restore color tables.
; get subscripts inside area...
if keyword_set(noregion) then a=0 else a = polyfillv(xverts,yverts,sx,sy)

; get x and y axis roi...

if n_elements(xaxis) ge 2 and n_elements(yaxis) ge 2 then begin
    s_x=n_elements(xaxis)
    s_y=n_elements(yaxis)
    x1=float(x_1)/sx*(xaxis(s_x-1)-xaxis(0))+xaxis(0)
    x2=float(xx)/sx*(xaxis(s_x-1)-xaxis(0))+xaxis(0)
    y1=float(y_1)/sy*(yaxis(s_y-1)-yaxis(0))+yaxis(0)
    y2=float(yy)/sy*(yaxis(s_y-1)-yaxis(0))+yaxis(0)
    xroi=xaxis(where(xaxis gt x1 and xaxis lt x2))
    yroi=yaxis(where(yaxis gt y1 and yaxis lt y2))
endif

tvcrs,0                         ; turn off cursor.
finish:                         ;
return,a
end
