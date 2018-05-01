;+
; NAME:
; 
;	PROFILE_NI
;
; PURPOSE:
;
;	Extract a profile from an image noninteractively.
;	
; CALLING SEQUENCE:
; 
;	Result = PROFILE_NI(IMAGE,COORDS)
;	
; INPUTS:
; 
;	IMAGE - data array.  May be any type except complex.
;		
;	COORDS - 2 x 2 array of x and y coordinate of profile endpoints,
;                [[X0,Y0],[X1,Y1]]
;	
; KEYWORD PARAMETERS:
; 
;       XSTART, YSTART - starting (x,y) location of lower left corner
;                        of image.
;	
; OUTPUTS:
; 
;	Result = 1-D array of image values along the line from
;
; MODIFICATION HISTORY:
; 
;	Adapted from PROFILE
;	
;	D. L. Windt, Bell Laboratories, November 1991.
;	windt@bell-labs.com
;	
;-
function profile_ni, image, coords, xstart = x0, ystart = y0
on_error,2                      ;Return to caller if an error occurs

if n_params() ne 2 then message,'usage: PROFILE_NI, IMAGE, COORDS'
s=size(image)
sx = s(1) & sy=s(2)
if n_elements(x0) le 0 then x0 = 0
if n_elements(y0) le 0 then y0 = 0
x=coords(0,0)-x0
y=coords(1,0)-y0
x1=coords(0,1)-x0
y1=coords(1,1)-y0
dx = float(x1-x)		;delta x
dy = float(y1-y)
n = abs(dx) > abs(dy)
if n eq 0 then message, 'Zero length line.'
;
r = fltarr(n+1)
;
if abs(dx) gt abs(dy) then begin
    if x1 ge x then s=1 else s=-1
    sy = (y1-y)/abs(dx)
endif else begin
    if y1 ge y then sy=1 else sy=-1
    s = (x1-x)/abs(dy)
endelse
;
xx = indgen(n+1l)*s+x		;X values, make into longwords.
yy = indgen(n+1l)*sy+y		;Y values
return,image(long(yy)*sx + xx)
end
