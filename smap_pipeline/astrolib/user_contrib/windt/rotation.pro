;+
; NAME:
;
;           ROTATION
; 
; PURPOSE:
; 
;	    Rotate two vectors by a specified amount.
;	    
; CALLING SEQUENCE:
; 
;	    ROTATION,X,Y,DEG,NX,NY
;	    
; INPUTS:
;
;           X,Y   :orignal data point pairs
;           
;	    DEG   :degrees to rotate.
;	    
; OUTPUTS:
; 
;	    Nx, Ny = rotated point pairs.
;	    
; MODIFICATION HISTORY:
; 
;	    Jeff Bennett, U of Colorado
;-

PRO ROTATION,X,Y,DEG,NX,NY
ang=deg*!dtor

;convert to polar coordinates for rotation
r = sqrt(x*x + y*y)
theta = r*0.
;get angle in for loop so that zero radii will be left as zero angle
for i = 0,n_elements(r)-1 do $
if r(i) ne 0 then theta(i) = atan(y(i),x(i))  ;range from -pi to +pi
;
;add rotation angle
theta = theta + ang
;
;convert back to rectangular coordinates, now rotated
nx = r * cos(theta)
ny = r * sin(theta)
;
return
end
