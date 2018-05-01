function check_image, image, x_pos, y_pos, size

;+
; NAME:
;      check_image
; PURPOSE:
;      Make a check image with detected sources circled in a circle of 
;       radius size 
; EXPLANATION:
;     This is intended as a subroutine of ps_extractor, but can be
;     used independently  
;
; CALLING SEQUENCE:
;     checkim=check_image(bgsub250, ,x_pos, ypos, 9)
;
; INPUTS:
;     image   - background subtracted image
;     size - single scalar size circle around sources
;     x_pos, y_pos - x and y positions of detected sources in pixels 
;
; OPTIONAL INPUTS:
;     NONE - should implement possibility of taking ra,dec
;
; OPTIONAL KEYWORD INPUTS:
;     NONE
;
; RETURNS:
;     check image - background subtracted image with sources indicated 
;       in circles of radius size
;
; EXAMPLE:
;	make a background image from an image called im250 using a
;	32x32 subimages and put into bgim250:
;       IDL> checkim=check_image(bgsub250, x_pos, ypos, 9)
;       
; PROCEDURES USED:
;       READFITS(), AVG, SORT, PS_OPEN, PS_CLOSE
 On_error,2                         ;Return to caller
 compile_opt idl2

; COPY BACKGROUND SUBTRACTED MAP
tempim=image


;FOR i=0,n_elements(x_pos)-1 DO BEGIN
;tempim[round(x_pos[i])-5,round(y_pos[i])-5:round(y_pos[i])-5+size]=!VALUES.F_NAN
;tempim[round(x_pos[i])-5+size,round(y_pos[i])-5:round(y_pos[i])-5+size]=!VALUES.F_NAN
;tempim[round(x_pos[i])-5:round(x_pos[i])-5+size,round(y_pos[i])-5]=!VALUES.F_NAN
;tempim[round(x_pos[i])-5:round(x_pos[i])-5+size,round(y_pos[i])-5+size]=!VALUES.F_NAN
;ENDFOR
size=2
imsize=size(image,/dimensions)
FOR i=0,n_elements(x_pos)-1 DO BEGIN
IF (round(y_pos[i])-size) gt 0. and (round(x_pos[i])-size) gt 0. and (round(x_pos[i])+size) lt imsize[0] and (round(y_pos[i])+size) lt imsize[1] THEN BEGIN 
tempim[round(x_pos[i]),round(y_pos[i])-size:round(y_pos[i])+size]=!VALUES.F_NAN
tempim[round(x_pos[i])-size:round(x_pos[i])+size,round(y_pos[i])]=!VALUES.F_NAN
ENDIF
ENDFOR

return, tempim


END
