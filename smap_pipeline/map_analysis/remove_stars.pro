function remove_stars, image, x_pos, y_pos, flux, psf, alg

;+
; NAME:
;      remove_stars
; PURPOSE:
;      Make a check image with detected sources circled in a circle of radius, size 
; EXPLANATION:
;     This is intended as a subroutine of ps_extractor, but can be used independentsly  
;
; CALLING SEQUENCE:
;     rm_im=remove_stars(im250, ,x250, y250, f250, psf_P*W, 1)
;
; INPUTS:
;     image   - original input image
;     flux - flux estimated from ps_extractor
;     x_pos, y_pos - x and y positions of detected sources in pixels 
;     psf - should be the instrument psf, for a specific array, with odd dimensions and the center at (dim-1)/2
;     alg - algorithm for starfinding (1 = find2, 2 = starfinder)
;
; OPTIONAL INPUTS:
;     NONE - should implement possibility of taking ra,dec
;
; OPTIONAL KEYWORD INPUTS:
;     NONE
;
; RETURNS:
;     source free image - original image with sources removed
;
; EXAMPLE:
;	make a source removed image from an image called im250:
;       IDL> rm_im=remove_stars(im250, ,x250, y250, f250, psf_PSW)
;       
; PROCEDURES USED:
;       READFITS(), AVG, SORT, PS_OPEN, PS_CLOSE
 On_error,2                         ;Return to caller
 compile_opt idl2

; COPY INPUT MAP
rm_im=image


dim=size(rm_im,/dimensions)

lowsub=10
hisub=20

sizepsf=size(psf,/dimensions)
cx=(sizepsf[0]-1)/2
cy=(sizepsf[1]-1)/2

FOR i=0,n_elements(x_pos)-1 DO BEGIN
FOR l = 0, sizepsf[0]-1 DO BEGIN
FOR m = 0, sizepsf[1]-1 DO BEGIN
IF round(x_pos[i])-cx+l ge 0 and round(y_pos[i])-cy+m gt 0  and round(x_pos[i])-cx+l lt dim[0] and round(y_pos[i])-cy+m lt dim[1] THEN BEGIN
IF alg eq 2 THEN rm_im[round(x_pos[i])-cx+l,round(y_pos[i])-cy+m]-=flux[i]*psf[l,m]/total(psf) 
IF alg eq 1 THEN rm_im[round(x_pos[i])-cx+l,round(y_pos[i])-cy+m]-=flux[i]*psf[l,m]
; TRY ONE MORE THING: REMOVE STARS BASED ON ACTUAL PIXEL VALUE NOT ON ESTIMATED FLUX
;IF alg eq 2 THEN rm_im[round(x_pos[i])-cx+l,round(y_pos[i])-cy+m]-=rm_im[round(x_pos[i])-cx+l,round(y_pos[i])-cy+m]*psf[l,m]/total(psf) 
;IF alg eq 1 THEN rm_im[round(x_pos[i])-cx+l,round(y_pos[i])-cy+m]-=rm_im[round(x_pos[i])-cx+l,round(y_pos[i])-cy+m]*psf[l,m]
ENDIF
ENDFOR; m
ENDFOR; l
ENDFOR; i

; SAVE THIS, IT WORKS GREAT EXCEPT FOR THE EDGE OF THE IMAGE
;FOR i=0,n_elements(x_pos)-1 DO BEGIN
;IF round(x_pos[i])-lowsub ge 0 and round(y_pos[i])-lowsub gt 0  and round(x_pos[i])-lowsub+hisub lt dim[0] and round(y_pos[i])-lowsub+hisub lt dim[1] THEN BEGIN
;subrm_im=rm_im[round(x_pos[i])-lowsub:round(x_pos[i])-lowsub+hisub,round(y_pos[i])-lowsub:round(y_pos[i])-lowsub+hisub]
;subrm_im-=flux[i]*psf 
;;subrm_im-=subrm_im[lowsub,lowsub]*psf_psw 
;rm_im[round(x_pos[i])-lowsub:round(x_pos[i])-lowsub+hisub,round(y_pos[i])-lowsub:round(y_pos[i])-lowsub+hisub]=subrm_im
;ENDIF
;ENDFOR

return, rm_im
END