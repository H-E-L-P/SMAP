;+
; NAME:
;
;       REC_IMAGE
;       
; PURPOSE:
; 
;       Extract a rectangular portion of a previously displayed image.
;       
; CALLING SEQUENCE:
; 
;	SMALL_IMAGE=REC_IMAGE(BIG_IMAGE)
;	
; INPUTS:
; 
;       BIG_IMAGE = array containing original image
;		
; OUTPUTS:
; 
;	SMALL_IMAGE = portion of big_image
;		
; PROCEDURE:
; 
;	RECROI is used to digitize a portion of the image.
;		
; MODIFICATION HISTORY:
; 
;	David L. Windt, Bell Labs, Feb. 1992.
;	windt@bell-labs.com
;	
;-
function rec_image,image
on_error,2
sz=size(image)
roi=recroi(sz(1),sz(2),xverts,yverts)
return,image(xverts(0):xverts(1),yverts(1):yverts(2))
end
