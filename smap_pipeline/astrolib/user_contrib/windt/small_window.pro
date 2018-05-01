;+
; NAME:
;
;       SMALL_WINDOW
;       
; PURPOSE:
;
;       Make a 500x400 graphics window.
;       
; CATEGORY:
; 
;       Stupid little convenience routines.
;       
; CALLING SEQUENCE:
; 
;	SMALL_WINDOW [,WINDOW_NUM]
;	
; OPTIONAL INPUT PARAMETERS:
; 
;	WINDOW_NUM - the window number.
;	
; KEYWORD PARAMETERS:
; 
;       LAPTOP - set this to make a nice window (400x300) for a
;                small-screened laptop
;	
; MODIFICATION HISTORY:
; 
;	David L. Windt, Bell Laboratories, March 1990.
;	windt@bell-labs.com
;	
;	
;-
pro small_window,window_num,laptop=laptop,_extra=_extra
on_error,2

if keyword_set(laptop) then begin
    if n_elements(window_num) ne 0 then $
      window,xsize=400,ysize=300,xpos=230,ypos=180, $
      window_num,_extra=_extra else $
      window,xsize=400,ysize=300,xpos=230,ypos=180,/free,_extra=_extra
endif else $
  if n_elements(window_num) ne 0 then $
  window,xsize=500,ysize=400,window_num,_extra=_extra $
else window,xsize=500,ysize=400,/free,_extra=_extra
end





