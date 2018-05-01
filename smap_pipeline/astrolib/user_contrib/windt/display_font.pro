;+
; NAME:
;
;       DISPLAY_FONT
;       
; PURPOSE:
;
;       Display the font sets listed in the IDL User's Guide.
;       
; CALLING SEQUENCE:
; 
;       DISPLAY_FONT[,FONT_NUMBER,HARDWARE=HARDWARE]
;       
; OPTIONAL INPUT PARAMETERS:
; 
;      FONT_NUMBER - The font index. If not supplied, the user is
;                    prompted for input.
;			
; KEYWORD PARAMETERS:
; 
;      HARDWARE - set to use the hardware fonts (i.e. PostScript for
;                 !d.name='PS') set; otherwise Hershey sets are used.
;			
; MODIFICATION HISTORY:
; 
;	D. L. Windt, Bell Laboratories, Sept. 1991
;	windt@bell-labs.com
;-

pro display_font,font_number,hardware=hardware

tek_color

if n_params() ne 1 then begin
	font_number='3'
	read,'Enter the font number to display [3 -- 20]: ',font_number
	endif
font='!'+strtrim(font_number,2)

!p.font=-1
if keyword_set(hardware) then !p.font=0

chars=['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q', $
	'R','S','T','U','V','W','X','Y','Z','[','\',']','^','_',"`",'a','b','c', $
	'd','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u', $
	'v','w','x','y','z','!','"','#','$','%','&',"'",'(',')','*','+',',','-', $
	'.','/','0','1','2','3','4','5','6','7','8','9',':',';','<', $
	'=','>','?','@']
n_chars=n_elements(chars)
erase
for i=0,9 do for j=0,8 do begin
	xyouts,j/9.,.9-i/10.,/normal,'!3'+chars(i*9+j),size=(!p.charsize>1)
	xyouts,j/9.+.025,.9-i/10.+.025,/normal,font+chars(i*9+j), $
		size=(!p.charsize>1)*1.5,color=5
	endfor

return
end

	
	
