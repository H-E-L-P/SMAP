;+
; NAME:
; 
;	PWD
;	
; CATEGORY:
; 
;       Stupid little convenience routines.
;   
; PURPOSE:
; 
;       Print the current directory, like the Unix 'pwd' command.
;	
; CALLING SEQUENCE:
; 
;       PWD
; MODIFICATION HISTORY:
;
;       David L. Windt, Bell Labs, February 1998
;       windt@bell-labs.com
;
;-
pro pwd
on_error,2

if !version.os_family eq 'unix' then begin
    command='pwd'
    spawn,command
endif else begin
    cd,'',current=cwd
    cd,cwd
    print,cwd
endelse
end
