;+
; NAME:
;
;     FILE_DATE
;
;
; PURPOSE:
;
;    Determine Unix file creation date.
;
;
; CALLING SEQUENCE:
;
;   Result=FILE_DATE(FILE_NAME)
;
; 
; INPUTS:
;
;   FILE_NAME - A string specifying the name of the file
;
;
; OUTPUTS:
;
;   Result - a string specifying the file creation date.
;
; RESTRICTIONS:
;
;   Probably won't work the way you want.  So sue me.
;
; MODIFICATION HISTORY:
;
;   David L. Windt, Bell Labs, May 1997
;   windt@bell-labs.com 
;
;-

function file_date,file_name

spawn,"ls -l "+file_name+" | awk '{print $6;}'",month
spawn,"ls -l "+file_name+" | awk '{print $7;}'",day
spawn,"ls -l "+file_name+" | awk '{print $8;}'",year

return,day+' '+month+' '+year
end




