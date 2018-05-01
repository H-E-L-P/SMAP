;+
; NAME:
; 
;       GHOSTVIEW
;
; PURPOSE:
; 
;       Use the Unix ghostview program to view an IDL postscript file
;
; CALLING SEQUENCE:
; 
;       GHOSTVIEW [,FILE=FILE]
; 
; KEYWORD PARAMETERS:
; 
;       FILE - the name of the file to view.  Default is idl.ps
;
; RESTRICTIONS:
;
;       Since the procedure spawns a "ghostview" process, 
;       such an executable must exist or it ain't goin' nowhere.
;
; PROCEDURE:
;     
;       If the current device is PS, the program will issue
;       a DEVICE,/CLOSE command.
;
;       It will then SPAWN,'ghostview file_name&'
;
; MODIFICATION HISTORY:
; 
;            David L. Windt, Bell Labs, March 1997
;            windt@bell-labs.com
;-
pro ghostview,file=file
on_error,2
if n_elements(file) eq 0 then file='idl.ps'
if !d.name eq 'PS' then device,/close
spawn,'ghostview '+file+' &'
return
end
