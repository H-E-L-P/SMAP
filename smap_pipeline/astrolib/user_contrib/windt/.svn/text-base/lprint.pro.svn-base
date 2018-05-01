;+
; NAME:
;
;       LPRINT
;       
; PURPOSE:
;
;       Close an IDL graphics file and print it.
;       
; CALLING SEQUENCE:
; 
;       LPRINT
;       
; KEYWORD PARAMETERS:
;
;       NORETURN - set this keyword to inhibit executing
;                  set_plot,getenv('IDL_DEVICE') followed by
;                  !p.font=-1
;                          
;       FILE - the name of the file to print. Default is device
;              dependent: idl.ps for PS, idl.hp for HP, and idl.pcl
;              for PCL devices.
;		       
;       PRINTER - set to the name of the printer to use. Default = lp
;		
;       COMMAND - set to the name of the printer command to 
;                 use. Default = lpr.
;		          
;		          
;       Note: the COMMAND, PRINTER, and FILE keywords are combined as
;               follows:
;
;       if COMMAND='lpr', then the program spawns the unix command
;                        "lpr -Pprinter file"
;                        
;       if COMMAND='lp', then the program spawns the unix command
;                        "lp dprinter file"
;
;       if COMMAND is anything else, the program simply ignores the
;                        printer and file keywords, and spawns the
;                        command as is.
; 
; MODIFICATION HISTORY:
; 
;	D. L. Windt, Bell Laboratories, November 1989
;	Added PRINTER keyword, June 1993.
;		
;       Added COMMAND keywrd, replaced RETURN with NORETURN keyword,
;                     and added code to execute !p.font=-1 unless
;                     NORETURN keyword is set. March, 1997.
;		      
;      windt@bell-labs.com
;-
pro lprint,file=file,noreturn=noreturn,printer=printer,command=command

on_error,2
if n_elements(file) eq 0 then begin
    case !d.name of
        'PS': file='idl.ps'
        'HP': file='idl.hp'
        'PCL': file='idl.pcl'
        else: message,"Sorry - I can't print from the current graphics device."
    endcase
endif
device,/close
if n_elements(printer) eq 0 then printer='lp'
if n_elements(command) eq 0 then command='lp'
case command of
    'lpr': command='lpr -P'+strtrim(printer,2)+' '+file
    'lp': command='lp -d'+strtrim(printer,2)+' '+file
    else: ;;
endcase
spawn,command
if keyword_set(noreturn) eq 0 then begin
    set_plot,getenv('IDL_DEVICE')
    !p.font=-1
endif
return
end
