;+
; NAME:
; 
;	XWD2GIF
;
; PURPOSE:
; 
;       Convert an XWD image file to a GIF image file.
;
; CALLING SEQUENCE:
; 
;	XWD2GIF[,FILE=FILE]
;
; KEYWORDS:
; 
;       FILE - The name of the XWD file.  If the XWD file is called
;              file.xwd, then the newly created gif file will be called
;              file.gif.  
;
; PROCEDURE:
;
;       The procedure is just a simple interface to the READ_XWD and
;       WRITE_GIF routines.
;
; MODIFICATION HISTORY:
; 
; 	David L. Windt, Bell Labs, May 1998.
; 	
;       windt@bell-labs.com
;                    
;-
pro xwd2gif,file=file

on_error,2

;; get the file name:
if keyword_set(file) eq 0 then begin
    file=dialog_pickfile(title='Convert which XWD file?',/must_exist, $
                         filter='*.xwd')
    if file eq '' then return
endif

;; read the XWD file:
image=read_xwd(file,r,g,b)

;; extract the root file name:
pos=strpos(file,'.xwd')
if pos ne -1 then root=strmid(file,0,pos) else root=file

;; define the GIF file name:
file=root+'.gif'

;; check if the file exists or not:
ff=findfile(file,count=count)

;; if so, overwrite?
if count gt 0 then begin
    answer=widget_message(/question,'File exists - Overwrite?', $
                          /cancel,/default_cancel,dialog_parent=parent) 
    if answer eq 'Cancel' then return
    if answer eq 'No' then begin
        file=dialog_pickfile(title='Write to which GIF file?', $
                             write=!version.os eq 'MacOS', $
                             filter='*.gif') 
        if file eq '' then return
    endif
endif

;; write the GIF file
write_gif,file,image,r,g,b

return
end
