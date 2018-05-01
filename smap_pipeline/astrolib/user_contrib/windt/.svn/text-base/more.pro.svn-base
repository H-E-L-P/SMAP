;+
; NAME:
;
;      MORE
;      
; PURPOSE:
;
;       Print one or more variables on the screen or to a file, using
;       the MORE keyword to printf.
;
; CALLING SEQUENCE:
;
;       MORE,v0[,v1,v2,...v19]
;
; INPUTS:
;
;       V0,V1,...V19 - Any type of array variables; they must all be
;                      the same length.
;
; KEYWORD PARAMETERS:
;
;   FILE - string specifying the name of an output file.
;
;   INDEX - set this to one to print the array indices in the first
;           column.
;
;   TITLE - string array of variable names.
;
;   COMMENT - string array of comments
;
;   TAB - set this keyword to create tab-separated data; this is
;         useful when writing to a file if any of the variables are
;         strings, in which case the data can be read using EROM,/TAB
;
; MODIFICATION HISTORY:
; 
;	David L. Windt, Bell Labs, March 1990
;
;	Added comment keyword, August 1992
;	
;       March 1997 - Title and comment lines are now written with
;                    preceding semicolons.  Fixed bug to correctly
;                    deal with string arrays.  Added TAB keyword.
;                    Removed NOINDEX keyword.  Added INDEX keyword.
;
;       November 1997 - Removed Unix-specific stuff, so that it now
;                       works (somewhat) under Windows and MacOS.
;                    
;       windt@bell-labs.com
;-

pro more,v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,v18,v19, $
         index=index,file=file,title=title,comment=comment,tab=tab
on_error,2

unix=!version.os_family eq 'unix'
if n_params() eq 0 then return
if keyword_set(file) then openw,lun,file,/get_lun,width=120 else  $
  if unix then openw,lun,'/dev/tty',/more,/get_lun  $
  else lun=-1
if keyword_set(comment) then $
  for i=0,n_elements(comment)-1 do printf,lun,';  '+comment(i)

if keyword_set(title) then begin
    if keyword_set(index) then line='; index' else line='; ' 
    for i=0,n_elements(title)-1	do line=line+' '+title(i)
    printf,lun,line
    printf,lun,';'+string(replicate(45b,strlen(line)))
endif

sz=n_elements(v0)		; check that all variables are the same size.
for i=0,n_params()-1 do begin
    e=execute('var=v'+strtrim(i,2))
    if sz ne n_elements(var) then  $
      message,'more: variables must be of the same size.'
endfor
if keyword_set(index) then st='j,' else st='' 
if keyword_set(tab) then sep=" '	' " else sep=" ' ' "
for i=0,n_params()-1 do begin
    if i gt 0 then st=st+','+sep+','
    st=st+'v'+strtrim(i,2)+'(j)'
endfor
for j=0,sz-1 do e=execute('printf,lun,'+st) 

if lun gt 0 then free_lun,lun
return
end


