;+
; NAME:
;
;      MAKE_LATEX_TBL
;
; PURPOSE:
; 
;	Create a LaTeX format table.
;	
; CALLING SEQUENCE:
; 
;	MAKE_LATEX_TBL,ARRAY,TFILE
;	
; INPUTS:
; 
;	ARRAY - (n,m) array of data.
;	
;	TFILE - string specifying the name of the '.tex' file to create.
;	
; KEYWORD PARAMETERS:
; 
;	COLUMNS - An n-element string specifying the LaTeX column
;                 format. For example, if array = (3,m), then an
;                 acceptable value for columns would be
;                 ['|l|','|c|','|c|'], which would left-justify the
;                 first column of data, and center the remaining two.
;	    
;       FORMAT - an n-element string specifying the FORMAT used to
;                PRINTF the data.  This must conform to IDL FORMAT
;                standards.  If not set, the default the data are
;                printed using the IDL free format.
;	    
;	TITLE - a string specifying the title of the table.
;	
;       HEADINGS - an n-element string array containing the table
;                  headings.
;	    
;	NOHLINES - set to inhibit printing \hline between rows of data.
;	
; SIDE EFFECTS:
; 
;		The '.tex' file is created.
;		
; RESTRICTIONS:
; 
;	The TITLE is printed with vertical lines on either ; side,
;       regardless of how the COLUMNS parameter may be ; set.  It is
;       thus necessary to edit the file to remove the vertical line
;       commands if desired.
;	
; PROCEDURE:
; 
;	The data contained in ARRAY are printed to a file ; with the
;       appropriate '&' and '\\' symbols necessary ; for use as in the
;       LaTeX tabular environment.  If ; COLUMNS is not set, the
;       default is '|c|' which centers the data in columns, with
;       vertical line separators.
;	
; MODIFICATION HISTORY:
; 
;	David L. Windt, Bell Laboratories, December 1989.
;	windt@bell-labs.com
;-
pro make_latex_tbl,array,tfile,format=format,columns=columns,title=title, $
    headings=headings,nohlines=nohlines
on_error,2

a_size=size(array)		; get size of array.
n_columns=a_size(1)		; number of columns.

print,'Writing data to '+tfile+'.tex...'
if n_elements(headings) ne 0 then begin
    h_size=size(headings)	; get size of headings.
    if n_columns ne h_size(1) then begin
	print,'make_latex_tbl: HEADINGS keyword parameter not dimensioned correctly.'
	print,'make_latex_tbl: ...inserting blank headings instead...'
	headings=strarr(a_size(1))
	endif
    endif

; make LaTeX columns specification...
if keyword_set(columns) eq 0 then begin & $
    columns='|' & for i=0,n_columns-1 do columns=columns+'c|' & end

openw,lun,tfile+'.tex',/get_lun	    ; open the file.

printf,lun,'\begin{tabular}{'+columns+'}'   ; first line.
printf,lun,'\hline'

; title...
if keyword_set(title) then printf,lun,'\multicolumn{'+ $
    strtrim(n_columns,2)+'}{|c|}{'+title+'} \\ \hline\hline'

; headings...
if n_elements(headings) ne 0 then begin	   
    line=headings(0)
    for i=1,n_columns-1 do line=line+' & '+headings(i)
    line=line+' \\ \hline'
    printf,lun,line
    endif

; data...
for j=0,a_size(2)-1 do begin
    if n_elements(format) ne 0 then begin
	line=strtrim(string(array(0,j),format=format(0)),2)
	for i=1,n_columns-1 do line=line+' & '+strtrim(string(array(i,j), $
	    format=format(i)),2)
	endif else begin
	    line=strtrim(string(array(0,j)),2)
	    for i=1,n_columns-1 do line=line+' & '+strtrim(string(array(i,j)),2)
	    endelse
    line=line+' \\'
    printf,lun,line
    if keyword_set(nohlines) eq 0 then printf,lun,'\hline'
    endfor

; finish up
printf,lun,'\hline'
printf,lun,'\end{tabular}'
free_lun,lun

print,'Done.'
return
end

