;+
; NAME:
;
;      CHEM2LBL
;
; PURPOSE:
;
;      Convert a 'chemical name', i.e. a string containing characters
;      and numbers - H2O, for example - into a string containing
;      formatting commands so that the numbers become subscripts when
;      using the result in IDL graphics.
;
;      For example: "H2O" would come back as "H!d2!nO".
;
; CALLING SEQUENCE:
;
;      Result = CHEM2LBL(CHEMICAL)
; 
; INPUTS:
;
;     CHEMICAL - a string specifying the chemical name
;
; KEYWORD PARAMETERS:
; 
;     NOREFERENCE - if this keyword is set, text following an
;                   underscore character in CHEMICAL will be
;                   ignored.  The default behavior is that
;                   any text following an underscore character
;                   will be surrounded by brackets (i.e. < >)
;                   and subscripted.  For example, "SiO2_tetragonal"
;                   will be returned as "!nSiO!d2 <tetragonal>!n"
;
; MODIFICATION HISTORY:
;
;     David L. Windt, March 1997
;     windt@bell-labs.com
;
;-
function chem2lbl,chem,noreference=noreference

on_error,2

if n_params() ne 1 then message,'Usage: Result=CHEM2LBL(CHEM)'

;; first separate chemical symbol from reference:
str=str_sep(chem,'_')
chem=strtrim(str(0),2)
if n_elements(str) gt 1 then ref=strtrim(str(1),2) else ref=''

;; now find any numbers in chemical name - those become
;; subscripts:

lbl=''
down=0
f='!n'
for i=0,strlen(chem)-1 do begin
    ;; get byte value of character:
    char_val=byte(strmid(chem,i,1))
    if (char_val(0) ge 46) and (char_val(0) le 57) then begin
        ;; if the value is between 46 and 57, it's a number, and
        ;; should be subscripted:
        if down then f='' else f='!d'
        down=1
    endif else begin
        ;; otherwise, it's not a subscript
        if not down then f='' else f='!n'
        down=0
    endelse
    lbl=lbl+f+strmid(chem,i,1)
endfor

if (keyword_set(noreference) eq 0) and ref ne '' then begin
    if down then f=' ' else f=' !d'
    lbl=lbl+f+'<'+ref+'>'
    down=1
endif
if down then f='!n' else f=''
return,lbl+f
end

