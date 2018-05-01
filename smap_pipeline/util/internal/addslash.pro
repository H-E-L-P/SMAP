;+
;NAME
; addslash
;PURPOSE
; To add a slash to the end of a directory string if one isn't present
;USAGE
; dir = addslash('/home/user/test')
;MODIFICATION HISTORY
; Author: Alex Conley, Apr 12, 2006
;-
FUNCTION addslash,name
  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN
  ON_ERROR,2

  IF N_ELEMENTS(name) EQ 0 THEN MESSAGE,"No argument provided to ADDSLASH"

  path_sep_char = PATH_SEP()
  IF(SIZE(name,/DIMENSIONS) GT 0)THEN BEGIN
     ;; array
     FOR i=0,N_ELEMENTS(name)-1 DO BEGIN
        IF(STRMID(name[i],0,1,/REVERSE_OFFSET) NE path_sep_char)THEN name[i]=name[i]+path_sep_char
     ENDFOR
     RETURN,name
  ENDIF ELSE BEGIN
     ;; scalar
     IF STRMID(name,0,1,/REVERSE_OFFSET) NE path_sep_char THEN $
        RETURN,name + path_sep_char ELSE RETURN,name
  ENDELSE

END
