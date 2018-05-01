;+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  function isfield.pro
;;  Aug 21, 2009
;;  Mike Zemcov
;;  This function takes a structure and a field name and returns 1 if
;;   that field name is part of the structure or 0 if not.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-
FUNCTION ISFIELD,instruct,infield

  mytags = STRLOWCASE(TAG_NAMES(instruct))
  thispos = STRPOS(infield,mytags)
  
  IF thispos GE 0 THEN youn = 1 ELSE youn = 0

  RETURN,youn

END
