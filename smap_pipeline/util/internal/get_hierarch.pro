;+
; NAME: 
;        get_hierarch
; CATEGORY
;        Herschel SPIRE SMAP pipeline
; PURPOSE: 
;       Obtain the value of the Hierarch parameters in a fits header
;       for Herschel HCSS data.
; CALLING SEQUENCE: 
;       Result = get_hierarch( HDR [, COUNT= ])
; REQUIRED INPUTS: 
;       HDR     = FITS header string array (e.g. as returned by FXREAD).  Each
;                 element should have a length of 80 characters
; OUTPUT: 
;       A Herschel HIERARCH header is of the form
;        HIERARCH key.META_17='maskDead'
;       This returns a count by 2 array, where the [*,0] returns the
;        key names and [*,1] the value.  The above, for example, would
;        be ['META_17','maskDead'].
; OPTIONAL OUTPUT KEYWORD:
;       COUNT   = Optional keyword to return a value equal to the number of
;                 parameters found by FXPAR.
;       SUCCESS = 1b if some found, 0 if not
; PROCEDURES CALLED
;       herschel_fxpar
; REVISION HISTORY: 
;       Written by Alex Conley, May 1, 2009
;-

FUNCTION get_hierarch, hdr, COUNT=nfound, SUCCESS=success

COMPILE_OPT IDL2, STRICTARRSUBS

success = 0b

IF N_PARAMS() LT 1 THEN BEGIN
   PRINT,'Syntax:  result =  get_hierarch( HDR )'
   RETURN, -1
ENDIF

ON_ERROR,2

values = HERSCHEL_FXPAR(hdr,'HIERARCH', COUNT=nfound)
IF nfound EQ 0 THEN RETURN,!VALUES.F_NAN

;;Make sure they are key.whatever values
keyval = STRMID(values,0,3)
wkey = WHERE( keyval EQ 'key', nkey, NCOMPLEMENT=nbad )
IF nkey EQ 0 THEN RETURN,!VALUES.F_NAN
IF nbad NE 0 THEN values = values[wkey]

;;Now we split off the key tags
key_removed = STRMID(values,4)

;;Split at the equals now
eqpos = STRPOS(key_removed,'=')
wgood = WHERE( eqpos NE -1, nfound, NCOMPLEMENT=nbad )
IF nfound EQ 0 THEN RETURN,!VALUES.F_NAN
IF nbad NE 0 THEN key_removed = key_removed[wgood]
;;Have to for loop this, unfortunately
retarr = STRARR(nfound,2)
FOR i=0, nfound-1 DO $
   retarr[i,0] = STRMID( key_removed[i], 0, eqpos[i] )
FOR i=0, nfound-1 DO BEGIN
   st = STRMID( key_removed[i], eqpos[i]+1 )
   ;;Remove leading/trailing '
   firstchar = STRMID(st,0,1)
   lastchar = STRMID(st,STRLEN(st)-1)
   remove_firstchar = ( firstchar EQ "'" OR firstchar EQ '"')
   remove_lastchar = ( lastchar EQ "'" OR lastchar EQ '"')
   IF ~ ( remove_firstchar OR remove_lastchar ) THEN retarr[i,1]=st ELSE BEGIN
      IF ( remove_firstchar && remove_lastchar ) THEN BEGIN
         retarr[i,1] = STRMID( st, 1, STRLEN(st)-2 ) 
      ENDIF ELSE IF remove_firstchar THEN BEGIN
         retarr[i,1] = STRMID( st, 1 )
      ENDIF ELSE retarr[i,1] = STRMID( st, 0, STRLEN(st)-2 )
   ENDELSE
ENDFOR

success=1b
RETURN,retarr

END
