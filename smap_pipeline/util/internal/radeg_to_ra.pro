;+
; NAME:
;  radeg_to_ra
; PURPOSE:
;  Convert ra in decimal degrees to sexagesimal
; CALLING SEQUENCE:
;  radeg_to_ra(radeg)
; INPUTS
;  radeg         RA in decimal degrees, array or scalar
; OPTIONAL INPUTS
;  delimiter     Delimiter in output string, def:':'
;  dp            Number of decimal places (def: 3)
; KEYWORDS
;  None.
; RETURNS
;  Sexagesimal equivalents of input RAs.
; MODIFICATION HISTORY:
;  Author: Mark Sullivan
;-

FUNCTION radeg_to_ra,radeg,DELIMITER=delimiter,DP=dp

COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN

IF(N_ELEMENTS(dp) EQ 0)THEN dp=3
IF(N_ELEMENTS(radeg) EQ 0)THEN BEGIN
   PRINT,'ERROR in RADEG_TO_RA: radeg must be passed'
   RETURN,!Values.D_NAN
ENDIF

input=SIZE(radeg,/TYPE)
IF(input NE 4 && input NE 5 && input NE 2 && input NE 3 && input NE 14 && input NE 15 && input NE 12 && input NE 13)THEN BEGIN
   PRINT,'ERROR in RADEG_TO_RA: radeg must be numerical'
   RETURN,!Values.D_NAN
ENDIF
myradeg=DOUBLE(radeg)
nvalues=N_ELEMENTS(myradeg)

FOR i=0L,nvalues-1 DO BEGIN
   IF(radeg[i] LT 0.d0)THEN BEGIN
      PRINT,'ERROR in RADEG_TO_RA: All passed radegs must be GE zero'
      RETURN,!Values.D_NAN
   ENDIF
ENDFOR

IF(N_ELEMENTS(delimiter) EQ 0)THEN delimiter=':'

ra=STRARR(nvalues)

temp=myradeg/15.d0

ss=ABS(3600.0d0*temp)
mm=ABS(60.0d0*temp) 
dd=ABS(temp) 

result=DBLARR(3)
FOR i=0L,nvalues-1 DO BEGIN
   result[0]=DOUBLE(FIX(dd[i]))
   result[1]=DOUBLE(FIX(mm[i]-60.0d0*result[0]))
   result[2]=DOUBLE(ss[i]-3600.d0*result[0]-60.0d0*result[1])

   IF(result[2] GT 59.)THEN BEGIN
      IF(FLOAT(STRING(result[2],FORMAT='(F'+STRN(dp+3)+'.'+STRN(dp)+')')) GE 60.d0)THEN BEGIN
         result[2]=0.00
         result[1]++
         IF(result[1] GE 60.d0)THEN BEGIN
            result[1]=0.00
            result[0]++
         ENDIF
      ENDIF
   ENDIF
   
   ra[i]=STRING(result[0],FORMAT='(i2.2)')+':'+STRN(result[1],FORMAT='(i2.2)')+':'
   ra[i]+=STRING(result[2],FORMAT='(F0'+STRN(dp+3)+'.'+STRN(dp)+')')
ENDFOR

IF ( SIZE(radeg,/DIMENSION) EQ 0 ) THEN RETURN, ra[0] ELSE RETURN,ra
END
