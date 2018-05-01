;+
; NAME:
;  decdeg_to_dec
; PURPOSE:
;  Convert dec in decimal degrees to sexagesimal
; CALLING SEQUENCE:
;  decdeg_to_dec(decdeg)
; INPUTS
;  decdeg         DEC in decimal degrees, array or scalar
; OPTIONAL INPUTS
;  delimiter     Delimiter in output string, def:':'
;  dp            Number of decimal places (def: 2)
; KEYWORDS
;  None.
; RETURNS
;  Sexagesimal equivalents of input DECs.
; MODIFICATION HISTORY:
;  Author: Mark Sullivan
;-

FUNCTION decdeg_to_dec,decdeg,DELIMITER=delimiter,DP=dp

COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN

IF(N_ELEMENTS(dp) EQ 0)THEN dp=2
IF(N_ELEMENTS(decdeg) EQ 0)THEN BEGIN
   PRINT,'ERROR in DECDEG_TO_DEC: decdeg must be passed'
   RETURN,''
ENDIF

input=SIZE(decdeg,/TYPE)
IF(input NE 4 && input NE 5 && input NE 2 && input NE 3 && input NE 14 && input NE 15 && input NE 12 && input NE 13)THEN BEGIN
   PRINT,'ERROR in DECDEG_TO_DEC: decdeg must be numerical'
   RETURN,''
ENDIF
mydecdeg=DOUBLE(decdeg)
nvalues=N_ELEMENTS(mydecdeg)

IF(N_ELEMENTS(delimiter) EQ 0)THEN delimiter=':'

dec=STRARR(nvalues)

ss=ABS(3600.0d0*mydecdeg)
mm=ABS(60.0d0*mydecdeg) 
dd=ABS(mydecdeg) 

nfail=0
result=DBLARR(3)
FOR i=0L,nvalues-1 DO BEGIN
   IF(decdeg[i] GT 90.d0 || decdeg[i] LT -90.d0)THEN BEGIN
      PRINT,'ERROR in decdeg_to_dec: All DECDEGs must be  -90<=decdeg<=90'
      PRINT,'Entry was '+STRING(decdeg[i])
      nfail=nfail+1
      IF(nfail GE 5)THEN BEGIN
         PRINT,STRING(nfail)+' failues, giving up.'
         RETURN,''
      ENDIF
      dec[i]=''
      CONTINUE
   ENDIF
   result[0]=DOUBLE(FIX(dd[i]))
   result[1]=DOUBLE(FIX(mm[i]-60.0d0*result[0]))
   result[2]=DOUBLE(ss[i]-3600.d0*result[0]-60.0d0*result[1])
   
   IF(result[2] GT 59.)THEN BEGIN
      IF(FLOAT(STRING(result[2],FORMAT='(f'+STRN(dp+3)+'.'+STRN(dp)+')')) GE 60.d0)THEN BEGIN
         result[2]=0.00
         result[1]++
         IF(result[1] GE 60.d0)THEN BEGIN
            result[1]=0.00
            result[0]++
         ENDIF
      ENDIF
   ENDIF

   sign='+'
   IF (mydecdeg[i] LT 0.0d0) THEN BEGIN 
      sign='-'
   ENDIF
   

   dec[i]=sign+STRING(result[0],FORMAT='(i2.2)')+':'+STRING(result[1],FORMAT='(i2.2)')+':'
   dec[i]=dec[i]+STRING(result[2],FORMAT='(F0'+STRN(dp+3)+'.'+STRN(dp)+')')
   IF(STREGEX(dec[i],'\*',/BOOLEAN))THEN stop
ENDFOR

IF ( SIZE(decdeg,/DIMENSION) EQ 0 ) THEN RETURN, dec[0] ELSE RETURN,dec
END


