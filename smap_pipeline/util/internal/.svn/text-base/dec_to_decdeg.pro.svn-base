;+
; NAME:
;  dec_to_decdeg
; PURPOSE:
;  Convert sexagesimal declinations to decimal degrees
; CALLING SEQUENCE:
;  dec_to_decdeg(dec)
; INPUTS
;  dec       - String or array of strings containing the declinations
;              to convert
; RETURNS
;  decdeg    - declinations converted to decimal degrees
; OPTIONAL INPUTS
;  delimiter - the character that delimits in the sexagesimal
;              string. Def: ':'
; OPTIONAL OUTPUTS
;  success   - success flag. 1=ok, 0=fail, -1=partial fail (<=3 failures)
;  nfail     - for success=-1, the number of conversions that failed
; MODIFICATION HISTORY:
;  Author: Mark Sullivan
;-

FUNCTION dec_to_decdeg,dec,DELIMITER=delimiter,SUCESS=success,NFAIL=nfail

COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN

nfailmax=3
success=0
IF(N_ELEMENTS(dec) EQ 0)THEN BEGIN
   PRINT,'ERROR in DEC_TO_DECDEG: dec must be passed'
   RETURN,!Values.D_NAN
ENDIF
IF(N_ELEMENTS(delimiter) EQ 0)THEN delimiter=':'

nvalues=N_ELEMENTS(dec)
decdeg=DBLARR(nvalues)

nfail=0
FOR i=0L,nvalues-1 DO BEGIN
   temp=STRSPLIT(dec[i],delimiter,/EXTRACT,COUNT=nsplit)
   IF(nsplit NE 3 && nsplit NE 2)THEN BEGIN
      PRINT,'ERROR in DEC_TO_DECDEG: Split did not work'
      PRINT,'Input was '+dec[i]
      nfail=nfail+1
      IF(nfail GT nfailmax)THEN BEGIN
         PRINT,STRN(nfail)+' failues, giving up.'
         success=0
         RETURN,!Values.D_NAN
      ENDIF
      decdeg[i]=!Values.D_NAN
      success=-1
      CONTINUE
   ENDIF
   minus=1.0
   IF(STRMID(dec[i],0,1) EQ '-')THEN minus=-1.d0
   IF(nsplit EQ 2)THEN temp=[temp,0.d0]
   decdeg[i]=(ABS(DOUBLE(temp[0]))+(DOUBLE(temp[1])/60.d0)+(DOUBLE(temp[2])/3600.d0))*minus
ENDFOR

IF(nfail EQ 0)THEN success=1
IF ( SIZE(dec,/DIMENSION) EQ 0 ) THEN RETURN, decdeg[0] ELSE RETURN,decdeg
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
