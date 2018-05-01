;+
; NAME:
;  ra_to_radeg
; PURPOSE:
;  Convert sexagesimal right ascensions to decimal degrees
; CALLING SEQUENCE:
;  ra_to_radeg(ra)
; INPUTS
;  ra        - String or array of strings containing the RAs
;              to convert
; RETURNS
;  radeg     - RAs converted to decimal degrees
; OPTIONAL INPUTS
;  delimiter - the character that delimits in the sexagesimal
;              string. Def: ':'
; OPTIONAL OUTPUTS
;  success   - success flag. 1=ok, 0=fail, -1=partial fail (<=3 failures)
;  nfail     - for success=-1, the number of conversions that failed
; MODIFICATION HISTORY:
;  Author: Mark Sullivan
;-
FUNCTION ra_to_radeg,ra,DELIMITER=delimiter,SUCCESS=success,NFAIL=nfail

COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN

nfailmax=3
success=0
IF(N_ELEMENTS(ra) EQ 0)THEN BEGIN
   PRINT,'ERROR in RA_TO_RADEG: ra must be passed'
   RETURN,!Values.D_NAN
ENDIF

IF(N_ELEMENTS(delimiter) EQ 0)THEN delimiter=':'

nvalues=N_ELEMENTS(ra)
radeg=DBLARR(nvalues)

nfail=0
FOR i=0L,nvalues-1 DO BEGIN
   temp=STRSPLIT(ra[i],delimiter,/EXTRACT,COUNT=nsplit)
   IF(nsplit NE 3 && nsplit NE 2)THEN BEGIN
      PRINT,'ERROR in RA_TO_RADEG: Split did not work'
      PRINT,'Input was '+ra[i]
      nfail=nfail+1
      IF(nfail GT nfailmax)THEN BEGIN
         PRINT,STRN(nfail)+' failues, giving up.'
         success=0
         RETURN,!Values.D_NAN
      ENDIF
      radeg[i]=!Values.D_NAN
      success=-1
      CONTINUE
   ENDIF
   IF(nsplit EQ 2)THEN temp=[temp,0.d0]
   radeg[i]=DOUBLE(temp[0])*15.d0+(DOUBLE(temp[1])/60.d0)*15.d0+(DOUBLE(temp[2])/3600.d0)*15.d0
ENDFOR

IF(nfail EQ 0)THEN success=1
IF ( SIZE(ra,/DIMENSION) EQ 0 ) THEN RETURN, radeg[0] ELSE RETURN,radeg
END
