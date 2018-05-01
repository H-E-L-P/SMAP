PRO SMAP_CHECK_ASTROM_ALL, date, STOP=stop, PS=ps

;+
; SMAP_CHECK_ASTROM_ALL, date
;
; Check astrometry on all fields for 'date'.
;
; CREATED BY: gmarsden 2011-07-07
;-

mapbasedir = "/data/spire/maps"

mapbasedir = ADDSLASH(mapbasedir)
allfields =  FILE_SEARCH(mapbasedir+'*', /TEST_DIR, COUNT=nfields)

IF KEYWORD_SET(ps) THEN BEGIN
    SET_PLOT, 'ps'
    DEVICE, FILENAME="astrom_"+date+".ps", XSIZE=15, YSIZE=15, $
            /COL, /DECOMPOSED
    pcolsave = !P.COLOR
    !P.COLOR = 2L^24 - 1
ENDIF

; find fields for which there is a "date"
FOR i=0,nfields-1 DO BEGIN
    fieldname = FILE_BASENAME(allfields[i])
    IF STRCMP(STRMID(fieldname, 3, /REV), "nest") OR $
       STRCMP(STRMID(fieldname, 5, /REV), "filter") THEN CONTINUE
    IF FILE_TEST(ADDSLASH(allfields[i]) + date, /DIR) THEN BEGIN
        PRINT, fieldname+":"
        SMAP_CHECK_ASTROM, fieldname, date, PS=ps
        IF KEYWORD_SET(stop) THEN STOP
    ENDIF ELSE $
        MESSAGE, "Field '"+fieldname+"' does not have map for date", /INF
ENDFOR

IF KEYWORD_SET(ps) THEN BEGIN
    DEVICE, /CLOSE
    SET_PLOT, 'x'
    !P.COLOR = pcolsave
ENDIF

END
