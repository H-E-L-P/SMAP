FUNCTION SMAP_GET_ASTROMOFFSETS, dataname, VERBOSE=verbose, ERROR=error

;+
;
; offset_info = SMAP_GET_ASTROMOFFSETS(dataname, VERBOSE=verbose, ERROR=error)
;
; Looks up astrometry offset info file for data set "dataname" and
; returns array of info structures:
;
;   offset_struct = {obsid:     0L, $
;                    delta_RA:  0.0, $
;                    delta_Dec: 0.0, $
;                    RA_0:      0.0, $
;                    Dec_0:     0.0}
;
; This is a shift of (delta_RA, delta_Dec) in a tangent plane centered
; at (RA_0, Dec_0). All quantities in degrees.
;
; If there is an error in finding the obsid, offset_info = -1 is
; returned and error is set to 1.
; 
; Data is read from the file <dataname>.dat in <astromoffsetdir>
; (defined below)
;
; Multiple datanames can be specified as array of strings.
; 
; CREATED BY GMARSDEN 2011-02-15
;
; CHANGELOG
;   20110225 (GM) remove "description" field from offsets file
;   20110228 (GM) add ability to read multiple datanames
;   20110303 (GM) change location of astromoffsets
;-

; astrometry offset data file info
astromoffsetsdir = "/data/spire/astromoffsets/"
filesuffix = ".dat"

offsets_info = -1
error = 1B

offsets_struct = {obsid:     0L, $
                  delta_RA:  0.0, $
                  delta_Dec: 0.0, $
                  RA_0:      0.0, $
                  Dec_0:     0.0}

FOR n=0,N_ELEMENTS(dataname)-1 DO BEGIN

    offsetsfile = astromoffsetsdir + dataname[n] + filesuffix

    IF KEYWORD_SET(verbose) THEN $
       MESSAGE, /INF, "Reading astrom offsets file '"+offsetsfile+"'"

    ; Test if file exists
    IF NOT FILE_TEST(offsetsfile, /READ) THEN BEGIN
        IF KEYWORD_SET(verbose) THEN $
           MESSAGE, /INF, "No astrometry offsets file found"
        RETURN, -1
    ENDIF

    ; read file (ignore field name column)
    READCOL, offsetsfile, FORM="L,D,D,D,D", COMM="#", /SILENT, $
             fobsid, fraoff, fdecoff, fra0, fdec0

    thisoi = REPLICATE(offsets_struct, N_ELEMENTS(fobsid))

    thisoi.obsid     = fobsid
    thisoi.delta_RA  = fraoff / 3600.0
    thisoi.delta_Dec = fdecoff / 3600.0
    thisoi.RA_0      = fra0
    thisoi.Dec_0     = fdec0

    IF n EQ 0 THEN $
       offsets_info =  thisoi $
    ELSE $
       offsets_info = [offsets_info, thisoi]
ENDFOR

error = 0B
RETURN, offsets_info

END
