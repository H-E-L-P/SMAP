PRO SMAP_CROSSLINK_MASK, mapdir, fieldname

;+
;
; SMAP_CROSSLINK_MASK, mapdir, fieldname
;
; Modify mask extension for all map files named 'fieldname' in 'mapdir'
; to show where maps have cross-linking. For each band, reads in jk_ang*
; hits maps and finds overlap region (after smoothing by beam).
;
; INPUTS:
;   mapdir:    directory containing maps
;   fieldname: name of field (file prefix)
;
; CREATED BY: gmarsden 2011-07-18
;
;-

bands = ['PSW', 'PMW', 'PLW']
fwhms = [18.0, 25.0, 36.0] / 3600.0

jkname = 'jk_ang'
ext = '.fits'
hitsfitsext = 3

crosslinkval = 2 ; mask byte value

; hardwire "direction" keyword for maps with multiple jk "angles"
CASE fieldname OF
    "goodss": direction = [0, 0, 1, 1]
    "bootes": direction = [0, 0, 1, 1]
    ELSE: direction = 0
ENDCASE


; loop over bands
FOR b=0,N_ELEMENTS(bands)-1 DO BEGIN

    ; find jk maps
    searchname = fieldname + "*" + jkname + "*" + bands[b] + ext
    jkfiles = FILE_SEARCH(mapdir, searchname, COUNT=njkmaps)

    ; get number of pixels in jkfiles[0]
    h = HEADFITS(jkfiles[0], EXT=hitsfitsext)
    nx = SXPAR(h, "NAXIS1")
    ny = SXPAR(h, "NAXIS2")

    pixsize = SQRT(SXPAR(h, "CD1_1")^2 + SXPAR(h, "CD1_2")^2)

    jkhits = LONARR(njkmaps, nx, ny)
    FOR i=0,njkmaps-1 DO $
       jkhits[i,*,*] = READFITS(jkfiles[i], EXT=hitsfitsext)

    fwhmpix = fwhms[b] / pixsize
    mask = MAKE_CROSSLINK_MASK(jkhits, fwhmpix, DIRECTION=direction)
    mask *= crosslinkval

    ; modify masks of all maps
    searchname = fieldname + "*" + bands[b] + ext
    allfiles = FILE_SEARCH(mapdir, searchname, COUNT=nallmaps)

    FOR i=0,nallmaps-1 DO BEGIN

        ; format filename for reading
        filebase = FILE_BASENAME(allfiles[i], ext)
        words = STREGEX(filebase, "^(.*)_"+bands[b], /EXTRACT, /SUBEXP)
        filebase = words[1]

        map = READ_SMAP_FITSMAP(filebase, bands[b], DIR=mapdir)

        ; modify mask
        map.mask = map.mask OR mask

        ; write back to file
        stat = WRITE_SMAP_FITSMAP(map, filebase, DIR=mapdir)

    ENDFOR

ENDFOR

END
