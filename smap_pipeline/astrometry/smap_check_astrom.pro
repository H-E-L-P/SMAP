PRO SMAP_CHECK_ASTROM, fieldname, mapdate, BAND=band, PS=ps

;+
; SMAP_CHECK_ASTROM, mapfile, catfile
;
; Stack SMAP map on catalogue for visualizing astrometry. Displays stacked
; thumbnail with FWHM contour of Gaussian fit overlaid.
;
; CREATED BY: gmarsden 2011-07-07
;-

mapbasedir = "/data/spire/maps"
catbasedir = "/data/spire/ancillary/cats/formatted"
thumbsize = 41

IF NOT KEYWORD_SET(band) THEN band = 'PSW'

mapdir = ADDSLASH(mapbasedir) + ADDSLASH(fieldname) + ADDSLASH(mapdate)
;mapfile = mapdir + fieldname + "_itermap_" + mapdate + "_" + band + ".fits"
mapfile = $
   FILE_SEARCH(mapdir, fieldname + "_itermap_????????_" + band + ".fits")

; test map file
IF NOT FILE_TEST(mapfile) THEN $
   MESSAGE, "Map file '"+mapfile+"' not found."

; catalog filename (copied from smap_runall_astrom.pro):
fields = $
   [ $
   {f:"abell0370",     c:"a0370_24um.dat"}, $
   {f:"abell1689",     c:"a1689_24um.dat"}, $
   {f:"abell1835",     c:"a1835_24um.dat"}, $
   {f:"abell2218",     c:"abell2218_24.dat"}, $
   {f:"abell2219",     c:"a2219_24um.dat"}, $
   {f:"abell2390",     c:"a2390_24um.dat"}, $
   {f:"adfs",          c:"adfs_24.dat"}, $
   {f:"bootes",        c:"bootes_24.dat"}, $
   {f:"cdfs-swire",    c:"cdfs_24.dat"}, $
   {f:"cl0024",        c:"cl0024_24um.dat"}, $
   {f:"cosmos",        c:"cosmos_24.dat"}, $
   {f:"ecdfs",         c:"cdfs_24.dat"}, $
   {f:"egroth",        c:"egs_24.dat"}, $
   {f:"egs-scuba",     c:"egs_24.dat"}, $
   {f:"elais-n1",      c:"elaisn1_24.dat"}, $
   {f:"elais-n2",      c:"elaisn2_24.dat"}, $
   {f:"elais-s1",      c:"elaiss1_24.dat"}, $
   {f:"fls",           c:"fls_24.dat"}, $
   {f:"goodsn",        c:"goodsn_24.dat"}, $
   {f:"goodss",        c:"cdfs_24.dat"}, $
   {f:"lockman-east",  c:"lockswire_24.dat"}, $
   {f:"lockman-north", c:"lockdeep_24.dat"}, $
   {f:"lockman-swire", c:"lockswire_24.dat"}, $
   {f:"ms0451",        c:"ms0451_24um.dat"}, $
   {f:"ms1054",        c:"ms1054_24um.dat"}, $
   {f:"ms1358",        c:"ms1358_24um.dat"}, $
   {f:"rxj0152",       c:"rxj0152_24um.dat"}, $
   {f:"rxj1347",       c:"rxj1347_24um.dat"}, $
   {f:"s1-video",      c:"elaiss1_24.dat"}, $
   {f:"uds",           c:"xmm_24.dat"}, $
   {f:"vvds",          c:"xmm_24.dat"}, $
   {f:"xmm-lss",       c:"xmm_24.dat"} $
   ]

; find field in list
found = 0B
FOR i=0,N_ELEMENTS(fields)-1 DO BEGIN
    IF STRCMP(fieldname, fields[i].f, STRLEN(fields[i].f)) THEN BEGIN
        catname = fields[i].c
        found = 1B
        BREAK;
    ENDIF
ENDFOR

IF found EQ 0B THEN $
   MESSAGE, "Field '"+fieldname+"' not found in list."

catfile = ADDSLASH(catbasedir) + catname

; test cat file
IF NOT FILE_TEST(catfile) THEN $
   MESSAGE, "Catalog file '"+catname+"' not found."

READCOL, catfile, ra, dec, /SIL

SMAP_ASTROM_STACK, mapfile, ra, dec, thumbsize, $
                   sigthumb, errthumb, params, shift, width, theta, nsrcs

TVIMAGE, sigthumb, /NOINT, /KEEP, POS=pos, /ERASE, $
         MINV=MIN(sigthumb), MAXV=MAX(sigthumb)
PLOT, [0,thumbsize-1], [0,thumbsize-1], POS=pos, /NOD, /NOER, XS=4, YS=4

GAUSS_ELLIPSE, params, xx, yy

; dumb hack for 1/2 pixel offset between X and PS displays
IF KEYWORD_SET(ps) THEN BEGIN
    OPLOT, xx, yy, COL=255
    PLOTS, params[4], params[5], PSYM=1, COL=255
ENDIF ELSE BEGIN
    OPLOT, xx+0.5, yy+0.5, COL=255
    PLOTS, params[4]+0.5, params[5]+0.5, PSYM=1, COL=255
ENDELSE

XYOUTS, thumbsize*.1, thumbsize*.9, fieldname, CHARS=2
XYOUTS, thumbsize*.1, thumbsize*.8, $
        "shift: "+STRJOIN(STRING(shift, '(F6.3)'), ", "), CHARS=2

PRINT, "shift [arcsec]: ", shift

END
