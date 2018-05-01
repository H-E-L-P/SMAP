pro aorstack_cosmos

;to perform stacking on the COSMOS individual aor maps.

mapdir = !SMAP_MAPS
astromoffsetsdir = "/data/spiredaq/astrometry/"
dataversion = 'L1e'
outfile = astromoffsetsdir+'COSMOS_'+dataversion+'_jlw.dat'

file24 = "/data/spire/astrometry/formattedcats/cosmos_24.dat"

;mappre = '0x50004890_0x50004891_0x50004892_0x50004893_0x50004894_' + $
;         '0x50004895_0x50004896_0x50004897_itermap_'
mappre = 'cosmos_itermap_'
date = '20110216'
bands = ['PSW'];, 'PMW', 'PLW']
mapsuf = '.fits'

thumbsize = 41
subpixres = 1
aornamelen = 10

READCOL, file24, ra24, dec24, flux24, error24, COMM="#", /SILENT

nbands = N_ELEMENTS(bands)

FOR b=0,nbands-1 DO BEGIN

    maps = FILE_SEARCH(mapdir+mappre+date+"_*_"+bands[b]+mapsuf, COUNT=nmaps)
    
    FOR m=0,nmaps-1 DO BEGIN
        
        PRINT, maps[m]

        SMAP_ASTROM_STACK, maps[m], ra24, dec24, thumbsize, $
                           sigthumb, errthumb, pars, shift, width, $
                           theta, nsrcs, SUBPIXFACT=subpixres

        ; set up plot window
        PLOT, [0,thumbsize*subpixres], [0,thumbsize*subpixres], $
              /NODATA, XS=5, YS=5, /ISO
              
        pos = [!x.window[0],!y.window[0],!x.window[1],!y.window[1]]

        TVSCALE, sigthumb / errthumb, /NOINT, /KEEP, POS=pos
        PLOT, [0,thumbsize*subpixres], [0,thumbsize*subpixres], $
              POS=pos, /NOERASE, /NODATA, /XS, /YS

        npts = 360
        tt = FINDGEN(npts + 1) / npts * 2 * !PI

        xoft = pars[2] * COS(tt) * SQRT(2 * ALOG(2))
        yoft = pars[3] * SIN(tt) * SQRT(2 * ALOG(2))

        xell = xoft * COS(pars[6]) - yoft * SIN(pars[6]) + pars[4] + 0.5
        yell = xoft * SIN(pars[6]) + yoft * COS(pars[6]) + pars[5] + 0.5

        LOADCT, 11, /SILENT
        PLOTS, pars[4]+0.5, pars[5]+0.5, PS=1, COL=255
        OPLOT, xell, yell, COL=255
        LOADCT, 0

        PRINT, "shift: ", shift
        PRINT, "width: ", width


        ;Collate file and offset input and write it to a file.
        fchar = STRLEN(mapdir+mappre+date+'_')
        aorname = STRMID(maps[m], fchar, aornamelen)
        shortname = 'cosmos_'+STRCOMPRESS(m+1,/REMOVE_ALL)
        hdr = HEADFITS(maps[m],EXTEN=1)
        EXTAST, hdr, astrom
        ratanp = astrom.crval[0]
        dectanp = astrom.crval[1]
        IF m EQ 0 THEN OPENW, lun, outfile, /GET_LUN, WIDTH=150
        IF m EQ 0 THEN PRINTF, lun, '# obsID       obsName    offsetEast[arcsec]  offsetNorth[arcsec]  RA_tanpoint[deg]  Dec_tanpoint[deg]'
        printf,lun,  aorname, '     ',shortname, shift[0], -1.*shift[1], $
               ratanp, dectanp
        IF m EQ nmaps-1 THEN CLOSE, lun

    ENDFOR

ENDFOR



END
