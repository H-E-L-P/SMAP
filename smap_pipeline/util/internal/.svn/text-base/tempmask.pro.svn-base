PRO TEMPMASK

mapdir = "/data/spire/maps"
outdir = '/data/spire/maps/tempmasks/20131002/'

dofields = 1B
donest = 1B
dofilter = 1B

bands = ['PSW', 'PMW', 'PLW']
fwhms = [18.0, 25.0, 36.0] / 3600.0

jkname = 'jk_ang'
ext = '.fits'
hitsfitsext = 3

crosslinkval = 2 ; mask byte value
filterval    = 4 ; 

fields = [ $
         "abell0370", $
         "abell1689", $
         "abell1835", $
         "abell2218", $
         "abell2219", $
         "abell2390", $
         "adfs", $
         "bootes", $
         "cdfs-swire", $
         "cdfs-swire3", $
         "cdfs-shallow", $
         "cl0024", $
         "cosmos", $
         "cosmos2", $
         "ecdfs", $
         "egroth", $
         "egs-scuba", $
         "elais-n1", $
         "elais-n1-new-ivan", $
         "elais-n2", $
         "elais-s1", $
         "fls", $
         "global-epicentre1", $
         "goodsn", $
         "goodsn-gh", $
         "goodss", $
         "lockman-east", $
         "lockman-north", $
         "lockman-swire", $
         "lockman-swire3", $
         "lockman-shallow", $
         "ms0451", $
         "ms1054", $
         "ms1358", $
         "rxj0152", $
         "rxj1347", $
         "s1-video", $
         "uds", $
         "video-xmm1", $
         "video-xmm2", $
         "video-xmm3", $
         "vvds", $
         "xmm-lss" $
         ]

nestfields = ["cdfs-nest", $
              "cosmos-nest", $
              "egs-nest", $
              "elais-n1-nest", $
              "goodsn-nest", $
              "lockman-nest", $
              "xmm-nest"]

filterfields = ["cosmos-filter", $
                "egroth-filter", $
                "goodsn-filter", $
                "goodss-filter", $
                "lockman-east-filter", $
                "lockman-north-filter", $
                "uds-filter", $
                "vvds-filter"]

filterfields_src = ["cosmos-nest", $
                    "egs-nest", $
                    "goodsn-nest", $
                    "cdfs-nest", $
                    "lockman-nest", $
                    "lockman-nest", $
                    "xmm-nest", $
                    "xmm-nest"]

IF NOT FILE_TEST(outdir, /DIR) THEN FILE_MKDIR, outdir

IF dofields THEN BEGIN
FOR f=0,N_ELEMENTS(fields)-1 DO BEGIN
    fieldname = fields[f]

    CASE fieldname OF
        ; list fields for which there are more than two ang jackknife maps
        ; (eg ang1a/ang1b/ang2a/ang2b
        ; use 0 and 1 to indicate matching directions of jk_ang maps (in 
        ; alphabetical order)
        "bootes": direction = [0, 0, 1, 1]
        "cosmos2": direction = [0, 0, 1, 1]
        "goodss": direction = [0, 0, 1, 1]
        ELSE: direction = 0
    ENDCASE

    thismapdir = ADDSLASH(mapdir) + ADDSLASH(fieldname) + "current/"

    FOR b=0,N_ELEMENTS(bands)-1 DO BEGIN

        ; find jk maps
        searchname = fieldname + "*" + jkname + "*" + bands[b] + ext
        jkfiles = FILE_SEARCH(thismapdir, searchname, COUNT=njkmaps)

        ; if no jkmaps, set mask to 1
        IF njkmaps EQ 0 THEN BEGIN
            searchname = fieldname + "*" + bands[b] + ext
            files = FILE_SEARCH(thismapdir, searchname)
            h = HEADFITS(files[0], EXT=hitsfitsext)
            nx = SXPAR(h, "NAXIS1")
            ny = SXPAR(h, "NAXIS2")
            mask = REPLICATE(1UL, nx, ny)
        ENDIF ELSE BEGIN

            ; get number of pixels in jkfiles[0]
            h = HEADFITS(jkfiles[0], EXT=hitsfitsext)
            nx = SXPAR(h, "NAXIS1")
            ny = SXPAR(h, "NAXIS2")

            pixsize = SQRT(SXPAR(h, "CD1_1")^2 + SXPAR(h, "CD1_2")^2)
            
            jkhits = DBLARR(njkmaps, nx, ny)
            FOR i=0,njkmaps-1 DO $
               jkhits[i,*,*] = READFITS(jkfiles[i], EXT=hitsfitsext)
            
            fwhmpix = fwhms[b] / pixsize
            mask = MAKE_CROSSLINK_MASK(jkhits, fwhmpix, DIRECTION=direction)
        ENDELSE

        mask *= crosslinkval

        WRITEFITS, outdir + fieldname + "_" + bands[b] + "_mask.fits", $
                   ULONG(mask), h
    ENDFOR

ENDFOR
ENDIF

IF donest THEN BEGIN
FOR f=0,N_ELEMENTS(nestfields)-1 DO BEGIN
    
    fieldname = nestfields[f]
    CASE fieldname OF
        ; list all nest maps here with source sub-fields
        ; indicate matching directions of jk_ang maps
        "cdfs-nest": BEGIN
            srcdirs = ['cdfs-swire', 'cdfs-swire3', 'ecdfs', 'goodss']
            direction = [0, 1, 1, 0, 0, 1, 0, 0, 1, 1]
        END
        "cosmos-nest": BEGIN
            srcdirs = ['cosmos', 'cosmos2']
            direction = [0, 1, 0, 0, 1, 1]
        END
        "egs-nest": BEGIN
            srcdirs = ['egroth', 'egs-scuba']
            direction = 0
        END
        "elais-n1-nest": BEGIN
            srcdirs = ['elais-n1', 'elais-n1-new-ivan']
            direction = 0
        END
        "goodsn-nest": BEGIN
            srcdirs = ['goodsn', 'goodsn-gh']
            direction = 0
        END
        "lockman-nest": BEGIN
            srcdirs = ['global-epicentre1', 'lockman-east', 'lockman-north', $
                       'lockman-swire', 'lockman-swire3']
            direction = 0
        END
        "xmm-nest": BEGIN
            srcdirs = ['uds', 'video-xmm1', 'video-xmm2', 'video-xmm3', $
                       'vvds', 'xmm-lss']
            direction = [0,1,1,0,0,1,0,1,0,1,1,0]
        END
    ENDCASE

    thismapdir = ADDSLASH(mapdir) + ADDSLASH(fieldname) + "current/"
    
    FOR b=0,N_ELEMENTS(bands)-1 DO BEGIN

        ; find jk maps
        searchpaths = ADDSLASH(mapdir) + srcdirs + "/current/" + $
                      srcdirs + "*" + jkname + "*" + bands[b] + ext
        jkfiles = FILE_SEARCH(searchpaths, COUNT=njkmaps)

        ; find map file for footprint
        searchname = fieldname + "*" + bands[b] + ext
        files = FILE_SEARCH(thismapdir, searchname)
        h = HEADFITS(files[0], EXT=hitsfitsext)
        nx = SXPAR(h, "NAXIS1")
        ny = SXPAR(h, "NAXIS2")
    
        pixsize = SQRT(SXPAR(h, "CD1_1")^2 + SXPAR(h, "CD1_2")^2)
            
        jkhits = LONARR(njkmaps, nx, ny)
        FOR i=0,njkmaps-1 DO BEGIN
            hits = READFITS(jkfiles[i], hhead, EXT=hitsfitsext)
            HASTROM, hits, hhead, hitsnew, hheadnew, h, MISSING=0, INTERP=0
            jkhits[i,*,*] = hitsnew
        ENDFOR
            
        fwhmpix = fwhms[b] / pixsize
        mask = MAKE_CROSSLINK_MASK(jkhits, fwhmpix, DIRECTION=direction)

        mask *= crosslinkval

        WRITEFITS, outdir + fieldname + "_" + bands[b] + "_mask.fits", $
                   ULONG(mask), h

    ENDFOR


ENDFOR
ENDIF

IF dofilter THEN BEGIN
FOR f=0,N_ELEMENTS(filterfields)-1 DO BEGIN
    
    fieldname = filterfields[f]
    srcname = filterfields_src[f]

    thismapdir = ADDSLASH(mapdir) + ADDSLASH(fieldname) + "current/"
    
    FOR b=0,N_ELEMENTS(bands)-1 DO BEGIN

        ; read in src mask
        srcmask = READFITS(outdir + srcname + "_" + bands[b] + "_mask.fits", $
                           srchead)
    
        ; find footprint
        searchname = fieldname + "*" + bands[b] + ext
        files = FILE_SEARCH(thismapdir, searchname)
        h = HEADFITS(files[0], EXT=hitsfitsext)
        nx = SXPAR(h, "NAXIS1")
        ny = SXPAR(h, "NAXIS2")

        ; project srcmask onto footprint
        HASTROM, srcmask, srchead, masknew, maskheadnew, h, MISSING=0, INTERP=0

        ; do filter masking
        
        ; read in src map noise
        searchname = srcname + "_itermap_201?????_" + bands[b] + ext
        srcdir = ADDSLASH(mapdir) + ADDSLASH(srcname) + "current/"
        files = FILE_SEARCH(srcdir, searchname)
        srcnse = READFITS(files[0], srchead, EXT=2)
        HASTROM, srcnse, srchead, srcnew, srcnewhead, h

        ; scale noise for pixel size
        pixsize_src = SQRT(SXPAR(srchead, 'CD1_1')^2 + $
                           SXPAR(srchead, 'CD1_2')^2)
        pixsize_new = SQRT(SXPAR(h, 'CD1_1')^2 + SXPAR(h, 'CD1_2')^2)
        fact = pixsize_new / pixsize_src
        srcnew /= fact
        
        fwhmpix = fwhms[b] / pixsize_new

        filtermask = MAKE_FILTER_MASK(srcnew, fwhmpix)
        filtermask *= filterval

        mask = masknew OR filtermask
         
        WRITEFITS, outdir + fieldname + "_" + bands[b] + "_mask.fits", $
                   ULONG(mask), h

    ENDFOR

ENDFOR
ENDIF

END
