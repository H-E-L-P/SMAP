PRO COSMOS_AOR,out_dir=out_dir 

;+
;NAME
; cosmos_aor
;PURPOSE
;  Creates *** COSMOS 250um *** maps, one for all the data and then a 
;  set for each aor separately. 
;USAGE
;  cosmos_aor [,out_dir=out_dir]
;KEYWORDS
;  out_dir   Directory to output final maps to. Default = !SMAP_MAPS. 
;            Note that the diagnostic files always go to !SMAP_MAPS, and 
;            have the long hexadecimal names.
;-

if not keyword_Set(out_dir) then out_dir = !SMAP_MAPS
PRINT, 'Output directory is ', out_dir

t0 = SYSTIME(/sec)

CREATE_ITERMAP_DEFPARAMS, badbolos, excludemask, exname 

datadir = "/data/spiredaq/reprocessed/COSMOS_L1e/" 

iterparams =  {niter:10, first_offs:1, first_gain:0, first_wt:5, $
                 first_clip:0, nterms:2, min_hits:1} 

pixsize = [3, 25./6, 6] / 3600.0 

; add "_" to exname if not already there (_ or -)
char = STRMID(exname, 0, 1)  
IF char NE "_" AND char NE "-" THEN exname = "_" + exname

obsids = FILE_BASENAME(FILE_SEARCH(datadir+"*", /TEST_DIR))  

files = FILE_SEARCH(datadir+"*/level1/herschel*","*.fits") 
tods = smap_read_and_filter(files, mapparam, /PTRS, SUCCESS=success,$
                             ERRMSG=errmsg, /MEDFILT, /VERB )  
IF success EQ 0 THEN MESSAGE,"Error on initial read/filter: "+errmsg

iterdiag = 3

t1 = SYSTIME(/sec)

; FULL DATA SET. Run 250um mapmaker on the whole dataset first so can get astrometry parameters to feed to the individual scans, so that they're all on the same grid.
smap_make_maps, tods, mapparam, map250, map350, map500, SUCCESS=success,$
                ERRMSG=errmsg,/VERB,/ITERMAP,PIXSCALE=pixsize,$
                ITER_PARAMS=iterparams,/ITER_PROPERROR, ITER_DIAG=iterdiag,$
                BADBOLOS=badbolos,EXCLUDEMASK=excludemask,EXNAME=exname,$
                /no350,/no500
IF success EQ 0 THEN MESSAGE,"Error on map build"

mapname = "cosmos"

st=SMAP_WRITE3COLORS(mapname,map250,map350,map500,$
                       /SILENT,ERRMSG=swc_errmsg,EXNAME="_itermap"+exname, $
                    DIR=out_dir,/no350,/no500)   

t2 = SYSTIME(/sec)
print, 'Time for whole dataset(hrs) =', (t2-t1)/3600.

;get the astrometry params to feed to each aor run
; give it the 250um params for all of them, because we need something 
; for 350 and 500 but wont be running them so it doesn't matter what 
; the values are.
crvalx = [map250.astrometry.crval[0], map250.astrometry.crval[0], $   
          map250.astrometry.crval[0]]
crvaly = [map250.astrometry.crval[1], map250.astrometry.crval[1], $
          map250.astrometry.crval[1]]
nxpix  = [map250.xsize, map250.xsize, map250.xsize]
nypix  = [map250.ysize, map250.ysize, map250.ysize]
crpixx = [map250.astrometry.crpix[0], map250.astrometry.crpix[0], $
          map250.astrometry.crpix[0]]
crpixy = [map250.astrometry.crpix[1], map250.astrometry.crpix[1], $
          map250.astrometry.crpix[1]]

;to do each aor separately
t3 = SYSTIME(/sec)
FOR oind=0,N_ELEMENTS(obsids)-1 DO BEGIN 

   oid = obsids[oind]
   todindex = WHERE(STRMATCH(files, "*"+oid+"*"))
   thisexname = exname + "_" + oid

   smap_make_maps, tods, mapparam, map250, map350, map500, SUCCESS=success,$
                   CRVALX=crvalx, CRVALY=crvaly, $
                   CRPIXX=crpixx, CRPIXY=crpixy, NXPIX=nxpix, NYPIX=nypix, $
                   ERRMSG=errmsg,/VERB,/ITERMAP,PIXSCALE=pixsize,$
                   ITER_PARAMS=iterparams,/ITER_PROPERROR, $
                   BADBOLOS=badbolos,EXCLUDEMASK=excludemask,$
                   TOD_INDEX=todindex,ITER_FIXED=mapparam.obsids_short+exname,$
                   /no350,/no500
   IF success EQ 0 THEN MESSAGE,"Error on map build"
     st=SMAP_WRITE3COLORS(mapname,map250,map350,map500,$
                          /SILENT,ERRMSG=swc_errmsg, $
                          EXNAME="_itermap"+thisexname, DIR=out_dir, $
                         /no350, /no500)

ENDFOR
t4 = SYSTIME(/sec)
print, 'Time for all individual aors (hrs) = ', (t4-t3)/3600.
print, 'Total time taken (hrs) = ', (t4-t0)/3600.

PTR_FREE,tods

END
