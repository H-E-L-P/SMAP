PRO LOCKMANSWIRE_itermap, DATADIR=datadir, EXNAME=exname_in, $
                          DO_JACKKNIFE=do_jackknife, $
                          BADBOLOS=badbolos, EXCLUDEMASK=excludemask, $
                          ITERPARAMS=iterparams, SHORTNAME=shortname
  COMPILE_OPT IDL2

  CREATE_ITERMAP_DEFPARAMS, badbolos_def, excludemask_def, exname_def, pixscale

  IF KEYWORD_SET(exname_in) THEN exname = exname_in ELSE exname = 'lockman-swire_L1g_'+exname_def
  IF NOT KEYWORD_SET(badbolos)    THEN badbolos    = badbolos_def
  IF NOT KEYWORD_SET(excludemask) THEN excludemask = excludemask_def

  IF NOT KEYWORD_SET(datadir) THEN $
     datadir = "/data/spiredaq/reprocessed/LockmanSw_L1g/"
     ;datadir = "/data/spiredaq/reprocessed/LockmanSw_L1e/"

  IF NOT KEYWORD_SET(iterparams) THEN $
     iterparams =  {niter:20, first_offs:1, first_gain:0, first_wt:10, $
                    first_clip:0, nterms:3, min_hits:1}

; add "_" to exname if not already there (_ or -)
  char = STRMID(exname, 0, 1)
  IF char NE "_" AND char NE "-" THEN exname = "_" + exname

  iterdiag=3

  doptrs = 0
  tods = smap_read_and_filter(datadir, mapparam, PTRS=doptrs, SUCCESS=success,$
                              ERRMSG=errmsg, /MEDFILT, /VERB )
  IF success EQ 0 THEN MESSAGE,"Error on initial read/filter: "+errmsg

; FULL DATA SET
  smap_make_maps, tods, mapparam, map250, map350, map500, SUCCESS=success,$
                  CRVALX=crvalx, CRVALY=crvaly, $
                  CRPIXX=crpixx, CRPIXY=crpixy, NXPIX=nxpix, NYPIX=nypix, $
                  ERRMSG=errmsg,/VERB,/ITERMAP,$
                  ITER_PARAMS=iterparams,/ITER_PROPERROR, ITER_DIAG=iterdiag,$
                  BADBOLOS=badbolos,EXCLUDEMASK=excludemask,EXNAME=exname, $
		                    PIXSCALE=pixscale
  IF success EQ 0 THEN MESSAGE,"Error on map build"

  IF N_ELEMENTS(shortname) EQ 0 THEN mapname = mapparam.obsids_short ELSE $
     mapname = shortname

  st=SMAP_WRITE3COLORS(mapname,map250,map350,map500,$
                       /SILENT,ERRMSG=swc_errmsg,EXNAME="_itermap"+exname)

  IF KEYWORD_SET(do_jackknife) THEN BEGIN

; get info
     crvalx = [map250.astrometry.crval[0], map350.astrometry.crval[0], $
               map500.astrometry.crval[0]]
     crvaly = [map250.astrometry.crval[1], map350.astrometry.crval[1], $
               map500.astrometry.crval[1]]
     nxpix  = [map250.xsize, map350.xsize, map500.xsize]
     nypix  = [map250.ysize, map350.ysize, map500.ysize]
     crpixx = [map250.astrometry.crpix[0], map350.astrometry.crpix[0], $
               map500.astrometry.crpix[0]]
     crpixy = [map250.astrometry.crpix[1], map350.astrometry.crpix[1], $
               map500.astrometry.crpix[1]]

; FIRST HALF
     todindex = INDGEN(80)
     thisexname = exname+"_jk_half1"

     smap_make_maps, tods, mapparam, map250, map350, map500, SUCCESS=success,$
                     CRVALX=crvalx, CRVALY=crvaly, $
                     CRPIXX=crpixx, CRPIXY=crpixy, NXPIX=nxpix, NYPIX=nypix, $
                     ERRMSG=errmsg,/VERB,/ITERMAP,$
                     ITER_PARAMS=iterparams,/ITER_PROPERROR, $
                     BADBOLOS=badbolos,EXCLUDEMASK=excludemask,$
                     TOD_INDEX=todindex,ITER_FIXED=mapparam.obsids_short+exname, $
		                       PIXSCALE=pixscale
     IF success EQ 0 THEN MESSAGE,"Error on map build"
     st=SMAP_WRITE3COLORS(mapname,map250,map350,map500,$
                          /SILENT,ERRMSG=swc_errmsg,EXNAME="_itermap"+thisexname)

; SECOND HALF
     todindex = INDGEN(80)+80
     thisexname = exname+"_jk_half2"

     smap_make_maps, tods, mapparam, map250, map350, map500, SUCCESS=success,$
                     CRVALX=crvalx, CRVALY=crvaly, $
                     CRPIXX=crpixx, CRPIXY=crpixy, NXPIX=nxpix, NYPIX=nypix, $
                     ERRMSG=errmsg,/VERB,/ITERMAP,$
                     ITER_PARAMS=iterparams,/ITER_PROPERROR, $
                     BADBOLOS=badbolos,EXCLUDEMASK=excludemask,$
                     TOD_INDEX=todindex,ITER_FIXED=mapparam.obsids_short+exname, $
		                       PIXSCALE=pixscale
     IF success EQ 0 THEN MESSAGE,"Error on map build"
     st=SMAP_WRITE3COLORS(mapname,map250,map350,map500,$
                          /SILENT,ERRMSG=swc_errmsg,EXNAME="_itermap"+thisexname)

;FOCAL PLANE

     SMAP_FOCALPLANE_JK, bololist, jkindex

; FIRST BOLO
     thisexname = exname+"_jk_bolo1"
     fpbadbolos = bololist[WHERE(jkindex NE 0)]

     smap_make_maps, tods, mapparam, map250, map350, map500, SUCCESS=success,$
                     CRVALX=crvalx, CRVALY=crvaly, $
                     CRPIXX=crpixx, CRPIXY=crpixy, NXPIX=nxpix, NYPIX=nypix, $
                     ERRMSG=errmsg,/VERB,/ITERMAP,$
                     ITER_PARAMS=iterparams,/ITER_PROPERROR, $
                     BADBOLOS=fpbadbolos,EXCLUDEMASK=excludemask,$
                     ITER_FIXED=mapparam.obsids_short+exname, $
		                       PIXSCALE=pixscale
     IF success EQ 0 THEN MESSAGE,"Error on map build"
     st=SMAP_WRITE3COLORS(mapname,map250,map350,map500,$
                          /SILENT,ERRMSG=swc_errmsg,EXNAME="_itermap"+thisexname)

; SECOND BOLO
     thisexname = exname+"_jk_bolo2"
     fpbadbolos = bololist[WHERE(jkindex NE 1)]

     smap_make_maps, tods, mapparam, map250, map350, map500, SUCCESS=success,$
                     CRVALX=crvalx, CRVALY=crvaly, $
                     CRPIXX=crpixx, CRPIXY=crpixy, NXPIX=nxpix, NYPIX=nypix, $
                     ERRMSG=errmsg,/VERB,/ITERMAP,$
                     ITER_PARAMS=iterparams,/ITER_PROPERROR, $
                     BADBOLOS=fpbadbolos,EXCLUDEMASK=excludemask,$
                     ITER_FIXED=mapparam.obsids_short+exname, $
		                       PIXSCALE=pixscale
     IF success EQ 0 THEN MESSAGE,"Error on map build"
     st=SMAP_WRITE3COLORS(mapname,map250,map350,map500,$
                          /SILENT,ERRMSG=swc_errmsg,EXNAME="_itermap"+thisexname)


; HORIZONTAL 
     thisexname = exname+"_jk_ang1"
     todindex = INDGEN(80) + (INDGEN(80) / 40) * 40 

     smap_make_maps, tods, mapparam, map250, map350, map500, SUCCESS=success,$
                     CRVALX=crvalx, CRVALY=crvaly, $
                     CRPIXX=crpixx, CRPIXY=crpixy, NXPIX=nxpix, NYPIX=nypix, $
                     ERRMSG=errmsg,/VERB,/ITERMAP,$
                     ITER_PARAMS=iterparams,/ITER_PROPERROR, $
                     EXCLUDEMASK=excludemask,$
                     TOD_INDEX=todindex,ITER_FIXED=mapparam.obsids_short+exname, $
		                       PIXSCALE=pixscale
     IF success EQ 0 THEN MESSAGE,"Error on map build"
     st=SMAP_WRITE3COLORS(mapname,map250,map350,map500,$
                          /SILENT,ERRMSG=swc_errmsg,EXNAME="_itermap"+thisexname)

; VERTICAL
     thisexname = exname+"_jk_ang2"
     todindex += 40

     smap_make_maps, tods, mapparam, map250, map350, map500, SUCCESS=success,$
                     CRVALX=crvalx, CRVALY=crvaly, $
                     CRPIXX=crpixx, CRPIXY=crpixy, NXPIX=nxpix, NYPIX=nypix, $
                     ERRMSG=errmsg,/VERB,/ITERMAP,$
                     ITER_PARAMS=iterparams,/ITER_PROPERROR, $
                     EXCLUDEMASK=excludemask,$
                     TOD_INDEX=todindex,ITER_FIXED=mapparam.obsids_short+exname, $
		                       PIXSCALE=pixscale
     IF success EQ 0 THEN MESSAGE,"Error on map build"
     st=SMAP_WRITE3COLORS(mapname,map250,map350,map500,$
                          /SILENT,ERRMSG=swc_errmsg,EXNAME="_itermap"+thisexname)

; 1+4
;thisexname = exname+"_jk_1+4"
;todindex = INDGEN(80) + (INDGEN(80) / 40) * 80

;smap_make_maps, tods, mapparam, map250, map350, map500, SUCCESS=success,$
;                CRVALX=crvalx, CRVALY=crvaly, $
;                CRPIXX=crpixx, CRPIXY=crpixy, NXPIX=nxpix, NYPIX=nypix, $
;                ERRMSG=errmsg,/VERB,/ITERMAP,$
;                ITER_PARAMS=iterparams,/ITER_PROPERROR, $
;                EXCLUDEMASK=excludemask,$
;                TOD_INDEX=todindex,ITER_FIXED=mapparam.obsids_short+exname
;IF success EQ 0 THEN MESSAGE,"Error on map build"
;st=SMAP_WRITE3COLORS(mapname,map250,map350,map500,$
;                     /SILENT,ERRMSG=swc_errmsg,EXNAME="_itermap"+thisexname)

; 2+3
;thisexname = exname+"_jk_2+3"
;todindex = INDGEN(80) + 40

;smap_make_maps, tods, mapparam, map250, map350, map500, SUCCESS=success,$
;                CRVALX=crvalx, CRVALY=crvaly, $
;                CRPIXX=crpixx, CRPIXY=crpixy, NXPIX=nxpix, NYPIX=nypix, $
;                ERRMSG=errmsg,/VERB,/ITERMAP,$
;                ITER_PARAMS=iterparams,/ITER_PROPERROR, $
;                EXCLUDEMASK=excludemask,$
;                TOD_INDEX=todindex,ITER_FIXED=mapparam.obsids_short+exname
;IF success EQ 0 THEN MESSAGE,"Error on map build"
;st=SMAP_WRITE3COLORS(mapname,map250,map350,map500,$
;                     /SILENT,ERRMSG=swc_errmsg,EXNAME="_itermap"+thisexname)

  ENDIF ;; end jackknife maps

; get obstimes
  GET_LEVEL1_OBSTIME, datadir, obsbeg, obsend, meanmjd
  FORPRINT, [obsbeg, obsend, STRING(meanmjd, '(F14.8)')], $
            TEXTOUT=!SMAP_MAPS+'/'+mapname+exname+"_obstime.txt", /NOCOMM

  IF doptrs EQ 1 THEN PTR_FREE,tods

END

