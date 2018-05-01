PRO XMM_ITERMAP, DATADIR=datadir, EXNAME=exname_in, DO_JACKKNIFE=do_jackknife, $
                 BADBOLOS=badbolos, EXCLUDEMASK=excludemask, $
                 ITERPARAMS=iterparams, SHORTNAME=shortname
  COMPILE_OPT IDL2

  CREATE_ITERMAP_DEFPARAMS, badbolos_def, excludemask_def, exname_def, pixscale

  IF NOT KEYWORD_SET(datadir) THEN $
     datadir = "/data/spiredaq/reprocessed/XMM-LSS_L1e/"

  IF KEYWORD_SET(exname_in) THEN exname = exname_in ELSE exname = 'xmm-lss_'+exname_def
  IF NOT KEYWORD_SET(badbolos)    THEN badbolos    = badbolos_def
  IF NOT KEYWORD_SET(excludemask) THEN excludemask = excludemask_def

  IF NOT KEYWORD_SET(iterparams) THEN $
     iterparams =  {niter:20, first_offs:1, first_gain:0, first_wt:10, $
                    first_clip:0, nterms:3, min_hits:1}

; add "_" to exname if not already there (_ or -)
  char = STRMID(exname, 0, 1)
  IF char NE "_" AND char NE "-" THEN exname = "_" + exname

  iterdiag=3

  doptrs = 1b
  tods = smap_read_and_filter(datadir, mapparam, PTRS=doptrs, SUCCESS=success,$
                              ERRMSG=errmsg, /MEDFILT, /VERB )
  IF success EQ 0 THEN MESSAGE,"Error on initial read/filter: "+errmsg

  ntods = N_ELEMENTS(tods)
  obsids = ULONARR(ntods)
  shortfiles = STRARR(ntods)
  FOR i=0,ntods-1 DO obsids[i] = (*tods[i]).obsid
  FOR i=0,ntods-1 DO shortfiles[i] = (*tods[i]).shortfile

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
     todindex = INDGEN(35+38+38)

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
     todindex = INDGEN(35+38+38) + 35+38+38

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
                          /SILENT,ERRMSG=swc_errmsg,EXNAME="_itermap"+$
                          thisexname)

; HALVES
     ;;Jan 5, feb 8 split
     todindex = WHERE( obsids EQ 1342189003uL OR obsids EQ 1342189004uL OR $
                       obsids EQ 1342189031uL,$
                       COMPLEMENT=todindex2, ni1, NCOMPLEMENT=ni2 )
     IF ni1 NE 0 AND ni2 NE 0 THEN BEGIN
        thisexname = exname+"_jk_half1"
        smap_make_maps, tods, mapparam, map250, map350, map500, $
                        SUCCESS=success,$
                        CRVALX=crvalx, CRVALY=crvaly, $
                        CRPIXX=crpixx, CRPIXY=crpixy, NXPIX=nxpix, $
                        NYPIX=nypix, ERRMSG=errmsg,/VERB,/ITERMAP,$
                        ITER_PARAMS=iterparams,/ITER_PROPERROR, $
                        EXCLUDEMASK=excludemask,$
                        TOD_INDEX=todindex,$
                        ITER_FIXED=mapparam.obsids_short+exname, $
			PIXSCALE=pixscale
        IF success EQ 0 THEN MESSAGE,"Error on map build"
        st=SMAP_WRITE3COLORS(mapname,map250,map350,map500,$
                             /SILENT,ERRMSG=swc_errmsg,$
                             EXNAME="_itermap"+thisexname)
        thisexname = exname+"_jk_half2"
        smap_make_maps, tods, mapparam, map250, map350, map500, $
                        SUCCESS=success,$
                        CRVALX=crvalx, CRVALY=crvaly, $
                        CRPIXX=crpixx, CRPIXY=crpixy, NXPIX=nxpix, $
                        NYPIX=nypix, ERRMSG=errmsg,/VERB,/ITERMAP,$
                        ITER_PARAMS=iterparams,/ITER_PROPERROR, $
                        EXCLUDEMASK=excludemask,$
                        TOD_INDEX=todindex2,$
                        ITER_FIXED=mapparam.obsids_short+exname, $
			PIXSCALE=pixscale
        IF success EQ 0 THEN MESSAGE,"Error on map build"
        st=SMAP_WRITE3COLORS(mapname,map250,map350,map500,$
                             /SILENT,ERRMSG=swc_errmsg,$
                             EXNAME="_itermap"+thisexname)

     ENDIF

  ENDIF ;; end jackknife maps

; get obstimes
  GET_LEVEL1_OBSTIME, datadir, obsbeg, obsend, meanmjd
  FORPRINT, [obsbeg, obsend, STRING(meanmjd, '(F14.8)')], $
            TEXTOUT=!SMAP_MAPS+'/'+mapname+exname+"_obstime.txt", /NOCOMM


  IF KEYWORD_SET(doptrs) THEN PTR_FREE,tods

END

