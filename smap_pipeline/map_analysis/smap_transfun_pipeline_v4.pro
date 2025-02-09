FUNCTION SMAP_TRANSFUN_PIPELINE_V4,map_template, field_name,EXNAMEBASE=exnamebase, $ 
				USE_SIMS=use_sims, DIRSIM=dirsim, $
				SKYPIX_SIDE=skypix_side, SKYPIX_PIXSIZE=skypix_pixsize,$
				STARTNUM=startnum,ENDNUM=endnum, $
				HI_CUT=hi_cut,CLEAN_SKIES=clean_skies, TWO_HALO=two_halo, $ 
				NOISE=noise,FKNEE=fknee,$
				USE_MAPS=use_maps, DIRMAP=dirmap, MAPNAME=mapname, $
;				JK_ONLY=jk_only, HALFS=halfs, ANGS=angs, $
;				OMIT_BAD_SCANS=omit_bad_scans, $
			        CONFFILE=conffile,$
				USEFITS=usefits,$
				PROJTYPE=projtype, $
				NXPIX=nxpix, NYPIX=nypix, CRVALX=crvalx, CRVALY=crvaly, $
				CRPIXX=crpixx, CRPIXY=crpixy, SPEEDCUT=speedcut, $
				BADOBSID=badobsid,$
				SAVEMAPDIR=savemapdir
;+
;NAME
; SMAP_TRANSFUN_PIPELINE_V4
;PURPOSE
; Run a small version of the smap pipeline on multiple simulation
; and write maps.  
; Does not calculate the transfer function here.  Use XXX to do that.
;USAGE
; SMAP_TRANSFUN_PIPELINE_V4,map_template,field_name, SAVEMAPDIR=savemapdir, & either:
;     [USE_SIMS=use_sims, DIRSIM=dirsim, SKYPIX_SIDE=skypix_side, SKYPIX_PIXSIZE=skypix_pixsize, $
;      STARTNUM=startnum,ENDNUM=endnum, HI_CUT=hi_cut,CLEAN_SKIES=clean_skies, $
;      TWO_HALO=two_halo,NOISE=noise,FKNEE=fknee] 
;   or:
;     [USE_MAPS=use_maps, DIRMAP=dirmap, MAPNAME=mapname]
;INPUTS
; map_template name of the maps trying to imitate 
; field_name   "name" of the map, e.g., 'xmm-lss', 'cdfs-swire', 'lockman-swire', 'adfs'
;
;OPTIONAL INPUTS
; dirsim/dirmap	  directory where simulations/map
; exnamebase   extension name to give to the maps 
; 
;  IF USING M.VIERO'S SIMULATIONS, then must declare /USE_SIMS, plus: 
;     i)   DIRSIM directory where they are located (I use /data/viero/sim_transfunc/)
;     ii)  CLEAN_SKIES=0 simmaps with sources, OR CLEAN_SKIES=1 for sourcelss, cmb-style simmaps 
;     iii) SKYPIX_SIDE=size of simulated map on a side (they are square, default = 10580.)
;     iv)  SKYPIX_PIXSIZE=pixsize (in arcsec) of the simulated maps (default = 2)
;     v) TWO_HALO=0 if you want only large scale linear clustering
;     vi) NOISE if you want simmaps to have noise added, suggested [4.22885e+06,  2.03370e+06,  1.51030e+06] Jy/sr/sqrt(hits)
;     vii) FKNEE if you want 1/f noise, suggested [0.005,0.005,0.005]
;     viii)STARTNUM/ENDNUM is the index range of simulations to use, e.g, 0 to 99.
;     
;  IF USING YOUR OWN MAP (SAY, YOU WANT TO SIMULATE OBSERVATIONS OF HI 21CM MAPS), THEN NEED 
;     i)   DIRSIM directory where they are located (I use /data/viero/sim_transfunc/)
;     ii)  MAPNAME is the name of the map you want to "observe" 
;
;AUTHORS
; Alex Amblard
; Marco Viero
bands=['250','350','500']

IF NOT(KEYWORD_SET(use_sims)) AND NOT(KEYWORD_SET(use_maps)) THEN stop
IF NOT(KEYWORD_SET(exnamebase)) THEN exnamebase=''
IF NOT(KEYWORD_SET(skypix_side)) THEN skypix_side=10580
IF NOT(KEYWORD_SET(skypix_pixsize)) THEN skypix_pixsize=2.0
IF NOT(KEYWORD_SET(savemapdir)) THEN savemapdir = !SMAP_MAPS

;IF NOT(KEYWORD_SET(OMIT_BAD_SCANS)) THEN OMIT_BAD_SCANS=1

;MAKE SURE GENERAL.CONF (in createmap/conffiles) IS HACKED TO HAVE SAME INFO AS LSS_14_GENERAL.CONF (in custom_createmap_v4/conffiles/)
IF NOT(KEYWORD_SET(conffile)) THEN $
   conffile = !SMAP_PIPELINE_PATH + 'map_making/createmap/conffiles/' + $
              field_name+'.conf'
COMPILE_OPT IDL2
info = CREATEMAP_READ_CONF(conffile)

;; required parameters
datadirbase = info.datadirbase
dataname    = info.dataname
dataversion = info.dataversion
; path to main data directory
datadir = ADDSLASH(datadirbase) + *dataname + "_" + dataversion

;; optional parameters
IF info.do_ptrs      GE 0  THEN do_ptrs      = info.do_ptrs
IF info.do_oneband   GE 0  THEN do_oneband   = info.do_oneband
IF info.do_tdc       GE 0  THEN do_tdc       = info.do_tdc
IF info.astromname   NE "" THEN astromname   = info.astromname
IF info.noastrom     GE 0  THEN noastrom     = info.noastrom
IF info.do_jkbolo    GE 0  THEN do_jkbolo    = info.do_jkbolo
IF info.do_aormaps   GE 0  THEN do_aormaps   = info.do_aormaps
IF info.do_matched   GE 0  THEN do_matched   = info.do_matched
IF info.niter        GE 0  THEN niter        = info.niter
IF info.first_offs   GE 0  THEN first_offs   = info.first_offs
IF info.first_gain   GE 0  THEN first_gain   = info.first_gain
IF info.first_wt     GE 0  THEN first_wt     = info.first_wt
IF info.first_clip   GE 0  THEN first_clip   = info.first_clip
IF info.grow_clip    GE 0  THEN grow_clip    = info.grow_clip
IF info.clipsigma    GE 0  THEN clipsigma    = info.clipsigma
IF info.fixed_nt     GE 0  THEN fixed_nt     = info.fixed_nt
IF info.nterms       GE 0  THEN nterms       = info.nterms
IF info.nt_scale     GE 0  THEN nt_scale     = info.nt_scale
IF info.min_exposure GE 0  THEN min_exposure = info.min_exposure
IF info.shortname    NE "" THEN shortname    = info.shortname
IF info.exname       NE "" THEN exname       = info.exname
IF info.exapp        NE "" THEN exapp        = info.exapp
IF info.iterdiag     GE 0  THEN iterdiag     = info.iterdiag
IF info.no250        GE 0  THEN no250        = info.no250
IF info.no350        GE 0  THEN no350        = info.no350
IF info.no500        GE 0  THEN no500        = info.no500

IF info.speedcut     GE 0  THEN speedcut     = info.speedcut
IF info.badobsid NE PTR_NEW() THEN badobsid  = *info.badobsid

IF info.jklist NE PTR_NEW() THEN jklist      = *info.jklist
IF info.pixsize[0]   GE 0 THEN pixsize       = info.pixsize
IF info.instnoise[0] GE 0 THEN instnoise     = info.instnoise

;IF info.nxpix[0] GT 0     THEN tnxpix  = info.nxpix
;IF info.nypix[0] GT 0     THEN tnypix  = info.nypix
;IF FINITE(info.crvalx[0]) THEN tcrvalx = info.crvalx
;IF FINITE(info.crvaly[0]) THEN tcrvaly = info.crvaly
;IF FINITE(info.crpixx[0]) THEN tcrpixx = info.crpixx
;IF FINITE(info.crpixy[0]) THEN tcrpixy = info.crpixy

;;linear astrometry correction
IF FINITE( info.lin_ra_slope ) || FINITE( info.lin_dec_slope ) || $
   FINITE( info.lin_basetime ) THEN BEGIN
   ;;Rely on later code to complain if they are not all set
   linearcorr = { basetime: info.lin_basetime, $
      ra_slope: info.lin_ra_slope, $
      dec_slope: info.lin_dec_slope }
ENDIF

;; fixed params structure
fixedparams = {dofp:0B, fpdir:"", fpname:"", fpdate:""}
IF info.fixeddir NE "" THEN BEGIN
   fixedparams.dofp = 1B
   fixedparams.fpdir = info.fixeddir
ENDIF
IF info.fixedname NE "" THEN BEGIN
   fixedparams.dofp = 1B
   fixedparams.fpname = info.fixedname
ENDIF
IF info.fixeddate NE "" THEN BEGIN
   fixedparams.dofp = 1B
   fixedparams.fpdate = info.fixeddate
ENDIF
;;Masks, bolometers
CREATEMAP_DEFPARAMS, badbolos, excludemask, exname_def
nmask = N_ELEMENTS( excludemask )
IF nmask NE 0 && PTR_VALID( info.nomask ) THEN BEGIN
   skipmasks = *info.nomask
   keepmask = BYTARR( nmask, /NOZERO )
   keepmask[*] = 1b
   FOR i=0, N_ELEMENTS(skipmasks) - 1 DO BEGIN
      wexcl = WHERE(STRMATCH(excludemask,skipmasks[i],/FOLD_CASE),nexcl )
      IF nexcl NE 0 THEN keepmask[wexcl] = 0b
   ENDFOR
   wkeep = WHERE( keepmask,nkeep )
   IF nkeep EQ 0 THEN excludemask = [''] ELSE $
      excludemask = excludemask[wkeep]
   END

; Prepare a map structure to be read by A. Conley's simulator 
; - IF use_sims is flagged, will make a skypix_side by skypix_side structure
; - IF use_maps is flagged, will make a skypix_side_x by skypix_side_y structure

head=headfits(map_template[0],ext=1)
CRVALX=sxpar(head,'CRVAL1')
CRVALY=sxpar(head,'CRVAL2')
CRPIXX=dblarr(3)
CRPIXY=dblarr(3)
nxpix=intarr(3)
nypix=intarr(3)
pixscale=dblarr(3)
FOR i=0,2 DO BEGIN
   head=headfits(map_template[i],ext=1)
   CRPIXX[i]=sxpar(head,'CRPIX1')
   CRPIXY[i]=sxpar(head,'CRPIX2')
   NXPIX[i]=sxpar(head,'NAXIS1')
   NYPIX[i]=sxpar(head,'NAXIS2')
   PIXSCALE[i]=sxpar(head,'CD2_2')
ENDFOR 

IF (KEYWORD_SET(USE_SIMS)) THEN BEGIN
      pixsize_degrees=skypix_pixsize/3600.
      skypix_side_x=skypix_side
      skypix_side_y=skypix_side
      crpix_x=skypix_side_x/2.
      crpix_y=skypix_side_y/2.
ENDIF ELSE BEGIN
   IF NOT(KEYWORD_SET(USE_MAPS)) THEN STOP
   ;stopped if USE_MAPS flag not set
   ;simmaphead=headfits(dirmap+mapname, ext=1)
   ;IF SXPAR(simmaphead,'CDELT2') eq 0 THEN BEGIN
   simmaphead=headfits(dirmap+mapname+'.fits')
   if sxpar(simmaphead,'NAXIS') eq 0 then $
   simmaphead=headfits(dirmap+mapname+'.fits',ext=1)
      ;pixsize_degrees=sxpar(simmaphead,'CD2_2')
   pixsize_degrees=sxpar(simmaphead,'CDELT2')
   if pixsize_degrees eq 0 then $
   pixsize_degrees=sxpar(simmaphead,'CD2_2')
   ;ENDIF ELSE BEGIN
   ;   pixsize_degrees=sxpar(simmaphead,'CDELT2')
   ;ENDELSE
   skypix_side_x=sxpar(simmaphead,'NAXIS1')
   skypix_side_y=sxpar(simmaphead,'NAXIS2')
   skypix_pixsize=pixsize_degrees*3600.
   crpix_x=sxpar(simmaphead,'CRPIX1')
   crpix_y=sxpar(simmaphead,'CRPIX2')
ENDELSE

mapstruct250=get_smap_mapstruct(npixx=skypix_side_x,npixy=skypix_side_y,band='PSW',/noerr,/noexp,/nomask)
mapstruct350=get_smap_mapstruct(npixx=skypix_side_x,npixy=skypix_side_y,band='PMW',/noerr,/noexp,/nomask)
mapstruct500=get_smap_mapstruct(npixx=skypix_side_x,npixy=skypix_side_y,band='PLW',/noerr,/noexp,/nomask)
mapstruct500.astrometry.cd=identity(2)
mapstruct500.astrometry.crval=[crvalx,crvaly]
mapstruct500.astrometry.cdelt=[-1.*pixsize_degrees,pixsize_degrees]
mapstruct500.astrometry.crpix=[crpix_x,crpix_y]
mapstruct250.astrometry=mapstruct500.astrometry
mapstruct350.astrometry=mapstruct500.astrometry
maps=ptrarr(3)
maps[0]=ptr_new(temporary(mapstruct250))
maps[1]=ptr_new(temporary(mapstruct350))
maps[2]=ptr_new(temporary(mapstruct500))

mapfiles=strarr(3)

IF KEYWORD_SET(USE_SIMS) THEN BEGIN
   usefits=0
   size_prefix=strcompress('_'+string(skypix_pixsize,format='(i10)')+'x'+ $
      string(skypix_side,format='(i10)')+'x'+string(skypix_side,format='(i10)'),/remove_all)
   if KEYWORD_SET(HI_CUT) THEN $
      cut_suffix=strcompress('_'+string(hi_cut*1000.,format='(i101)')+'mJy',/remove_all) $
      ELSE BEGIN
      HI_CUT=100
      cut_suffix=''
   ENDELSE

   noise_in=noise
   fknee_in=fknee
   FOR ii=startnum,endnum DO BEGIN
      ;READ TODS TO GET POINTING SOLUTION, WILL USE LATER WHEN "OBSERVING" MAPS
      doptrs=1
      tods = smap_read_and_filter(datadir, mapparam, $
	 PTRS=doptrs, $
	 ASTROMOFFSETS=offsets_info, $
	 BADOBSID=badobsid, SPEEDCUT=speedcut, $
	 SUCCESS=success, ERRMSG=errmsg,/VERB)
;      stop

      noise0=noise_in
      fknee0=fknee_in

      ; READ THE SKY SIMULATIONS AND MAKE NOISELESS MAPS
      if KEYWORD_SET(two_halo) then sky_prefix='M2HSky' else sky_prefix='MSky'
      if KEYWORD_SET(clean_skies) then $
	 for i=0,2 do $
	 mapfiles[i]=strcompress(sky_prefix+size_prefix+'_'+strcompress(ii,/remove_all)+'.dat',/remove_all) $
	 else $
	 for i=0,2 do $
	 mapfiles[i]=strcompress(sky_prefix+size_prefix+'_'+bands[i]+cut_suffix+'_'+strcompress(ii,/remove_all)+'.dat',/remove_all) 

      exnamebasewnum=exnamebase+'_'+strcompress(ii,/remove_all)
      SMAP_TRANSFUN_MAKEMAP_V4, datadirbase, dataname, dataversion, $
	 dirsim,mapfiles,maps,tods,mapparam, $
	 skypix_side_x,skypix_side_y, $
	 SAVETODS=savetods,EXNAME=exnamebasewnum, EXAPP=exapp,  $ 
	 DO_PTRS=do_ptrs, DO_ONEBAND=do_oneband, DO_TDC=do_tdc, $
	 ASTROMNAME=astromname, NOASTROM=noastrom, $
	 JKLIST=jklist, DO_JKBOLO=do_jkbolo, DO_AORMAPS=do_aormaps, $
	 DO_MATCHED=do_matched, INSTNOISE=instnoise,$
	 BADBOLOS=badbolos, EXCLUDEMASK=excludemask, $
	 ITERPARAMS=iterparams, NITER=niter, FIRST_OFFS=first_offs, $
	 FIRST_GAIN=first_gain, FIRST_WT=first_wt, $
	 FIRST_CLIP=first_clip, FIXED_NT=fixed_nt, NTERMS=nterms, $
	 NT_SCALE=nt_scale, MIN_EXPOSURE=min_exposure, $
	 GROW_CLIP=grow_clip, CLIPSIGMA=clipsigma,$
	 ITERDIAG=iterdiag, PIXSIZE=pixsize, SHORTNAME=shortname, $
	 NO250=no250, NO350=no350, NO500=no500, $
	 NXPIX=nxpix, NYPIX=nypix, CRVALX=crvalx, CRVALY=crvaly, $
	 CRPIXX=crpixx, CRPIXY=crpixy, SPEEDCUT=speedcut, $
	 BADOBSID=badobsid, FIXEDPARAMS=fixedparams,$
	 SAVEMAPDIR=savemapdir, $
	 noise=noise0, fknee=fknee0, $
	 USEFITS=usefits,$
	 LINEARCORR=linearcorr

;      CREATEMAP_FREE_CONF, info
;      ptr_free, maps
   ENDFOR
ENDIF ELSE BEGIN
   doptrs=1
   tods = smap_read_and_filter(datadir, mapparam, $
      PTRS=doptrs, $
      ASTROMOFFSETS=offsets_info, $
      BADOBSID=badobsid, SPEEDCUT=speedcut, $
      SUCCESS=success, ERRMSG=errmsg,/VERB)
;   stop

   if n_elements(mapname) eq 1 then begin
      for i=0,2 do $
	 if keyword_set(USEFITS) then $
	    mapfiles[i]=mapname+'.fits' $
	    else $
	    mapfiles[i]=mapname+'.dat' 
   endif else begin
	 if keyword_set(USEFITS) then $
	    mapfiles=mapname+'.fits' $
	    else $
	    mapfiles=mapname+'.dat' 
   endelse

   exnamebasewnum=exnamebase;+strcompress('_filtered_w_simulator',/remove_all)
   SMAP_TRANSFUN_MAKEMAP_V4, datadirbase, dataname, dataversion, $
      dirmap,mapfiles,maps,tods,mapparam, $
      skypix_side_x,skypix_side_y, $
      SAVETODS=savetods,EXNAME=exnamebasewnum, EXAPP=exapp,  $
      DO_PTRS=do_ptrs, DO_ONEBAND=do_oneband, DO_TDC=do_tdc, $
      ASTROMNAME=astromname, NOASTROM=noastrom, $
      JKLIST=jklist, DO_JKBOLO=do_jkbolo, DO_AORMAPS=do_aormaps, $
      DO_MATCHED=do_matched, INSTNOISE=instnoise,$
      BADBOLOS=badbolos, EXCLUDEMASK=excludemask, $
      ITERPARAMS=iterparams, NITER=niter, FIRST_OFFS=first_offs, $
      FIRST_GAIN=first_gain, FIRST_WT=first_wt, $
      FIRST_CLIP=first_clip, FIXED_NT=fixed_nt, NTERMS=nterms, $
      NT_SCALE=nt_scale, MIN_EXPOSURE=min_exposure, $
      GROW_CLIP=grow_clip, CLIPSIGMA=clipsigma,$
      ITERDIAG=iterdiag, PIXSIZE=pixsize, SHORTNAME=shortname, $
      NO250=no250, NO350=no350, NO500=no500, $
      NXPIX=nxpix, NYPIX=nypix, CRVALX=crvalx, CRVALY=crvaly, $
      CRPIXX=crpixx, CRPIXY=crpixy, SPEEDCUT=speedcut, $
      BADOBSID=badobsid, FIXEDPARAMS=fixedparams,$
      SAVEMAPDIR=savemapdir, $
      noise=noise, fknee=fknee, $
      USEFITS=usefits,$
      LINEARCORR=linearcorr

;   CREATEMAP_FREE_CONF, info
ENDELSE
END
