FUNCTION SMAP_TRANSFUN_PIPELINE_V5, map_template, field_name,EXNAMEBASE=exnamebase, $ 
				NOISE=noise,FKNEE=fknee,$
				DIRMAP=dirmap, MAPNAME=mapname, $
;				JK_ONLY=jk_only, HALFS=halfs, ANGS=angs, $
;				OMIT_BAD_SCANS=omit_bad_scans, $
			        CONFFILE=conffile,$
				USEFITS=usefits,$
				PROJTYPE=projtype, $
				NXPIX=nxpix, NYPIX=nypix, CRVALX=crvalx, CRVALY=crvaly, $
				CRPIXX=crpixx, CRPIXY=crpixy, SPEEDCUT=speedcut, $
				BADOBSID=badobsid,$
				WRITETODS=writetods,$
				SAVEMAPDIR=savemapdir
;+
;NAME
; SMAP_TRANSFUN_PIPELINE_V4
;PURPOSE
; Run a small version of the smap pipeline on multiple simulation
; and write maps.  
; Does not calculate the transfer function here.  Use XXX to do that.
;USAGE
; SMAP_TRANSFUN_PIPELINE_V5,map_template,field_name, SAVEMAPDIR=savemapdir, & either:
;     [DIRMAP=dirmap, MAPNAME=mapname]
;INPUTS
; map_template name of the maps trying to imitate 
; field_name   "name" of the map, e.g., 'xmm-lss', 'cdfs-swire', 'lockman-swire', 'adfs'
;
;OPTIONAL INPUTS
; dirsim/dirmap	  directory where simulations/map
; exnamebase   extension name to give to the maps 
; 
;  IF USING YOUR OWN MAP (SAY, YOU WANT TO SIMULATE OBSERVATIONS OF HI 21CM MAPS), THEN NEED 
;     i)   DIRSIM directory where they are located (I use /data/viero/sim_transfunc/)
;     ii)  MAPNAME is the name of the map you want to "observe" 
;
;AUTHORS
; Alex Amblard
; Marco Viero
bands=['250','350','500']

IF NOT(KEYWORD_SET(exnamebase)) THEN exnamebase=''
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
IF info.no_medfilt   GE 0  THEN no_medfilt   = info.no_medfilt
IF info.do_tdc       GE 0  THEN do_tdc       = info.do_tdc
IF info.astromname   NE "" THEN astromname   = info.astromname
IF info.noastrom     GE 0  THEN noastrom     = info.noastrom
IF info.todmean      GE 0  THEN todmean      = info.todmean
IF info.do_jkbolo    GE 0  THEN do_jkbolo    = info.do_jkbolo
IF info.do_aormaps   GE 0  THEN do_aormaps   = info.do_aormaps
IF info.do_matched   GE 0  THEN do_matched   = info.do_matched
IF info.do_redsource GE 0  THEN do_redsource = info.do_redsource
IF info.red_brute    GE 0  THEN red_brute    = info.red_brute
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
IF info.badbolos NE PTR_NEW() THEN badbolos  = *info.badbolos
IF info.jklist NE PTR_NEW() THEN jklist      = *info.jklist
IF info.pixsize[0]   GE 0 THEN pixsize       = info.pixsize
IF info.instnoise[0] GE 0 THEN instnoise     = info.instnoise
first_clip=21.
;IF info.nxpix[0] GT 0     THEN tnxpix  = info.nxpix
;IF info.nypix[0] GT 0     THEN tnypix  = info.nypix
;IF FINITE(info.crvalx[0]) THEN tcrvalx = info.crvalx
;IF FINITE(info.crvaly[0]) THEN tcrvaly = info.crvaly
;IF FINITE(info.crpixx[0]) THEN tcrpixx = info.crpixx
;IF FINITE(info.crpixy[0]) THEN tcrpixy = info.crpixy
;IF info.projtype     NE "" THEN projtype     = info.projtype
IF info.nolatlon     GE 0  THEN nolatlon     = info.nolatlon
IF info.store_pixinfo GE 0 THEN store_pixinfo = info.store_pixinfo

IF STRLEN( info.allowedobsids ) NE 0 THEN $
   allowed_obsids = ULONG( STRSPLIT( info.allowedobsids, ",", /EXTRACT ) )

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
IF info.do_fixeddeglitch GE 1 THEN BEGIN
   IF ~ fixedparams.dofp THEN $
      MESSAGE,"Must set fixedparams if doing fixed deglitching"
   fixedparams.deglitch = 1B
   ;;turn off additional deglitching
   first_clip = 0
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

   ;;;;;;;;;;;;;;;;;; KEEP ?? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ; bad bolos (replaces default)
   IF PTR_VALID(info.badbolos) THEN badbolos = *info.badbolos

   ; todmask
   IF PTR_VALID(info.todmask) THEN BEGIN
      ntodmask = N_ELEMENTS(*info.todmask)
      todmask = REPLICATE({obsid:"", $
	 type:"", $
	 params:PTR_NEW()}, $
	 ntodmask)
      FOR i=0,ntodmask-1 DO BEGIN
	 nwords = N_ELEMENTS(*(*info.todmask)[i])
	 IF nwords LT 3 THEN $
	    MESSAGE, "todmask must have at least 3 parameters"

	 todmask[i].obsid  = (*(*info.todmask)[i])[1]
	 todmask[i].type   = (*(*info.todmask)[i])[0]
	 todmask[i].params = PTR_NEW(DOUBLE((*(*info.todmask)[i])[2:*]))

	 ; check formatting
	 CASE todmask[i].type OF
	 "circ": IF nwords NE 5 THEN $ ; 3 params
	    MESSAGE, "bad formatting: todmask=circ obsid rac decc rad"
	 "poly": IF nwords MOD 2 NE 0 THEN $ ; even number of params
	    MESSAGE, "bad formatting: todmask=poly obsid ra1 dec1 ... ran decn"
	 ELSE: MESSAGE, "Undefined todmask type '"+todmask[i].type+"'"
	 ENDCASE
      ENDFOR
   ENDIF
   ;;;;;;;;;;;;;;;;;; KEEP ?? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


head=headfits(map_template[0],ext=1)
CRVALX=sxpar(head,'CRVAL1')
CRVALY=sxpar(head,'CRVAL2')
CRPIXX=dblarr(3)
CRPIXY=dblarr(3)
nxpix=intarr(3)
nypix=intarr(3)
pixscale=dblarr(3)
if keyword_set(map_template) then begin
   FOR i=0,2 DO BEGIN
      head=headfits(map_template[i],ext=1)
      CRPIXX[i]=sxpar(head,'CRPIX1')
      CRPIXY[i]=sxpar(head,'CRPIX2')
      NXPIX[i]=sxpar(head,'NAXIS1')
      NYPIX[i]=sxpar(head,'NAXIS2')
      PIXSCALE[i]=sxpar(head,'CD2_2')
   ENDFOR 
   PROJTYPE=sxpar(head,'PROJTYPE')
endif else begin
   IF FINITE(info.crvalx[0]) THEN crvalx = info.crvalx
   IF FINITE(info.crvaly[0]) THEN crvaly = info.crvaly
   IF FINITE(info.crpixx[0]) THEN crpixx = info.crpixx
   IF FINITE(info.crpixy[0]) THEN crpixy = info.crpixy
   IF info.projtype     NE "" THEN projtype     = info.projtype
endelse

simmaphead=headfits(dirmap+mapname+'.fits')
if sxpar(simmaphead,'NAXIS') eq 0 then $
   simmaphead=headfits(dirmap+mapname+'.fits',ext=1)
pixsize_degrees=sxpar(simmaphead,'CDELT2')
if pixsize_degrees eq 0 then $
   pixsize_degrees=sxpar(simmaphead,'CD2_2')
skypix_side_x=sxpar(simmaphead,'NAXIS1')
skypix_side_y=sxpar(simmaphead,'NAXIS2')
skypix_pixsize=pixsize_degrees*3600.
crpix_x=sxpar(simmaphead,'CRPIX1')
crpix_y=sxpar(simmaphead,'CRPIX2')

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

doptrs=1
tods = smap_read_and_filter(datadir, mapparam, $
   PTRS=doptrs, $
   ASTROMOFFSETS=offsets_info, $
   BADOBSID=badobsid, SPEEDCUT=speedcut, $
   SUCCESS=success, ERRMSG=errmsg,/VERB)

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

exnamebasewnum=exnamebase
SMAP_TRANSFUN_MAKEMAP_V5, datadirbase, dataname, dataversion, $
   dirmap,mapfiles,maps,tods,mapparam, $
   skypix_side_x=skypix_side_x,skypix_side_y=skypix_side_y, $
   SAVETODS=savetods,EXNAME=exnamebasewnum, EXAPP=exapp,  $
   DO_PTRS=do_ptrs, DO_ONEBAND=do_oneband, DO_TDC=do_tdc, $
   ASTROMNAME=astromname, NOASTROM=noastrom, $
   JKLIST=jklist, DO_JKBOLO=do_jkbolo, DO_AORMAPS=do_aormaps, $
   DO_MATCHED=do_matched, INSTNOISE=instnoise,$
   DO_REDSOURCE=do_redsource, RED_BRUTE=red_brute, $
   BADBOLOS=badbolos, EXCLUDEMASK=excludemask, $
   ITERPARAMS=iterparams, NITER=niter, FIRST_OFFS=first_offs, $
   FIRST_GAIN=first_gain, FIRST_WT=first_wt, $
   FIRST_CLIP=first_clip, FIXED_NT=fixed_nt, NTERMS=nterms, $
   NT_SCALE=nt_scale, MIN_EXPOSURE=min_exposure, $
   GROW_CLIP=grow_clip, CLIPSIGMA=clipsigma,$
   ITERDIAG=iterdiag, PIXSIZE=pixsize, SHORTNAME=shortname, $
   NO250=no250, NO350=no350, NO500=no500, $
   PROJTYPE=projtype, $
   NXPIX=nxpix, NYPIX=nypix, CRVALX=crvalx, CRVALY=crvaly, $
   CRPIXX=crpixx, CRPIXY=crpixy, SPEEDCUT=speedcut, $
   BADOBSID=badobsid, FIXEDPARAMS=fixedparams,$
   SAVEMAPDIR=savemapdir, $
   noise=noise, fknee=fknee, $
   USEFITS=usefits,$
   WRITETODS=writetods,$
   LINEARCORR=linearcorr

END
