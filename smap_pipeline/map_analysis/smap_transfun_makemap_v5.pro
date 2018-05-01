PRO SMAP_TRANSFUN_MAKEMAP_V5,datadirbase, dataname, dataversion, $
			  dirsim,mapfiles,maps,tods,mapparam, $
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
		       ;+
		       ;NAME
; SMAP_TRANSFUN_MAKEMAP
;PURPOSE
; Run the iterative map making for the transfer function (only with 10 iterations
; compared to the 20 in the original pipeline to gain some speed)
;USAGE
; SMAP_TRANSFUN_PIPELINE,mapfiles,maps,tods,exnamebase,mapparam
;INPUTS
; mapfiles     names of the 3 sky simulated map (1 per frequency)
; maps         empty map structure array to be read by A. Conley's simulator,
;              its has the 3 frequencies  
; tods         tods of the real data which will give the pointing information
; exnamebase   extension name to give to the maps 
; mapparam     structure containing the map parameters 
;
;OPTIONAL INPUTS
; dirsim   directory where the sky maps are located (default is my sim directory : 
;          /data/amblard/sim_transfunc/locksw/)
; crvalx,crvaly,crpixx,crpixy,nxpix,nypix astrometry informations to produce a map
;                                         identical to the ones produced with real
;                                         data  
;
;ADDITIONAL REMARKS
;
;     - for now the simulated sky need to be skypix_side by skypix_side of skypix_pixsize (default 2) arcseconds resolution
;     - for now the name of the simulated sky have the following format :
;         NO LONGER TRUE ('Sky_'+['250','350','500']+'_50mJy'+$i+'_c.dat') 
;         'Sky_'+$skypix_pixsize+'x'+$skypix_side+'x'+$skypix_side+['250','350','500']+$flux_cut+'mJy_'+$i+'.dat' 
;       where $i is the indice of the simulation
;
;
;AUTHOR
; Alex Amblard
; Marco Viero
;-
  IF NOT(KEYWORD_SET(noise)) THEN noise=[0.,0,0]; else noise eq -1
  IF NOT(KEYWORD_SET(fknee)) THEN fknee=[0.,0,0]

  if keyword_set(USEFITS) then begin
     if sxpar(headfits(dirsim+mapfiles[0]),'NAXIS') eq 0 then begin
	(*maps[0]).image=readfits(dirsim+mapfiles[0],hd0,ext=1)
	(*maps[1]).image=readfits(dirsim+mapfiles[1],hd0,ext=1)
	(*maps[2]).image=readfits(dirsim+mapfiles[2],hd0,ext=1)
     endif else begin
	(*maps[0]).image=readfits(dirsim+mapfiles[0],hd0)
	(*maps[1]).image=readfits(dirsim+mapfiles[1],hd0)
	(*maps[2]).image=readfits(dirsim+mapfiles[2],hd0)
     endelse
  endif else begin 
     x=dblarr(skypix_side_x,skypix_side_y)  
     openr,1,dirsim+mapfiles[0]
     readu,1,x
     close,1
     (*maps[0]).image=x
     openr,1,dirsim+mapfiles[1]
     readu,1,x
     close,1
     (*maps[1]).image=x
     openr,1,dirsim+mapfiles[2]
     readu,1,x
     close,1
     (*maps[2]).image=x
  endelse

; noiseless timelines
  simulate_scan,tods,maps,noise=noise,fknee=fknee,/ver
;  ptr_free,maps
;  maps=0.
;  stop

; just apply the medfilt
  doptrs=1
  todsu = SMAP_READ_AND_FILTER(tods, mapparam, PTRS=doptrs, $
     ASTROMOFFSETS=offsets_info, $
     BADOBSID=badobsid, SPEEDCUT=speedcut, $
     SUCCESS=success, ERRMSG=errmsg, $
     MEDFILT=do_medfilt, $
     LINEARCORR=linearcorr, /VERB)
  IF success EQ 0 THEN MESSAGE,"Error on initial read/filter: "+errmsg
  IF KEYWORD_SET(do_tdc) THEN BEGIN
     SMAP_TCDRIFT_CORRECT,todsu,EXCLUDEMASK=excludemask,$
	/VERBOSE,SUCCESS=stc_success,ERRMSG=stc_errmsg
  ENDIF

  ptr_free,tods
  tods=todsu
;  todsu=0
;  stop

  IF KEYWORD_SET(writetods) THEN BEGIN
     for i=0,n_elements(tods)-1 do begin
	outfile = savemapdir+(*tods[i]).shortfile
	smap_writetod, *tods[i], outfile, /verb
     endfor
  ENDIF ELSE BEGIN

  iterdiag=2
  ;first_clip=0
  CREATE_ITERMAP, datadirbase, dataname, dataversion, $
     SAVETODS=savetods, TODS=tods, MAPPARAM=mapparam, $
     EXNAME=exnamebasewnum, EXAPP=exapp, $
     DO_PTRS=do_ptrs, DO_ONEBAND=do_oneband, DO_TDC=do_tdc, $
     NO_MEDFILT=no_medfilt, TODMEAN=todmean,$  
     ASTROMNAME=astromname, NOASTROM=noastrom, $
     JKLIST=jklist, DO_JKBOLO=do_jkbolo, DO_AORMAPS=do_aormaps, $
     DO_MATCHED=do_matched, INSTNOISE=instnoise,$
     BADBOLOS=badbolos, EXCLUDEMASK=excludemask, $
     ITERPARAMS=iterparams, NITER=niter, $
     FIRST_OFFS=first_offs, FIRST_GAIN=first_gain, $
     FIRST_WT=first_wt, FIRST_CLIP=first_clip, FIXED_NT=fixed_nt, $
     NTERMS=nterms, $
;     NT_SCALE=nt_scale, MIN_EXPOSURE=min_exposure, $
     GROW_CLIP=grow_clip, CLIPSIGMA=clipsigma,$
     ITERDIAG=iterdiag, PIXSIZE=pixsize, SHORTNAME=shortname, $
     NO250=no250, NO350=no350, NO500=no500, $
     PROJTYPE=projtype, $
     NXPIX=nxpix, NYPIX=nypix, CRVALX=crvalx, CRVALY=crvaly, $
     CRPIXX=crpixx, CRPIXY=crpixy, SPEEDCUT=speedcut, $
     BADOBSID=badobsid, FIXEDPARAMS=fixedparams,$
     LINEARCORR=linearcorr, ALLOWED_OBSIDS=allowed_obsids,$
     SAVEMAPDIR=savemapdir, GETDATA=getdata, NOLATLON=nolatlon, $
     STORE_PIXINFO=store_pixinfo, $
     TODMASK=todmask

   ;CREATEMAP_FREE_CONF, info
   ENDELSE

  END
