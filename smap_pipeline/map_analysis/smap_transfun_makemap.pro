PRO SMAP_TRANSFUN_MAKEMAP,mapfiles,maps,tods,exnamebase,mapparam,dirsim=dirsim,CRVALX=crvalx, CRVALY=crvaly, $
                          CRPIXX=crpixx, CRPIXY=crpixy, NXPIX=nxpix, NYPIX=nypix,$ 
			  SKYPIX_SIDE_X=skypix_side_x,SKYPIX_SIDE_Y=skypix_side_y,$
			  noise=noise,fknee=fknee, $
			  jk_only=jk_only, halfs=halfs,angs=angs, no_jk=no_jk, $
			  PIXSCALE=pixscale,DIRWRITE=dirwrite, FIELD_NUM=field_num
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
;     - for now the simulated sky need to be skypix_side_x by skypix_side_y of skypix_pixsize (default 2) arcseconds resolution
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

  IF NOT(KEYWORD_SET(noise)) THEN noise=[0.,0,0]
  IF NOT(KEYWORD_SET(fknee)) THEN fknee=[0.,0,0]
  IF NOT(KEYWORD_SET(dirsim)) THEN dirsim = './'

  ;iterparams =  {niter:10, first_offs:1, first_gain:0, first_wt:10, $
  ;               first_clip:0, nterms:2, min_hits:2}
  iterparams =  {niter:10, first_offs:1, first_gain:0, first_wt:10, $
                 first_clip:0, nterms:3, min_hits:1}

  badbolos = ['PSWD15', 'PSWC12', 'PSWG8', 'PSWG11','PSWA10','PSWA11', 'PSWA13']
  excludemask=["maskMaster","maskUncorrectedTruncation","maskDead","maskNoisy",$
               "maskSlow", "maskGlitchL1Detected"]

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

; noiseless timelines
  simulate_scan,tods,maps,noise=noise,fknee=fknee,/ver
; just apply the medfilt
  todsu = smap_read_and_filter( tods, mapparam, /PTRS, SUCCESS=success,$
                                ERRMSG=errmsg, /MEDFILT)

  ptr_free,tods
  tods=todsu

;------------------------------------------------------------------
; SET BAD SCANS AND BOLOS
;
; not necessary anymore since jump correction  
; except : '1342188651_a103001d' from bootes field
;
;------------------------------------------------------------------
  
  nscans = N_ELEMENTS(tods)
  iterdiag=2

  ;   todindex1 = INDGEN(nscans/2)
  ;   exname1 = exnamebase+"_jk_half1"
  ;   todindex2 = INDGEN(nscans-nscans/2)+nscans/2
  ;   exname2 = exnamebase+"_jk_half2"
  ;   exname1 = exnamebase+"_jk_ang1"  
  ;endelse
  ;BOOTES
;  if field_num eq 0 then begin
     halfs=0
     angs=1
     ;SMALL MAP
     ;todindex1= INDGEN(80)
     ;todindex2= INDGEN(80) + 80
     ;BIG MAP
     todindex1= INDGEN(80) + 160
     todindex2= INDGEN(80) + 240
     exname1 = exnamebase+"_jk_ang1"  
     exname2 = exnamebase+"_jk_ang2"  
;  endif
  ;ELAIS-S1
  if field_num eq 2 then begin
     halfs=0
     angs=1
     todindex1= INDGEN(50)
     todindex2= INDGEN(72) + 50
     exname1 = exnamebase+"_jk_ang1"  
     exname2 = exnamebase+"_jk_ang2"  
  endif
  ;LOCKMAN-SWIRE
  if field_num eq 5 then begin
     halfs=1
     angs=0
     ;todindex1= INDGEN(80) + (INDGEN(80) / 40) * 40 
     ;todindex2= todindex1+40 
     ;exname1 = exnamebase+"_jk_ang1"  
     ;exname2 = exnamebase+"_jk_ang2"  
     todindex1= INDGEN(80)
     todindex2= INDGEN(80) + 80
     exname1 = exnamebase+"_jk_half1"
     exname2 = exnamebase+"_jk_half2"
  endif
  ;ADFS
  if field_num eq 6 then begin
     halfs=0
     angs=1
     todindex1= INDGEN(35)
     todindex2= INDGEN(97) + 35
     exname1 = exnamebase+"_jk_ang1"  
     exname2 = exnamebase+"_jk_ang2"  
  endif
  ;XMM-LSS
  if field_num eq 7 then begin
     halfs=1
     angs=0
     ;todindex1= INDGEN(35+38+38)
     ;todindex2= INDGEN(35+38+38) + 35+38+38 
     todindex1= INDGEN(111)
     todindex2= INDGEN(111) + 111
     exname1 = exnamebase+"_jk_half1"
     exname2 = exnamebase+"_jk_half2"
  endif
  ;FLS
  if field_num eq 8 then begin
     halfs=0
     angs=1
     todindex1= INDGEN(54)
     todindex2= INDGEN(63) + 54
     exname1 = exnamebase+"_jk_ang1"  
     exname2 = exnamebase+"_jk_ang2"  
  endif
  ;COSMOS
  if field_num eq 9 then begin
     todindex1= INDGEN(128) + (INDGEN(128) / 16) * 16 
     todindex2= INDGEN(128) + (INDGEN(128) / 16) * 16 + 16
  endif
  ;CDFS-SWIRE
  if field_num eq 10 then begin
     halfs=1
     angs=0
     todindex = INDGEN(315)
     todindex = INDGEN(315) + 315
     exname1 = exnamebase+"_jk_half1"
     exname2 = exnamebase+"_jk_half2"
  endif

; FULL DATA SET
; we removed iter_propperror and put iter_diag to 0
;stop
if NOT(KEYWORD_SET(JK_ONLY)) then begin
  mapname=mapparam.obsids
  smap_make_maps, tods, mapparam, map250, map350, map500, SUCCESS=success,$
                  CRVALX=crvalx, CRVALY=crvaly, $
                  CRPIXX=crpixx, CRPIXY=crpixy, NXPIX=nxpix, NYPIX=nypix, $
                  ERRMSG=errmsg,/VERB,/ITERMAP,$
                  ITER_PARAMS=iterparams, ITER_DIAG=iterdiag,$
                  ;BADBOLOS=badbolos,EXCLUDEMASK=excludemask,EXNAME=exnamebase, $
                  BADBOLOS=badbolos,EXCLUDEMASK=excludemask,EXNAME=mapname+exnamebase, $
	          PIXSCALE=pixscale
  IF success EQ 0 THEN MESSAGE,"Error on map build"
  st=SMAP_WRITE3COLORS(mapparam.obsids,map250,map350,map500,$
                       /SILENT,ERRMSG=swc_errmsg,EXNAME="_itermap"+exnamebase,DIR=dirwrite)
 endif
		    
 if keyword_set(no_jk) eq 1 then begin
    angs=0
    halfs=0
 endif
 
if keyword_set(angs) then begin

; ANG 1 
  ;todindex = INDGEN(nscans/2)
  ;exname = exnamebase+"_jk_half1"

  mapname=mapparam.obsids
  
  smap_make_maps, tods, mapparam, map250, map350, map500, SUCCESS=success,$
                  CRVALX=crvalx, CRVALY=crvaly, $
                  CRPIXX=crpixx, CRPIXY=crpixy, NXPIX=nxpix, NYPIX=nypix, $
                  ERRMSG=errmsg,/VERB,/ITERMAP,$
                  ITER_PARAMS=iterparams,/ITER_PROPERROR, $
                  BADBOLOS=badbolos,EXCLUDEMASK=excludemask,$
                  TOD_INDEX=todindex1,ITER_FIXED=mapname+exnamebase, $
		  PIXSCALE=pixscale
  IF success EQ 0 THEN MESSAGE,"Error on map build"
  st=SMAP_WRITE3COLORS(mapparam.obsids,map250,map350,map500,$
                       /SILENT,ERRMSG=swc_errmsg,EXNAME="_itermap"+exname1,DIR=dirwrite)
; ANG 2
  ;todindex = INDGEN(nscans-nscans/2)+nscans/2
  ;exname = exnamebase+"_jk_half2"

  smap_make_maps, tods, mapparam, map250, map350, map500, SUCCESS=success,$
                  CRVALX=crvalx, CRVALY=crvaly, $
                  CRPIXX=crpixx, CRPIXY=crpixy, NXPIX=nxpix, NYPIX=nypix, $
                  ERRMSG=errmsg,/VERB,/ITERMAP,$
                  ITER_PARAMS=iterparams,/ITER_PROPERROR, $
                  BADBOLOS=badbolos,EXCLUDEMASK=excludemask,$
                  TOD_INDEX=todindex2,ITER_FIXED=mapname+exnamebase,$
		  PIXSCALE=pixscale
  IF success EQ 0 THEN MESSAGE,"Error on map build"
  st=SMAP_WRITE3COLORS(mapparam.obsids,map250,map350,map500,$
                       /SILENT,ERRMSG=swc_errmsg,EXNAME="_itermap"+exname2,DIR=dirwrite)

endif
if keyword_set(halfs) then begin

; FIRST HALF
  todindex = INDGEN(nscans/2)
  exname = exnamebase+"_jk_half1"

  mapname=mapparam.obsids
  
  smap_make_maps, tods, mapparam, map250, map350, map500, SUCCESS=success,$
                  CRVALX=crvalx, CRVALY=crvaly, $
                  CRPIXX=crpixx, CRPIXY=crpixy, NXPIX=nxpix, NYPIX=nypix, $
                  ERRMSG=errmsg,/VERB,/ITERMAP,$
                  ITER_PARAMS=iterparams,/ITER_PROPERROR, $
                  BADBOLOS=badbolos,EXCLUDEMASK=excludemask,$
                  TOD_INDEX=todindex,ITER_FIXED=mapname+exnamebase, $
		  PIXSCALE=pixscale
  IF success EQ 0 THEN MESSAGE,"Error on map build"
  st=SMAP_WRITE3COLORS(mapparam.obsids,map250,map350,map500,$
                       /SILENT,ERRMSG=swc_errmsg,EXNAME="_itermap"+exname,DIR=dirwrite)
; SECOND HALF
  todindex = INDGEN(nscans-nscans/2)+nscans/2
  exname = exnamebase+"_jk_half2"

  smap_make_maps, tods, mapparam, map250, map350, map500, SUCCESS=success,$
                  CRVALX=crvalx, CRVALY=crvaly, $
                  CRPIXX=crpixx, CRPIXY=crpixy, NXPIX=nxpix, NYPIX=nypix, $
                  ERRMSG=errmsg,/VERB,/ITERMAP,$
                  ITER_PARAMS=iterparams,/ITER_PROPERROR, $
                  BADBOLOS=badbolos,EXCLUDEMASK=excludemask,$
                  TOD_INDEX=todindex,ITER_FIXED=mapname+exnamebase,$
		  PIXSCALE=pixscale
  IF success EQ 0 THEN MESSAGE,"Error on map build"
  st=SMAP_WRITE3COLORS(mapparam.obsids,map250,map350,map500,$
                       /SILENT,ERRMSG=swc_errmsg,EXNAME="_itermap"+exname,DIR=dirwrite)

endif

END

