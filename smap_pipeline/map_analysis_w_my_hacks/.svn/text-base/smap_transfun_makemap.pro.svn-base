PRO SMAP_TRANSFUN_MAKEMAP,mapfiles,maps,tods,exnamebase,mapparam,dirsim=dirsim,CRVALX=crvalx, CRVALY=crvaly, $
                          CRPIXX=crpixx, CRPIXY=crpixy, NXPIX=nxpix, NYPIX=nypix
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
;     - for now the simulated sky need to be 8000x8000 pixels of 2 arcseconds resolution
;     - for now the name of the simulated sky have the following format :
;         'Sky_'+['250','350','500']+'_50mJy'+$i+'_c.dat' 
;       where $i is the indice of the simulation
;
;
;AUTHOR
; Alex Amblard
;-


  iterparams =  {niter:10, first_offs:1, first_gain:0, first_wt:10, $
                 first_clip:0, nterms:2, min_hits:2}

  badbolos = ['PSWD15', 'PSWC12', 'PSWG8', 'PSWG11','PSWA10','PSWA11', 'PSWA13']
  excludemask=["maskMaster","maskUncorrectedTruncation","maskDead","maskNoisy",$
               "maskSlow", "maskGlitchL1Detected"]


  IF NOT(KEYWORD_SET(dirsim)) THEN dirsim = './'

  x=dblarr(8000,8000)

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
  simulate_scan,tods,maps,noise=[0.,0,0],/ver
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

; FULL DATA SET
; we removed iter_propperror and put iter_diag to 0
  smap_make_maps, tods, mapparam, map250, map350, map500, SUCCESS=success,$
                  CRVALX=crvalx, CRVALY=crvaly, $
                  CRPIXX=crpixx, CRPIXY=crpixy, NXPIX=nxpix, NYPIX=nypix, $
                  ERRMSG=errmsg,/VERB,/ITERMAP,$
                  ITER_PARAMS=iterparams, ITER_DIAG=iterdiag,$
                  BADBOLOS=badbolos,EXCLUDEMASK=excludemask,EXNAME=exnamebase
  IF success EQ 0 THEN MESSAGE,"Error on map build"
  st=SMAP_WRITE3COLORS(mapparam.obsids,map250,map350,map500,$
                       /SILENT,ERRMSG=swc_errmsg,EXNAME="_itermap"+exnamebase)

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
                  TOD_INDEX=todindex,ITER_FIXED=mapname+exnamebase
  IF success EQ 0 THEN MESSAGE,"Error on map build"
  st=SMAP_WRITE3COLORS(mapparam.obsids,map250,map350,map500,$
                       /SILENT,ERRMSG=swc_errmsg,EXNAME="_itermap"+exname)
; SECOND HALF
  todindex = INDGEN(nscans-nscans/2)+nscans/2
  exname = exnamebase+"_jk_half2"

  smap_make_maps, tods, mapparam, map250, map350, map500, SUCCESS=success,$
                  CRVALX=crvalx, CRVALY=crvaly, $
                  CRPIXX=crpixx, CRPIXY=crpixy, NXPIX=nxpix, NYPIX=nypix, $
                  ERRMSG=errmsg,/VERB,/ITERMAP,$
                  ITER_PARAMS=iterparams,/ITER_PROPERROR, $
                  BADBOLOS=badbolos,EXCLUDEMASK=excludemask,$
                  TOD_INDEX=todindex,ITER_FIXED=mapname+exnamebase
  IF success EQ 0 THEN MESSAGE,"Error on map build"
  st=SMAP_WRITE3COLORS(mapparam.obsids,map250,map350,map500,$
                       /SILENT,ERRMSG=swc_errmsg,EXNAME="_itermap"+exname)


END

