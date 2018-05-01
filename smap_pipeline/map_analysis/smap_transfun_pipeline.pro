FUNCTION SMAP_TRANSFUN_PIPELINE,dir_data,map_template,startnum,endnum,exnamebase,maskfile, $
                                deltal=deltal,lmin=lmin,dirsave=dirsave, dirsim=dirsim,$
				HI_CUT=hi_cut, SKYPIX_SIDE=skypix_side, SKYPIX_PIXSIZE=skypix_pixsize, $
				noise=noise,fknee=fknee, $
				jk_only=jk_only, halfs=halfs,angs=angs, $
				clean_skies=clean_skies, two_halo=two_halo, dirwrite=dirwrite, PIXSCALE=pixscale, FIELD_NUM=field_num
;+
;NAME
; SMAP_TRANSFUN_PIPELINE
;PURPOSE
; Run a small version of the smap pipeline on multiple simulation
; and compute the power spectrum of the output maps in order
; to calculate the transfer function
;USAGE
; SMAP_TRANSFUN_PIPELINE,dir_data,map_template,startnum,endnum,exnamebase,maskfile
;INPUTS
; dir_data     directory where L1 data are located, it helps give the pointing
;              information to the simulator 
; map_template name of the files containing the map observed to have the same
;              astrometry as in the data
;
; startnum     index of the simulated map to start with, this option allows
;              to run the program on different simulations with different nodes
;              to speed up the computation (1 simulation ~ 20 minute on spire0)
; endnum       last index of the simulated map for which to compute the power spectrum
; exnamebase   extension name to give to the maps 
; maskfile     name of the 3 files containing the correlation matrix mkkl and real space mask maskl, 
;              it need to be an IDL save file and the 2 variables need to be named : mkkl and maskl
;OPTIONAL INPUTS
; dirsim   directory where the sky maps are located (default is my sim directory : 
;          /data/amblard/sim_transfunc/locksw/)
; deltal   resolution in the k (ell) space, the default is deltal = 100 (deltak = 
;          4.6e-3 arcmin^(-1) )
; dirsave  directory where to save the file contaning the Cls estimated on the 
;          simulation (default is /data/$USER/)
;
;ADDITIONAL REMARKS
;
;     - for now the simulated sky need to be 8000x8000 pixels of 2 arcseconds resolution
;     - for now the name of the simulated sky have the following format :
;          'Sky_'+['250','350','500']+'_50mJy'+$i+'_c.dat'
;        where $i is the indice of the simulation         
;
;AUTHORS
; Alex Amblard
; Marco Viero




; CREATE SOME SKY SIMULATIONS,
; Here Gaussian Map Realizations
; but it could be changed


IF NOT(KEYWORD_SET(dirsim)) THEN dirsim='/data/amblard/sim_transfunc/locksw/'
IF NOT(KEYWORD_SET(dirwrite)) THEN dirwrite=!SMAP_MAPS
IF NOT(KEYWORD_SET(field_num)) THEN field_num=-1
IF NOT(KEYWORD_SET(deltal)) THEN deltal=100.
;IF NOT(KEYWORD_SET(lmin)) THEN lmin=50.
IF NOT(KEYWORD_SET(dirsave)) THEN dirsave='/data/'+GETENV('USER')+'/'
IF NOT(KEYWORD_SET(skypix_side)) THEN skypix_side=2.^(12.);8000
IF NOT(KEYWORD_SET(skypix_pixsize)) THEN skypix_pixsize=2.0
IF NOT(KEYWORD_SET(pixscale)) THEN defparams = SMAP_GETDEFPARAMS(PIXSIZE=pixscale) 


; READ the TODS to use for the simulated observation
; and FIX the parameters of the maps
; example of dir_data :
; dir_data = "/data/spiredaq/reprocessed/LockmanSw_L1b/" + $
;        ['1342186108', '1342186109'] + $
;        '/level1/herschel.spire.ia.dataset.PointedPhotTimeline/'

;REPLACE THIS WITH NEXT
;files = FILE_SEARCH(dir_data+'*.fits',/FULLY)

; Define mapparam and tods
;tods = smap_read_and_filter( files, mapparam, /PTRS, SUCCESS=success,$
;                             ERRMSG=errmsg, /MEDFILT, /VERB )
;REPLACED files with dir_data HERE
tods = smap_read_and_filter(dir_data, mapparam, /PTRS, SUCCESS=success,$
                             ERRMSG=errmsg);, /MEDFILT, /VERB )
;DONE REPACE.

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


; Prepare a map structure to be read by A. Conley's simulator 
; for now simulation size fixed at 8000x8000 pixel of 2"
; will make it flexible in a future version

pixsize_degrees=skypix_pixsize/3600.

mapstruct250=get_smap_mapstruct(npixx=skypix_side,npixy=skypix_side,band='PSW',/noerr,/noexp,/nomask)
mapstruct350=get_smap_mapstruct(npixx=skypix_side,npixy=skypix_side,band='PMW',/noerr,/noexp,/nomask)
mapstruct500=get_smap_mapstruct(npixx=skypix_side,npixy=skypix_side,band='PLW',/noerr,/noexp,/nomask)
mapstruct500.astrometry.cd=identity(2)
mapstruct500.astrometry.crval=[crvalx,crvaly]
mapstruct500.astrometry.cdelt=[-1.*pixsize_degrees,pixsize_degrees]
mapstruct500.astrometry.crpix=[skypix_side/2.,skypix_side/2.]
mapstruct250.astrometry=mapstruct500.astrometry
mapstruct350.astrometry=mapstruct500.astrometry
maps=ptrarr(3)
maps[0]=ptr_new(temporary(mapstruct250))
maps[1]=ptr_new(temporary(mapstruct350))
maps[2]=ptr_new(temporary(mapstruct500))


; READ THE SKY SIMULATIONS AND MAKE NOISELESS MAPS
mapfiles=strarr(3)

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

   noise0=noise_in
   fknee0=fknee_in

   bands=['250','350','500']
; Need to make it more flexible here
blanks=['','','']
if KEYWORD_SET(two_halo) then sky_prefix='M2HSky' else sky_prefix='MSky'
if KEYWORD_SET(clean_skies) then $
for i=0,2 do $
      mapfiles[i]=strcompress(sky_prefix+size_prefix+'_'+blanks[i]+strcompress(ii,/remove_all)+'.dat',/remove_all) $
      else $
for i=0,2 do $
      mapfiles[i]=strcompress(sky_prefix+size_prefix+'_'+bands[i]+cut_suffix+'_'+strcompress(ii,/remove_all)+'.dat',/remove_all) 
      ;mapfiles[i]='Sky_'+bands[i]+'_50mJy'+strcompress(ii,/remove_all)+'_c.dat'

   exnamebasewnum=exnamebase+'_'+strcompress(ii,/remove_all)
   SMAP_TRANSFUN_MAKEMAP,mapfiles,maps,tods,exnamebasewnum,mapparam,$
                         dirsim=dirsim,CRVALX=crvalx, CRVALY=crvaly, $
                         CRPIXX=crpixx, CRPIXY=crpixy, NXPIX=nxpix, NYPIX=nypix, $
		         SKYPIX_SIDE_X=skypix_side, SKYPIX_SIDE_Y=skypix_side, $
			 PIXSCALE=pixscale, DIRWRITE=dirwrite,$
			 noise=noise0,fknee=fknee0, $
			 jk_only=jk_only, halfs=halfs,angs=angs, $
			 FIELD_NUM=field_num

ENDFOR

END
