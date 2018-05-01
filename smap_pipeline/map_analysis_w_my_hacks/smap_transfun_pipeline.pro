FUNCTION SMAP_TRANSFUN_PIPELINE,dir_data,map_template,startnum,endnum,exnamebase,maskfile,dirsim=dirsim,$
                                deltal=deltal,dirsave=dirsave
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
;AUTHOR
; Alex Amblard
;-




; CREATE SOME SKY SIMULATIONS,
; Here Gaussian Map Realizations
; but it could be changed

IF NOT(KEYWORD_SET(dirsim)) THEN dirsim='/data/amblard/sim_transfunc/locksw/'
IF NOT(KEYWORD_SET(deltal)) THEN deltal=100.
IF NOT(KEYWORD_SET(dirsave)) THEN dirsave='/data/'+GETENV('USER')+'/'


; READ the TODS to use for the simulated observation
; and FIX the parameters of the maps
; example of dir_data :
; dir_data = "/data/spiredaq/reprocessed/LockmanSw_L1b/" + $
;        ['1342186108', '1342186109'] + $
;        '/level1/herschel.spire.ia.dataset.PointedPhotTimeline/'

files = FILE_SEARCH(dir_data+'*.fits',/FULLY)

; Define mapparam and tods
tods = smap_read_and_filter( files, mapparam, /PTRS, SUCCESS=success,$
                             ERRMSG=errmsg, /MEDFILT, /VERB )
head=headfits(map_template[0])
CRVALX=sxpar(head,'CRVAL1') 
CRVALY=sxpar(head,'CRVAL2')
CRPIXX=dblarr(3)
CRPIXY=dblarr(3)
nxpix=intarr(3)
nypix=intarr(3)

FOR i=0,2 DO BEGIN
   head=headfits(map_template[i])
   CRPIXX[i]=sxpar(head,'CRPIX1') 
   CRPIXY[i]=sxpar(head,'CRPIX2')
   NXPIX[i]=sxpar(head,'NAXIS1')
   NYPIX[i]=sxpar(head,'NAXIS2')
ENDFOR  


; Prepare a map structure to be read by A. Conley's simulator 
; for now simulation size fixed at 8000x8000 pixel of 2"
; will make it flexible in a future version


mapstruct250=get_smap_mapstruct(npixx=8000,npixy=8000,band='PSW',/noerr,/noexp,/nomask)
mapstruct350=get_smap_mapstruct(npixx=8000,npixy=8000,band='PMW',/noerr,/noexp,/nomask)
mapstruct500=get_smap_mapstruct(npixx=8000,npixy=8000,band='PLW',/noerr,/noexp,/nomask)
mapstruct500.astrometry.cd=identity(2)
mapstruct500.astrometry.crval=[crvalx,crvaly]
mapstruct500.astrometry.cdelt=[-5.555556e-04,5.555556e-04]
mapstruct500.astrometry.crpix=[4000,4000]
mapstruct250.astrometry=mapstruct500.astrometry
mapstruct350.astrometry=mapstruct500.astrometry
maps=ptrarr(3)
maps[0]=ptr_new(temporary(mapstruct250))
maps[1]=ptr_new(temporary(mapstruct350))
maps[2]=ptr_new(temporary(mapstruct500))



; READ THE SKY SIMULATIONS AND MAKE NOISELESS MAPS
mapfiles=strarr(3)



FOR ii=startnum,endnum DO BEGIN

   bands=['250','350','500']
; Need to make it more flexible here
for i=0,2 do $
      mapfiles[i]='Sky_'+bands[i]+'_50mJy'+strcompress(ii,/remove_all)+'_c.dat'

   exnamebasewnum=exnamebase+'_'+strcompress(ii,/remove_all)
   SMAP_TRANSFUN_MAKEMAP,mapfiles,maps,tods,exnamebasewnum,mapparam,$
                         dirsim=dirsim,CRVALX=crvalx, CRVALY=crvaly, $
                         CRPIXX=crpixx, CRPIXY=crpixy, NXPIX=nxpix, NYPIX=nypix

ENDFOR

; COMPUTE SOME POWER SPECTRA FOR THE TRANSFER FUNCTION


; order is opposed because of the alphabetical ordering of the files
bands=['PLW','PMW','PSW']
fres=['500','350','250']

; maximum ell number is 2000 for now
cllia=dblarr(2000,endnum-startnum+1,3,2)

for fre=0,2 do begin

   restore,maskfile[fre]
   sizel=n_elements(mkkl[*,0])

   for ii=startnum,endnum do begin

      exnamebasewnum2='_itermap'+exnamebase+'_'+strcompress(ii,/remove_all)
      map=read_smap_fitsmap('./',bands[fre],filenamein=!SMAP_MAPS+mapparam.obsids+exnamebasewnum2+'_'+bands[fre]+'.fits')
      map1=read_smap_fitsmap('./',bands[fre],filenamein=!SMAP_MAPS+mapparam.obsids+exnamebasewnum2+'_'+bands[fre]+'.fits')
      map2=read_smap_fitsmap('./',bands[fre],filenamein=!SMAP_MAPS+mapparam.obsids+exnamebasewnum2+'_'+bands[fre]+'.fits')
      

      clli=smap_get_angular_spec(map,deltal,callc=8,mkk=mkkl,mask=maskl,ell=elll)
      cllia[0:sizel-1,ii-startnum,fre,0]=clli
      cllix=smap_get_angular_spec(map1,deltal,callc=8,mkk=mkkl,mask=maskl,ell=elll,secondmap=map2)
      cllia[0:sizel-1,ii-startnum,fre,1]=cllix
   endfor

endfor



save,file=dirsave+'Cls_transfun_'+mapparam.obsids+exnamebase+'_'+strcompress(startnum,/remove_all)+'_'+strcompress(endnum,/remove_all)+'.save',cllia,elll






END
