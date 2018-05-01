INIT_SIM

COMMON SIM_PARAMS, $
   wavelength, $
   mapname, $
   fwhm, $
   pixsize, $
   data_directories, $
   field_stamps, $
   field_obsids, $
   field_dates, $
   field_ids

;0 bootes
;1 elais-n1
;2 elais-s1
;3 edfs
;4 lockman-north
;5 lockman-swire
;6 sep
;7 xmm-lss
;8 fls
;9 cosmos
;10 cdfs-swire

field_num=8

gass=[1,1,1,1,1,0,1,1,0,1,1]
components=[3,3,3,3,3,3,2,2,2,2,1]
halfs=[0,0,0,0,0,1,0,1,0,1,1]
angs= [1,0,1,0,0,0,1,0,1,1,0]
name=['L','H','I']

allmap_pixsize=0;24.;0.;50.
if allmap_pixsize eq 0 then $
   defparams = SMAP_GETDEFPARAMS(PIXSIZE=pixscale) $
   else $
   pixscale=[allmap_pixsize,allmap_pixsize,allmap_pixsize]/3600.
if gass[field_num] eq 1 then inst_suffix='_GASS' else inst_suffix='_GBT' 

pix=pixscale*3600.

if allmap_pixsize ne 0 then allmap_suffix=strcompress('_'+STRING(allmap_pixsize,FORM='(D10.1)')+'_arcsec_pixels') $
   else allmap_suffix=''
exname = strcompress(STRING(pix,FORM='(D10.1)')+'_arcsec_pixels',/remove_all)
mapdir = strcompress(!SMAP_MAPS+field_ids[field_num]+'/'+STRING(pix,FORM='(D10.1)')+'_arcsec_pixels/',/remove_all)
; Script to Setup and Run Transfer Function Code 
; give the location of the L1 data we want to compare to
dir_data='/data/spiredaq/reprocessed/'+data_directories[field_num]+'/'  ;LockmanSw_L1e/'
; give the location of the map we want to compare too, be careful to be consistent with L1 data
;map_template=mapdir+field_obsids[field_num]+'_itermap_'+exname+mapname+'.fits'
map_template=mapdir+field_ids[field_num]+'_v4_itermap_'+exname+mapname+'.fits'
; first index of the simulated sky to run on

; directory where the simulated sky signal is located 
if gass[field_num] eq 1 then dirsim='/data/viero/maps/HI/GASS/' else dirsim='/data/viero/maps/HI/GBT/'
dirwrite=strcompress('/data/viero/sim_transfunc/HI/'+field_ids[field_num]+'/',/remove_all)

cut_suffix='_cut_100mJy'
deltal=80.
lmin=0.
dl_suffix=strcompress('_dl_'+string(deltal, format='(i20)') )
if keyword_set(lmin) then $
   lmin_suffix=strcompress('_lmin_'+string(lmin, format='(i10)') ) $
   else lmin_suffix=''
maskfile=strcompress('/data/viero/mkk/'+field_ids[field_num]+cut_suffix+'_'+['500X500','350X350','250X250']+$
   dl_suffix+lmin_suffix+'_'+STRING(reverse(pix),FORM='(D10.1)')+'_arcsec_pixels_mode_coupling_matrix.sav',/remove_all)

for j=0, components[field_num]-1 do begin
   filename=strcompress(field_ids[field_num]+inst_suffix+'_'+$
      name[j]+'VC.fits',/remove_all)
      ;name[j]+'VC_tan.fits',/remove_all)
   datfilename=strcompress(field_ids[field_num]+inst_suffix+'_'+$
      name[j]+'VC.dat',/remove_all)
      ;name[j]+'VC_tan.dat',/remove_all)
   head=headfits(map_template[0],ext=1)
   CRVALX=sxpar(head,'CRVAL1')
   CRVALY=sxpar(head,'CRVAL2')
   CRPIXX=dblarr(3)
   CRPIXY=dblarr(3)
   nxpix=intarr(3)
   nypix=intarr(3)
   pixscale=dblarr(3)

   ;REPLACED files with dir_data HERE
   tods = smap_read_and_filter(dir_data, mapparam, /PTRS, SUCCESS=success,$
      ERRMSG=errmsg);, /MEDFILT, /VERB )

   FOR i=0,2 DO BEGIN
      head=headfits(map_template[i],ext=1)
      CRPIXX[i]=sxpar(head,'CRPIX1')
      CRPIXY[i]=sxpar(head,'CRPIX2')
      NXPIX[i]=sxpar(head,'NAXIS1')
      NYPIX[i]=sxpar(head,'NAXIS2')
      PIXSCALE[i]=sxpar(head,'CD2_2')
   ENDFOR
   if field_num eq 8 then begin
      hi_head=headfits(dirsim+filename, ext=1)
      pixsize_degrees=sxpar(hi_head,'CDELT2')
   endif else begin
      hi_head=headfits(dirsim+filename)
      pixsize_degrees=sxpar(hi_head,'CD2_2')
   endelse
   skypix_side_x=sxpar(hi_head,'NAXIS1') 
   skypix_side_y=sxpar(hi_head,'NAXIS2')
   skypix_pixsize=pixsize_degrees*3600.
   crpix_x=sxpar(hi_head,'CRPIX1')
   crpix_y=sxpar(hi_head,'CRPIX2')

   mapstruct250=get_smap_mapstruct(npixx=skypix_side_x,npixy=skypix_side_y,band='PSW',/noerr,/noexp,/nomask)
   mapstruct350=get_smap_mapstruct(npixx=skypix_side_x,npixy=skypix_side_y,band='PMW',/noerr,/noexp,/nomask)
   mapstruct500=get_smap_mapstruct(npixx=skypix_side_x,npixy=skypix_side_y,band='PLW',/noerr,/noexp,/nomask)
   mapstruct500.astrometry.cd=identity(2)
   mapstruct500.astrometry.crval=[crvalx,crvaly]
   mapstruct500.astrometry.cdelt=[-1.*pixsize_degrees,pixsize_degrees]
   ;mapstruct500.astrometry.crpix=[skypix_side_x/2.,skypix_side_y/2.]
   mapstruct500.astrometry.crpix=[crpix_x,crpix_y]
   mapstruct250.astrometry=mapstruct500.astrometry
   mapstruct350.astrometry=mapstruct500.astrometry
   maps=ptrarr(3)
   maps[0]=ptr_new(temporary(mapstruct250))
   maps[1]=ptr_new(temporary(mapstruct350))
   maps[2]=ptr_new(temporary(mapstruct500))

   ; READ THE SKY SIMULATIONS AND MAKE NOISELESS MAPS
   mapfiles=strarr(3)
   for i=0,2 do $
      mapfiles[i]=datfilename

   exnamebasewnum=strcompress('_filtered'+inst_suffix+'_'+name[j]+'VC',/remove_all)
   SMAP_TRANSFUN_MAKEMAP,mapfiles,maps,tods,exnamebasewnum,mapparam,$
      dirsim=dirsim,CRVALX=crvalx, CRVALY=crvaly, $
      CRPIXX=crpixx, CRPIXY=crpixy, NXPIX=nxpix, NYPIX=nypix, $
      SKYPIX_SIDE_X=skypix_side_x, SKYPIX_SIDE_Y=skypix_side_y,PIXSCALE=pixscale, DIRWRITE=dirwrite,$
      halfs=halfs[field_num],angs=angs[field_num], $
      FIELD_NUM=field_num

endfor

end
