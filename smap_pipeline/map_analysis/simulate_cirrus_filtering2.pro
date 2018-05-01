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

field_num=7;7;10;7

components=[3,3,3,3,3,3,2,3,2,2,2]
name=['L','I','H']
j=0
noise=[0.,0.,0.]
fknee=[0.,0.,0.]

allmap_pixsize=0;24.;0.;50.

if allmap_pixsize eq 0 then $
   defparams = SMAP_GETDEFPARAMS(PIXSIZE=pixscale) $
   else $
   pixscale=[allmap_pixsize,allmap_pixsize,allmap_pixsize]/3600.

pix=pixscale*3600.

if allmap_pixsize ne 0 then allmap_suffix=strcompress('_'+STRING(allmap_pixsize,FORM='(D10.1)')+'_arcsec_pixels') $
   else allmap_suffix=''
exname = strcompress(STRING(pix,FORM='(D10.1)')+'_arcsec_pixels',/remove_all)
mapdir = strcompress(!SMAP_MAPS+field_ids[field_num]+'/'+STRING(pix,FORM='(D10.1)')+'_arcsec_pixels/',/remove_all)
; Script to Setup and Run Transfer Function Code 
; give the location of the L1 data we want to compare to
dir_data='/data/spiredaq/reprocessed/'+data_directories[field_num]+'/'  ;LockmanSw_L1e/'
; give the location of the map we want to compare too, be careful to be consistent with L1 data
map_template=mapdir+field_obsids[field_num]+'_itermap_'+exname+mapname+'.fits'
map_template=mapdir+field_ids[field_num]+'_itermap_'+exname+mapname+'.fits'
; first index of the simulated sky to run on

; extension name of the produced maps and of the Cls file
exnamebase=strcompress('_filtered_GASS_'+name[j]+'VC',/remove_all)
; directory where the simulated sky signal is located 
;dirsim='/data/viero/sim_transfunc/lockman-swire/'
dirsim='/data/viero/maps/HI/GASS/'
dirwrite=strcompress('/data/viero/sim_transfunc/HI/'+field_ids[field_num]+'/',/remove_all)

; location of the files containing the mask applied to the data and the correction matrix mkk (to be computed beforehand)
;maskfile='~/work/pk_hermes/locksw-mkkbis_50mJy_'+['500','350','250']+'mi.save'

mapfiles=strarr(3)
for i=0,2 do begin 
   mapfiles[i]=strcompress(field_ids[field_num]+'_GASS_'+name[j]+'VC_embedded.fits',/remove_all)
   head=headfits(dirsim+mapfiles[i])
   skypix_pixsize=sxpar(head,'CDELT2')*3600.
   skypix_side=sxpar(head,'NAXIS1')>sxpar(head,'NAXIS2')
endfor
;stop
maskfile=0.
test=SMAP_TRANSFUN_PIPELINE_CUSTOM_MAPS(dir_data,map_template,exnamebase,mapfiles, $
   dirsim=dirsim,dirsave=dirwrite,hi_cut=0.5,skypix_side=skypix_side, $
   skypix_pixsize=skypix_pixsize, $
   noise=noise,fknee=fknee, $
   dirwrite=dirwrite,pixscale=pixscale,field_num=field_num);,lmin=0)


end
