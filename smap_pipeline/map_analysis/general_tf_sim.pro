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

field_num=8;0;7;10;7
halfs=0
angs=0;leave these two 0, they are taken care of later
jk_only=0
two_halo=1
clean_skies=1
allmap_pixsize=0;24.;0.;50.
; first and last index of the simulated sky to run on
startnum=2;64;56;0;78;28;77;0;27;0
endnum=2;99;39;2

if clean_skies ne 0 then cs_suffix='_cs' else cs_suffix=''
if two_halo ne 0 then th_suffix='_2H' else th_suffix=''
;fknee=[0.048,0.0414,0.0267]
;fknee=[0.,0.,0.];[0.005,0.005,0.005]
fknee=[0.005,0.005,0.005]
;fknee=[0.04,0.04,0.04]
noise=[4913503.6,3236836.4, 2219022.1]/[1.1619,1.5916,1.46926];Jy/sr/sqrt(hits);=[0.04313,0.04997,0.07763];Jy/beam/sqrt(hits)
if clean_skies eq 1 then noise=[0.,0.,0.]
act_on=0
if field_num eq 6 and act_on eq 1 then $
   act_prefix='_act' else act_prefix=''

if allmap_pixsize eq 0 then $
   defparams = SMAP_GETDEFPARAMS(PIXSIZE=pixscale) $
   else $
   pixscale=[allmap_pixsize,allmap_pixsize,allmap_pixsize]/3600.

pix=pixscale*3600.

if allmap_pixsize ne 0 then allmap_suffix=strcompress('_'+STRING(allmap_pixsize,FORM='(D10.1)')+'_arcsec_pixels') $
   else allmap_suffix=''
if fknee[0] ne 0 and noise[0] ne 0 then fknee_suffix='_w_1_over_f' else fknee_suffix=''
if noise[0] ne 0 then noise_suffix='_wnoise' else noise_suffix=''
;exname = field_ids[field_num]+'_'+strcompress(STRING(pix,FORM='(D10.1)')+'_arcsec_pixels',/remove_all)
exname = strcompress(STRING(pix,FORM='(D10.1)')+'_arcsec_pixels',/remove_all)
mapdir = strcompress(!SMAP_MAPS+field_ids[field_num]+'/'+STRING(pix,FORM='(D10.1)')+'_arcsec_pixels/',/remove_all)
; Script to Setup and Run Transfer Function Code 
; give the location of the L1 data we want to compare to
dir_data='/data/spiredaq/reprocessed/'+data_directories[field_num]+'/'  ;LockmanSw_L1e/'
; give the location of the map we want to compare too, be careful to be consistent with L1 data
map_template=mapdir+field_obsids[field_num]+'_itermap_'+exname+mapname+'.fits'
map_template=mapdir+field_ids[field_num]+'_itermap_'+exname+mapname+'.fits'
; first index of the simulated sky to run on

skypix_pixsize=2.0
;if field_num eq 5 or field_num eq 6 or field_num eq 7 then $
;skypix_side=10580. $ 
;else $
;skypix_side=2.^(13.)
skypix_side=10580.  
; extension name of the produced maps and of the Cls file
exnamebase=strcompress(act_prefix+'_'+string(skypix_pixsize,format='(i10)')+'x'+ $
   string(skypix_side,format='(i10)')+'x'+string(skypix_side,format='(i10)')+$
   cs_suffix+$
   allmap_suffix+$
   fknee_suffix+noise_suffix+th_suffix $
   ,/remove_all)
; directory where the simulated sky signal is located 
;dirsim='/data/viero/sim_transfunc/lockman-swire/'
if clean_skies eq 1 then $
dirsim='/data/viero/sim_transfunc/clean_skies/' $
else $
dirsim='/data/viero/sim_transfunc/source_filled_skies/' 

; location of the files containing the mask applied to the data and the correction matrix mkk (to be computed beforehand)
;maskfile='~/work/pk_hermes/locksw-mkkbis_50mJy_'+['500','350','250']+'mi.save'
cut_suffix='_cut_100mJy'
deltal=80.
lmin=0.
dl_suffix=strcompress('_dl_'+string(deltal, format='(i20)') )
if keyword_set(lmin) then $
   lmin_suffix=strcompress('_lmin_'+string(lmin, format='(i10)') ) $
   else lmin_suffix=''
maskfile=strcompress('/data/viero/mkk/'+field_ids[field_num]+cut_suffix+'_'+['500X500','350X350','250X250']+$
   dl_suffix+lmin_suffix+'_'+STRING(reverse(pix),FORM='(D10.1)')+'_arcsec_pixels_mode_coupling_matrix.sav',/remove_all)
outputfiles='/data/viero/tf/'
dirwrite=strcompress('/data/viero/sim_transfunc/'+field_ids[field_num]+'/',/remove_all)

stop
test=SMAP_TRANSFUN_PIPELINE(dir_data,map_template,startnum,endnum,exnamebase,maskfile, $
   deltal=deltal,dirsim=dirsim,dirsave=outputfiles,hi_cut=0.5,skypix_side=skypix_side, $
   skypix_pixsize=skypix_pixsize,clean_skies=clean_skies, two_halo=two_halo, $
   noise=noise, fknee=fknee, $
   jk_only=jk_only, halfs=halfs,angs=angs, $
   dirwrite=dirwrite,pixscale=pixscale,field_num=field_num);,lmin=0)


end
