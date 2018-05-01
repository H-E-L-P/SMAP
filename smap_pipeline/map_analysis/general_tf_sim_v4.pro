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
;4 lockman-swire3
;5 lockman-swire
;6 sep
;7 xmm-lss
;8 fls
;9 cosmos
;10 cdfs-swire

field_num=2;4
act_on=0

use_sims=1
use_maps=0
iterdiag=2
jk_only=0
two_halo=1
clean_skies=2
allmap_pixsize=0;24.;0.;50.
; first and last index of the simulated sky to run on
startnum=50;21;44;1;64;56;0;78;28;77;0;27;0
endnum=51;24;50;99;39;2

if clean_skies ne 0 then cs_suffix='_cs' else cs_suffix=''
if clean_skies eq 2 then cs_suffix='_test' 
if two_halo ne 0 then th_suffix='_2H' else th_suffix=''
;fknee=[0.048,0.0414,0.0267]
;fknee=[0.,0.,0.];[0.005,0.005,0.005]
fknee=[0.005,0.005,0.005]
;fknee=[0.04,0.04,0.04]
noise=[4913503.6,3236836.4, 2219022.1]/[1.1619,1.5916,1.46926];Jy/sr/sqrt(hits);=[0.04313,0.04997,0.07763];Jy/beam/sqrt(hits)
if clean_skies eq 1 then noise=[0.,0.,0.]
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

exname = strcompress(STRING(pix,FORM='(D10.1)')+'_arcsec_pixels',/remove_all)
dirtemplate = strcompress(!SMAP_MAPS+field_ids[field_num]+'/'+STRING(pix,FORM='(D10.1)')+'_arcsec_pixels/',/remove_all)
map_template=dirtemplate+field_ids[field_num]+'_v4_itermap_'+exname+mapname+'.fits'

skypix_pixsize=2.0
skypix_side=10580.  
; extension name of the produced maps and of the Cls file
exnamebase=strcompress(act_prefix+'_'+string(skypix_pixsize,format='(i10)')+'x'+ $
   string(skypix_side,format='(i10)')+'x'+string(skypix_side,format='(i10)')+$
   cs_suffix+$
   allmap_suffix+$
   fknee_suffix+noise_suffix+th_suffix $
   +'_v4' $
   ,/remove_all)
if clean_skies ne 0 then $
dirsim='/data/viero/sim_transfunc/clean_skies/' $
else $
dirsim='/data/viero/sim_transfunc/source_filled_skies/' 

savemapdir=strcompress('/data/viero/sim_transfunc/'+field_ids[field_num]+'/',/remove_all)
test=SMAP_TRANSFUN_PIPELINE_V4(map_template,field_ids[field_num], $ 
   startnum=startnum,endnum=endnum,exnamebase=exnamebase, $
   use_sims=use_sims, dirsim=dirsim,skypix_side=skypix_side,skypix_pixsize=skypix_pixsize,$ 
   hi_cut=0.5,clean_skies=clean_skies, two_halo=two_halo, $
   noise=noise, fknee=fknee);, SAVEMAPDIR=savemapdir) ;$
;   jk_only=jk_only, halfs=halfs,angs=angs, $

end
