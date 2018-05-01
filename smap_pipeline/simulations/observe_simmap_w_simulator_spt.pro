mapname=['_PSW','_PMW','_PLW']
pname=['PSW','PMW','PLW']
wavelength=[250,350,500]

its=1;24;40;3;42;16;50;74;1;0.;00.;5.;0.
it0=99;76;47;57;33;50;25;9;8
res=10.;4;16;9;10.;1.*8.;12.;6.0
side=5000.;2.*4096;2048;10650;4500.;10580;2.^11.
bias=1.;1.0;1.0
hi_cut=1.0;0.5
;jy_beam=0
wv1=0
wv2=2

field_name='spt'
exname='_spt_6band'
DIRSIM='/data/viero/testdir/'
;DIRSPIRE='/data/spire/maps/'+field_name+'/current/'

noise_on=0.
fknee=[0.005,0.005,0.005]
noise=noise_on*[4913503.6,3236836.4, 2219022.1]/[1.1619,1.5916,1.46926]
if fknee[0] ne 0 and noise[0] ne 0 then fknee_suffix='_w_1_over_f' else fknee_suffix=''
if noise[0] ne 0 then noise_suffix='_w_noise' else noise_suffix=''
if bias eq 0 then $
   clust_suffix='random' else $
   clust_suffix='clustering_bias_'+string(bias,format='(d10.1)')
if hi_cut eq -1 then mask_suffix='' $
   else if hi_cut eq 0 then mask_suffix='_extended_sources_only' $
   else $
   mask_suffix=strcompress('_cut_'+ $
   string(1000.*hi_cut,format='(i10)')+'mJy',/remove_all)
if keyword_set(jy_beam) then $
   exnameplus='_jy_beam' $
   else $
   exnameplus=''

conffile='/home/viero/smaproot/smap_pipeline/map_making/createmap/conffiles/spt_zea_jks.conf'
conffile='/home/viero/smaproot/smap_pipeline/map_making/createmap/conffiles/spt_zea_tf.conf'
for it=it0,its+it0-1 do begin
   templatemap=strcompress('/data/viero/maps/hermes/'+$
      'spt_zea_itermap_10_iterations'+mapname[0:2]+'.fits',/remove_all)

   cleanskymapdir='/data/viero/sim_transfunc/clean_skies/'
   simmapname=strcompress('clean_sky_'+$
      clust_suffix+mask_suffix+'_'+$
      string(res, format='(i10)')+'x'+$
      string(side, format='(i10)')+'x'+$
      string(side, format='(i10)')+'_'+$
      string(it, format='(i10)')+$
      exname,/remove_all)
   ;print, file_test(cleanskymapdir+simmapname+'.fits')
   ;print, cleanskymapdir+simmapname+'.fits'
   ;stop

   exnamebase=strcompress(exname+exnameplus+'_'+clust_suffix+mask_suffix+$
      noise_suffix+'_'+string(it, format='(i10)'),/remove_all)
      ;stop
   itest=SMAP_TRANSFUN_PIPELINE_V5(templatemap, field_name,EXNAMEBASE=exnamebase, $
      NOISE=noise,FKNEE=fknee,$
      DIRMAP=cleanskymapdir, MAPNAME=simmapname, $
      CONFFILE=conffile,$
      USEFITS=1,$
      SAVEMAPDIR=dirsim)

endfor
end

