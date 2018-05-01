mapname=['_PSW','_PMW','_PLW']
pname=['PSW','PMW','PLW']
wavelength=[250,350,500]

its=1.;00;00;;0.;00.;5.;0.
it0=0;99;0;14;9;8
res=1.0;10.;4;16;9;10.;1.*8.;12.;6.0
side=21200.
bias=1.

field_name='uds_test'
exname='_test_pix_window'
v_name='_v3'
dirsim='/data/viero/testdir/'
;DIRSPIRE='/data/spire/maps/'+field_name+'/current/'
cleanskymapdir='/data/viero/sim_transfunc/clean_skies/'
conffile_dir='/home/viero/smaproot/smap_pipeline/map_making/createmap/conffiles/'

clean_or_source_sky=0
writetods=0;0: just maps; 1: just tods; 2 both maps and tods

fknee=[0.005,0.005,0.005]
fknee*=0.0
if keyword_set(clean_or_source_sky) then begin
   noise_on=1.
   hi_cut=0.10 ; 100mJy
   skymapdir='/data/viero/sim_transfunc/source_filled_skies/'+field_name+'/'
   suf='source_filled_sky_'
   conffile=conffile_dir+'uds_30arcsec_pix.conf'
   ;TEMP
   ;conffile=conffile_dir+'spt_zea_tf.conf'
endif else begin
   noise_on=0.
   ;hi_cut=-1 ; mask_suffix='' 
   hi_cut=1.0 ; 1000mJy
   skymapdir=cleanskymapdir
   suf='clean_sky_'
   ;conffile=conffile_dir+'spt_zea_tf.conf'
   conffile=conffile_dir+'uds_30arcsec_pix.conf'
   ;TEMPORARY!!
   ;conffile=conffile_dir+'spt_zea_testonemap.conf'
endelse

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

for it=it0,its+it0-1 do begin
   templatemap=strcompress('/data/viero/maps/hermes/'+$
      'uds_itermap_30_arcsec_pixels'+mapname[0:2]+'.fits',/remove_all)
   if keyword_set(clean_or_source_sky) then $
      wave_suf='_'+string(wavelength,format='(i10)') $
      else $
      wave_suf='' 
   simmapname=strcompress(suf+$
      clust_suffix+mask_suffix+'_'+$
      string(res, format='(i10)')+'x'+$
      string(side, format='(i10)')+'x'+$
      string(side, format='(i10)')+'_'+$
      string(it, format='(i10)')+$
      wave_suf+$
      exname+'.fits',/remove_all)
   ;print, file_test(skymapdir+simmapname)
   ;print, skymapdir+simmapname
   ;stop

   exnamebase=strcompress(exname+exnameplus+'_'+clust_suffix+mask_suffix+$
      noise_suffix+fknee_suffix+v_name+'_'+string(it, format='(i10)'),/remove_all)
   ;stop
   itest=SMAP_TRANSFUN_PIPELINE_V6(conffile, $
      skymapdir, simmapname, $
      map_template=templatemap,$
      EXNAMEBASE=exnamebase, $
      NOISE=noise,FKNEE=fknee,$
      WRITETODS=writetods,$
      SAVEMAPDIR=dirsim)

endfor
end

