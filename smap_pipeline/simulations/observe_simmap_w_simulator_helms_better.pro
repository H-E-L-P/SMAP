mapname=['_PSW','_PMW','_PLW']
pname=['PSW','PMW','PLW']
wavelength=[250,350,500]
date='20120730'

its=10.;25.;5.;98
it0=0.;75.;20.;50;7;1
res=10.
side=11500;8000.
bias=1.;1.0;1.0
hi_cut=0.10 ; 100mJy
jy_beam=0
wv1=2
wv2=2

helms_or_hers=0; 0=helms, 1=hers 

if helms_or_hers then begin
   print,'reading in HeRS'
   writetods=1;0: just maps; 1: just tods; 2 both maps and tods
   field_name='hers'
   exname='_hers_5band'
   date='20120927'
   mapdir='/data/viero/maps/hermes/hers/'
;   mapfile=strcompress(mapdir+$
;      'hers_itermap_'+date+'_'+mapname+'.fits',/remove_all)
endif else begin
   print,'reading in HeLMS'
   writetods=1;0: just maps; 1: just tods; 2 both maps and tods
   field_name='helms'
   exname='_helms_5band'
   date='20120730'
   mapdir='/data/spire/maps/helms/'+date+'/'
;   mapfile=strcompress(mapdir+$
;      'helms_itermap_'+date+'_'+mapname+'.fits',/remove_all)
endelse

;dirsim='/data/viero/testdir/'
dirsim='/data/viero/sim_transfunc/'
cleanskymapdir='/data/viero/sim_transfunc/clean_skies/'
conffile_dir='/home/viero/smaproot/smap_pipeline/map_making/createmap/conffiles/'

clean_or_source_sky=0;1

fknee=[0.005,0.005,0.005]
if keyword_set(clean_or_source_sky) then begin
   noise_on=1.
   skymapdir=DIRSIM
   suf='source_filled_sky_'
   ;TEMP> THIS NEEDS TO BE THREE MAPS!
   ;conffile=conffile_dir+'helms_tf.conf'
endif else begin
   noise_on=0.
   skymapdir=cleanskymapdir
   suf='clean_sky_'
   conffile=conffile_dir+field_name+'_tf.conf'
   templatemap=$;strcompress('/data/spire/maps/'+field_name+'/'+$
      ;strcompress(string(date,format='(i10)'),/remove_all)+'/'+$
      mapdir+$
      strcompress($
      field_name+'_itermap_'+date+mapname[2]+'.fits',/remove_all)
;   ;TEMPORARY FLS TEST
;   date='20111129'
;   conffile=conffile_dir+'fls_test.conf'
;   templatemap=strcompress('/data/spire/maps/fls/current/'+$
;      'fls_itermap_'+date+mapname[0:2]+'.fits',/remove_all)
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
      '_helms_5band'+$
      ;exname+$
      '.fits',/remove_all)
   ;print, file_test(skymapdir+simmapname)
   ;print, skymapdir+simmapname
   ;stop

   exnamebase=strcompress(exname+exnameplus+'_'+clust_suffix+mask_suffix+$
      noise_suffix+fknee_suffix+'_'+string(it, format='(i10)'),/remove_all)
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

