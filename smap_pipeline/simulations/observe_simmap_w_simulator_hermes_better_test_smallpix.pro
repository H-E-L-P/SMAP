INIT_SIMS

COMMON SIMS_PARAMS, $
   wavelength, $
   mapname, $
   fwhm, $
   pixsize, $
   field_stamps, $
   field_ids

;0) abell0370
;1) abell1689
;2) abell1835
;3) abell2218
;4) abell2219
;5) abell2390
;6) adfs
;7) bootes
;8) cdfs-nest
;9) cdfs-swire
;10) cdfs-swire3
;11) cl0024
;12) cosmos
;13) cosmos2
;14) cosmos-nest
;15) ecdfs
;16) egroth
;17) egs-nest
;18) egs-scuba
;19) elais-n1
;20) elais-n2 
;21) elais-s1
;22) fls
;23) goodsn
;24) goodss
;25) lockman-east
;26) lockman-nest
;27) lockman-north
;28) lockman-swire
;29) lockman-swire3
;30) ms0451
;31) ms1054
;32) ms1358
;33) rxj0512
;34) rxj1347
;35) s1-video
;36) uds
;37) vvds
;38) xmm-lss
;39) xmm-nest

pname=['PSW','PMW','PLW']

;field_num=12.;7;21;36;9;12;21;7;3;29;36;12;0;21;9;22;0;21;9
field_nums=[6,7,8,13,14,15,16,19,20,21,22,23,24,28,29,36,37,38,39]
field_nums=[15,16,19,20,21,22,23,24,28,29,36,37,38,39]
field_nums=[0,1,2,3,4,5,6,30,31,32,33,34,35]
field_nums=[9,35]
;field_nums=[22,36,21,28,9,35]
;field_nums=[29];lockman-swire3
field_nums=[38];xmm-lss
;field_nums=[12];cosmos
;field_nums=[9];cdfs-swire
;field_nums=[28];lockman-swire
;field_nums=[6];adfs
;field_nums=[7];bootes
;field_nums=[36];uds
;field_nums=[21];elais-s1
;field_nums=[22];fls
;field_nums=[24];goods-s
field_nums=[19];elais-n1

its=1
it0=0
res=2.
res=1.
side=10600.
side=21200.
bias=1.;1.0;1.0
hi_cut=1.0
jy_beam=0
mcs=10.

writetods=0;0: just maps; 1: just tods; 2 both maps and tods

noise_on=0.
if keyword_set(noise_on) then noise = -1 else noise=0
fknee=[0.005,0.005,0.005]
;noise=noise_on*[4913503.6,3236836.4, 2219022.1]/[1.1619,1.5916,1.46926]
if fknee[0] ne 0 and  keyword_set(noise_on) then fknee_suffix='_w_1_over_f' else fknee_suffix=''
if noise[0] ne 0 then noise_suffix='_w_noise' else noise_suffix=''
if keyword_set(noise_on) then noise_suffix='_w_noise' else noise_suffix=''
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

exname='_hermes_counts_sims'
exname='_test_pix_window'
dirsim='/data/viero/hermes_simmaps/'
wv1=0
wv2=2

nfields=n_elements(field_nums)
for nf=0,nfields-1 do begin
   field_num=field_nums[nf]
   DIRSPIRE='/data/spire/maps/'+field_ids[field_num]+'/current/'
   conffile='/home/viero/smaproot/smap_pipeline/map_making/createmap/conffiles/'+$
      field_ids[field_num]+'.conf'
   ;print, file_test(conffile), conffile
   ;stop
   templatemap=strcompress(dirspire+field_stamps[field_num]+mapname[0:2]+'.fits',/remove_all)
   for it=it0,its+it0-1 do begin

      filled_sky_dir=dirsim
      clean_sky_dir='/data/viero/sim_transfunc/clean_skies/'
      simmapname=strcompress('clean_sky_'+$
	 clust_suffix+mask_suffix+'_'+$
	 string(res, format='(i10)')+'x'+$
	 string(side, format='(i10)')+'x'+$
	 string(side, format='(i10)')+'_'+$
	 string(it, format='(i10)')+$
	 ;string(wavelength,format='(i10)')+$
	 exname+exnameplus+'.fits',/remove_all)
      stop

      exnamebase=strcompress(exname+exnameplus+'_'+clust_suffix+mask_suffix+$
	 noise_suffix+'_'+string(it, format='(i10)'),/remove_all)
      itest=SMAP_TRANSFUN_PIPELINE_V6(conffile, $
	 clean_sky_dir,simmapname,$
	 map_template=templatemap,$
	 EXNAMEBASE=exnamebase, $
	 NOISE=noise,FKNEE=fknee,$
	 WRITETODS=writetods,$
	 SAVEMAPDIR=dirsim)
   endfor
endfor
end

