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

;field_nums=[18];egs-scuba
field_nums=[36];uds
field_nums=[17];egs-nest

its=100;85;1
it0=0;16
res=1.
side=10580.

writetods=0;0: just maps; 1: just tods; 2 both maps and tods

exname='_observed_clean_sky_hipe12'
dirsim='/data/viero/sim_transfunc/clean_skies/'

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

      simmapname=strcompress('clean_sky_'+$
	 string(res, format='(i10)')+'x'+$
	 string(side, format='(i10)')+'x'+$
	 string(side, format='(i10)')+'_'+$
	 string(it, format='(i10)')+$
	 '.fits',/remove_all)

      exnamebase=strcompress('_'+field_ids[field_num]+exname+'_'+string(it, format='(i10)'),/remove_all)
      itest=SMAP_TRANSFUN_PIPELINE_V6(conffile, $
	 dirsim,simmapname,$
	 map_template=templatemap,$
	 EXNAMEBASE=exnamebase, $
	 NOISE=noise,FKNEE=fknee,$
	 WRITETODS=writetods,$
	 SAVEMAPDIR=dirsim)
   endfor
endfor
end

