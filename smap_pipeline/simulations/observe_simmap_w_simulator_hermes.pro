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

its=1
it0=0
res=2.
side=10600.
bias=1.;1.0;1.0
hi_cut=1.0
jy_beam=1
mcs=10.

noise_on=1.
if keyword_set(noise_on) then noise = -1
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
DIRSIM='/data/viero/hermes_simmaps/'
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
      tit=floor(randomu(seed)*mcs)
      for wv=wv1,wv2 do begin
	 spiremap=clean_nans(readfits(templatemap[wv],hd,ext=1))
	 cat_filename=strcompress(dirsim+'source_filled_sky_'+$
	    clust_suffix+mask_suffix+'_'+$
	    string(res, format='(i10)')+'x'+$
	    string(side, format='(i10)')+'x'+$
	    string(side, format='(i10)')+'_catalog_'+$
	    string(tit, format='(i10)')+$
	    exname+exnameplus+'.sav',/remove_all)
	 restore, cat_filename
	 x=X_Y_FLUX[*,0]
	 y=X_Y_FLUX[*,1]
	 s=X_Y_FLUX[*,2+wv]

	 mapstruct=get_smap_mapstruct(npixx=side,npixy=side,band=pname[wv],/noerr,/noexp,/nomask)
	 mapstruct.astrometry.cd=identity(2)
	 mapstruct.astrometry.crval=[sxpar(hd,'CRVAL1'),sxpar(hd,'CRVAL2')]
	 mapstruct.astrometry.cdelt=[-1.*res/3600.,res/3600.]
	 mapstruct.astrometry.crpix=[side/2.,side/2.]
	 mkhdr, shd, fltarr(side,side)
	 putast, shd,mapstruct.astrometry
	 save,filename=strcompress(dirsim+field_ids[field_num]+$
	    '_header_file_'+string(it, format='(i10)')+'.sav',/remove_all), shd

	 xyad,shd,x,y,ra,dec
	 adxy,hd,ra,dec,x2,y2

	 ind_above=where(spiremap[x2,y2] ne 0,nfx)

	 cat_ra_dec_x_y_flux=[transpose(ra[ind_above]),transpose(dec[ind_above]),$
	    transpose(x[ind_above]),transpose(y[ind_above]),transpose(s[ind_above])]
	 out_cat_filename=strcompress(dirsim+field_ids[field_num]+$
	    '_catalog_'+clust_suffix+mask_suffix+'_'+$
	    string(it, format='(i10)')+$
	    mapname[wv]+'.sav',/remove_all)
	 save, filename=out_cat_filename, cat_ra_dec_x_y_flux
      endfor
      ;;;;;;;;;;;;;;;;;;;;;;;
      ;;;;;;;;;;;;;;;;;;;;;;;

      filled_sky_dir=dirsim
      simmapname=strcompress('source_filled_sky_'+$
	 clust_suffix+mask_suffix+'_'+$
	 string(res, format='(i10)')+'x'+$
	 string(side, format='(i10)')+'x'+$
	 string(side, format='(i10)')+'_'+$
	 string(tit, format='(i10)')+'_'+$
	 string(wavelength,format='(i10)')+$
	 exname+exnameplus,/remove_all)

      exnamebase=strcompress(exname+exnameplus+'_'+clust_suffix+mask_suffix+$
	 noise_suffix+'_'+string(it, format='(i10)'),/remove_all)
      itest=SMAP_TRANSFUN_PIPELINE_V5(templatemap, field_ids[field_num],EXNAMEBASE=exnamebase, $
	 NOISE=noise,FKNEE=fknee,$
	 DIRMAP=filled_sky_dir, MAPNAME=simmapname, $
	 CONFFILE=conffile,$
	 USEFITS=1,$
	 SAVEMAPDIR=dirsim)
   endfor
endfor
end

