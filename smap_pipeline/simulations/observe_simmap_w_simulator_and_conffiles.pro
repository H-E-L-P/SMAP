mapname=['_PSW','_PMW','_PLW']
pname=['PSW','PMW','PLW']
wavelength=[250,350,500]

its=50;1;0.;00.;5.;0.
res=1.;4;16;9;10.;1.*8.;12.;6.0
side=2.*4096;2048;10650;4500.;10580;2.^11.
bias=0.;1.0;1.0
hi_cut=0.5
jy_beam=0
wv0=3;which index 250 starts
wv1=0
wv2=2

field_name='uds'
exname='_stacking_tests'
DIRSIM='/data/viero/testdir/'
DIRSPIRE='/data/spire/maps/'+field_name+'/current/'

noise_on=1.
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

conffile='/home/viero/smaproot/smap_pipeline/map_making/createmap/custom_conffiles/uds_stacking.conf'
for it=0,its-1 do begin
   templatemap=strcompress('/data/viero/maps/hermes/uds/3.0_arcsec_pixels/'+$
      'uds_v4_itermap_3.0_arcsec_pixels'+mapname[0:2]+'.fits',/remove_all)
   for wv=wv1,wv2 do begin
      spiremap=clean_nans(readfits(templatemap[wv],hd,ext=1))
      cat_filename=strcompress(dirsim+'source_filled_sky_'+$
	 clust_suffix+mask_suffix+'_'+$
	 string(res, format='(i10)')+'x'+$
	 string(side, format='(i10)')+'x'+$
	 string(side, format='(i10)')+'_catalog_'+$
	 string(it, format='(i10)')+$
	 exname+exnameplus+'.sav',/remove_all)
      restore, cat_filename
      x=X_Y_FLUX[*,0];[wv,0,*]
      y=X_Y_FLUX[*,1];wv,1,*]
      s=X_Y_FLUX[*,2+wv0+wv];wv,2,*]
      ;stop

      mapstruct=get_smap_mapstruct(npixx=side,npixy=side,band=pname[wv],/noerr,/noexp,/nomask)
      mapstruct.astrometry.cd=identity(2)
      mapstruct.astrometry.crval=[sxpar(hd,'CRVAL1'),sxpar(hd,'CRVAL2')]
      mapstruct.astrometry.cdelt=[-1.*res/3600.,res/3600.]
      mapstruct.astrometry.crpix=[side/2.,side/2.]
      mkhdr, shd, fltarr(side,side) 
      putast, shd,mapstruct.astrometry
      save,filename=strcompress(dirsim+field_name+$
	 '_header_file_'+string(it, format='(i10)')+$
	 exname+exnameplus+'.sav',/remove_all), shd

      xyad,shd,x,y,ra,dec
      adxy,hd,ra,dec,x2,y2

      ind_above=where(spiremap[x2,y2] ne 0,nfx)

      cat_ra_dec_x_y_flux=[transpose(ra[ind_above]),transpose(dec[ind_above]),$
	 transpose(x[ind_above]),transpose(y[ind_above]),transpose(s[ind_above])]
      out_cat_filename=strcompress(dirsim+field_name+$
	 '_catalog_'+clust_suffix+mask_suffix+'_'+$
	 string(it, format='(i10)')+$
	 mapname[wv]+exname+exnameplus+'.sav',/remove_all)
      save, filename=out_cat_filename, cat_ra_dec_x_y_flux
      ;stop
   endfor

   simmapname=strcompress('source_filled_sky_'+$
      clust_suffix+mask_suffix+'_'+$
      string(res, format='(i10)')+'x'+$
      string(side, format='(i10)')+'x'+$
      string(side, format='(i10)')+'_'+$
      string(it, format='(i10)')+'_'+$
      ;noise_suffix+$
      string(wavelength[0:2],format='(i10)')+$
      exname+exnameplus,/remove_all)

   exnamebase=strcompress(exname+exnameplus+'_'+clust_suffix+mask_suffix+$
      noise_suffix+'_'+string(it, format='(i10)'),/remove_all)
      ;stop
   itest=SMAP_TRANSFUN_PIPELINE_V5(templatemap, field_name,EXNAMEBASE=exnamebase, $
      NOISE=noise,FKNEE=fknee,$
      DIRMAP=dirsim, MAPNAME=simmapname, $
      CONFFILE=conffile,$
      USEFITS=1,$
      SAVEMAPDIR=dirsim)

endfor
end

