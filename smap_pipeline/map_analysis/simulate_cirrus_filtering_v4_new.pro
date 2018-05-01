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

field_num=6;10;7;2

gass=[1,1,1,1,1,0,1,1,0,1,1]
components=[3,3,3,3,3,3,2,2,2,2,1]
components=[1,1,1,1,1,1,1,1,1,1,1]
halfs=[0,0,0,0,0,1,0,1,0,1,1]
angs= [1,0,1,0,0,0,1,0,1,1,0]
name=['L','H','I']

allmap_pixsize=0;24.;0.;50.
if allmap_pixsize eq 0 then $
   defparams = SMAP_GETDEFPARAMS(PIXSIZE=pixscale) $
   else $
   pixscale=[allmap_pixsize,allmap_pixsize,allmap_pixsize]/3600.
if gass[field_num] eq 1 then inst_suffix='_GASS' else inst_suffix='_GBT' 

pix=pixscale*3600.

if allmap_pixsize ne 0 then allmap_suffix=strcompress('_'+STRING(allmap_pixsize,FORM='(D10.1)')+'_arcsec_pixels') $
   else allmap_suffix=''
exname = strcompress(STRING(pix,FORM='(D10.1)')+'_arcsec_pixels',/remove_all)
mapdir = strcompress(!SMAP_MAPS+field_ids[field_num]+'/'+STRING(pix,FORM='(D10.1)')+'_arcsec_pixels/',/remove_all)
dir_data='/data/spiredaq/reprocessed/'+data_directories[field_num]+'/'  
map_template=mapdir+field_ids[field_num]+'_v4_itermap_'+exname+mapname+'.fits'

; directory where the map to "observe" is located 
if gass[field_num] eq 1 then dirsim='/data/viero/maps/HI/GASS/' else dirsim='/data/viero/maps/HI/GBT/'
dirwrite=strcompress('/data/viero/sim_transfunc/HI/'+field_ids[field_num]+'/',/remove_all)

for j=0, components[field_num]-1 do begin
   filename=strcompress(field_ids[field_num]+inst_suffix+'_'+$
      name[j]+'VC_CD',/remove_all)

   exnamebase=strcompress('_filtered'+inst_suffix+'_'+name[j]+'VC',/remove_all)
   use_maps=1
   test=SMAP_TRANSFUN_PIPELINE_V4(map_template,field_ids[field_num], $
      exnamebase=exnamebase, $
      use_maps=use_maps, dirmap=dirsim,mapname=filename);,$
   ;   SAVEMAPDIR=savemapdir)

endfor

end
