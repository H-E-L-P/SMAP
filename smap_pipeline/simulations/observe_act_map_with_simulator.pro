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

;MUST EDIT EVERYTHING IN THIS BOX TO YOUR NEEDS                  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;CHOOSE FROM ABOVE LIST                                          ;
;LSS 14 FIELDS = [7,21,29,9,38]
field_num=6;21;7;12                                              ;
                                                                 ;
;if you want to do 250,350,500, then wv1=0, wv2=2                ;
wv1=0							         ;
wv2=2							         ;
							         ;
;DIRECTORY WHERE THE MAP YOU WANT TO "OBSERVE" IS                ;
dir_map_to_observe='/data/viero/maps/ACT/CEA/'
;THIS STRING SHOULD BE A 1X3 STRING ARRAY FOR 250,350,500        ;
freqs=[220,148]
for freq=0,1 do begin
for set=3,3 do begin

   filemapname=strcompress('ACT_'+$
      string(freqs[freq],format='(i10)')+$
      '_south'+$
      '_4way'+$
      '_season_4'+$   
      '_set_'+string(set,format='(i10)')+$
      '_map'+$
      '_TAN'$
      ,/remove_all)
;IF YOU WANT NOISE IN THE MAP                                    ;
   ;-NOTE, CAN'T HAVE 1/F IF NOISE_ON=0			         ;
noise_on=0.;1.                                                   ;
one_over_f_on=1.                                                 ;
                                                                 ;
print, file_test(dir_map_to_observe+filemapname+'.fits')
; stop
;WHAT TO APPEND TO THE OUTPUT MAPS                               ;
custom_exname=strcompress($
   'act_'+string(freqs[freq],format='(i10)')+$
   '_season_4'+$
   '_set_'+string(set,format='(i10)')+$
   '_map'+$
   '_TAN'$
   ,/remove_all)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DIRSPIRE='/data/spire/maps/'+field_ids[field_num]+'/current/'

fknee=one_over_f_on*[0.005,0.005,0.005]
noise=1e-6*noise_on*[4913503.6,3236836.4, 2219022.1]/[1.1619,1.5916,1.46926]
if fknee[0] ne 0 and noise[0] ne 0 then fknee_suffix='_w_1_over_f' else fknee_suffix=''
if noise[0] eq 0 then noise_suffix='_noiseless' else noise_suffix='_w_noise'
if noise[0] ne 0 then noise_suffix='_w_noise' else noise_suffix=''

templatemap=strcompress(dirspire+field_stamps[field_num]+mapname[0:2]+'.fits',/remove_all)
dir_adfs='/data/viero/maps/hermes/adfs/12.0_arcsec_pixels/'
templatemap=strcompress(dir_adfs+'adfs_v4_itermap_12.0_arcsec_pixels'+mapname[0:2]+'.fits',/remove_all)
exnamebase=strcompress(custom_exname+'_simulated_observation'+noise_suffix,/remove_all)
   
;templatemap=strcompress('/data/viero/maps/hermes/adfs/12.0_arcsec_pixels/'+$
;   field_ids[field_num]+'_v4_itermap_12.0_arcsec_pixels'+mapname[0:2]+'.fits',/remove_all)
;exnamebase=strcompress(custom_exname+'_simulated_act2_observation'+noise_suffix,/remove_all)
;stop
itest=SMAP_TRANSFUN_PIPELINE_V4(templatemap, field_ids[field_num],EXNAMEBASE=exnamebase, $
   NOISE=noise,FKNEE=fknee,$
   USE_MAPS=1, DIRMAP=dir_map_to_observe, MAPNAME=filemapname, $
   ;CONFFILE=conffile,$
   USEFITS=1);,$
   ;SAVEMAPDIR=dirsim) ;SAVEMAPDIR DOES NOT WORK YET, I'VE STRUGGLED TO FIGURE OUT HOW TO IMPLIMENT IT IN CREATE_ITERMAP

endfor
endfor
end

