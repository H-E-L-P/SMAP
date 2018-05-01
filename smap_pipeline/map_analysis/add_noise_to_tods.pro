;FUNCTION add_noise_to_tods, tods, white_level, correlated_noise=correlated_noise

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

field_num=6

dir_data='/data/spiredaq/reprocessed/'+data_directories[field_num]+'/' 

tods = smap_read_and_filter(dir_data, mapparam, /PTRS, SUCCESS=success,$
   ERRMSG=errmsg, /MEDFILT, /VERB )

;tods = smap_readtod(dir_data, SUCCESS=success,$
;   ERRMSG=errmsg)

end
