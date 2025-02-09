PRO INIT_SIMS

COMMON SIMS_PARAMS, $
	wavelength, $
	mapname, $
	fwhm, $
	pixsize, $
	field_stamps, $
	field_ids


wavelength=[250, 350, 500]
mapname=['_PSW','_PMW','_PLW']
fwhm=[18.1, 25.2, 36.6]
pixsize=[6.,8.333333,12.]

field_dates=[$
   '20111129',$;0) abell0370
   '20111129',$;1) abell1689
   '20111129',$;2) abell1835
   '20111129',$;3) abell2218
   '20111129',$;4) abell2219
   '20111129',$;5) abell2390
   '20111129',$;6) adfs
   '20111129',$;7) bootes
   '20111130',$;8) cdfs-nest
   '20111129',$;9) cdfs-swire
   '20110908',$;10) cdfs-swire3
   '20110829',$;11) cl0024
   '20111129',$;12) cosmos
   '20111129',$;13) cosmos2
   '20150205',$;14) cosmos-nest
   '20111129',$;15) ecdfs
   '20111129',$;16) egroth
   '20150205',$;16) egroth
   '20111129',$;17) egs-nest
   '20111129',$;18) egs-scuba
   '20111129',$;19) elais-n1
   '20111129',$;20) elais-n2
   '20111129',$;21) elais-s1
   '20111129',$;22) fls
   '20120411',$;23) goodsn
   '20111129',$;24) goodss
   '20111129',$;25) lockman-east
   '20111207',$;26) lockman-nest
   '20111129',$;27) lockman-north
   '20111129',$;28) lockman-swire
   '20111129',$;29) lockman-swire3
   '20111129',$;30) ms0451
   '20111129',$;31) ms1054
   '20111129',$;32) ms1358
   '20111129',$;33) rxj0512
   '20111129',$;34) rxj1347
   '20120201',$;35) s1-video
   '20111129',$;36) uds
   '20111129',$;37) vvds
   '20111129',$;38) xmm-lss
   '20111207' $;39) xmm-nest
   ]

field_ids=['abell0370','abell1689','abell1835','abell2218','abell2219','abell2390','adfs',$
   'bootes','cdfs-nest','cdfs-swire','cdfs-swire3','cl0024','cosmos','cosmos2','cosmos-nest',$
   'ecdfs','egroth','egs-nest','egs-scuba','elais-n1','elais-n2', 'elais-s1', $
   'fls','goodsn','goodss','lockman-east','lockman-nest','lockman-north','lockman-swire','lockman-swire3',$
   'ms0451','ms1054','ms1358','rxj0512','rxj1347','s1-video','uds','vvds','xmm-lss','xmm-nest']
field_stamps=field_ids+'_itermap_'+field_dates
END
