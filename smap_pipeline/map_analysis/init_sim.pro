PRO INIT_SIM

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


wavelength=[250, 350, 500]
mapname=['_PSW','_PMW','_PLW']
fwhm=[18.1, 25.2, 36.6]
pixsize=[6.,8.33333,12.]

xmm_datadir='XMM-LSS_L1e'
xmm_datadir='XMM-LSS_ntL1g'
xmm_obsid='0x50002DCB_0x50002DCC_0x50002DE7_0x500032E9_0x500032F4_0x500032F5'
xmm_date='20100915'
ecdfs_datadir=''
ecdfs_obsid=''
ecdfs_date=''
lockman_swire_datadir='LockmanSw_L1e'
lockman_swire_datadir='LockmanSw_ntL1g'
lockman_swire_obsid='0x5000227C_0x5000227D'
lockman_swire_date='20100820'
lockman_north_datadir='LockmanNorth_L1e'
lockman_north_datadir='LockmanNorth_ntL1g'
lockman_north_obsid=''
lockman_north_date=''
bootes_datadir='BooSp_L1e/*/level1/herschel.spire.ia.dataset.PointedPhotTimeline/'
bootes_datadir='BooSp_ntL1g'
bootes_obsid='0x500028BF_0x500028C0_0x500028C1_0x50002A3A_0x50002C6A_0x50002C6B_0x50002C89_0x50002C8A_0x50002E34'
bootes_date='20100930'
elais_n1_datadir=''
elais_n1_obsid='0x5000287E_0x5000287F_0x50002880_0x50002881_0x50002882'
elais_n1_date='20100818'
elais_s1_datadir='ELAIS-S1-NewSWIRE_L1e'
elais_s1_datadir='ELAIS-S1-NewSWIRE_ntL1g'
elais_s1_obsid='0x50004811_0x5000481F'
elais_s1_date='20100818'
adfs_datadir='ADFS_L1e'
adfs_obsid='0x50002A40_0x50002A41'
adfs_date='20100818'
fls_datadir='FLS_L1e'
fls_datadir='FLS_ntL1g'
fls_obsid='0x5000228B_0x50002336'
fls_date='20100818'
;cosmos_obsid='0x50004890_0x50004891_0x50004892_0x50004893_0x50004894_0x50004895_0x50004896_0x50004897'
;cosmos_date='20100818'
cosmos_datadir='/data/spiredaq/reprocessed/COSMOS_L1e/*/level1/herschel.spire.ia.dataset.PointedPhotTimeline/'
;cosmos_obsid='cosmos'
cosmos_date='20110104'
cosmos_obsid='cosmos'
cosmos_date='20110104'
cdfs_swire_obsid='0x5000228B_0x50002336'
cdfs_swire_date='20110325'
cdfs_swire_datadir='CDFS-SWIRE_ntL1g'
data_directories=[bootes_datadir, elais_n1_datadir, elais_s1_datadir, ecdfs_datadir, lockman_north_datadir, lockman_swire_datadir, adfs_datadir, xmm_datadir, fls_datadir, cosmos_datadir, cdfs_swire_datadir]
field_ids=['bootes', 'elais-n1', 'elais-s1', 'ecdfs', 'lockman-swire3', 'lockman-swire', 'adfs', 'xmm-lss','fls','cosmos','cdfs-swire']
field_obsids=[bootes_obsid, elais_n1_obsid, elais_s1_obsid, ecdfs_obsid, lockman_north_obsid, lockman_swire_obsid, adfs_obsid, xmm_obsid, fls_obsid, cosmos_obsid, cdfs_swire_obsid]
field_dates=[bootes_date, elais_n1_date, elais_s1_date, ecdfs_date, lockman_north_date, lockman_swire_date, adfs_date, xmm_date, fls_date, cosmos_date, cdfs_swire_date]
field_stamps=field_obsids+'_itermap_'+field_dates
field_stamps=field_ids+'_itermap_'+field_dates

END
