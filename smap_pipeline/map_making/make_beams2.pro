;+
;New neptune beam maker
;-

;;chose one of the below
;;normal scan mode
obsids = ['1342186522','1342186523','1342186524','1342186525']
;;bright scan mode
;obsids = ['1342187438','1342187439','1342187507','1342187508']

;;assumed on the cluster
basedir = '/data/spiredaq/reprocessed/fineScanMaps_ntL1g/'
;basedir = '/data/spiredaq/reprocessed/fineScanMaps_L1g/'

datadirs = basedir + obsids + $
           '/level1/herschel.spire.ia.dataset.PointedPhotTimeline'

;;masks, bad bolos
CREATEMAP_DEFPARAMS, badbolos, excludemask, exname_def, excludeweightmask
Nmask = N_ELEMENTS( excludemask )

;;don't trust deglitcher on neptune
;;truncation doing bad things
skipmasks = ['maskGlitch*','maskTruncated','maskUncorrectedTruncation'] 
keepmask = BYTARR( nmask, /NOZERO )
keepmask[*] = 1b
FOR i=0, N_ELEMENTS(skipmasks) - 1 DO BEGIN
   wexcl = WHERE(STRMATCH(excludemask,skipmasks[i],/FOLD_CASE),nexcl )
   IF nexcl NE 0 THEN keepmask[wexcl] = 0b
ENDFOR
wkeep = WHERE( keepmask,nkeep )
IF nkeep EQ 0 THEN excludemask = [''] ELSE $
   excludemask = excludemask[wkeep]



;;Read em
tods = smap_read_and_filter(datadirs,mapparam,/PTRS,/VERB, SUCCESS=success,$
                            ERRMSG=errmsg)
IF success EQ 0 THEN MESSAGE,"Error on initial read/filter: "+errmsg

MESSAGE,"TC drift correction",/INF
SMAP_TCDRIFT_CORRECT,tods,EXCLUDEMASK=excludemask,/VERB

;;Correct for neptune drift
;;Time zeropoint
t1 = 1635479014.0d0 ;;2009-10-29 03:43:00 (earliest is 3:43:05.136100)
t2 = 1635536494.0d0 ;;2009-10-29 19:41:00 (latest is 19:40:54.019488)
ra1 = 326.0096280d0
ra2 = 326.0073823d0
dec1 = -14.0725849d0
dec2 = -14.0734141d0 
m_ra  = (ra2  - ra1)  / (t2 - t1)
m_dec = (dec2 - dec1) / (t2 - t1)
FOR i=0,N_ELEMENTS(tods)-1 DO BEGIN
   nchans = (*tods[i]).nchans
   prodarr = ((*tods[i]).samptime - t1) ## REPLICATE(1.0d0,nchans)
   (*tods[i]).ra  -= m_ra * prodarr
   (*tods[i]).dec -= m_dec * prodarr
ENDFOR

;;mask out around neptune with no-poly mask
smap_set_region_mask, tods, 'maskManualNoParamWeight',$
                      ra1, dec1, 85.0

pixsize_arcsec=6.
;;1/5 size pixels
pixscale = [6,8.333333333333333,12]/(5*3600.0)
pixscale = [2.,2.,2.]/(3600.0)
pixscale = [1.,1.,1.]/(3600.0)
pixscale = pixsize_arcsec*[1.,1.,1.]/(3600.0)
;;Normal pixels
;pixscale = [6,8.333333333333333,12]/3600.0

exname='neptune_'+$
   strcompress(string(pixsize_arcsec,format='(d10.1)'),/remove_all)+$
   '_arcsec_pixels'
pixsize_suffix='_'+$
   strcompress(string(pixsize_arcsec,format='(d10.1)'),/remove_all)+$
   '_arcsec_pixels'
;;ok, map making
iterparams =  {niter:10, first_offs:1, first_gain:0, first_wt:10, $
               first_clip:0, fixed_nt:0, nterms:2, nt_scale:0, min_hits:1,$
               clipsigma: 10.0, grow_clip: 3}
smap_make_maps,tods,mapparam,map250,map350,map500,/ITERMAP,$
               ITER_PARAMS=iter_params,/VERB,EXCLUDEMASK=excludemask,$
               BADBOLOS=badbolos, EXCLUDEWEIGHTMASK=excludeweightmask, $
               PIXSCALE=pixscale, SUCCESS=success, ERRMSG=errmsg,$
               ITER_DIAG=3, EXNAME=exname
IF success EQ 0 THEN MESSAGE,"Problem building map: "+errmsg

st=SMAP_WRITE3COLORS("neptune",map250,map350,map500,$
                     /SILENT,ERRMSG=errmsg,EXNAME=pixsize_suffix)

end
