;+
;NAME
; make_beams
;PURPOSE
; Make beams using Neptune observations.
;USAGE
; make_beams, basename
;INPUTS
; basename        Base name of outputs
;KEYWORDS
; /writeall       Write neptune and shadow map pieces as well
;MODIFICATION HISTORY
; Written: Feb 2015, A. Conley
;NOTES:
; This is all fairly hardwired, but the basic idea is to make
;  a Neptune map, then make a shadow map (observations of the
;  same field but without Neptune) and subtract that.
; This requires special care because the data is not quite normal
;  (it isn't really calibrated, etc.), Neptune is moving, and because
;  things like the deglitcher fail miserably with something so bright.
; The output is a set of beam maps that have been
;  1) corrected for Neptune's motion
;  2) corrected to be at the color of the CIB rather than Neptune
;  3) corrected for the fact that Neptune is extended
;  4) had the background subtracted
; It is assumed this will be run on the SPIRE cluster.
;-

;; This is the neptune bit
PRO neptune_batch, map250, map350, map500, pixfactor, basename, $
                   exname, ITERDIAG=iterdiag, OUTDIR=outdir,$
                   NAIVE=naive, DO_TDC=do_tdc, QUICK=quick,$
                   NODRIFT=nodrift, DATAVERSION=dataversion

  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN

  IF N_ELEMENTS(naive) EQ 0 THEN itermap = 1b ELSE itermap = 0b
  IF N_ELEMENTS(iterdiag) EQ 0 THEN iterdiag=2

  ;; Hardwiring
  obsids = ['1342186522','1342186523','1342186524','1342186525']
  ;; If not running on the cluster, these need to be changed
  basedir = '/data/spiredaq/reprocessed/fineScanMaps_'+dataversion+'/' 
  datadirs = basedir + obsids + $
             '/level1/herschel.spire.ia.dataset.PointedPhotTimeline'

  ;; Specify map astrometric params assuming pixfactor of 5
  ;;  order PSW, PMW, PLW
  crpixx = [862, 622, 431]
  crpixy = [992, 715, 496]
  ra1 = 326.0096280d0  ;; position of Neptune at t1
  dec1 = -14.0725849d0 ;;  as defined below in drift correction
  
  ;;masks, bad bolos
  CREATEMAP_DEFPARAMS, badbolos, excludemask, exname_def, excludeweightmask
  nmask = N_ELEMENTS(excludemask)
  
  ;; Turn off some masks on Neptune -- we don't trust the deglitcher,
  ;;  and the truncation mask seems to be causing issues
  ;;  However, ignoring maskUncorrectedTruncation seems to be a rather
  ;;  bad idea as it leads to severe striping.
  skipmasks = ['maskGlitch*', 'maskTruncated']
  keepmask = BYTARR( nmask, /NOZERO )
  keepmask[*] = 1b
  FOR i=0, N_ELEMENTS(skipmasks) - 1 DO BEGIN
     wexcl = WHERE(STRMATCH(excludemask, skipmasks[i], /FOLD_CASE), nexcl)
     IF nexcl NE 0 THEN keepmask[wexcl] = 0b
  ENDFOR

  wkeep = WHERE(keepmask, nkeep)
  IF nkeep EQ 0 THEN excludemask = [''] ELSE $
     excludemask = excludemask[wkeep]
  
  ;; Map making params
  IF KEYWORD_SET(quick) THEN BEGIN
     iter_params =  {niter:10, first_offs:1, first_gain:0, first_wt:5, $
                     first_clip:0, fixed_nt:0, nterms:2, nt_scale:0, $
                     min_hits:1, clipsigma: 40.0, grow_clip: 3}
  ENDIF ELSE BEGIN
     iter_params =  {niter:20, first_offs:1, first_gain:0, first_wt:10, $
                     first_clip:0, fixed_nt:0, nterms:2, nt_scale:0, $
                     min_hits:1, clipsigma: 40.0, grow_clip: 3}
  ENDELSE
  
  ;; Pixel scale stuff
  pixscale = [6, 8.333333333333333, 12] / 3600.0
  IF ABS(pixfactor - 1.0) GT 1d-3 THEN pixscale /= pixfactor
  IF ABS(pixfactor-5.0) GT 1e-2 THEN BEGIN
     crpixx *= 0.2 * pixfactor
     crpixy *= 0.2 * pixfactor
  ENDIF

  ;; A further complication: the Neptune beams are not quite what
  ;; we want for two reasons 1) Neptune is warmer (70K) than the CIB
  ;; (19K).  Thus, the effective CIB beam should be slightly larger
  ;; than the one for Neptune 2) Neptune is extended (2.3" at the time
  ;; of observation), which already inflates the size of the beam.
  ;; We want to correct for these.  The effects of both terms are
  ;; very complex, so, for simplicitly, we assume that
  ;; the beams are Gaussian.  The net result of both effects is that
  ;; we need to effectively -shrink- the resulting beams by
  ;; 0.6, 0.4, 0.6%, respectively.  We do this by making the map
  ;; in slightly over-sized pixels, then pretending they are
  ;; the usual size
  corrfac = 1.0 + [0.006, 0.004, 0.006]
  pixscale_build = pixscale * corrfac

  ;; Get bad scans
  MESSAGE, " Reading Neptune bad scan info", /INF
  badscandir = ADDSLASH(!SMAP_PIPELINE_PATH) + $
               "map_making/createmap/conffiles/"
  badscanfile = badscandir + 'neptune.badscan'
  finfo = FILE_TEST(badscanfile, /READ)
  IF ~ finfo THEN MESSAGE, "Unable to read bad scan file: " + badscanfile
  info = CREATEMAP_READ_CONF(badscanfile)
  badobsid = *info.badobsid

  MESSAGE, " Reading Neptune data", /INF
  tods = smap_read_and_filter(datadirs, mapparam, /PTRS, /VERB, $
                              BADOBSID=badobsid, SUCCESS=success, $
                              ERRMSG=errmsg)
  IF success EQ 0 THEN MESSAGE, "Error on initial read/filter: " + errmsg

  ;; Don't use positions near Neptune in polynomial weighting
  ;;  or very bad things happen.
  ;;  Mask out 85 arcsec circle.  It's okay we do this before
  ;;  correcting for neptune's drift, since 85" is comparatively huge
  MESSAGE, "Masking neptune position", /INF
  smap_set_region_mask, tods, 'maskManualNoParamWeight',$
                        ra1, dec1, 95.0, /DOTHERMS

  IF do_tdc THEN BEGIN
     MESSAGE, " Doing Neptune TC drift correction", /INF
     ;; Need to add some mask bits because Neptune 'leaks'
     excludemask_tc = [excludemask, 'maskManualNoParamWeight']
     SMAP_TCDRIFT_CORRECT, tods, EXCLUDEMASK=excludemask_tc, /VERB
  ENDIF

  ;; Okay -- so now things get a little complicated.  This issue
  ;; is that we want to correct for the motion of Neptune during
  ;; the observations.  However, in principle this could interfere
  ;; with the proper convergence of the map maker.  The solution
  ;; is to make the maps twice -- once without drift correction,
  ;; once with, and save the parameters for re-use.
  ;; So -- first make the map without drift correcting
  MESSAGE, "Making maps without drift correction", /INF
  smap_make_maps, tods, mapparam, map250, map350, map500, ITERMAP=itermap,$
                  ITER_PARAMS=iter_params, /VERB, EXCLUDEMASK=excludemask,$
                  BADBOLOS=badbolos, EXCLUDEWEIGHTMASK=excludeweightmask, $
                  PIXSCALE=pixscale_build, SUCCESS=success, ERRMSG=errmsg,$
                  ITER_DIAG=iterdiag, SAVEMAPDIR=outdir, EXNAME=exname,$
                  CRVALX=ra1, CRVALY=dec1, CRPIXX=crpixx, CRPIXY=crpixy,$
                  SPARAMS250=sparams250, SPARAMS350=sparams350,$
                  SPARAMS500=sparams500
  IF success EQ 0 THEN MESSAGE,"Problem building map: "+errmsg

  IF ~ KEYWORD_SET(nodrift) THEN BEGIN
     ;; Now we drift correct and -remake- the map with that data
     ;; These positions are from JPL/HORIZONS.  Herschel's body code is
     ;;  -486.
     MESSAGE, "Positional drift correction", /INF
     t1 = 1635479014.0d0 ;;2009-10-29 03:43:00 (earliest is 3:43:05.136100)
     t2 = 1635536494.0d0 ;;2009-10-29 19:41:00 (latest is 19:40:54.019488)
     ra2 = 326.0073823d0
     dec2 = -14.0734141d0 
     m_ra  = (ra2  - ra1)  / (t2 - t1)
     m_dec = (dec2 - dec1) / (t2 - t1)
     FOR i=0,N_ELEMENTS(tods)-1 DO BEGIN
        nchans = (*tods[i]).nchans
        prodarr = ((*tods[i]).samptime - t1) ## REPLICATE(1.0d0,nchans)
        (*tods[i]).ra  -= m_ra * prodarr
        (*tods[i]).dec -= m_dec * prodarr
     ENDFOR
     MESSAGE, "Making maps with drift correction", /INF
     smap_make_maps, tods, mapparam, map250, map350, map500, ITERMAP=itermap,$
                     ITER_PARAMS=iter_params, /VERB, EXCLUDEMASK=excludemask,$
                     BADBOLOS=badbolos, EXCLUDEWEIGHTMASK=excludeweightmask, $
                     PIXSCALE=pixscale_build, SUCCESS=success, ERRMSG=errmsg,$
                     EXNAME=exname,$
                     CRVALX=ra1, CRVALY=dec1, CRPIXX=crpixx, CRPIXY=crpixy,$
                     IFIXED250=sparams250, IFIXED350=sparams350,$
                     IFIXED500=sparams500
     IF success EQ 0 THEN MESSAGE,"Problem building map: "+errmsg
  ENDIF

  ;; Free memory
  PTR_FREE,tods

  ;; Correct astrometry
  map250.pixscale = pixscale[0] * 3600.0
  map350.pixscale = pixscale[1] * 3600.0
  map500.pixscale = pixscale[2] * 3600.0
  map250.astrometry.cd[0,0] = -pixscale[0]
  map250.astrometry.cd[1,1] = pixscale[0]
  map350.astrometry.cd[0,0] = -pixscale[1]
  map350.astrometry.cd[1,1] = pixscale[1]
  map500.astrometry.cd[0,0] = -pixscale[2]
  map500.astrometry.cd[1,1] = pixscale[2]

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO shadow_batch, map250, map350, map500, pixfactor, basename, exname, $
                  ITERDIAG=iterdiag, NAIVE=naive, DO_TDC=do_tdc,$
                  NODRIFT=nodrift, DATAVERSION=dataversion, OUTDIR=outdir,$
                  QUICK=quick

  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN

  ;; Note that deglitching works fine for this data, so we put it
  ;; on by default
  
  IF N_ELEMENTS(naive) EQ 0 THEN itermap = 1b ELSE itermap = 0b
  IF N_ELEMENTS(iterdiag) EQ 0 THEN iterdiag=2

  ;; Specify map astrometric params for pixfactor=5
  ;;  order PSW, PMW, PLW.
  ;; These are identical to those in the neptune routine above
  crpixx = [862, 622, 431]
  crpixy = [992, 715, 496]
  ra1 = 326.0096280d0
  dec1 = -14.0725849d0

  obsids = ['1342255134', '1342255135']
  basedir = '/data/spiredaq/reprocessed/fineScanMaps_' + dataversion + '/'
  datadirs = basedir + obsids + $
             '/level1/herschel.spire.ia.dataset.PointedPhotTimeline'

  ;;masks, bad bolos
  CREATEMAP_DEFPARAMS, badbolos, excludemask, exname_def, excludeweightmask
  nmask = N_ELEMENTS(excludemask)

  ;; Unlike the Neptune maps, we actually more or less trust these
  ;; obs to be well behaved.  Helps not to have a many Jy source in
  ;; the map!
  
  ;; Get bad scans
  MESSAGE, " Reading shadow bad scan info", /INF
  badscandir = ADDSLASH(!SMAP_PIPELINE_PATH) + $
               "map_making/createmap/conffiles/"
  badscanfile = badscandir + 'shadow.badscan'
  finfo = FILE_TEST(badscanfile, /READ)
  IF ~ finfo THEN MESSAGE, "Unable to read bad scan file: " + badscanfile
  info = CREATEMAP_READ_CONF(badscanfile)
  badobsid = *info.badobsid

  MESSAGE, " Reading shadow data", /INF
  tods = smap_read_and_filter(datadirs, mapparam, /PTRS, /VERB, $
                              BADOBSID=badobsid, SUCCESS=success, $
                              ERRMSG=errmsg)
  IF success EQ 0 THEN MESSAGE, "Error on initial read/filter: " + errmsg

  ;; Don't mask the center like we did for Neptune.  It's a bit
  ;; hard to decide if we should or shouldn't... but the data should
  ;; be good
  IF do_tdc THEN BEGIN
     MESSAGE, "TC drift correction", /INF
     SMAP_TCDRIFT_CORRECT, tods, EXCLUDEMASK=excludemask, /VERB
  ENDIF

  ;; Map making params
  IF KEYWORD_SET(quick) THEN BEGIN
     iter_params =  {niter:10, first_offs:1, first_gain:0, first_wt:5, $
                     first_clip:6, fixed_nt:0, nterms:2, nt_scale:0, $
                     min_hits:1, clipsigma: 5.0, grow_clip: 3}
  ENDIF ELSE BEGIN
     iter_params =  {niter:20, first_offs:1, first_gain:0, first_wt:10, $
                     first_clip:11, fixed_nt:0, nterms:2, nt_scale:0, $
                     min_hits:1, clipsigma: 5.0, grow_clip: 3}
  ENDELSE
  
  ;; Pixscale stuff
  pixscale = [6, 8.333333333333333, 12] / 3600.0
  IF ABS(pixfactor - 1) GT 1d-3 THEN pixscale /= pixfactor
  IF ABS(pixfactor - 5.0) GT 1d-3 THEN BEGIN
     crpixx *= 0.2 * pixfactor
     crpixy *= 0.2 * pixfactor
  ENDIF

  ;; See neptune batch for an explanation
  corrfac = 1.0 + [0.006, 0.004, 0.006]
  pixscale_build = pixscale * corrfac

  ;; As is done for neptune, we may do this twice -- once with
  ;; drift correction, once without using the same params.
  ;; See neptune_batch for further discussion
  MESSAGE, " Making shadow map without drift correction", /INF
  smap_make_maps, tods, mapparam, map250, map350, map500, ITERMAP=itermap,$
                  ITER_PARAMS=iter_params, /VERB, EXCLUDEMASK=excludemask,$
                  BADBOLOS=badbolos, EXCLUDEWEIGHTMASK=excludeweightmask, $
                  PIXSCALE=pixscale_build, SUCCESS=success, ERRMSG=errmsg,$
                  ITER_DIAG=iterdiag, SPARAMS250=sparams250, $
                  SPARAMS350=sparams350, SPARAMS500=sparams500,$
                  DEGLITCH250=deglitch250, DEGLITCH350=deglitch350, $
                  DEGLITCH500=deglitch500, SAVEMAPDIR=outdir,$
                  EXNAME=exname,$
                  CRVALX=ra1, CRVALY=dec1, CRPIXX=crpixx, CRPIXY=crpixy
  IF success EQ 0 THEN MESSAGE,"Problem building map: "+errmsg
  SAVE, deglitch250, deglitch350, deglitch500, $
        FILE=outdir + basename + '_' + exname + '_info.sav', /COMPRESS

  IF ~ KEYWORD_SET(nodrift) THEN BEGIN
     ;; The pos drift correction tries to generate the same blurring
     ;;  which means we keep the same delta ra and dec but over the
     ;;  shorter timeline of these obs -- thus, we blur 'faster' here
     ;; The first obsid (1342255134) goes from
     ;;    2012-11-15T22:37:59.264354 to 2012-11-16T01:45:05.196137
     ;;  or SPIRE clock times 1731710314.26 to 1731721540.196
     ;; The second (1342255135) goes from
     ;;    2012-11-16T01:46:52.268288 to 2012-11-16T05:40:03.218279
     ;;  or SPIRE clock times 1731721647.2683 to 1731735693.3219
     MESSAGE, "Positional drift correction", /INF
     t1 = 1731710314.264d0
     t2 = 1731735693.321d0
     ra2 = 326.0073823d0
     dec2 = -14.0734141d0 
     m_ra  = (ra2  - ra1)  / (t2 - t1)
     m_dec = (dec2 - dec1) / (t2 - t1)
     FOR i=0,N_ELEMENTS(tods)-1 DO BEGIN
        nchans = (*tods[i]).nchans
        prodarr = ((*tods[i]).samptime - t1) ## REPLICATE(1.0d0,nchans)
        (*tods[i]).ra  -= m_ra * prodarr
        (*tods[i]).dec -= m_dec * prodarr
     ENDFOR

     MESSAGE, " Making shadow map with drift correction", /INF
     smap_make_maps, tods, mapparam, map250, map350, map500, ITERMAP=itermap,$
                     ITER_PARAMS=iter_params, /VERB, EXCLUDEMASK=excludemask,$
                     BADBOLOS=badbolos, EXCLUDEWEIGHTMASK=excludeweightmask, $
                     PIXSCALE=pixscale_build, SUCCESS=success, ERRMSG=errmsg,$
                     IFIXED250=sparams250, IFIXED350=sparams350, $
                     IFIXED500=sparams500, /FIXEDDEGLITCH, $
                     DEGLITCH250=deglitch250, DEGLITCH350=deglitch350, $
                     DEGLITCH500=deglitch500, SAVEMAPDIR=outdir,$
                     EXNAME='neptune_' + exname,$
                     CRVALX=ra1, CRVALY=dec1, CRPIXX=crpixx, CRPIXY=crpixy
     IF success EQ 0 THEN MESSAGE,"Problem building map: "+errmsg
  
  ENDIF

  ;; Free memory
  PTR_FREE,tods

  ;; Correct astrometry
  map250.pixscale = pixscale[0] * 3600.0
  map350.pixscale = pixscale[1] * 3600.0
  map500.pixscale = pixscale[2] * 3600.0
  map250.astrometry.cd[0,0] = -pixscale[0]
  map250.astrometry.cd[1,1] = pixscale[0]
  map350.astrometry.cd[0,0] = -pixscale[1]
  map350.astrometry.cd[1,1] = pixscale[1]
  map500.astrometry.cd[0,0] = -pixscale[2]
  map500.astrometry.cd[1,1] = pixscale[2]

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO write_beam_diffmap, nmap, smap, band, basename, exname, $
                        OUTDIR=outdir, KAPPA=kappa, PIXFACTOR=pixfactor

  COMPILE_OPT IDL2, HIDDEN

  IF N_ELEMENTS(kappa) EQ 0 THEN kappa = 0.85
  IF kappa LT 0 THEN MESSAGE, "Invalid (non-positive) kappa"

  ;; The tricky bit here is that, while pixel i, j in the neptune
  ;; map corresponds to the same pixel in the shadow one, the
  ;; maps are not quite the same extent because the scans covered
  ;; slightly different areas.  We can align them by clipping off
  ;; the tops
  approx_fwhms = [17.6, 23.9, 35.2]
  bands = ['PSW', 'PMW', 'PLW']
  wband = WHERE(band EQ bands, nband)
  IF nband NE 1 THEN MESSAGE, "Band " + band + " unknown"
  approx_fwhm = approx_fwhms[wband[0]]
  
  IF N_ELEMENTS(pixfactor) NE 0 && ABS(pixfactor - 5.0) GT 0.1 THEN BEGIN
     cmx_arr *= ROUND(pixfactor / 5.0)
     cmy_arr *= ROUND(pixfactor / 5.0)
  ENDIF

  bands = ['PSW', 'PMW', 'PLW']
  wband = WHERE(band EQ bands, nband)
  IF nband NE 1 THEN MESSAGE, "Failed to identify band: " + band
  
  MESSAGE, STRING(band, FORMAT='("Doing band: ", A0)'), /INF

  ;; Clipping region.  Clip from the top.
  cmx = nmap.xsize < smap.xsize
  cmy = nmap.ysize < smap.ysize
  
  newastr = nmap.astrometry
  newastr.naxis = [cmx, cmy]
  newmap = get_smap_mapstruct(NPIXX=cmx, NPIXY=cmy, /NOMASK, /NOERR, $
                              BAND=band, ASTROMETRY=newastr, /EXP_DBL,$
                              /SILENT)
  newmap.image = nmap.image[0:cmx-1, 0:cmy-1]
  IF kappa GT 0 THEN newmap.image -= kappa * smap.image[0:cmx-1, 0:cmy-1]
  newmap.exposure = beam.exposure[0:cmx-1, 0:cmy-1]
  IF kappa GT 0 THEN newmap.exposure +=  $
     kappa * smap.exposure[0:cmx-1, 0:cmy-1]

  ;; Center normalize the beam; this depends on the pixel size a bit
  ;; see 5.44 in the SPIRE observers manual.  This is assuming
  ;; the center is nicely Gaussian
  t = newmap.pixscale / approx_fwhm
  expmax = !PI * (ERF(t * SQRT(ALOG(2))))^2 / (4.0 * ALOG(2) * t^2)
  newmap.image *= expmax / MAX(newmap.image, /NAN)

  ;; Set up extra header info
  opthdr = ['']
  SXADDPAR, opthdr, 'KAPPA', kappa, "Shadow map multiplier"
  SXADDPAR, opthdr, 'NORMED', 'T', "Map center normalized to pixel size"

  ;; Write the oversampled beams
  MESSAGE, " Writing beam", /INF
  st = write_smap_fitsmap(newmap, basename + '_' + exname, DIR=outdir, $
                          /SILENT, OPTHDR=opthdr)
  IF st EQ 0 THEN MESSAGE, "Error writing combined beam"
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO make_beams, PIXFACTOR=pixfactor, NAIVE=naive, NO_TDC=no_tdc, $
                NODRIFT=nodrift, DATAVERSION=dataversion, ITERDIAG=iterdiag,$
                WRITEALL=writeall, OUTDIR=outdir, QUICK=quick
                
  COMPILE_OPT IDL2, STRICTARRSUBS
  
  ;; Set up extension, etc
  IF N_ELEMENTS(outdir) EQ 0 THEN outdir = ADDSLASH(!SMAP_MAPS)
  IF N_ELEMENTS(naive) EQ 0 THEN itermap = 1b ELSE itermap = 0b
  IF N_ELEMENTS(iterdiag) EQ 0 THEN iterdiag=0
  IF N_ELEMENTS(dataversion) EQ 0 THEN dataversion = 'ntL1i'
  IF N_ELEMENTS(pixfactor) EQ 0 THEN pixfactor = 5
  do_tdc = ~ KEYWORD_SET(no_tdc)

  ;; extension name including date
  CALDAT, SYSTIME(/JUL), m, d, y
  this_exname = STRING([y,m,d], FORM='(I04,I02,I02)')
  
  IF do_tdc THEN this_exname = "tdcmask_" + this_exname
  IF ~ do_tdc THEN this_exname = 'hipetdc_' + this_exname
  IF pixfactor NE 1 THEN $
     this_exname += STRING(pixfactor, FORMAT='("_",I0,"x")')
  IF KEYWORD_SET(nodrift) THEN this_exname = 'nodrift_' + this_exname

  ;; Neptune beam
  summsg = "Making Neptune beam maps"
  IF itermap THEN summsg += " iteratively"
  IF do_tdc THEN summsg += " using SMAP TDC"
  IF ~ do_tdc THEN summsg += " using HIPE TDC"
  MESSAGE, summsg, /INF
  
  neptune_batch, nmap250, nmap350, nmap500, pixfactor,$
                 "neptune", this_exname, ITERDIAG=iterdiag, $
                 NAIVE=naive, DO_TDC=do_tdc, NODRIFT=nodrift, $
                 DATAVERSION=dataversion, OUTDIR=outdir,$
                 QUICK=quick
  MESSAGE, "Done making Neptune map", /INF
  ;; Write
  IF KEYWORD_SET(writeall) THEN BEGIN
     st=SMAP_WRITE3COLORS("neptune", nmap250, nmap350, nmap500,$
                          /SILENT, ERRMSG=errmsg, EXNAME='_'+this_exname,$
                          DIR=outdir)
     IF st EQ 0 THEN MESSAGE, "Error writing neptune base map: " + errmsg
  ENDIF

  ;; Shadow beam
  summsg = "Making Neptune Shadow beam maps"
  IF itermap THEN summsg += " iteratively"
  IF do_tdc THEN summsg += " using SMAP TDC"
  IF ~ do_tdc THEN summsg += " using HIPE TDC"
  MESSAGE, summsg, /INF
  
  ;; Write
  shadow_batch, smap250, smap350, smap500, pixfactor,$
                "shadow", this_exname, ITERDIAG=iterdiag, $
                NAIVE=naive, DO_TDC=do_tdc, NODRIFT=nodrift, $
                DATAVERSION=dataversion, OUTDIR=outdir,$
                QUICK=quick
  MESSAGE, "Done making shadow map", /INF
  ;; Write
  IF KEYWORD_SET(writeall) THEN BEGIN
     st=SMAP_WRITE3COLORS("shadow", smap250, smap350, smap500,$
                          /SILENT, ERRMSG=errmsg, EXNAME='_'+this_exname,$
                          DIR=outdir)
     IF st EQ 0 THEN MESSAGE, "Error writing shadow map: " + errmsg
  ENDIF

  ;; Combine and write
  kappa = 0.85
  write_beam_diffmap, nmap250, smap250, 'PSW', "beam", this_exname,$
                      OUTDIR=outdir, KAPPA=kappa, PIXFACTOR=pixfactor
  write_beam_diffmap, nmap350, smap350, 'PMW', "beam", this_exname,$
                      OUTDIR=outdir, KAPPA=kappa, PIXFACTOR=pixfactor
  write_beam_diffmap, nmap500, smap500, 'PLW', "beam", this_exname,$
                      OUTDIR=outdir, KAPPA=kappa, PIXFACTOR=pixfactor
  
END
