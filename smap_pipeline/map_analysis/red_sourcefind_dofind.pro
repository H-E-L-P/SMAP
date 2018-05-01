;+
;NAME
; red_sourcefind_dofind
;PURPOSE
; Make a linear combination of input maps and find sources.  Inner
;  function for red_sourcefind that does everything but read in the
;  maps.  Note that the maps should already be smoothed.
;
;USAGE
; cat = red_sourcefind_dofind(map250, map350, map500, [, COEFFS=,
;                             BASENAME=, MASKFILES=, MASKDIR=,
;                             SN=, BGSUBSIZE=, MIN_CORR=, FLUXRAT=,
;                             SCATFILE=, RAREFILE=, OUTDIR=, CATRAD=,
;                             MINS500=]
;
;INPUTS
;  map[250,350,500]    Input maps
;OUTPUTS
;  cat                 Resulting catalog as a STRUCT
;OPTIONAL INPUTS
; fwhm          FWHM of maps, in arcsec.  The default is sqrt(2) * 35.2
; bgsubsize     Background subtraction size, in arcsec (def: 192)
; mins500       Minimum 500um flux density to keep [in mJy]
;OPTIONAL OUTPUTS
; cathdr        Header information for catalog (useful for writing it
;               as a FITS table)
; success       0 on failure, 1 on success, 2 if no sources found
;-

FUNCTION red_sourcefind_dofind_bgmask, map
  COMPILE_OPT IDL2, HIDDEN

  mask = BYTARR(map.xsize, map.ysize)
  IF map.has_mask AND map.has_exposure THEN BEGIN
     wmask = WHERE(map.mask NE 0 OR map.exposure LE 0.0, nmask)
     IF nmask NE 0 THEN mask[wmask] = 1b
  ENDIF ELSE IF map.has_mask THEN BEGIN
     wmask = WHERE(map.mask NE 0, nmask)
     IF nmask NE 0 THEN mask[wmask] = 1b
  ENDIF ELSE IF map.has_exposure THEN BEGIN
     wmask = WHERE(map.exposure LE 0.0, nmask)
     IF nmask NE 0 THEN mask[wmask] = 1b
  ENDIF
  RETURN, mask
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION red_sourcefind_dofind, map250, map350, map500, BASENAME=basename,$
                                COEFFS=coeffs, NOSMOOTH=nosmooth,$
                                OPTFILT=optfilt, MASKFILES=maskfiles,$
                                MASKDIR=maskdir, SN=sn, BGSUBSIZE=bgsubsize,$
                                MIN_CORR=min_corr, FLUXRAT=fluxrat,$
                                SCATFILE=scatfile, RAREFILE=rarefile,$
                                OUTDIR=outdir, CATRAD=catrad, CATHDR=cathdr,$
                                CONF=conf, INST=inst, NOWRITE=nowrite,$
                                USEMAPMAKERERROR=usemapmakererror,$
                                SUCCESS=success, ERRMSG=errmsg,$
                                MAPDIFF=mapdiff, FINDPEAKPIX=findpeakpix,$
                                PEAKSEARCHRAD=peaksearchrad, $
                                USEPEAKPOS=usepeakpos, FWHM=fwhm,$
                                ALG=alg, VERBOSE=verbose, NPAD=npad,$
                                MAPCOMB=mapcomb, MINS500=mins500

  COMPILE_OPT IDL2, STRICTARRSUBS

  success = 0b
  errmsg = ""

  ;;The errors depend on the coeffs, so make the user enter them
  ;; if they want something non-standard
  IF N_ELEMENTS(coeffs) EQ 0 THEN coeffs = [-SQRT(1-0.92^2),0.0,0.92]
  IF N_ELEMENTS(conf) EQ 0 THEN conf = 4.23e-3 ;; sig conf, Jy

  IF N_ELEMENTS(sn) EQ 0 THEN sn=4.0
  IF N_ELEMENTS(bgsubsize) EQ 0 THEN bgsubsize = 192
  IF N_ELEMENTS(fluxrat) EQ 0 THEN fluxrat = [0.1, 1.0]
  IF N_ELEMENTS(min_corr) EQ 0 THEN min_corr = 0.85
  IF N_ELEMENTS(catrad) EQ 0 THEN catrad = 12.0
  IF N_ELEMENTS(basename) EQ 0 THEN basename='redmap'
  IF N_ELEMENTS(peaksearchrad) EQ 0 THEN peaksearchrad = 12.0 ;arcsec
  IF KEYWORD_SET(usepeakpos) THEN findpeakpix = 1b
  IF KEYWORD_SET(findpeakpix) && peaksearchrad LE 0.0 THEN BEGIN
     errmsg = "Invalid (non-positive) peaksearchrad"
     GOTO, err_handler
  ENDIF
  IF N_ELEMENTS(alg) EQ 0 THEN alg = 2

  IF KEYWORD_SET(nosmooth) AND N_ELEMENTS(fwhm) EQ 0 THEN fwhm = 35.2 * SQRT(2)
  has_fwhm = KEYWORD_SET(fwhm) ;; Otherwise try to estimate
  IF has_fwhm && fwhm LE 0.0 THEN BEGIN
     errmsg = "Invalid (non-positive) FWHM"
     GOTO, err_handler
  ENDIF
  ;; size of simple phot box in pixels -- approx 0.7 * FWHM
  npix_simple = ROUND(0.7 * fwhm / map500.pixscale)
  IF npix_simple MOD 2 EQ 0 THEN npix_simple += 1
  npix_simple >= 3
  ;; npix_simple = 9  ; hardwired

  ;; Add masks; do this here rather than in linear_map_combine
  ;;  because we want them for the background estimate as well
  IF N_ELEMENTS(maskfiles) NE 0 THEN BEGIN
     IF ~ map250.has_mask THEN $
        MESSAGE,"Can't add user mask to 250um map if none present in map"
     add_user_mapmask, map250, maskfiles, /ANDPLUS, MASKDIR=maskdir
     IF ~ map350.has_mask THEN $
        MESSAGE,"Can't add user mask to 350um map if none present in map"
     add_user_mapmask, map350, maskfiles, /ANDPLUS, MASKDIR=maskdir
     IF ~ map500.has_mask THEN $
        MESSAGE,"Can't add user mask to 500um map if none present in map"
     add_user_mapmask, map500, maskfiles, /ANDPLUS, MASKDIR=maskdir
  ENDIF

  ;; Do smoothing if needed
  IF ~ KEYWORD_SET(nosmooth) THEN BEGIN
     IF KEYWORD_SET(optfilt) THEN BEGIN
        ;; optimal filtering
        IF KEYWORD_SET(verbose) THEN $
           MESSAGE," Optimally filtering maps", /INF
        matched_filter_red, map250, map350, map500, conf, $
                            BEAM=beam, NPAD=npad, K1=coeffs[0], K2=coeffs[1]
     
        ;; estimate FWHM.  Clip down beam so this is faster
        IF ~ has_fwhm THEN BEGIN
           init_fwhm = 35.2 / map500.pixscale ;;in pix
           maxbm = MAX(beam, wmax)
           szbm = SIZE(beam)
           init_pos_x = wmax MOD szbm[1]
           init_pos_y = wmax / szbm[1]
           censize = 7 * CEIL(init_fwhm)
           minx = (init_pos_x - censize) > 0
           maxx = (init_pos_x + censize) < (szbm[1] - 1)
           miny = (init_pos_y - censize) > 0
           maxy = (init_pos_y + censize) < (szbm[2] - 1)
           beam = beam[minx:maxx, miny:maxy]
           maxbm = MAX(beam, wmax)
           szbm = SIZE(beam)
           init_pos_x = wmax MOD szbm[1]
           init_pos_y = wmax / szbm[1]
           init_params = [0.0, 1.0, 0.5 * init_fwhm, 0.5 * init_fwhm, $
                          init_pos_x, init_pos_y, 1.0]
           yfit = MPFIT2DPEAK(beam, params, /CIRCULAR,$
                              ESTIMATES=init_params, /GAUSSIAN)
           fwhm = 2.355 * params[2] * map500.pixscale
           has_fwhm = 1b
        ENDIF
     ENDIF ELSE BEGIN
        ;; simple smoothing
        ;; non-optimal filtering, as in RARE 70
        IF KEYWORD_SET(verbose) THEN $
           MESSAGE," Smoothing maps to fixed SQRT(2)*FWHM_500", /INF
        smap_redsource_smooth, map250, /BRUTE, /NOERRORSMOOTH
        smap_redsource_smooth, map350, /BRUTE, /NOERRORSMOOTH
        smap_redsource_smooth, map500, /BRUTE, /NOERRORSMOOTH
     ENDELSE
  ENDIF

  ;;Get background maps on smoothed maps
  bgsubsize_pix = ROUND(bgsubsize / map250.pixscale)
  mask = red_sourcefind_dofind_bgmask(map250)
  bgmap250 = smap_bgestimator(map250.image, TEMPORARY(mask), bgsubsize_pix)

  mask = red_sourcefind_dofind_bgmask(map350)
  bgmap350 = smap_bgestimator(map350.image, TEMPORARY(mask), bgsubsize_pix)

  mask = red_sourcefind_dofind_bgmask(map500)
  bgmap500 = smap_bgestimator(map500.image, TEMPORARY(mask), bgsubsize_pix)

  ;; Finally combine; makes some sort of error estimate
  mapcomb = linear_map_combine(coeffs[0], map250, coeffs[1], map350,$
                               coeffs[2], map500)

  ;; Source finding
  IF KEYWORD_SET(verbose) THEN MESSAGE,"  Doing sourcefind",/INF
  mask = red_sourcefind_dofind_bgmask(mapcomb)
  bgmapcomb = smap_bgestimator(mapcomb.image, TEMPORARY(mask), bgsubsize_pix)
  cat = ps_extractor_doband(mapcomb, sn, "LINCOMB",$
                            FWHM=fwhm, PRE_SMOOTH=0b,$
                            ALG=alg, MIN_CORR=min_corr, MAPMASKBITS=1uL,$
                            BGINPUT=bgmapcomb, MASKCHECKSZ=5, $
                            SUCCESS=success_doband)
  IF ~ success_doband THEN BEGIN
     ;; Didn't find any sources -- but that isn't necessarily a failure
     ;;  mode!  Clean up starfndr fits file if it exists
     success = 2b
     RETURN, !VALUES.D_NAN
  ENDIF
  IF KEYWORD_SET(verbose) THEN BEGIN
     fmt = '("   Found ",I0," sources in linearly combined map")'
     MESSAGE, STRING(N_ELEMENTS(cat),FORMAT=fmt), /INF
  ENDIF

  ;; Write even if we didn't find any sources.  Do this after
  ;; sourcefinding so we can subtract the bgmap
  mapcomb.image -= TEMPORARY(bgmapcomb)
  IF ~ KEYWORD_SET(nowrite) THEN BEGIN
     ;; Additional header info for combined map
     hdr = ['']
     SXADDPAR, hdr, 'HISTORY', 'Linearly combined map'
     SXADDPAR, hdr, 'K1', coeffs[0], 'map250 coeff'
     SXADDPAR, hdr, 'K2', coeffs[1], 'map350 coeff'
     SXADDPAR, hdr, 'K3', coeffs[2], 'map500 coeff'
     SXADDPAR, hdr, "BGSUB", "T", "Background subtracted"
     IF ~ KEYWORD_SET(nosmooth) THEN BEGIN
        SXADDPAR, hdr, 'SMOOTHED', 'T', "Additonal smoothing in combination"
        IF KEYWORD_SET(optfilt) THEN BEGIN
           SXADDPAR, hdr, 'OPTFILT', 'T', "Optimal filtering used"
        ENDIF ELSE $
           SXADDPAR, hdr, 'OPTFILT', 'F', "Optimal filtering used"
     ENDIF ELSE $
        SXADDPAR, hdr, 'SMOOTHED', 'F', "Additonal smoothing in sourcefind"
     SXADDPAR, hdr, "FWHM", fwhm, "Estimated FWHM in combined map"
     opthdr = hdr[0:N_ELEMENTS(hdr)-2] ;;clip off END

     ;; Write
     st = write_smap_fitsmap(mapcomb, basename+'_bgsub', DIR=outdir,$
                             OPTHDR=opthdr, /NO_ABORT)
     IF st EQ 0 THEN BEGIN
        errmsg =  "Error writing output map"
        GOTO, err_handler
     ENDIF
  ENDIF
  
  ;; Process sourcefinding results
  ncat = N_ELEMENTS(cat)
  XY2AD, cat.x, cat.y, mapcomb.astrometry, ra, dec

  ;; At Darren's urging, keep track of the ra/dec of the
  ;; brightest pixel within user specified radius i the combined map
  ;; as an alternative location
  IF KEYWORD_SET(findpeakpix) THEN BEGIN
     pixsearchrad = ROUND(peaksearchrad / mapcomb.pixscale) > 1
     maxx = mapcomb.xsize-1
     maxy = mapcomb.ysize-1
     peak_x = FLTARR(ncat)
     peak_y = FLTARR(ncat)
     FOR i=0, ncat-1 DO BEGIN
        xmid = ROUND(cat[i].x)
        ymid = ROUND(cat[i].y)
        
        xbot = (xmid - pixsearchrad) > 0
        xtop = (xmid + pixsearchrad) < maxx
        ybot = (ymid - pixsearchrad) > 0
        ytop = (ymid + pixsearchrad) < maxy
        
        maxval = MAX(mapcomb.image[xbot:xtop,ybot:ytop], wmax)
        nxs = xtop - xbot + 1
        peak_x[i] = (wmax MOD nxs) + xbot
        peak_y[i] = (wmax / nxs) + ybot
     ENDFOR
     XY2AD, peak_x, peak_y, mapcomb.astrometry, peak_ra, peak_dec
  ENDIF

  ;; Cull things too close to the edge since simplephot doesn't
  ;; like that.  First get the positions we will use
  IF KEYWORD_SET(use_peakpos) THEN BEGIN
     xsimple = peak_x
     ysimple = peak_y
  ENDIF ELSE BEGIN
     xsimple = cat.x
     ysimple = cat.y
  ENDELSE
  ;; And cull
  hfsz = npix_simple / 2
  wgood = WHERE(xsimple GE hfsz AND xsimple LE map250.xsize - hfsz - 1 AND $
                ysimple GE hfsz AND ysimple LE map250.ysize - hfsz - 1, ngood,$
                NCOMPLEMENT=nbad)
  IF ngood EQ 0 THEN MESSAGE, "All sources too close to edge"
  IF nbad NE 0 THEN BEGIN
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE, STRING(nbad, FORMAT='("   Removing ",I0," edge sources")'), $
                 /INF
     xsimple = xsimple[wgood]
     ysimple = ysimple[wgood]
     ra = ra[wgood]
     dec = dec[wgood]
     IF KEYWORD_SET(findpeakpix) THEN BEGIN
        peak_x = peak_x[wgood]
        peak_y = peak_y[wgood]
        peak_ra = peak_ra[wgood]
        peak_dec = peak_dec[wgood]
     ENDIF
     cat = cat[TEMPORARY(wgood)]
  ENDIF
  
  ;;Now -- go back to smothed one-band maps and check fluxes
  flux250 = smap_simplephot(map250, xsimple, ysimple, /XYPOS, FWHM=fwhm, $
                            ERR=flux_err250, /USEMASK,$
                            NPIX=npix_simple, BGRND=TEMPORARY(bgmap250))
  flux350 = smap_simplephot(map350, xsimple, ysimple, /XYPOS, FWHM=fwhm, $
                            ERR=flux_err350, /USEMASK,$
                            NPIX=npix_simple, BGRND=TEMPORARY(bgmap350))
  flux500 = smap_simplephot(map500, TEMPORARY(xsimple), TEMPORARY(ysimple), $
                            /XYPOS, FWHM=fwhm, ERR=flux_err500, /USEMASK,$
                            NPIX=npix_simple, BGRND=TEMPORARY(bgmap500))

  IF N_ELEMENTS(mins500) NE 0 THEN BEGIN
     IF mins500 LT 0.0 THEN BEGIN
        errmsg = "Invalid (negative) mins500"
        GOTO, err_handler
     ENDIF
     wgood = WHERE(flux350 GE fluxrat[0] * flux250 AND $
                   flux500 GE fluxrat[1] * flux350 AND $
                   flux500 GE mins500 * 1e-3,$  ;; mJy -> Jy for mins500
                   ngood, NCOMPLEMENT=nbad)
  ENDIF ELSE BEGIN
     wgood = WHERE(flux350 GE fluxrat[0] * flux250 AND $
                   flux500 GE fluxrat[1] * flux350,$
                   ngood, NCOMPLEMENT=nbad)
  ENDELSE
  IF ngood EQ 0 THEN BEGIN
     delname = basename+'_starfndr.fits'
     IF N_ELEMENTS(outdir) NE 0 THEN delname = ADDSLASH(outdir)+delname
     FILE_DELETE,delname,/ALLOW
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE, "Found no sources after fluxratio cut; exiting", /INF
     success = 2b
     RETURN, !VALUES.F_NAN
  ENDIF ELSE $
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,STRING(ngood,FORMAT='("   Found ",I0," final sources")'),/INF

  ;;cull
  cat = cat[wgood]
  ra  = ra[wgood] & dec = dec[wgood]
  IF KEYWORD_SET(findpeakpix) THEN BEGIN
     peak_x = peak_x[wgood] & peak_y = peak_y[wgood]
     peak_ra = peak_ra[wgood] & peak_dec = peak_dec[wgood]
  ENDIF
  flux250 = flux250[wgood] & flux_err250 = flux_err250[wgood]
  flux350 = flux350[wgood] & flux_err350 = flux_err350[wgood]
  flux500 = flux500[wgood] & flux_err500 = flux_err500[wgood]
     
  ;;Check against SCAT if present
  IF N_ELEMENTS(scatfile) NE 0 THEN BEGIN
     scat = MRDFITS(scatfile,1,/SILENT,STATUS=status)
     IF status NE 0 THEN BEGIN
        errmsg = "Error reading in SCAT cat: "+scat
        GOTO, err_handler
     ENDIF
     SRCOR, ra, dec, scat.inra, scat.indec, catrad, indred, indscat, OPTION=1,$
            /SILENT,SPHERICAL=2,COUNT=countscat
  ENDIF ELSE countscat=0

  ;;And rare cat
  IF N_ELEMENTS(rarefile) NE 0 THEN BEGIN
     IF ~ FILE_TEST(rarefile,/READ) THEN BEGIN
        errmsg = "Can't read RARE list "+rarefile
        GOTO, err_handler
     ENDIF
     READCOL, rarefile, name, rarera, raredec, FORMAT='(A,F,F)',/SILENT
     SRCOR, ra, dec, rarera, raredec, catrad, indred2, indrare, OPTION=1,$
            /SILENT, SPHERICAL=2, COUNT=countrare
  ENDIF ELSE countrare=0

  ;;Make output structure
  base_cat = { x_sub: !VALUES.F_NAN, y_sub: !VALUES.F_NAN,$
               ra_sub: !VALUES.F_NAN, dec_sub: !VALUES.F_NAN,$
               f_sub: !VALUES.F_NAN, df_sub: !VALUES.F_NAN,$
               map_f250: !VALUES.F_NAN, map_df250: !VALUES.F_NAN,$
               map_f350: !VALUES.F_NAN, map_df350: !VALUES.F_NAN,$
               map_f500: !VALUES.F_NAN, map_df500: !VALUES.F_NAN,$
               corr: !VALUES.F_NAN }
  IF KEYWORD_SET(findpeakpix) THEN $
     base_cat = CREATE_STRUCT('x_peak', !VALUES.F_NAN,$
                              'y_peak', !VALUES.F_NAN,$
                              'ra_peak', !VALUES.F_NAN,$
                              'dec_peak', !VALUES.F_NAN, base_cat)
  IF countscat GT 0 THEN $
     base_cat = CREATE_STRUCT('scat_idx',-1L,'dist_scat',!VALUES.F_NAN,$
                              'ra_scat',!VALUES.F_NAN,'dec_scat',$
                              !VALUES.F_NAN,'scat_f250',!VALUES.F_NAN,$
                              'scat_df250',!VALUES.F_NAN,$
                              'scat_f350',!VALUES.F_NAN,$
                              'scat_df350',!VALUES.F_NAN,$
                              'scat_f500',!VALUES.F_NAN,$
                              'scat_df500',!VALUES.F_NAN,base_cat)
  IF countrare GT 0 THEN $
     base_cat = CREATE_STRUCT('rare_name','','dist_rare',!VALUES.F_NAN,$
                              'ra_rare',!VALUES.F_NAN,'dec_rare',$
                              !VALUES.F_NAN,base_cat)
  final_cat = REPLICATE(base_cat,ngood)

  final_cat.x_sub = cat.x
  final_cat.y_sub = cat.y
  final_cat.ra_sub = TEMPORARY(ra)
  final_cat.dec_sub = TEMPORARY(dec)
  final_cat.f_sub = cat.f*1e3 ;;to mJy
  final_cat.df_sub = cat.df*1e3
  final_cat.corr = cat.corr
  ;;To mJy with map fluxes too
  flux250 *= 1e3 & flux_err250 *= 1e3
  flux350 *= 1e3 & flux_err350 *= 1e3
  flux500 *= 1e3 & flux_err500 *= 1e3
  final_cat.map_f250  = TEMPORARY(flux250)
  final_cat.map_df250 = TEMPORARY(flux_err250)
  final_cat.map_f350  = TEMPORARY(flux350)
  final_cat.map_df350 = TEMPORARY(flux_err350)
  final_cat.map_f500  = TEMPORARY(flux500)
  final_cat.map_df500 = TEMPORARY(flux_err500)
  
  cathdr = ['']
  SXADDPAR, cathdr, 'K1', coeffs[0], 'map250 coeff'
  SXADDPAR, cathdr, 'K2', coeffs[1], 'map350 coeff'
  SXADDPAR, cathdr, 'K3', coeffs[2], 'map500 coeff'
  IF ~ KEYWORD_SET(usemapmakererror) THEN BEGIN
     SXADDPAR, cathdr, 'MAPNOISE', 'F', "Use map maker provided noise"
  ENDIF ELSE $
     SXADDPAR, cathdr, 'MAPNOISE', 'T', "Use map maker provided noise"
  SXADDPAR, cathdr, 'SN', sn, 'Required S/N'
  IF N_ELEMENTS(mins500) THEN $
     SXADDPAR, cathdr, 'MINS500', mins500, 'Min allowed S_500 [mJy]'
  SXADDPAR, cathdr, 'BGSUBSZ', bgsubsize, 'Background grid size [arcsec]'
  SXADDPAR, cathdr, 'BGSUBSZP', bgsubsize_pix, 'Background grid size [pix]'
  SXADDPAR, cathdr, 'MINCORR', min_corr, 'Minimum correlation coeff'
  SXADDPAR, cathdr, 'CATRAD', catrad, 'Catalog match radius'
  IF ~ KEYWORD_SET(nosmooth) THEN BEGIN
     SXADDPAR, cathdr, 'SMOOTHED', 'T', "Additonal smoothing in sourcefind"
     IF KEYWORD_SET(optimal) THEN BEGIN
        SXADDPAR, cathdr, 'OPTFILT', 'T', "Optimal filtering used"
     ENDIF ELSE $
        SXADDPAR, cathdr, 'OPTFILT', 'F', "Optimal filtering used"
  ENDIF ELSE $
     SXADDPAR, cathdr, 'SMOOTHED', 'F', "Additonal smoothing in sourcefind"
  SXADDPAR, cathdr, "FWHM", fwhm, "Estimated FWHM in combined map"
  IF KEYWORD_SET(usepeakpos) THEN $
     SXADDPAR, cathdr, "PEAKPOS", "T", "Use map peak for position" ELSE $
        SXADDPAR, cathdr, "PEAKPOS", "F", "Use map peak for position"
  IF alg EQ 1 THEN SXADDPAR, cathdr, "PHOTALG", "FIND2", "Phot algorithm" ELSE $
     IF alg EQ 2 THEN SXADDPAR, cathdr, "PHOTALG", "STARFINDER", "Phot algorithm"
  
  IF countscat GT 0 THEN BEGIN
     final_cat[indred].scat_idx   = indscat
     final_cat[indred].ra_scat    = scat[indscat].inra
     final_cat[indred].dec_scat   = scat[indscat].indec
     ;;Scat is in mJy, so no change required
     final_cat[indred].scat_f250  = scat[indscat].f250
     final_cat[indred].scat_df250 = scat[indscat].e250
     final_cat[indred].scat_f350  = scat[indscat].f350
     final_cat[indred].scat_df350 = scat[indscat].e350
     final_cat[indred].scat_f500  = scat[indscat].f500
     final_cat[indred].scat_df500 = scat[indscat].e500
     gcirc, 2, final_cat[indred].ra_sub, final_cat[indred].dec_sub,$
            scat[indscat].inra, scat[indscat].indec, dist
     final_cat[indred].dist_scat = dist
     SXADDPAR, cathdr, 'SCAT', scatfile, 'SCAT catalog'
  ENDIF
  IF countrare GT 0 THEN BEGIN
     final_cat[indred2].rare_name  = name[indrare]
     final_cat[indred2].ra_rare    = rarera[indrare]
     final_cat[indred2].dec_rare   = raredec[indrare]
     gcirc, 2, final_cat[indred2].ra_sub, final_cat[indred2].dec_sub,$
            rarera[indrare], raredec[indrare], dist
     final_cat[indred2].dist_rare = dist
  ENDIF
  IF KEYWORD_SET(findpeakpix) THEN BEGIN
     final_cat.x_peak = TEMPORARY(peak_x)
     final_cat.y_peak = TEMPORARY(peak_y)
     final_cat.ra_peak = TEMPORARY(peak_ra)
     final_cat.dec_peak = TEMPORARY(peak_dec)
  ENDIF

  success = 1b
  RETURN, final_cat

  err_handler:
  IF KEYWORD_SET(verbose) THEN MESSAGE, errmsg, /INF
  success = 0b
  RETURN, !VALUES.F_NAN
END
