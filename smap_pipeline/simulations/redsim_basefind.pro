;+
;NAME
; redsim_basefind
;PURPOSE
; Given an input set of maps and a red source catalog, forms linear
; combinations, searches them for red sources, and compares them to
; the input catalog.  This is similar to redsim_find_and_compare but
; tries to output all the sources it finds without much in the way of cuts
; so that the output can be fed to machine learning algorithms.  It also
; does not do multiple ks, SNs, etc, and the input maps should already
; be smoothed.  It also does forced photometry at the positions of
; sources that were not found from the input catalog.
;USAGE
;  recovery_info = redsim_basefind(mapbase, catfile, K1=k1, K2=k2,
;                                  DIR=dir, /VERBOSE, SN=sn, CORR=corr,
;                                  MATCHRAD=matchrad, CATEXTN=catextn)
;INPUTS
; mapbase            Base map name
; catfile            Catalog name (FITS file)
;OPTIONAL INPUTS
; fwhm               FWHM of maps.  If not present, is read from map header
; k1                 250um map coefficient (def: -sqrt(1.0-0.92**2))
; k2                 350um map coefficient (def: 0)
; dir                Directory to look for input maps and catalogs in.
;                     Default: the current directory
; SN                 Signal to noise for source finding (def: 4)
; corr               Minimum correlation coefficient (def: 0.8)
; mins500            Minimum allowed S_500um [mJy]
; matchrad           Matching radius, in arcsec (def: 14.0)
; catextn            Catalog fits extension (def: 1)
;KEYWORDS
; verbose            Print information messages as it runs 
;OPTIONAL OUTPUTS
; success            1 on success, 0 on failure
; errmsg             Error message on failure
;RETURNS
; A structure containing information about the red sources in the map,
; found or not.
;-

FUNCTION redsim_basefind_bkg, map, bgsubsize
  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN

  mask = BYTARR(map.xsize, map.ysize)
  IF map.has_mask THEN BEGIN
     IF map.has_exposure THEN BEGIN
        wmask = WHERE(map.mask NE 0 OR map.exposure LE 0.0, nmask)
     ENDIF ELSE BEGIN
        wmask = WHERE(map.mask NE 0, nmask)
     ENDELSE
  ENDIF ELSE IF map.has_exposure THEN BEGIN
     wmask = WHERE(map.exposure LE 0.0, nmask)
  ENDIF ELSE nmask = 0
  IF nmask NE 0 THEN mask[wmask]=1b

  bgsubsize_pix = ROUND(bgsubsize / map.pixscale)
  RETURN, smap_bgestimator(map.image, mask, bgsubsize_pix)
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Read in maps, add masks, smooth and compute backgrounds
;; We only call this once -- but it's conceptually easier to
;;                           split it off
;; Also figures out the FWHM by reading it from the maps
PRO redsim_basefind_prepmap, mapbase, dir, map250, map350,$
                             map500, bkg250, bkg350, bkg500,$
                             bgsubsize, VERBOSE=verbose,$
                             MASKFILES=maskfiles, MASKDIR=maskdir, $
                             SUCCESS=success, ERRMSG=errmsg, FWHM=fwhm

  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN

  success = 0b
  errmsg = ""

  ;;Read in base maps
  map250 = READ_SMAP_FITSMAP(mapbase, 'PSW', DIR=dir, $
                             /NO_ABORT, SUCCESS=msuccess, ERRMSG=merrmsg,$
                             /SILENT)
  IF msuccess EQ 0 THEN BEGIN
     errmsg = "Error opening 250um map: " + merrmsg
     RETURN
  ENDIF
  map350 = READ_SMAP_FITSMAP(mapbase, 'PMW', DIR=dir, $
                             /NO_ABORT, SUCCESS=msuccess, ERRMSG=merrmsg,$
                             /SILENT)
  IF msuccess EQ 0 THEN BEGIN
     errmsg = "Error opening 350um map: " + merrmsg
     RETURN
  ENDIF
  map500 = READ_SMAP_FITSMAP(mapbase, 'PLW', DIR=dir, $
                             /NO_ABORT, SUCCESS=msuccess, ERRMSG=merrmsg,$
                             /SILENT)
  IF msuccess EQ 0 THEN BEGIN
     errmsg = "Error opening 500um map: " + merrmsg
     RETURN
  ENDIF

  ;; Check sizes
  IF map250.xsize NE map350.xsize OR $
     map250.ysize NE map350.ysize THEN BEGIN
     errmsg = "Input 250, 350 maps not same size"
     RETURN
  ENDIF
  IF ABS((map250.pixscale - map350.pixscale)/map250.pixscale) $
     GT 1d-3 THEN BEGIN
     errmsg = "Input 250, 350 micron maps don't have same pixel scale"
     RETURN
  ENDIF
  IF map250.xsize NE map500.xsize OR $
     map250.ysize NE map500.ysize THEN BEGIN
     errmsg = "Input maps not same size"
     RETURN
  ENDIF
  IF ABS((map250.pixscale - map500.pixscale)/map250.pixscale) $
     GT 1d-3 THEN BEGIN
     errmsg = "Input 250, 500 micron maps don't have same pixel scale"
     RETURN
  ENDIF

  ;; Get FWHM
  hdr = HEADFITS(ADDSLASH(dir) +mapbase + "_PLW.fits", EXTEN=1)
  fwhm = SXPAR(hdr, 'FWHM', COUNT=nfwhm)
  IF nfwhm NE 1 THEN fwhm = !VALUES.F_NAN
  
  ;; Add user specified masks if present
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

  ;; Get background maps
  bkg250 = redsim_basefind_bkg(map250, bgsubsize)
  bkg350 = redsim_basefind_bkg(map350, bgsubsize)
  bkg500 = redsim_basefind_bkg(map500, bgsubsize)
  
  success = 1b

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION redsim_basefind, mapbase, catfile, K1=k1, K2=k2, FWHM=fwhm, $
                          DIR=dir, VERBOSE=verbose, SUCCESS=sucess, $
                          ERRMSG=errmsg, SN=sn, CORR=corr, MINS500=mins500, $
                          MATCHRAD=matchrad, MASKFILES=maskfiles, $
                          MASKDIR=maskdir, USEPEAKPOS=usepeakpos

  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg = ""

  IF N_ELEMENTS(k1) EQ 0 THEN k1 = -SQRT(1.0 - 0.92^2)
  IF N_ELEMENTS(k2) EQ 0 THEN k2 = 0.0
  ksqsum = k1^2 + k2^2
  IF ksqsum GT 1.0 THEN BEGIN
     errmsg = "Normalization error for k coefficients"
     GOTO, err_handler
  ENDIF
  k3 = SQRT(1.0 - ksqsum)

  IF N_ELEMENTS(sn) EQ 0 THEN sn = 4.0
  IF sn LE 0.0 THEN BEGIN
     errmsg = "Invalid (non-positive) S/N requirement"
     GOTO, err_handler
  ENDIF

  fluxrat = [0.3, 0.6]

  IF N_ELEMENTS(corr) EQ 0 THEN corr = 0.80
  IF corr LE 0.0 THEN BEGIN
     errmsg = "Invalid (non-positive) minimum correlation"
     GOTO, err_handler
  ENDIF
  
  IF N_ELEMENTS(matchrad) EQ 0 THEN matchrad = 14.0
  IF matchrad LE 0.0 THEN BEGIN
     errmsg = "Invalid (non-positive) matching radius"
     GOTO, err_handler
  ENDIF

  IF N_ELEMENTS(bgsubsize) EQ 0 THEN bgsubsize = 192 ;; in arcsec
  IF bgsubsize LE 0 THEN BEGIN
     errmsg = "Invalid (non-positive) background sub size"
     GOTO, err_handler
  ENDIF

  IF N_ELEMENTS(dir) EQ 0 THEN dir='./' ELSE dir = ADDSLASH(dir)
  IF N_ELEMENTS(catextn) EQ 0 THEN catextn = 1

  ;; Read in the input catalog -- we need to do this first so we know
  ;; how many sources we are dealing with
  catfilename = dir + catfile
  IF ~ FILE_TEST(catfilename, /READ) THEN BEGIN
     errmsg = "Unable to read catalog file: " + catfile
     GOTO, err_handler
  ENDIF
  true_cat = MRDFITS(catfilename, catextn, /SILENT, STATUS=rdst)
  IF rdst NE 0 THEN BEGIN
     errmsg = "Error reading catalog file " + catfile + " as FITS file"
     GOTO, err_handler
  ENDIF

  ntrue_cat = N_ELEMENTS(true_cat)
  IF ntrue_cat EQ 0 THEN BEGIN
     errmsg = "No red sources in catalog from " + catfile
     GOTO, err_handler
  ENDIF

  ;; Make sure we have information we expect from catalog
  reqtags = ['x', 'y', 'ra', 'dec', 'f250', 'f350', 'f500']
  FOR i=0, N_ELEMENTS(reqtags)-1 DO BEGIN
     IF ~ TAG_EXIST(true_cat, reqtags[i], /TOP_LEVEL) THEN BEGIN
        errmsg = "Unable to find required tag: "+ reqtags[i] + " in catalog"
        GOTO, err_handler
     ENDIF
  ENDFOR
  
  ;; Read in the maps and compute their backgrounds, add masks
  IF KEYWORD_SET(verbose) THEN $
     MESSAGE," Reading in initial maps", /INF
  redsim_basefind_prepmap, mapbase, dir, map250, map350,$
                           map500, bkg250, bkg350, bkg500, $
                           bgsubsize, MASKFILES=maskfiles, $
                           MASKDIR=maskdir, VERBOSE=verbose,$
                           SUCCESS=psuccess, ERRMSG=perrmsg,$
                           FWHM=mapfwhm
  IF psuccess EQ 0 THEN BEGIN
     errmsg = "Unable to read in and prepare maps: " + perrmsg
     GOTO, err_handler
  ENDIF
  IF N_ELEMENTS(fwhm) EQ 0 THEN BEGIN
     IF ~ FINITE(mapfwhm) THEN BEGIN
        errmsg = "Couldn't read FWHM from maps, no user provided value"
        GOTO, err_handler
     ENDIF
     fwhm = mapfwhm
  ENDIF
  IF fwhm LT 0.0 THEN BEGIN
     errmsg = STRING(fwhm, FORMAT='("Invalid (non-positive) FWHM: ",F0.3)')
     GOTO, err_handler
  ENDIF

  ;; Set simple phot box size based on FWHM
  npix_simple = ROUND(0.7 * fwhm / map500.pixscale) > 3
  IF npix_simple MOD 2 EQ 0 THEN npix_simple += 1
  
  ;; We can't do sources too near the edge (smap_simplephot doesn't
  ;; like them) so clip those out of the true catalog
  hfsz = npix_simple / 2
  wkeep = WHERE(true_cat.x GE hfsz AND $
                true_cat.x LE map250.xsize - hfsz - 1 AND $
                true_cat.y GE hfsz AND $
                true_cat.y LE map250.ysize - hfsz - 1, nkeep,$
                NCOMPLEMENT=nedge)
  IF nkeep EQ 0 THEN BEGIN
     err_msg = "All actual red sources too close to edge to find"
     GOTO, err_handler
  ENDIF
  IF nedge NE 0 THEN BEGIN
     true_cat = true_cat[wkeep]
     ntrue_cat = nkeep
  ENDIF

  ;; Find sources using inner redsource finder; note they are
  ;; already smoothed and masked.
  IF KEYWORD_SET(verbose) THEN $
     MESSAGE," Finding red sources in map", /INF
  redcat = red_sourcefind_dofind(map250, map350, map500, BASENAME=mapbase,$
                                 COEFFS=[k1, k2, k3], /NOSMOOTH, $
                                 BGSUBSIZE=bgsubsize, SN=sn, MIN_CORR=corr, $
                                 MINS500=mins500, FLUXRAT=fluxrat, $
                                 /NOWRITE, SUCCESS=redcat_success, $
                                 ERRMSG=redcat_errmsg, FWHM=fwhm,$
                                 PEAKSEARCHRAD=12.0, USEPEAKPOS=usepeakpos, $
                                 ALG=2, MAPCOMB=mapcomb)

  ;; Figure out what we need to do forced photometry of,
  ;; and how many extra sources we found
  true_found = BYTARR(ntrue_cat)
  nextra = 0
  nfound = 0
  IF redcat_success THEN BEGIN
     ;; Found some sources -- further processing required
     ;; We need to figure out which ones exist in the input catalog
     nfound = N_ELEMENTS(redcat)
     IF KEYWORD_SET(usepeakpos) THEN BEGIN
        red_ra = redcat.ra_peak
        red_dec = redcat.dec_peak
     ENDIF ELSE BEGIN
        red_ra = redcat.ra_sub
        red_dec = redcat.dec_sub
     ENDELSE

     ;; Figure out which ones correspond to true sources
     SRCOR, TEMPORARY(red_ra), TEMPORARY(red_dec),$
            true_cat.ra, true_cat.dec, matchrad, idx_red, $
            idx_true, OPTION=1, SPHERICAL=2, COUNT=ntrue_found, /SILENT

     IF ntrue_found GT 0 THEN true_found[idx_true] = 1b
     nextra = N_ELEMENTS(redcat) - ntrue_found
  ENDIF

  ;; Now we know how many total we have, so set up output
  n_output = ntrue_cat + nextra
  retstr = {red: 0b, found: 0b,$
            x: !VALUES.F_NAN, y: !VALUES.F_NAN,$
            f_sub: !VALUES.F_NAN, df_sub: !VALUES.F_NAN,$
            map_f250: !VALUES.F_NAN, map_f350: !VALUES.F_NAN,$
            map_f500: !VALUES.F_NAN, corr: !VALUES.F_NAN,$
            input_x: !VALUES.F_NAN, input_y: !VALUES.F_NAN,$
            input_f250: !VALUES.F_NAN, input_f350: !VALUES.F_NAN,$
            input_f500: !VALUES.F_NAN}
  outstr = REPLICATE(retstr, n_output)

  ;; Put found sources in first entries, since indexing in
  ;; the true values is easier.  This automatically
  ;; adds the extra sources
  IF nfound GT 0 THEN BEGIN
     outstr[0: nfound-1].found = 1b
     IF KEYWORD_SET(usepeakpos) THEN BEGIN
        outstr[0: nfound-1].x = redcat.x_peak
        outstr[0: nfound-1].y = redcat.y_peak
     ENDIF ELSE BEGIN
        outstr[0: nfound-1].x = redcat.x_sub
        outstr[0: nfound-1].y = redcat.y_sub
     ENDELSE
     outstr[0:nfound-1].f_sub = redcat.f_sub  ;; Already in mJy
     outstr[0:nfound-1].df_sub = redcat.df_sub
     outstr[0:nfound-1].map_f250 = redcat.map_f250
     outstr[0:nfound-1].map_f350 = redcat.map_f350
     outstr[0:nfound-1].map_f500 = redcat.map_f500
     outstr[0:nfound-1].corr = redcat.corr

     ;; Now add info for those that were real
     IF ntrue_found GT 0 THEN BEGIN
        ;; Must get them in the same order
        outstr[idx_red].red = 1b
        outstr[idx_red].input_x = true_cat[idx_true].x
        outstr[idx_red].input_y = true_cat[idx_true].y
        outstr[idx_red].input_f250 = 1e3 * true_cat[idx_true].f250 ;; to mJy
        outstr[idx_red].input_f350 = 1e3 * true_cat[idx_true].f350
        outstr[idx_red].input_f500 = 1e3 * true_cat[idx_true].f500
     ENDIF
  ENDIF
  
  
  ;; Do forced photometry on extra sources
  wtrue_notfound = WHERE(~TEMPORARY(true_found), ntrue_notfound)
  IF ntrue_notfound GT 0 THEN BEGIN
     IF ntrue_notfound + nfound NE n_output THEN $
        MESSAGE, "Logic error in output catalog # entries"

     IF KEYWORD_SET(verbose) THEN BEGIN
        msg = STRING(ntrue_notfound, $
                     FORMAT='(" Forced phot of ", I0, " sources")')
        MESSAGE, msg, /INF
     ENDIF
          
     ;; First store input information
     true_notfound_cat = true_cat[TEMPORARY(wtrue_notfound)]
     outstr[nfound:*].red = 1b
     outstr[nfound:*].input_x = true_notfound_cat.x
     outstr[nfound:*].input_y = true_notfound_cat.y
     outstr[nfound:*].input_f250 = 1e3 * true_notfound_cat.f250
     outstr[nfound:*].input_f350 = 1e3 * true_notfound_cat.f350
     outstr[nfound:*].input_f500 = 1e3 * true_notfound_cat.f500

     ;; Now do forced phot
     npix_simple = ROUND(0.7 * fwhm / map500.pixscale)
     IF npix_simple MOD 2 EQ 0 THEN npix_simple += 1
     npix_simple >= 3
     flux250 = smap_simplephot(TEMPORARY(map250), true_notfound_cat.x,$
                               true_notfound_cat.y, /XYPOS, FWHM=fwhm, $
                               /USEMASK, NPIX=npix_simple, $
                               BGRND=TEMPORARY(bkg250))
     flux350 = smap_simplephot(TEMPORARY(map350), true_notfound_cat.x,$
                               true_notfound_cat.y, /XYPOS, FWHM=fwhm, $
                               /USEMASK, NPIX=npix_simple, $
                               BGRND=TEMPORARY(bkg250))
     flux500 = smap_simplephot(TEMPORARY(map500), true_notfound_cat.x,$
                               true_notfound_cat.y, /XYPOS, FWHM=fwhm, $
                               /USEMASK, NPIX=npix_simple, $
                               BGRND=TEMPORARY(bkg250))
     ;; Note the combined map is already bkg subtracted
     f_sub = smap_simplephot(TEMPORARY(mapcomb), true_notfound_cat.x,$
                             true_notfound_cat.y, /XYPOS, FWHM=fwhm, $
                             /USEMASK, NPIX=npix_simple)
     
     ;; Add that info
     outstr[nfound:*].map_f250 = 1e3 * TEMPORARY(flux250)  ;; To mJy
     outstr[nfound:*].map_f350 = 1e3 * TEMPORARY(flux350)
     outstr[nfound:*].map_f500 = 1e3 * TEMPORARY(flux500)
     outstr[nfound:*].f_sub = 1e3 * TEMPORARY(f_sub)
     IF nfound NE 0 THEN $
        outstr[nfound:*].df_sub = MEAN(outstr[0:nfound-1].df_sub)
  ENDIF

  IF KEYWORD_SET(verbose) THEN BEGIN
     fmt = '("  Ntrue: ",I0," Nfound: ",I0, " Ntrue_found: ",I0,'+$
           '" Ntrue_notfound: ",I0," Nextra: ",I0)'
     msg = STRING(ntrue_cat, nfound, ntrue_found, ntrue_notfound,$
                  nextra, FORMAT=fmt)
     MESSAGE, " Source finding results:", /INF
     MESSAGE, msg, /INF
  ENDIF
  
  success = 1b
  RETURN, outstr

  err_handler:
  IF KEYWORD_SET(verbose) THEN MESSAGE, errmsg, /INF
  success = 0b
  RETURN, !VALUES.F_NAN

END
