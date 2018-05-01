;+
;NAME
; redsim_find_and_compare
;PURPOSE
; Given an input set of maps and a red source catalog, smooths them 
; to the same resolution, forms linear combinations, searches them for 
; red sources, and compares them to the input catalog.
;USAGE
;  recovery_info = redsim_find_and_compare(mapbase, catfile, K1=k1,
;                                          K2=k2, DIR=dir, /VERBOSE, SN=sn,
;                                          CORR=corr, MATCHRAD=matchrad,
;                                          FLUXRAT=fluxrat, CATEXTN=catextn)
;INPUTS
; mapbase            Base map name
; catfile            Catalog name (FITS file)
;OPTIONAL INPUTS
; k1                 Vector of 250um map coefficients (def: -sqrt(1.0-0.92**2))
; k2                 Vector of 350um map coefficients (def: 0)
; conf               Confusion noise estimate for combined maps, in Jy.
;                     Note that only one value is used, which is
;                     incorrect in principle
; inst               Instrument noise estimate for combined maps, in
;                     Jy.  Like conf, only one value is used.
;                     Otherwise this is computed from the center region
;                     of the observed maps using default k values.
; dir                Directory to look for input maps and catalogs in.
;                     Default: the current directory
; SN                 Signal to noise for source finding (def: 4).  Can
;                     be array.
; minf500            Minimum f500 um flux cut.
; corr               Minimum correlation coefficient (def: 0.85)
; matchrad           Matching radius, in arcsec (def: 10.0)
; fluxrat            Require map fluxes of f350 >= fluxrat[0]*f250 and
;                     f500 >= fluxrat[1]*f350.  (def: [1.0, 1.0]).
;                     Can be array -- e.g. [[1.0, 1.0], [0.2, 1.0],
;                     [0.5, 1.2]] is three sets of flux ratios
; catextn            Catalog fits extension (def: 1)
; maxfalse           Maximum number of 'extra' sources that we keep
;                     track of the properties (flux densities) of.  Ideally,
;                     this will be more than the number found.
;KEYWORDS
; verbose            Print information messages as it runs 
; simple             Use simple (non-optimal) smoothing, a la RARE 70
;OPTIONAL OUTPUTS
; success            1 on success, 0 on failure
; errmsg             Error message on failure
;RETURNS
; A structure containing information about which sources were found.
; For each source there is a n_coeffs by n_sn by n_fluxrat set of
; arrays saying if the source was found, where it was found, and what
; fluxes were measured, as well as a summary of the input information.
;-

FUNCTION redsim_find_and_compare_bkg, map, bgsubsize
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

  RETURN, smap_bgestimator(map.image, mask, bgsubsize)
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Read in maps, add masks, smooth and compute backgrounds
;; We only call this once -- but it's conceptually easier to
;;                           split it off
;; A failing is that this means the optimum filter is computed using
;;  one set of noise values rather than specialized for each k triplet.
PRO redsim_find_and_compare_prepmap, mapbase, dir, conf, map250, map350,$
                                     map500, bkg250, bkg350, bkg500,$
                                     fwhm, SIMPLE=simple, VERBOSE=verbose,$
                                     MASKFILES=maskfiles, INST=inst,$
                                     MASKDIR=maskdir, BGSUBSIZE=bgsubsize,$
                                     SUCCESS=success, ERRMSG=errmsg

  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN

  success = 0b
  errmsg = ""

  IF N_ELEMENTS(bgsubsize) EQ 0 THEN bgsubsize = 192

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

  ;; Smoothing
  IF KEYWORD_SET(simple) THEN BEGIN
     ;; non-optimal filtering, as in RARE 70
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE," Smoothing maps", /INF
     smap_redsource_smooth, map250, /BRUTE, /NOERRORSMOOTH
     smap_redsource_smooth, map350, /BRUTE, /NOERRORSMOOTH
     smap_redsource_smooth, map500, /BRUTE, /NOERRORSMOOTH
     fwhm = SQRT(2.0) * 35.2
  ENDIF ELSE BEGIN
     ;; optimal filtering
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE," Optimally filtering maps", /INF
     matched_filter_red, map250, map350, map500, conf, INST=inst, BEAM=beam
     
     ;; estimate FWHM.  Clip down beam so this is faster
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
  ENDELSE
  IF KEYWORD_SET(verbose) THEN $
     MESSAGE,STRING(fwhm,FORMAT='("  Estimated FWHM: ",F0.2, " arcsec")'), /INF

  ;; Get background maps
  bgsubsize_pix = ROUND(bgsubsize / map250.pixscale)
  bkg250 = redsim_find_and_compare_bkg(map250, bgsubsize_pix)
  bkg350 = redsim_find_and_compare_bkg(map350, bgsubsize_pix)
  bkg500 = redsim_find_and_compare_bkg(map500, bgsubsize_pix)
  
  success = 1b

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION redsim_find_and_compare, mapbase, catfile, K1=k1, K2=k2, $
                                  CONF=conf, DIR=dir, VERBOSE=verbose,$
                                  SUCCESS=sucess, ERRMSG=errmsg, SN=sn,$
                                  MINF500=minf500, CORR=corr, $
                                  MATCHRAD=matchrad, FLUXRAT=fluxrat, $
                                  SIMPLE=simple, MASKFILES=maskfiles, $
                                  MASKDIR=maskdir, INST=inst, $
                                  MAXFALSE=maxfalse, USEPEAKPOS=usepeakpos

  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg = ""

  IF N_ELEMENTS(k1) EQ 0 THEN k1 = [-SQRT(1.0 - 0.92^2)]
  ncoeffs = N_ELEMENTS(k1)
  IF N_ELEMENTS(k2) EQ 0 THEN k2 = REPLICATE(0.0, ncoeffs)
  IF N_ELEMENTS(k2) NE ncoeffs THEN BEGIN
     errmsg = "Number of k2 coeffs don't match k1 coeffs"
     GOTO, err_handler
  ENDIF
  ksum = k1^2 + k2^2
  IF MAX(ksum, /ABS) GT 1.0 THEN BEGIN
     errmsg = "Normalization error for k coefficients"
     GOTO, err_handler
  ENDIF
  k3 = SQRT(1.0 - ksum)

  IF N_ELEMENTS(minf500) EQ 0 THEN minf500 = 0.0
  IF minf500 LT 0 THEN MESSAGE,"Invalid minf500"

  IF N_ELEMENTS(conf) EQ 0 THEN conf = 4.2e-3
  IF conf LT 0.0 THEN BEGIN
     errmsg = "Invalid (negative) confusion noise"
     GOTO, err_handler
  ENDIF

  IF N_ELEMENTS(sn) EQ 0 THEN sn = [4.0]
  nsn = N_ELEMENTS(sn)
  IF MIN(sn) LE 0.0 THEN BEGIN
     errmsg = "Invalid (non-positive) S/N requirement"
     GOTO, err_handler
  ENDIF
  
  IF N_ELEMENTS(fluxrat) EQ 0 THEN fluxrat = [1.0, 1.0]
  szfr = SIZE(fluxrat)
  IF szfr[0] EQ 1 THEN BEGIN
     ;; 1D
     IF szfr[1] NE 2 THEN BEGIN
        errmsg = "Unexpected size of fluxrat"
        GOTO, err_handler
     ENDIF
     nfluxrat = 1
  ENDIF ELSE BEGIN
     IF szfr[0] NE 2 THEN BEGIN
        errmsg = "Unexpected dimension for fluxrat"
        GOTO, err_handler
     ENDIF
     IF szfr[1] NE 2 THEN BEGIN
        errmsg = "Unexpected size of fluxrat"
        GOTO, err_handler
     ENDIF
     nfluxrat = szfr[2]
  ENDELSE
  
  IF N_ELEMENTS(corr) EQ 0 THEN corr = 0.85
  IF corr LE 0.0 THEN BEGIN
     errmsg = "Invalid (non-positive) minimum correlation"
     GOTO, err_handler
  ENDIF
  
  IF N_ELEMENTS(matchrad) EQ 0 THEN matchrad = 10.0
  IF matchrad LE 0.0 THEN BEGIN
     errmsg = "Invalid (non-positive) matching radius"
     GOTO, err_handler
  ENDIF

  IF N_ELEMENTS(bgsubsize) EQ 0 THEN bgsubsize = 48
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
  cat = MRDFITS(catfilename, catextn, /SILENT, STATUS=rdst)
  IF rdst NE 0 THEN BEGIN
     errmsg = "Error reading catalog file " + catfile + " as FITS file"
     GOTO, err_handler
  ENDIF

  ncat = N_ELEMENTS(cat)
  IF ncat EQ 0 THEN BEGIN
     errmsg = "No red sources in catalog from " + catfile
     GOTO, err_handler
  ENDIF

  ;; Make sure we have information we expect from catalog
  reqtags = ['x', 'y', 'ra', 'dec', 'f250', 'f350', 'f500']
  FOR i=0, N_ELEMENTS(reqtags)-1 DO BEGIN
     IF ~ TAG_EXIST(cat, reqtags[i], /TOP_LEVEL) THEN BEGIN
        errmsg = "Unable to find required tag: "+ reqtags[i] + " in catalog"
        GOTO, err_handler
     ENDIF
  ENDFOR
  
  ;; If not set, make maxfalse equal 2x the the number of real ones
  IF N_ELEMENTS(maxfalse) EQ 0 THEN maxfalse = 2*ncat
  IF maxfalse EQ 0 THEN MESSAGE, "Maxfalse should be positive"

  ;; Read in the maps and smooth
  IF KEYWORD_SET(verbose) THEN $
     MESSAGE," Reading in initial maps", /INF
  redsim_find_and_compare_prepmap, mapbase, dir, conf, map250, map350,$
                                   map500, bkg250, bkg350, bkg500, fwhm, $
                                   SIMPLE=simple, MASKFILES=maskfiles,$
                                   MASKDIR=maskdir, SUCCESS=psuccess,$
                                   ERRMSG=perrmsg, BGSUBSIZE=bgsubsize,$
                                   INST=inst, VERBOSE=verbose
  IF psuccess EQ 0 THEN BEGIN
     errmsg = "Unable to read in and prepare maps: " + perrmsg
     GOTO, err_handler
  ENDIF

  ;; Set simple phot box size based on FWHM
  npix_simple = ROUND(0.7 * fwhm / map500.pixscale) > 3
  IF npix_simple MOD 2 EQ 0 THEN npix_simple += 1
  
  ;; We can't do sources too near the edge (smap_simplephot doesn't
  ;; like them) so clip those out
  hfsz = npix_simple / 2
  wkeep = WHERE(cat.x GE hfsz AND cat.y LE map250.xsize - hfsz - 1 AND $
                cat.y GE hfsz AND cat.y LE map250.ysize - hfsz - 1, nkeep,$
                NCOMPLEMENT=nedge)
  IF nkeep EQ 0 THEN BEGIN
     err_msg = "All actual red sources too close to edge to find"
     GOTO, err_handler
  ENDIF
  IF nedge NE 0 THEN BEGIN
     cat = cat[wkeep]
     ncat = nkeep
  ENDIF

  IF KEYWORD_SET(verbose) THEN $
     MESSAGE,STRING(ncat, FORMAT='("Number of actual red sources: ", I0)'),/INF

  ;; Now construct the catalog return structure
  retstr = {matchrad: matchrad, $
            input_x: cat.x, input_y: cat.y,$
            input_ra: cat.ra, input_dec: cat.dec, input_f250: cat.f250,$
            input_f350: cat.f350, input_f500: cat.f500, $
            ncoeffs: ncoeffs, k1: k1, k2: k2, k3: k3, $
            nsn: nsn, sn: sn, minf500: minf500, $
            nfluxrat: nfluxrat, fluxrat: fluxrat, corr: corr, $
            found: BYTARR(ncoeffs, nsn, nfluxrat, ncat),$
            nfound: BYTARR(ncoeffs, nsn, nfluxrat),$
            fsub: FLTARR(ncoeffs, nsn, nfluxrat, ncat), $
            f250: FLTARR(ncoeffs, nsn, nfluxrat, ncat), $
            f350: FLTARR(ncoeffs, nsn, nfluxrat, ncat), $
            f500: FLTARR(ncoeffs, nsn, nfluxrat, ncat),$
            nextra: INTARR(ncoeffs, nsn, nfluxrat),$
            fsub_extra: FLTARR(ncoeffs, nsn, nfluxrat, maxfalse), $
            f250_extra: FLTARR(ncoeffs, nsn, nfluxrat, maxfalse), $
            f350_extra: FLTARR(ncoeffs, nsn, nfluxrat, maxfalse), $
            f500_extra: FLTARR(ncoeffs, nsn, nfluxrat, maxfalse)}

  ;; Source finding loop.  We do something slightly tricky here --
  ;; which is to -not- call red_sourcefind_dofind because it does some
  ;; of the steps every time that we can share between iterations
  ;; (like combining the maps).
  fmt1 = '(" Doing k1: ",F0.3, " k2: ", F0.3, " k3: ", F0.3, " combination")'
  FOR i=0, ncoeffs-1 DO BEGIN
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE, STRING(k1[i], k2[i], k3[i], FORMAT=fmt1), /INF

     ;; Invalid coeffs
     IF k1[i]^2 + k2[i]^2 + k3[i]^2 GT 1.0 THEN CONTINUE

     ;; Combine maps, form background
     mapcomb = linear_map_combine(k1[i], map250, k2[i], map350, k3[i], map500)
     bkgcomb = redsim_find_and_compare_bkg(mapcomb, bgsubsize)

     FOR j=0, nsn-1 DO BEGIN
        curr_sn = sn[j]
        
        curr_cat = ps_extractor_doband(mapcomb, curr_sn, mapbase+'_starfndr',$
                                       ALG=2, FWHM=fwhm, PRE_SMOOTH=0b,$
                                       MIN_CORR=min_corr, $
                                       MAPMASKBITS=1uL,$
                                       BGINPUT=bgcomb, MASKCHECKSZ=5, $
                                       SUCCESS=success_doband)
        IF success_doband THEN BEGIN
           ;; Found some sources -- further processing required

           ;; Now -- potentially use the peak positions
           IF KEYWORD_SET(usepeakpos) THEN BEGIN
              peaksearchrad = 12.0 ;; arcsec
              pixsearchrad = ROUND(peaksearchrad / mapcomb.pixscale) > 1
              maxx = mapcomb.xsize-1
              maxy = mapcomb.ysize-1
              FOR idx=0, N_ELEMENTS(curr_cat)-1 DO BEGIN
                 xmid = ROUND(curr_cat[idx].x)
                 ymid = ROUND(curr_cat[idx].y)
                 xbot = (xmid - pixsearchrad) > 0
                 xtop = (xmid + pixsearchrad) < maxx
                 ybot = (ymid - pixsearchrad) > 0
                 ytop = (ymid + pixsearchrad) < maxy
        
                 maxval = MAX(mapcomb.image[xbot:xtop,ybot:ytop], wmax)
                 nxs = xtop - xbot + 1
                 curr_cat[idx].x = (wmax MOD nxs) + xbot
                 curr_cat[idx].y = (wmax / nxs) + ybot
              ENDFOR
           ENDIF
           
           ;; We have to clip out all sources found near
           ;; the edge, since we clipped them out of the catalog
           wkeep = WHERE(curr_cat.x GE hfsz AND $
                         curr_cat.x LE map250.xsize - hfsz - 1 AND $
                         curr_cat.y GE hfsz AND $
                         curr_cat.y LE map250.ysize - hfsz - 1, nkeep,$
                         NCOMPLEMENT=nedge)
           IF nkeep EQ 0 THEN BEGIN
              IF KEYWORD_SET(verbose) THEN $
                 MESSAGE,"  All found sources too near edge", /INF
              CONTINUE
           ENDIF
           IF nedge NE 0 THEN curr_cat = curr_cat[wkeep]
           nfound = N_ELEMENTS(curr_cat)

           ;; First, we need to get map based fluxes
           flux250 = smap_simplephot(map250, curr_cat.x, curr_cat.y, $
                                     FWHM=fwhm, /USEMASK, NPIX=npix_simple, $
                                     BGRND=bkg250, /XYPOS)
           flux350 = smap_simplephot(map350, curr_cat.x, curr_cat.y, $
                                     FWHM=fwhm, /USEMASK, NPIX=npix_simple, $
                                     BGRND=bkg350, /XYPOS)
           flux500 = smap_simplephot(map500, curr_cat.x, curr_cat.y, $
                                     FWHM=fwhm, /USEMASK, NPIX=npix_simple, $
                                     BGRND=bkg500, /XYPOS)

           ;; cut on 500
           IF minf500 GT 0 THEN BEGIN
              wkeepf500 = WHERE(flux500 GE minf500, nkeepf500, $
                                NCOMPLEMENT=nremovef500)
              IF nkeepf500 EQ 0 THEN CONTINUE
              IF nremovef500 NE 0 THEN BEGIN
                 flux250 = flux250[wkeepf500]
                 flux350 = flux350[wkeepf500]
                 flux500 = flux500[wkeepf500]
                 nfound = nkeepf500
              ENDIF
           END
           
           ;; ra/dec for matching
           xy2ad, curr_cat.x, curr_cat.y, mapcomb.astrometry, ra, dec

           ;; Now apply flux ratio cuts
           FOR k = 0, nfluxrat-1 DO BEGIN
              wremove = BYTARR(nfound)
              cut1 = fluxrat[0, k]
              IF FINITE(cut1) AND cut1 GT 0.0 THEN BEGIN
                 wbad = WHERE(flux350 LT cut1 * flux250, nbad)
                 IF nbad NE 0 THEN wremove[TEMPORARY(wbad)] = 1b
              ENDIF
              cut2 = fluxrat[1, k]
              IF FINITE(cut2) AND cut2 GT 0.0 THEN BEGIN
                 wbad = WHERE(flux500 LT cut2 * flux350, nbad)
                 IF nbad NE 0 THEN wremove[TEMPORARY(wbad)] = 1b
              ENDIF
              wgood = WHERE(wremove EQ 0b, ngood, NCOMPLEMENT=nbad)

              IF ngood EQ 0 THEN CONTINUE ;; no red sources found, moving on
             
              ;; Some red sources found -- we have to match against
              ;; the input catalog
              fr_sub = curr_cat[wgood].f
              fr_ra = ra[wgood]
              fr_dec = dec[wgood]
              fr_f250 = flux250[wgood]
              fr_f350 = flux350[wgood]
              fr_f500 = flux500[wgood]
              
              SRCOR, TEMPORARY(fr_ra), TEMPORARY(fr_dec), $
                     cat.ra, cat.dec, matchrad, idx_fr, $
                     idx_cat, OPTION=1, SPHERICAL=2, COUNT=countmatch, /SILENT

              retstr.nfound[i, j, k] = countmatch
              IF countmatch GT 0 THEN BEGIN
                 ;; and some of them even match!
                 retstr.found[i, j, k, idx_cat] = 1b
                 retstr.fsub[i, j, k, idx_cat] = fr_sub[idx_fr]
                 retstr.f250[i, j, k, idx_cat] = fr_f250[idx_fr]
                 retstr.f350[i, j, k, idx_cat] = fr_f350[idx_fr]
                 retstr.f500[i, j, k, idx_cat] = fr_f500[idx_fr]
              ENDIF
              ;; Things found that don't match the input catalog
              nextra = ngood - countmatch
              retstr.nextra[i, j, k] = nextra
              IF nextra GT 0 THEN BEGIN
                 ;; Process extra detections -- build index into the fr
                 ;;  variables of these 'extra' sources
                 marr = BYTARR(ngood) ;; ngood is the number of fr elements
                 IF (countmatch GT 0) THEN marr[idx_fr] = 1b    
                 idx_nomatch = WHERE(marr EQ 0, nadd) ;; So the 0s are not matched
                 IF nadd NE nextra THEN $
                    MESSAGE, STRING(ngood, countmatch, nextra, nadd, $
                                    FORMAT='("Logical error: found: '+$
                                    '",I0," matched: ",I0," extra: ",I0,'+$
                                    '" got: ",I0)')
                 IF nextra GT maxfalse THEN BEGIN
                    fmt = '("Found more sources (",I0," than there is room'+$
                          ' to store (",I0,")")'
                    MESSAGE, STRING(nextra, maxfalse, FORMAT=fmt), /INF
                    nextra = maxfalse
                    idx_nomatch = idx_nomatch[0:maxfalse-1]
                 ENDIF
                 retstr.fsub_extra[i, j, k, 0:nextra-1] = fr_sub[idx_nomatch]
                 retstr.f250_extra[i, j, k, 0:nextra-1] = fr_f250[idx_nomatch]
                 retstr.f350_extra[i, j, k, 0:nextra-1] = fr_f350[idx_nomatch]
                 retstr.f500_extra[i, j, k, 0:nextra-1] = fr_f500[idx_nomatch]
              ENDIF
           ENDFOR
        ENDIF
     ENDFOR
  ENDFOR

  success = 1b
  RETURN, retstr

  err_handler:
  IF KEYWORD_SET(verbose) THEN MESSAGE, errmsg, /INF
  success = 0b
  RETURN, !VALUES.F_NAN

END
