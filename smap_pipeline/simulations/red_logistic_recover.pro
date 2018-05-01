;+
;NAME
; red_logistic_recover
;PURPOSE
; Recovery simulations of a specified field using match filtered
; maps and a logistic model for source selection.
;USAGE
; red_logistic_recover, basename, fluxes, outname, $
;                       MAPDIR=, NSIMS=, /VERBOSE,$
;                       MASKFILES=, MASKDIR=,$
;                       MATCHRAD=, SEED=, SOURCEDENS=]
;INPUTS
; basename            Base name of input maps; note this should
;                      be the base red map pre-smoothing.
; fluxes              Triplet of injected 250/350/500 flux densities Jy
; outname             Name of output file
;OPTIONAL INPUTS
; mapdir              Directory of input maps (def: ./)
; maskdir             Directory to look for masks in
; maskfiles           Name of mask files; can be a list
; nsims               Number of simulations to do (def: 100)
; matchrad            Matching radius in arcsec to consider input and
;                      recovered sources matching (def: 14)
; seed                Random number seed
; sourcedens          Density of injected sources per sq deg (def: 25)
;KEYWORDS
; verbose             Print information messages during run
;MODIFICATION HISTORY
; Written: A. Conley, Jan 2015
;NOTES
; A great deal of the code used here is copied from other routines
; (red_logitistic_sourcefind, red_sourcefind_dofind, etc.) for the
; purposes of efficiency -- it is much faster to smooth the actual
; maps once and then insert copies of the PSF into those maps than
; to keep finding repeatedly.
;-

PRO  red_logistic_recover_read, basename, map250, map350, map500,$
                                MAPDIR=mapdir, MASKFILES=maskfiles,$
                                MASKDIR=maskdir, SUCCESS=success,$
                                ERRMSG=errmsg, MINEXP=minexp
  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN

  success = 0b
  errmsg = ""

  map250 = READ_SMAP_FITSMAP(basename, 'PSW', DIR=mapdir, $
                             /NO_ABORT, SUCCESS=msuccess, ERRMSG=merrmsg,$
                             /SILENT)
  IF msuccess EQ 0 THEN BEGIN
     errmsg = "Error opening 250um map: " + merrmsg
     RETURN
  ENDIF
  map350 = READ_SMAP_FITSMAP(basename, 'PMW', DIR=mapdir, $
                             /NO_ABORT, SUCCESS=msuccess, ERRMSG=merrmsg,$
                             /SILENT)
  IF msuccess EQ 0 THEN BEGIN
     errmsg = "Error opening 350um map: " + merrmsg
     RETURN
  ENDIF
  map500 = READ_SMAP_FITSMAP(basename, 'PLW', DIR=mapdir, $
                             /NO_ABORT, SUCCESS=msuccess, ERRMSG=merrmsg,$
                             /SILENT)
  IF msuccess EQ 0 THEN BEGIN
     errmsg = "Error opening 500um map: " + merrmsg
     RETURN
  ENDIF

  ;; Check extents and pixel sizes
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

  ;; Add basic masks
  IF N_ELEMENTS(maskfiles) NE 0 THEN BEGIN
     ADD_USER_MAPMASK, map250, maskfiles, /ANDPLUS, MASKDIR=maskdir,$
                       MINEXP=minexp
     ADD_USER_MAPMASK, map350, maskfiles, /ANDPLUS, MASKDIR=maskdir,$
                       MINEXP=minexp
     ADD_USER_MAPMASK, map500, maskfiles, /ANDPLUS, MASKDIR=maskdir,$
                       MINEXP=minexp
  ENDIF
  
  success = 1b
  
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clip beams for map injection
FUNCTION red_logistic_recover_clipbeam, beam, fwhm, pixscale
  COMPILE_OPT IDL2, HIDDEN

  maxbm = MAX(beam, wmax)
  szbm = SIZE(beam)
  pos_x = wmax MOD szbm[1]
  pos_y = wmax / szbm[1]

  hfbmpix = CEIL(4.5 * fwhm / (2 * pixscale))

  ;; Assume it's not too near the edge
  minx = pos_x - hfbmpix
  maxx = minx + 2* hfbmpix
  miny = pos_y - hfbmpix
  maxy = miny + 2* hfbmpix

  RETURN, beam[minx:maxx, miny:maxy]
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION red_logistic_recover_bgmap, map, bgsubsize
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

  bgsubsize_pix = ROUND(bgsubsize / map.pixscale)    
  RETURN, smap_bgestimator(map.image, TEMPORARY(mask), bgsubsize_pix)

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source finding routine; copied from red_logistic_sourcefind_dofind
;; (which does filtering, backgrounding, etc. -- stuff that we do
;;  once in this routine and then re-use).  Also contains bits of
;;  red_sourcefind_dofind
FUNCTION red_logistic_recover_sourcefind, map250, bgmap250, $
   map350, bgmap350, map500, bgmap500, mapcomb, bgmapcomb, fwhm, $
   sn, min_corr, fluxrat, mins500, SUCCESS=success,$
   ERRMSG=errmsg

  COMPILE_OPT IDL2, HIDDEN
  success = 0b
  errmsg = ""

  ;; Search combined map
  cat = ps_extractor_doband(mapcomb, sn, "LINCOMB",$
                            FWHM=fwhm, PRE_SMOOTH=0b,$
                            ALG=2, MIN_CORR=min_corr, MAPMASKBITS=1uL,$
                            BGINPUT=bgmapcomb, MASKCHECKSZ=5,$
                            SUCCESS=success_doband)
  IF ~ success_doband THEN BEGIN
     ;; Didn't find any sources -- but that isn't necessarily a failure
     ;;  mode!
     success = 2b
     RETURN, !VALUES.F_NAN
  ENDIF

  ;; Do simplephot photometry
  npix_simple = ROUND(0.7 * fwhm / map500.pixscale)
  IF npix_simple MOD 2 EQ 0 THEN npix_simple += 1
  npix_simple >= 3
  ;; It doesn't like stuff near the edge, so remove those
  hfsz = npix_simple / 2
  wgood = WHERE(cat.x GE hfsz AND cat.x LE map250.xsize - hfsz - 1 AND $
                cat.y GE hfsz AND cat.y LE map250.ysize - hfsz - 1, ngood,$
                NCOMPLEMENT=nbad)
  IF ngood EQ 0 THEN BEGIN
     success = 2b
     RETURN, !VALUES.F_NAN
  ENDIF
  IF nbad NE 0 THEN cat = cat[TEMPORARY(wgood)]

  flux250 = smap_simplephot(map250, cat.x, cat.y, /XYPOS, FWHM=fwhm, $
                            /USEMASK, NPIX=npix_simple, BGRND=bgmap250)
  flux350 = smap_simplephot(map350, cat.x, cat.y, /XYPOS, FWHM=fwhm, $
                            /USEMASK, NPIX=npix_simple, BGRND=bgmap350)
  flux500 = smap_simplephot(map500, cat.x, cat.y, /XYPOS, FWHM=fwhm, /USEMASK,$
                            NPIX=npix_simple, BGRND=bgmap500)

  ;; Process for redness
  wgood = WHERE(flux350 GE fluxrat[0] * flux250 AND $
                flux500 GE fluxrat[1] * flux350 AND $
                flux500 GE mins500 * 1e-3 AND $ ;; mJy -> Jy for mins500
                cat.corr GE min_corr, $  
                ngood, NCOMPLEMENT=nbad)
  IF ngood EQ 0 THEN BEGIN
     success = 2b
     RETURN, !VALUES.F_NAN
  ENDIF
  IF nbad NE 0 THEN BEGIN
     cat = cat[wgood]
     flux250 = flux250[wgood] 
     flux350 = flux350[wgood] 
     flux500 = flux500[wgood]
  ENDIF

  ;; Now we need to apply the logistic regression bit to select
  ;; actual red sources; note this works in mJy
  wred = red_logistic_model(cat.f * 1e3, cat.df * 1e3,$
                            flux250 * 1e3, flux350 * 1e3, flux500 * 1e3,$
                            cat.corr, N=nred)
  IF nred EQ 0 THEN BEGIN
     success = 2b
     RETURN, !VALUES.F_NAN
  ENDIF

  
  retstr = REPLICATE({xsub: !VALUES.F_NAN, ysub: !VALUES.F_NAN,$
                      fsub: !VALUES.F_NAN, dfsub: !VALUES.F_NAN,$
                      s250: !VALUES.F_NAN, s350: !VALUES.F_NAN,$
                      s500: !VALUES.F_NAN}, nred)
  retstr.xsub = cat[wred].x
  retstr.ysub = cat[wred].y
  retstr.fsub = cat[wred].f
  retstr.dfsub = cat[wred].df
  retstr.s250 = flux250[wred]
  retstr.s350 = flux350[wred]
  retstr.s500 = flux500[wred]

  success = 1b
  RETURN, retstr
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION red_logistic_recover_circmask, pixscale, fwhm, sourcemasksize
  COMPILE_OPT IDL2, HIDDEN

  masksize = sourcemasksize * fwhm ;; radius
  npix = CEIL(masksize / pixscale)
  IF npix MOD 2 EQ 0 THEN npix += 1
  RETURN, smap_create_circular_mask(masksize / 2, pixscale, npix)
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add object masks from cat
FUNCTION red_logistic_recover_maskpos, mapcomb, cat, cmask
  COMPILE_OPT IDL2, HIDDEN

  retmask = mapcomb.mask
  FOR i=0, N_ELEMENTS(cat)-1 DO BEGIN
     s = inject_index(retmask, cmask, cat[i].xsub, cat[i].ysub, $
                      SUCCESS=success)
     IF success THEN $
        retmask[s.mx0:s.mx1, s.my0:s.my1] OR= cmask[s.ox0:s.ox1, s.oy0:s.oy1]
  ENDFOR
  RETURN, retmask
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION red_logistic_recover_pos, ninject, xsize, ysize,$
                                   injection_mask, objmask, SEED=seed
  COMPILE_OPT IDL2, HIDDEN

  ;; The tricky part here is avoiding overlapping sources.
  ;; An assumption used here is that, after all the sources
  ;; are injected, the injection mask will still dominate
  ;; the total masked area.  Thus, we don't continually update it
  ;; and re-index the allowed areas, but instead generate using
  ;; that mask and then check if the positions have since been
  ;; masked
  wunmasked = WHERE(injection_mask EQ 0, nunmasked)
  ;; A bit crude, but should be okay (we assume objmask is -not-
  ;;  mostly emtpy, which is certainly true in our usage here)
  IF nunmasked LT ninject * N_ELEMENTS(objmask) THEN $
     MESSAGE, "Not enough space to insert sources"

  ;; Could be made a bit more efficient by inserting in batches,
  ;;  but this is unlikely to be the limiting factor in these
  ;;  simulations (should be: the source finding)
  addmask = BYTARR(xsize, ysize) ;; additional masked locations
  maxiter = 300
  pos = REPLICATE({x: 0, y: 0}, ninject) ;; shouldn't need 32 bits...
  FOR i = 0, ninject-1 DO BEGIN
     rval = wunmasked[ROUND(nunmasked * RANDOMU(seed))]
     xpos = rval MOD xsize
     ypos = rval / xsize

     iter = 0
     ;; This is the part where we handle avoiding injecting
     ;; multiple sources too close to each other.
     WHILE addmask[xpos, ypos] NE 0 DO BEGIN
        ;; Generate a new position
        rval = wunmasked[ROUND(nunmasked * RANDOMU(seed))]
        xpos = rval MOD xsize
        ypos = rval / xsize
        iter += 1
        IF iter GT maxiter THEN $
           MESSAGE, "Unable to insert source -- is sourcedens too high?"
     ENDWHILE

     pos[i].x = xpos
     pos[i].y = ypos

     ;; Add to addmask
     s = inject_index(addmask, objmask, xpos, ypos, SUCCESS=isuccess)
     IF isuccess THEN $
        addmask[s.mx0:s.mx1, s.my0:s.my1] OR= objmask[s.ox0:s.ox1, s.oy0:s.oy1]
  ENDFOR

  RETURN, pos
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Does actual source injection.  Assumes center normalized beams!
FUNCTION red_logistic_recover_injectmap, pos, flux, beam, map
  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN

  mapout = map ;; Don't want to muss up the original so we can reuse
  IF flux NE 0 THEN BEGIN
     src = flux * beam
     FOR i = 0, N_ELEMENTS(pos) - 1 DO BEGIN
        s = inject_index(mapout.image, src, pos[i].x, pos[i].y, $
                         SUCCESS=isuccess)
        IF isuccess THEN $
           mapout.image[s.mx0:s.mx1, s.my0:s.my1] += $
           src[s.ox0:s.ox1, s.oy0:s.oy1]
     ENDFOR
  ENDIF
  RETURN, mapout
END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION red_logistic_recover_inject, fluxes, ninject, coeffs, $
   injection_mask, objmask, beam, map250, map350, map500, mapcomb,$
   map250i, map350i, map500i, mapcombi, SEED=seed

  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN

  ;; The first task is to figure out where to inject the sources
  ;; -without overlapping-
  pos = red_logistic_recover_pos(ninject, map250.xsize, map250.ysize,$
                                 injection_mask, objmask, SEED=seed)

  ;; Now actually insert them into copies of maps
  map250i = red_logistic_recover_injectmap(pos, fluxes[0], beam, map250)
  map350i = red_logistic_recover_injectmap(pos, fluxes[1], beam, map350)
  map500i = red_logistic_recover_injectmap(pos, fluxes[2], beam, map500)
  cflux = TOTAL(coeffs * fluxes)
  mapcombi = red_logistic_recover_injectmap(pos, cflux, beam, mapcomb)

  RETURN, pos
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     
PRO red_logistic_recover, basename, fluxes, outname, MAPDIR=mapdir, $
                          NSIMS=nsims, VERBOSE=verbose,$
                          MASKFILES=maskfiles, MASKDIR=maskdir,$
                          MATCHRAD=matchrad, SOURCEDENS=sourcedens, SEED=seed

  ;; Input parsing
  IF N_ELEMENTS(fluxes) NE 3 THEN MESSAGE, "Wrong number of flux densities"
  IF N_ELEMENTS(mapdir) EQ 0 THEN mapdir = "./"
  IF N_ELEMENTS(nsims) EQ 0 THEN nsims = 100 ELSE nsims = FIX(nsims)
  IF nsims LE 0 THEN MESSAGE, "Non-positive number of simulations"
  IF N_ELEMENTS(matchrad) EQ 0 THEN matchrad = 14.0
  IF matchrad LE 0 THEN MESSAGE, "Invalid (non-positive) matching radius"
  IF N_ELEMENTS(sourcedens) EQ 0 THEN sourcedens = 25.0
  IF sourcedens LE 0 THEN MESSAGE, "Invalid (non-positive) source density"

  ;; Non-user settable parameters; must match those used to develop
  ;; the logistic source selection model
  coeffs = [-0.65, 0.35, SQRT(1 - 0.65^2 - 0.35^2)]
  filt_coeffs = [-0.65, 0.25, SQRT(1 - 0.65^2 - 0.25^2)]
  bgsubsize = 192 ;; arcsec
  sourcemasksize = 2.75 ;; number of fwhm source must avoid each other by
  sn = 3.0             ;; signal to noise ratio
  min_corr = 0.8       ;; Minimum correlation
  fluxrat = [0.3, 0.6] ;; First cut flux ratios
  mins500 = 20.0       ;; Min s500 in mJy
  conf = 4.2e-3        ;; Conf noise used in matched filter construction
  minexp = 0.2         ;; Minimum allowed exposure
  
  ;; Read in the data, with some input checks
  IF KEYWORD_SET(verbose) THEN MESSAGE, "Reading data", /INF
  red_logistic_recover_read, basename, map250, map350, map500, $
                             MAPDIR=mapdir, MASKFILES=maskfiles,$
                             MASKDIR=maskdir, MINEXP=minexp, $
                             SUCCESS=rsuccess, ERRMSG=rerrmsg
  IF rsuccess EQ 0b THEN MESSAGE, rerrmsg

  
  ;; Match filter, get beam
  IF KEYWORD_SET(verbose) THEN MESSAGE, "Match filtering", /INF
  red_optfilt_dofilt, map250, map350, map500, COEFFS=filt_coeffs, $
                      CONF=conf, FWHM=fwhm, BEAM=beam

  ;; Clip down beam to something more usable
  beam = red_logistic_recover_clipbeam(beam, fwhm, map250.pixscale)

  ;; Make combined map
  IF KEYWORD_SET(verbose) THEN MESSAGE, "Making combined map", /INF
  mapcomb = linear_map_combine(coeffs[0], map250, coeffs[1], map350,$
                               coeffs[2], map500)

  ;; Get smoothed background maps for reuse
  IF KEYWORD_SET(verbose) THEN MESSAGE, "Making background maps", /INF
  bgmap250 = red_logistic_recover_bgmap(map250, bgsubsize)
  bgmap350 = red_logistic_recover_bgmap(map350, bgsubsize)
  bgmap500 = red_logistic_recover_bgmap(map500, bgsubsize)
  bgmapcomb = red_logistic_recover_bgmap(mapcomb, bgsubsize)
  
  ;; Find actual red sources
  ;;  We need to know where these are before we start so that we
  ;;  can avoid sticking a fake source on top of a real one
  IF KEYWORD_SET(verbose) THEN MESSAGE, "Finding actual red sources", /INF
  cat_true = red_logistic_recover_sourcefind(map250, bgmap250,$
                                             map350, bgmap350,$
                                             map500, bgmap500,$
                                             mapcomb, bgmapcomb,$
                                             fwhm, sn, min_corr, fluxrat,$
                                             mins500, SUCCESS=ssuccess,$
                                             ERRMSG=serrmsg)
  IF ssuccess EQ 0 THEN $
     MESSAGE, "Error when searching for real red sources: " + serrmsg
  ;; We may not find any!  
  IF ssuccess EQ 1 THEN ntrue = N_ELEMENTS(cat_true) ELSE ntrue = 0
  IF KEYWORD_SET(verbose) THEN $
     MESSAGE, STRING(ntrue, FORMAT='(" Found ",I0," actual sources")'), /INF

  
  ;; Construct the source circular mask we will re-use many times
  IF KEYWORD_SET(verbose) THEN MESSAGE, "Masking actual red sources", /INF
  objmask = red_logistic_recover_circmask(mapcomb.pixscale, fwhm, $
                                          sourcemasksize)

  ;; We store the locations to avoid in the injection mask
  IF ntrue GT 0 THEN BEGIN
     injection_mask = red_logistic_recover_maskpos(mapcomb, cat_true,$
                                                   objmask)
  ENDIF ELSE injection_mask = mapcomb.mask

  ;; Figure out how many sources we will inject, avoiding the mask
  ;; locations
  wnomask = WHERE(injection_mask EQ 0, nunmask)
  IF nunmask EQ 0 THEN MESSAGE, "All positions masked in combined map"
  area = (mapcomb.pixscale / 3600.0)^2 * DOUBLE(nunmask)
  ninject = ROUND(sourcedens * area)
  IF ninject EQ 0 THEN MESSAGE, "Wouldn't inject any sources!"
  IF KEYWORD_SET(verbose) THEN $
     MESSAGE, STRING(ninject, FORMAT='("Injecting ",I0," sources per sim")'),$
              /INF
  
  ;; Set up output structure
  retstruct = REPLICATE({xtrue: !VALUES.F_NAN, ytrue: !VALUES.F_NAN,$
                         found: 0b, xsub: !VALUES.F_NAN, ysub: !VALUES.F_NAN,$
                         s250: !VALUES.F_NAN, s350: !VALUES.F_NAN,$
                         s500: !VALUES.F_NAN, fsub: !VALUES.F_NAN},$
                        ninject * nsims)

  ;; Main simulation loop
  FOR i = 0, nsims - 1 DO BEGIN
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE, STRING(i + 1, nsims, FORMAT='("Sim: ",I0, " of ",I0)'), /INF

     ;; Inject fake sources
     IF KEYWORD_SET(verbose) THEN MESSAGE,"  Injecting", /INF
     pos = red_logistic_recover_inject(fluxes, ninject, coeffs, $
                                       injection_mask, objmask, beam,$
                                       map250, map350, map500, mapcomb,$
                                       map250i, map350i, map500i,$
                                       mapcombi, SEED=seed)
     idxmin = i * ninject
     idxmax = idxmin + ninject - 1
     retstruct[idxmin:idxmax].xtrue = pos.x
     retstruct[idxmin:idxmax].ytrue = pos.y

     IF MAX(injection_mask[pos.x, pos.y]) NE 0 THEN $
        MESSAGE, "Invalid position generated"
     
     ;; Find them
     IF KEYWORD_SET(verbose) THEN MESSAGE,"  Finding", /INF
     cat_fake = red_logistic_recover_sourcefind(map250i, bgmap250, $
                                                map350i, bgmap350, $
                                                map500i, bgmap500, $
                                                mapcombi, bgmapcomb,$
                                                fwhm, sn, min_corr, fluxrat,$
                                                mins500, SUCCESS=ssuccess,$
                                                ERRMSG=serrmsg)
     IF ssuccess EQ 0 THEN $
        MESSAGE, "Error when searching for fake red sources: " + serrmsg
     ;; We may not find any!
     IF ssuccess EQ 1 THEN BEGIN
        ;; We did find some sources (fake or real).  Try to match
        ;;  against input catalog
        SRCOR, cat_fake.xsub, cat_fake.ysub, pos.x, pos.y,$
               matchrad / map250.pixscale, idxrecover, idxinject,$
               COUNT=count, /SILENT
        IF count GT 0 THEN BEGIN
           IF KEYWORD_SET(verbose) THEN BEGIN
              msg = STRING(count, ninject, FORMAT='("  Found ",I0," of ",I0)')
              MESSAGE, msg, /INF
           ENDIF
           retstruct[idxmin + idxinject].found = 1b
           retstruct[idxmin + idxinject].xsub = cat_fake[idxrecover].xsub
           retstruct[idxmin + idxinject].ysub = cat_fake[idxrecover].ysub
           retstruct[idxmin + idxinject].fsub = cat_fake[idxrecover].fsub
           retstruct[idxmin + idxinject].s250 = cat_fake[idxrecover].s250
           retstruct[idxmin + idxinject].s350 = cat_fake[idxrecover].s350
           retstruct[idxmin + idxinject].s500 = cat_fake[idxrecover].s500
        ENDIF
     ENDIF ;; nothing to do if none found (success > 1)
  ENDFOR

  ;; Write output information
  IF KEYWORD_SET(verbose) THEN MESSAGE, "Writing results", /INF
  cathdr = ['']
  SXADDPAR, cathdr, 'S250', fluxes[0], 'S250 of injected source'
  SXADDPAR, cathdr, 'S350', fluxes[1], 'S350 of injected source'
  SXADDPAR, cathdr, 'S500', fluxes[2], 'S500 of injected source'
  SXADDPAR, cathdr, 'NSIMS', nsims, 'Number of sims'
  SXADDPAR, cathdr, 'NINJECT', ninject, 'Number injected per sim'
  SXADDPAR, cathdr, 'SRCDENS', sourcedens, 'Injected source density [deg^-2]'
  SXADDPAR, cathdr, 'NTRUE', ntrue, 'Number of real sources found'
  SXADDPAR, cathdr, 'K1', coeffs[0], 'map250 sub coeff'
  SXADDPAR, cathdr, 'K2', coeffs[1], 'map350 sub coeff'
  SXADDPAR, cathdr, 'K3', coeffs[2], 'map500 sub coeff'
  SXADDPAR, cathdr, 'FILTK1', filt_coeffs[0], 'map250 filt coeff'
  SXADDPAR, cathdr, 'FILTK2', filt_coeffs[1], 'map350 filt coeff'
  SXADDPAR, cathdr, 'FILTK3', filt_coeffs[2], 'map500 filt coeff'
  SXADDPAR, cathdr, 'BGSUBSZ', bgsubsize, 'Background grid size [arcsec]'
  SXADDPAR, cathdr, 'MATCHRAD', matchrad, 'Source matching radius'
  SXADDPAR, cathdr, 'MINCORR', min_corr, 'Minimum correlation coeff'
  SXADDPAR, cathdr, 'MINS500', mins500, 'Min allowed S_500 [mJy]'
  SXADDPAR, cathdr, 'FLUXRAT0', fluxrat[0], 'Min F350/F250'
  SXADDPAR, cathdr, 'FLUXRAT1', fluxrat[1], 'Min F500/F350'
  SXADDPAR, cathdr, 'OPTFILT', 'T', "Optimal filtering used"
  SXADDPAR, cathdr, 'CONF', conf, "Assumed confusion noise in filtering"
  SXADDPAR, cathdr, "FWHM", fwhm, "Estimated FWHM in combined map"
  SXADDPAR, cathdr, "PHOTALG", "STARFINDER", "Phot algorithm"

  MWRFITS, retstruct, outname, cathdr, STATUS=status
  IF status NE 0 THEN MESSAGE, "Error writing output file: " + outname
END                     

