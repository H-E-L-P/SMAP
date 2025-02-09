;+
; procedure smap_itermap.pro
; Sep 24, 2009
; Gaelen Marsden (gmarsden@phas.ubc.ca)
;
;
; SMAP_ITERMAP, tod_ptrs, mapparam, map250, map350, map500, $
;               ITERMAP_PARAMS=itermap_params, SCAN_PARAMS=scan_params, $
;               FIXEDPARAMS=fixedparams, FIXEDPARDIR=fixedpardir, $
;               BADBOLOS=badbolos, EXCLUDEMASK=excludemask, $
;               EXCLUDEWEIGHTMASK=excludeweightmask, SAVEMAPS=savemaps, $
;               SAVEPARAMS=saveparams, SAVEMAPDIR=savemapdir, $
;               SUCCESS=success, ERRMSG=errmsg, VERBOSE=verbose, $
;               EXNAME=exname, NO250=no250, NO350=no350, NO500=no500,$
;               FIXEDDEGLITCH=fixeddeglitch, DEGLITCH250=deglitch250,$
;               DEGLITCH350=deglitch350, DEGLITCH500=deglitch500
;
; Builds maps at all 3 bands by iteratively fitting polynomials to the
; time streams, detector gains and detector weights. On each
; iteration, the current map is subtracted from the time stream before
; the fitting step. The iteration where the fitting of each of poly,
; gains and weights is introduced can be set separately. 
;
;
; INPUTS:
;
;   tod_ptrs:     array of pointers to tod structures
;   mapparam:     map parameter structure
;
;  OPTIONAL KEYWORDS
;
;   SAVEMAPS:     if set, save diagnostic map output 
;   SAVEPARAMS:   if set, save diagnostic gains/offsets output
;   VERBOSE:      if set, code is "chatty"
;   NO250/NO350/N0500 : Don't make the map in this band.  Note you
;                        still have to provide the dummy argument
;   FIXEDDEGLITCH: Use previous DEGLITCH250/350/500 to mask glitches
;
;  OPTIONAL INPUTS
;
;   ITERPARAMS:   structure containing map-making parameters (see below)
;   BADBOLOS:     string array of bolometers to ignore
;   EXCLUDEMASK:  string array of mask bit names to exclude
;   EXCLUDEWEIGHTMASK: string array of additional mask bits names to
;                  exclude from weighting, but not from final map.  Helpful
;                  for excluding bright objects from polynomial calculation.
;   SAVEMAPDIR:   output directory for diagnostics files
;   FIXEDPARAMS:  The base name of a FITS file containing the scan
;                  params.  If this is set, these are used instead of 
;                  building the map anew.
;   FPARAMS250/350/500 As an alternative to FIXEDPARAMS, the actual
;                  structures.  If set, fixedparams is ignored, and
;                  these are used to build the maps.
; OUTPUTS:
;
;   map250:       final PSW map structure
;   map350:       final PMW map structure
;   map500:       final PLW map structure
;
; OPTIONAL OUTPUTS:
; 
;   SUCCESS:      if 0B, problem was encoutered
;   ERRMSG:       if success EQ 0B, record error message here
;   DEGLITCH???:  if second level deglitching is on, this structure
;                  returns information about what was masked.  This
;                  is a structure that contains:
;                    .nglitches : number of 2nd level deglitches
;                    .nfiles:   number of files
;                    .filenames: short filename array 
;                    .bbids: bbid array (same order as filenames)
;                    .obsids: obsid array (same order as filenames)
;                    .nbols: Number of bolometers
;                    .bolometers: Bolometer names
;                    .samples: a subarray of structures identifying
;                       masked samples:
;                       .fileidx: Index into file/bbid/obsid arrays
;                       .bolindex: Bolometer index
;                       .sample: Which timesample is flagged
;                  Note that obsids/bbids are not unique, even in
;                  combination!  Uses as an input if /FIXEDDEGLITCH is
;                  set.  There are three sets of deglitch -- one for
;                  each band
;   SPARAMS250/350/500    A structure containing the tod by tod gain, offset,
;                   etc. parameters for the appropriate band.
; ITERPARAMS CONTENTS:
;
;   The ITERPARAMS structure should contain these tagnames:
;
;     * niter:       number of iterations to perform
;     * first_offs:  first iteration on which to perform offset (poly) fitting
;     * first_gain:  first iteration on which to perform gains fitting
;     * first_wt:    first iteration on which to perform weights fitting
;     * fixed_nt:    fixed number of offset terms
;     * nterms:      number of terms in poly fit (if fixed_nt-1)
;                    (poly order = nterms - 1)
;     * nt_scale:    scale (in sec) for order of polynomial (if
;                     fixed_nt=0)
;     * min_exposure: minimum exposure required for non-NaN value in
;                      output map, in seconds (def: 0.1)
;     * first_clip:  First iteration where 2nd level deglitching is
;                     applied.  Set to 0 to turn off. (default 0)
;     * clipsigma:   Sigma to reject, if deglitching is on.  In terms
;                     of the individual scan RMS.  (default: 10)
;     * grow_clip:   Amount to 'grow' masked samples in deglitching,
;                     in timesamples (default: 3).
;   Setting first_[offs|gain|wt|nt] = 0 disables fitting
;   Names not included in structure will use default values.
;    
; 
; A NOTE ON DIAGNOSTICS:
;
; Currently, the (optional) diagnostics are output to a pair of FITS
; files for each band. All images are cubes, with the 3rd dimension
; spanning iteration number. These are large files!
;
;   diagnostic_maps:
;     * extension [1] is signal map
;     * extension [2] is error map
;     * extension [3] is exposure (hits) map
;
;   diagnostic_params:
;     * extension [1] is weights parameters [nscan X nbolo X niter]
;     * extension [2] is gains parameters [1 X nbolo X niter]
;     * extension [3+i] is order `i' of poly fit [nscan X nbolo X niter]
;    Note that these are not the same thing as iterparams!
; 
;
; OTHER NOTES
;
;   If fixed parameters are being used, then the deglitcher is never
;   run because it depends on actually iterating.  However,
;   /FIXEDDEGLITCH still works
;
; TODO:
;
;   * fix up error checking! (it's weak and untested)
;   * add option for re-reading data from disk instead of keeping in
;     memory
;
;
; HISTORY:
;
; 2009-09-24 (gm): initial version
; 2009-10-30 (gm): initial upload to svn
; 2009-11-02 (gm): check for NANs in map in ITERMAP_CALCWEIGHTS
;                  modify diagnostics file format
; 2009-11-03 (gm): fix map outputs (wasn't setting map??? = map at end)
;                  add polynomial fitting (instead of just offset)
; 2009-11-04 (gm): change format of iteration params (k.w. instead of glob.)
; 2009-11-11 (gm): change format of data input (tod_ptrs instead of filelist)
; 2009-11-17 (gm): do makemap loop in call to C, use SMAP_GETMAP_XY
; 2009-11-18 (gm): update format of diagnostics files
; 2009-11-23 (gm): add missing factor of 1/sqrt(n) in error map
; 2009-11-24 (gm): add PROPERROR keyword (propagated error maps)
; 2009-11-25 (gm): data masking now done same as other routines
;                  separate map and param diagnostics files
; 2010-01-20 (gm): add BADBOLOS keyword
; 2010-11-22 (gm): add ability to set offset poly order based on
;                  length of time stream (see iterparams.fixed_nt)
; 2011-02-28 (ac): Add 'grow' ability for deglitching, add to header
;                   outputs, add mechanism for specifying what was
;                   masked.
; 2011-08-26 (ac): PROPERROR now the only option.  sampling rate
;                   weighting is applied now if inverse variance
;                   weighting is off.
;-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Determines what parts of the signal are good based on the mask
FUNCTION ITERMAP_ISGOOD, tod, bolometerind, excludemask, min_scanfrac, $
                         FIXEDDEGLITCH=fixeddeglitch, $
                         DEGLITCHINFO=deglitchinfo, BADBOLOS=badbolos

  COMPILE_OPT IDL2, HIDDEN

  ;;val will be 1 where the samples are good
  val = REPLICATE(1B, tod.nsamps)

  ; test signal
  val = val AND FINITE(tod.signal[bolometerind,*])

  IF TAG_EXIST(tod, 'mask_bits', /TOP_LEVEL) AND $
     N_ELEMENTS(excludemask) NE 0 THEN $
        mapmaskbits = construct_mask_bitmask(excludemask, tod.mask_bits)

  IF N_ELEMENTS( mapmaskbits ) NE 0 && $
     mapmaskbits NE 0 && $
     TAG_EXIST(tod,'mask',/TOP_LEVEL) $
  THEN $
     val AND= ( (tod.mask[bolometerind,*] AND mapmaskbits) EQ 0 )

  ; check badbolos
  IF N_ELEMENTS(badbolos) NE 0 THEN $
     val AND= (TOTAL(tod.chan[bolometerind] EQ badbolos) EQ 0)

  ; test min_scanfrac -- set all to 0 if number of good samples smaller than
  ; nsamp * min_scanfrac
  temp = WHERE(val, ngoodsamp)
  IF ngoodsamp LT min_scanfrac * tod.nsamps THEN val[*] = 0B

  IF KEYWORD_SET( fixeddeglitch ) && deglitchinfo.nglitches GT 0 THEN BEGIN
     ;;File name better be unique...
     filenumber = WHERE( deglitchinfo.filenames EQ tod.shortfile, nfilenumber )
     IF nfilenumber NE 1 THEN $
        MESSAGE,"Non-unique file match"
     bolnumber = WHERE( tod.chan[bolometerind] EQ deglitchinfo.bolometers, $
                        nbol )
     IF nbol EQ 0 THEN $
        MESSAGE,"Couldn't find bolometer: "+$
                tod.chan[bolometerind] + " in pre-existing deglitch info"
     IF nbol NE 1 THEN $
        MESSAGE,"Non-unique bolometer match for: "+$
                tod.chan[bolometerind]+"; found: "+$
                STRING(nbol,FORMAT='(I0)')
     wbad = WHERE( filenumber[0] EQ deglitchinfo.samples.fileidx AND $
                   bolnumber[0] EQ deglitchinfo.samples.bolidx, nbad )
     IF nbad NE 0 THEN $
        val[ deglitchinfo.samples[wbad].samp ] = 0b
  ENDIF

  RETURN, val
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Initializes the map based deglitching structure
FUNCTION ITERMAP_SETUPDEGLITCH, tods, glitches, SUCCESS=success, ERRMSG=errmsg
  COMPILE_OPT IDL2, HIDDEN
  
  success = 0b

  ;;Get first tod to get list of bolometers, etc.
  tod_type = SIZE( tods, /TNAME )
  IF tod_type EQ 'STRING' THEN BEGIN
     curr_tod = smap_readtod(tods[0], SUCCESS=rsuccess, ERRMSG=errmsg)
     IF rsuccess EQ 0 THEN BEGIN
        success = 0B
        errmsg = "Error reading tod from "+tods[0]+": "+errmsg
        RETURN,!VALUES.F_NAN
     ENDIF
  ENDIF ELSE curr_tod = *tods[0]

  ntods = N_ELEMENTS(tods)
  chans = curr_tod.chan[ SORT(curr_tod.chan) ]
  ;;Overall structure, samples left off for now
  nglitches = N_ELEMENTS(glitches)
  IF nglitches GT 0 THEN BEGIN
     deglitchinfo = { nglitches: nglitches, nfiles: ntods, $
                      filenames: STRARR(ntods),$
                      bbids: ULONARR(ntods), obsids: ULONARR(ntods),$
                      nbols: N_ELEMENTS(chans), bolometers: chans,$
                      samples: glitches}
  ENDIF ELSE BEGIN
     deglitchinfo = { nglitches: nglitches, nfiles: ntods, $
                      filenames: STRARR(ntods),$
                      bbids: ULONARR(ntods), obsids: ULONARR(ntods),$
                      nbols: N_ELEMENTS(chans), bolometers: chans }
  ENDELSE
  success = 1b
  RETURN,deglitchinfo
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Calculates offsets, gains, variances for current data given current
;;map
PRO ITERMAP_CALCPARAMS, mapparam, tods, x_ptrs, y_ptrs, boloind, map, $
                        dooffs, dogain, doclip, clipsigma, $
                        offsets, gains, variances, ntermsbyscan, excludemask, $
                        min_scanfrac, grow_clip, BADBOLOS=badbolos,$
                        VERBOSE=verbose, FIXEDDEGLITCH=fixeddeglitch,$
                        DEGLITCHINFO=deglitchinfo, SUCCESS=success, $
                        ERRMSG=errmsg

  ;; note: offsets and variances are per scan per detector
  ;;       gains are per detector

  COMPILE_OPT IDL2, HIDDEN

  IF KEYWORD_SET(verbose) THEN $
     MESSAGE, "calculating offsets, gains and variances", /INF

  nscans = mapparam.nscans
  nchans = N_ELEMENTS(boloind)
  nterms = N_ELEMENTS(offsets[0,0,*])

  ;; keep track of which elements are finite for normalizing at end
  goodgains   = BYTARR(nchans)

  ;; gain accumulators
  gainacc_numer = FLTARR(nchans)
  gainacc_denom = FLTARR(nchans)

  ;; loop over scans
  tod_type = SIZE( tods, /TNAME )
  FOR iscan=0,nscans-1 DO BEGIN

     ;; read data if necessary
     IF tod_type EQ 'STRING' THEN BEGIN
        curr_tod = smap_readtod(tods[iscan], SUCCESS=rsuccess, ERRMSG=errmsg)
        IF rsuccess EQ 0 THEN BEGIN
           success = 0B
           errmsg = "Error reading tod from "+tods[i]+": "+errmsg
           RETURN
        ENDIF
     ENDIF ELSE $
        curr_tod = *tods[iscan]

     IF iscan EQ 0 THEN bolometerlist = curr_tod.chan[boloind]

     ;; loop over bolos
     FOR i=0,nchans-1 DO BEGIN
        
        this_bolindex = WHERE( curr_tod.chan EQ bolometerlist[i],$
                               nthisbol )
        IF nthisbol EQ 0 THEN BEGIN
           errmsg = "Couldn't find bolometer: "+bolometerlist[i]+" in tod: "+$
                    curr_tod.shortfile
           success = 0b
           RETURN
        ENDIF
        IF nthisbol GT 1 THEN BEGIN
           errmsg = "Multiple matches to bolometer: "+bolometerlist[i]+$
                    " in tod: "+curr_tod.shortfile
           success = 0b
           RETURN
        ENDIF
        this_bolindex = this_bolindex[0]


        ;; find good data points
        wkeep = WHERE( ITERMAP_ISGOOD(curr_tod, this_bolindex, $
                                      excludemask, min_scanfrac, $
                                      FIXEDDEGLITCH=fixeddeglitch,$
                                      DEGLITCHINFO=deglitchinfo,$
                                      BADBOLOS=badbolos), $
                       nkeep )

        ;; skip loop if no good samples
        IF nkeep EQ 0 THEN CONTINUE

        signal = REFORM(curr_tod.signal[this_bolindex,wkeep])

        ;; time relative to time[0] (consistent with MAKEMAP)
        
        time   = curr_tod.samptime[wkeep] - $
                 curr_tod.samptime[0]

        IF tod_type EQ 'STRING' THEN BEGIN
           AD2XY, curr_tod.ra[this_bolindex,wkeep], $
                  curr_tod.dec[this_bolindex,wkeep], $
                  map.astrometry, $
                  thisx, thisy
        ENDIF ELSE BEGIN
            ; stored in tod or externally?
            IF curr_tod.proj_stored THEN BEGIN
                thisx = curr_tod.ra[this_bolindex,wkeep]
                thisy = curr_tod.dec[this_bolindex,wkeep]
            ENDIF ELSE BEGIN
                thisx = REFORM((*x_ptrs[iscan])[this_bolindex,wkeep])
                thisy = REFORM((*y_ptrs[iscan])[this_bolindex,wkeep])
            ENDELSE
        ENDELSE

        ; reject samples outside of map area
        mkeep = WHERE(thisx GE 0 AND thisx LT map.xsize AND $
                      thisy GE 0 AND thisy LT map.ysize, nmkeep)

        IF nmkeep EQ 0 THEN CONTINUE

        xpix   = ROUND(thisx[mkeep])
        ypix   = ROUND(thisy[mkeep])

        signal = signal[mkeep]
        time   = time[mkeep]
        model  = map.image[xpix,ypix]
        
        ;; skip bad map pixels
        mkeep = WHERE(FINITE(model), nmkeep)
        
        ;; skip loop if no good map pixels
        IF nmkeep EQ 0 THEN CONTINUE

        signal = signal[mkeep]
        time   = time[mkeep]
        model  = model[mkeep]
        
        ;;Time stream clipping
        IF doclip THEN BEGIN
           polyfit = DBLARR(nmkeep)
           FOR k=0,ntermsbyscan[iscan]-1 DO $
              polyfit += offsets[iscan,i,k] * time^DOUBLE(k)
           
           resid = signal - (model / gains[i] + polyfit)

           scan_rms = SQRT(variances[iscan,i])
           IF grow_clip LT 2 THEN BEGIN
              ;;Simple case, not growing masked areas
              clipkeep = WHERE(ABS(resid) LE $
                               clipsigma * scan_rms,$
                               nclipkeep, NCOMPLEMENT=nclipremove)
              IF nclipkeep LT (ntermsbyscan[iscan]+2) THEN CONTINUE
              IF nclipremove NE 0 THEN BEGIN
                 signal = signal[clipkeep]
                 time   = time[clipkeep]
                 model  = model[clipkeep]
                 nmkeep = nclipkeep
              ENDIF
           ENDIF ELSE BEGIN
              ;;Grow masked region around each masked object
              clipremove = WHERE(ABS(resid) GT $
                                 clipsigma * scan_rms, $
                                 nclipremove, NCOMPLEMENT=nclipkeep)
              IF nclipkeep LT (ntermsbyscan[iscan]+2) THEN CONTINUE ;;Skip scan
              IF nclipremove GT 0 THEN BEGIN
                 rem = BYTARR(nmkeep)
                 rem[clipremove] = 1b
                 rem = DILATE(rem,REPLICATE(1b,grow_clip))
                 rem[clipremove] = 1b ;; dilate can misbehave at the edges
                 clipkeep = WHERE( ~TEMPORARY(rem), nclipkeep, $
                                   NCOMPLEMENT=nclipremove )
                 IF nclipkeep LT (ntermsbyscan[iscan]+2) THEN CONTINUE
                 signal = signal[clipkeep]
                 time   = time[clipkeep]
                 model  = model[clipkeep]
                 nmkeep = nclipkeep
              ENDIF
           ENDELSE

        ENDIF  ;; end clip

        ;; offsets (poly fit to residuals)
        IF dooffs THEN BEGIN
           IF N_ELEMENTS(time) LT 5 THEN BEGIN
              errmsg = "Too few samples to do offset for bolometer: " + $
                       bolometerlist[i] + " in tod: " + curr_tod.shortfile
              success = 0b
              RETURN
           ENDIF
           offsets[iscan,i,0:ntermsbyscan[iscan]-1] = $
              POLY_FIT(time, signal - model / gains[i], $
                       ntermsbyscan[iscan]-1, /DOUBLE, STATUS=poly_success)
           IF poly_success EQ 1 THEN BEGIN
              errmsg = "Singular array in poly fit -- can't fit offset for: "+$
                       bolometerlist[i] + " in tod: " + curr_tod.shortfile
              success = 0b
              RETURN
           ENDIF
           IF poly_success EQ 2 THEN BEGIN
              errmsg = "Warning: Small pivot element for: "+$
                       bolometerlist[i] + " in tod: " + curr_tod.shortfile+$
                       " with " + STRING(N_ELEMENTS(time),FORMAT='(I0)')+$
                       " elements.  Continuing anyways"
              MESSAGE, errmsg, /INF
           ENDIF
        ENDIF
        
        ;; build poly based on offsets
        polyfit = DBLARR(nmkeep)
        FOR k=0,ntermsbyscan[iscan]-1 DO $
           polyfit += offsets[iscan,i,k] * time^DOUBLE(k)
        
        ;;The variance is calculated from (time-stream - map)
        
        sigmodel = model / gains[i] + polyfit
        variances[iscan,i] = MEAN( (signal - sigmodel)^2, /NAN )
        
        ;; gains
        IF dogain THEN BEGIN
           gainacc_numer[i] += TOTAL(model^2) 
           gainacc_denom[i] += $
              TOTAL(model * signal) - TOTAL(model * polyfit)
           goodgains[i] = 1B
        ENDIF                   ; end gains
        
     ENDFOR                     ; end loop over bolometers
  ENDFOR                        ; end loop over scans

  ;; calculate & normalize gains
  IF dogain THEN BEGIN
     gind = WHERE(goodgains EQ 1B, ng)
     IF ng GT 0 THEN BEGIN
        gains[gind] = gainacc_numer[gind] / gainacc_denom[gind]
        gains[gind] /= MEAN(gains[gind])
     ENDIF
  ENDIF
  
  success = 1B

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; create map based on current values of offsets, gains and variances
PRO ITERMAP_MAKEMAP, mapparam, tods, x_ptrs, y_ptrs, boloind, map, $
                     offsets, gains, variances, dowght, doclip, clipsigma, $
                     min_exposure, ntermsbyscan, excludemask, min_scanfrac, $
                     grow_clip, FIXEDDEGLITCH=fixeddeglitch,$
                     DEGLITCHINFO=deglitchinfo, BADBOLOS=badbolos, $
                     SUCCESS=success, ERRMSG=errmsg, $
                     VERBOSE=verbose

  COMPILE_OPT IDL2, HIDDEN
  success = 0b
  errmsg  = ""

  IF KEYWORD_SET(verbose) THEN $
     MESSAGE, "building map", /INF

  IF KEYWORD_SET( fixeddeglitch ) AND doclip THEN BEGIN
     errmsg = "Can't set both fixeddeglitch and do clipping"
     RETURN
  ENDIF

  nscans = mapparam.nscans
  nchans = N_ELEMENTS(boloind)
  nterms = N_ELEMENTS(offsets[0,0,*])

  IF doclip THEN oldmap = map.image

  ;; rezero maps
  map.image    = 0.0
  map.error    = 0.0
  map.exposure = 0.0

  ;; create local copies of maps -- can't use maps in the map
  ;; structure b/c they get copied by value [I think]
  signalmap   = map.image
  errormap    = signalmap
  weightmap   = signalmap
  exposuremap = map.exposure
  
  ;; loop over scans
  tod_type = SIZE( tods, /TNAME )

  ;;This will hold individual glitches if we need them
  deglitchsample = { fileidx: 0U, bolidx: 0U, samp: 0U }

  ;;helpful to have a list of bbids/obsids/filenames -- which
  ;; is easy cakes if they are pointers, but takes a bit more
  ;; work if they are TODs on disk
  shortfiles = STRARR( nscans )
  bbids      = ULONARR( nscans )
  obsids     = ULONARR( nscans )


  FOR iscan=0,nscans-1 DO BEGIN

     ;; read data if necessary
     IF tod_type EQ 'STRING' THEN BEGIN
        curr_tod = smap_readtod(tods[iscan], SUCCESS=rsuccess, ERRMSG=errmsg)
        IF rsuccess EQ 0 THEN BEGIN
           success = 0B
           errmsg = "Error reading tod from "+tods[i]+": "+errmsg
           RETURN
        ENDIF
     ENDIF ELSE $
        curr_tod = *tods[iscan]

     ;;Get list of all bols, used for deglitch info
     IF iscan EQ 0 THEN BEGIN
        allbols = curr_tod.chan
        allbols = allbols[SORT(allbols)]
        nallbols = N_ELEMENTS(allbols)
     ENDIF

     shortfiles[iscan] = curr_tod.shortfile
     bbids[iscan]      = curr_tod.bbid
     obsids[iscan]     = curr_tod.obsid

     ;;Make list of bolometers we will loop over
     ;;So this only includes this band
     IF iscan EQ 0 THEN BEGIN
        bolometerlist = curr_tod.chan[boloind]
        bolometerlist = bolometerlist[ SORT(bolometerlist) ]
        nbolometerlist = N_ELEMENTS(bolometerlist)
     ENDIF

     samptime = 1.0/DOUBLE( curr_tod.sampfreq )

     ;; loop over bolos
     FOR i=0,nchans-1 DO BEGIN
        
        ;;Find position of this bolometer in this channel
        ;;That is, we try -not- to assume that the bolometers
        ;; are in the same order in all tods!
        this_bolindex = WHERE( curr_tod.chan EQ bolometerlist[i],$
                               nthisbol )
        IF nthisbol EQ 0 THEN BEGIN
           errmsg = "Couldn't find bolometer: "+bolometerlist[i]+" in tod: "+$
                    curr_tod.shortfile
           success = 0b
           RETURN
        ENDIF
        IF nthisbol GT 1 THEN BEGIN
           errmsg = "Multiple matches to bolometer: "+bolometerlist[i]+$
                    " in tod: "+curr_tod.shortfile
           success = 0b
           RETURN
        ENDIF
        this_bolindex = this_bolindex[0]

        ;; find good data points
        wkeep = WHERE( ITERMAP_ISGOOD(curr_tod, this_bolindex, $
                                      excludemask, min_scanfrac, $
                                      FIXEDDEGLITCH=fixeddeglitch,$
                                      DEGLITCHINFO=deglitchinfo, $
                                      BADBOLOS=badbolos), nkeep )

        IF nkeep GT 0 THEN BEGIN
           ;; build poly based on offsets
           polyfit = DBLARR(nkeep)
           time = curr_tod.samptime[wkeep] - $
                  curr_tod.samptime[0]
           FOR k=0,ntermsbyscan[iscan]-1 DO $
              polyfit += offsets[iscan,i,k] * time^DOUBLE(k)
           
           signal = gains[i] * $
                    (curr_tod.signal[this_bolindex,wkeep] - polyfit)

           IF tod_type EQ 'STRING' THEN BEGIN
              AD2XY, curr_tod.ra[this_bolindex,wkeep], $
                     curr_tod.dec[this_bolindex,wkeep], $
                     map.astrometry, $
                     thisx, thisy
           ENDIF ELSE BEGIN
               ; stored in tod or externally?
               IF curr_tod.proj_stored THEN BEGIN
                   thisx = REFORM(curr_tod.ra[this_bolindex,wkeep])
                   thisy = REFORM(curr_tod.dec[this_bolindex,wkeep])
               ENDIF ELSE BEGIN
                   thisx = REFORM((*x_ptrs[iscan])[this_bolindex,wkeep])
                   thisy = REFORM((*y_ptrs[iscan])[this_bolindex,wkeep])
               ENDELSE
           ENDELSE
           
           ; reject samples outside of map area
           wkeep_map = WHERE(thisx GE 0 AND thisx LT map.xsize AND $
                             thisy GE 0 AND thisy LT map.ysize, nkeep,$
                             NCOMPLEMENT=nremove)
           IF nkeep EQ 0 THEN BEGIN
              st = N_ELEMENTS( TEMPORARY(thisx) )
              st = N_ELEMENTS( TEMPORARY(thisy) )
              CONTINUE
           ENDIF
           IF nremove NE 0 THEN BEGIN
              signal = signal[ wkeep_map ]
              thisx  = thisx[ wkeep_map ]
              thisy  = thisy[ wkeep_map ]
           ENDIF

           ;;Pixel index
           pixind = ROUND( TEMPORARY(thisx) ) + $
                    ROUND( TEMPORARY(thisy) ) * LONG(map.xsize)

      ;stop
           IF doclip THEN BEGIN
              ;;Here we are building new glitch info
              resid = ABS( REFORM(signal - oldmap[pixind]) )

              scan_rms = SQRT(variances[iscan,i])
              IF grow_clip LT 2 THEN BEGIN
                 ;;Simple case, not growing masked areas
                 clipkeep = WHERE(resid LE $
                                  clipsigma * scan_rms,$
                                  nclipkeep, NCOMPLEMENT=nclipremove,$
                                  COMPLEMENT=clipremove )
              ENDIF ELSE BEGIN
                 ;;Grow masked region around each masked object
                 clipremove = WHERE(resid GT $
                                    clipsigma*scan_rms,$
                                    nclipremove, NCOMPLEMENT=nclipkeep)
                 IF nclipremove GT 0 THEN BEGIN
                    rem = BYTARR(N_ELEMENTS(signal))
                    rem[clipremove] = 1b
                    rem = DILATE(rem,REPLICATE(1b,grow_clip))
                    rem[clipremove] = 1b ;; dilate can misbehave at the edges
                    clipkeep = WHERE( ~TEMPORARY(rem), nclipkeep, $
                                      NCOMPLEMENT=nclipremove,$
                                      COMPLEMENT=clipremove )
                 ENDIF

              ENDELSE

              ;;Mark clipped samples if present
              IF nclipremove GT 0 THEN BEGIN
                 ;;Mark samples
                 bolname = curr_tod.chan[this_bolindex]
                 chanpos = VALUE_LOCATE( allbols, bolname )
                 IF (chanpos LT 0) || (chanpos GE nallbols) || $
                    (allbols[chanpos] NE bolname) $
                 THEN BEGIN
                    success = 0B
                    errmsg = "Could not find current bolometer "+$
                             curr_tod.chan[i]+" in list of bolometers"+$
                             " from first TOD "
                    RETURN
                 ENDIF
                 
                 newdegsample = REPLICATE(deglitchsample,nclipremove)
                 newdegsample.fileidx = iscan
                 newdegsample.bolidx  = chanpos
                 newdegsample.samp    = wkeep[wkeep_map[clipremove]]
                 
                 ;;This is quite inefficient, but it's hard to
                 ;; see a better way without knowing how many we
                 ;; are going to reject ahead of time
                 IF N_ELEMENTS(glitchsamples) EQ 0 THEN $
                    glitchsamples=TEMPORARY(newdegsample) ELSE $
                       glitchsamples=[glitchsamples,TEMPORARY(newdegsample)]
                 
                 ;;If we have fewer good samples than number of poly
                 ;; terms plus a bit, this isn't going to work, so
                 ;; skip the entire scan.  Also, we have a minimum
                 ;; scan fraction requirement.  However, we don't mark
                 ;; these -- not sure if that is a mistake
                 IF nclipkeep LT ntermsbyscan[iscan]+2 OR $
                    nclipkeep LT min_scanfrac*nkeep THEN CONTINUE

                 ;;Do the actual removal
                 signal = signal[clipkeep]
                 pixind = pixind[TEMPORARY(clipkeep)]
                 nkeep  = nclipkeep
              ENDIF
              
           ENDIF            ;; end clip

           ;;This accumulates the actual map bits into the pixels
           ;; using a call to a c function for speed (and, yes, the
           ;; speed difference is huge)
           var         = DOUBLE( variances[iscan,i] )
           npix        = LONG(map.xsize) * LONG(map.ysize)
           dovarweight = LONG(dowght)

           ;; verify types of inputs
           IF (SIZE(signal,      /TNAME) NE 'DOUBLE' OR $
               SIZE(var,         /TNAME) NE 'DOUBLE' OR $
               SIZE(pixind,      /TNAME) NE 'LONG' OR $
               SIZE(nkeep,       /TNAME) NE 'LONG' OR $
               SIZE(signalmap,   /TNAME) NE 'DOUBLE' OR $
               SIZE(errormap,    /TNAME) NE 'DOUBLE' OR $
               SIZE(weightmap,   /TNAME) NE 'DOUBLE' OR $
               SIZE(exposuremap, /TNAME) NE 'DOUBLE' OR $
               SIZE(npix,        /TNAME) NE 'LONG' OR $
               SIZE(dovarweight, /TNAME) NE 'LONG' OR $
               SIZE(samptime,    /TNAME) NE 'DOUBLE') THEN BEGIN
              success = 0B
              errmsg = "Inputs to 'smap_accumulatemap_extern' are of "+$
                       "incorrect type."
              RETURN
           ENDIF

           status = CALL_EXTERNAL(SMAP_GET_CALLEXT(), $
                                  'smap_accumulatemap_extern', $  
                                  signal, var, pixind, nkeep, $
                                  signalmap, errormap, weightmap, $
                                  exposuremap, samptime, npix, dovarweight, $
                                  VALUE=[0,0,0,1,0,0,0,0,0,1,1], $
                                  /CDECL)

           IF ~status THEN BEGIN
              success = 0B
              errmsg = "Call to 'smap_accumulatmap_extern' failed."
              RETURN
           ENDIF
           st = N_ELEMENTS(TEMPORARY(signal))
           st = N_ELEMENTS(TEMPORARY(pixind))
        ENDIF                   ;; end nkeep test
        
     ENDFOR                     ;; end loop over bolometers
  ENDFOR                        ;; end loop over scans

  ;; copy back onto map structure
  map.image    = TEMPORARY(signalmap)
  map.error    = TEMPORARY(errormap)
  map.exposure = TEMPORARY(exposuremap)

  ;; finalize
  goodpix = WHERE(map.exposure GE min_exposure, ngood, COMPLEMENT=badpix)

  ;; good pixels
  IF ngood GT 0 THEN BEGIN
     ;;We need to normalize out the weights we used
     map.image[goodpix] /= weightmap[goodpix]
     map.error[goodpix] /= weightmap[goodpix]^2
     map.error[goodpix] = SQRT(map.error[goodpix]) ;;from var to sigma
     st = N_ELEMENTS(TEMPORARY(weightmap))
  ENDIF

  ;; bad pixels
  IF ngood LT N_ELEMENTS(map.exposure) THEN BEGIN
     map.image[badpix]    = !VALUES.D_NAN
     map.error[badpix]    = !VALUES.D_NAN
  ENDIF
  
  ;;Build up the deglitchinfo
  ;;Note this will blow away any previous one -unless- we are using
  ;; fixed deglitching, which is desired
  IF doclip THEN BEGIN
     deglitchinfo = itermap_setupdeglitch(tods, TEMPORARY(glitchsamples),$
                                          SUCCESS = dsuccess, $
                                          ERRMSG = derrmsg)
     IF dsuccess EQ 0 THEN BEGIN
        errmsg = "Error making deglitchinfo: "+derrmsg
        RETURN
     ENDIF
     deglitchinfo.filenames = TEMPORARY(shortfiles)
     deglitchinfo.bbids     = TEMPORARY(bbids)
     deglitchinfo.obsids    = TEMPORARY(obsids)
  ENDIF

  success = 1B

END

;; create a header for map structure
FUNCTION ITERMAP_MAKEHEADER, map, niter, first_offs, first_gain, $
                             first_wt, fixed_nt, nterms, nt_scale, $
                             min_exposure, first_clip, clipsigma, grow_clip

  COMPILE_OPT IDL2, HIDDEN

  ;; some of this copied form write_smap_fitsmap.pro

  ;; create basic header
  dummy = DBLARR(map.xsize, map.ysize, niter)
  MKHDR, header, dummy

  ;; add astrometry
  PUTAST, header, map.astrometry

  ;; other info
  SXADDPAR, header, 'DESC', map.bands
  SXADDPAR, header, 'WAVELN', map.lambda

  ;; itermap info
  SXADDPAR, header, 'NITER', niter, "Number of iterations"
  SXADDPAR, header, 'FIRSTO', first_offs, $
            "First iteration where offsets are calculated"
  SXADDPAR, header, 'FIRSTG', first_gain, $
            "First iteration where gains are calculated"
  SXADDPAR, header, 'FIRSTW', first_wt, $
            "First iteration where weights are calculated"
  IF first_clip NE 0 THEN BEGIN
     SXADDPAR, header, 'DOCLIP', 1b, '2nd level deglitching used'
     SXADDPAR, header, 'FIRSTCLP',first_clip,$
               "First iteration where clipping is applied"
     SXADDPAR, header, 'CLIPSIG',clipsigma,$
               "Clipping sigma in terms of scan RMS"
     SXADDPAR, header, 'CLIPGROW', grow_clip,$
               "Clipping growing radius"
  ENDIF ELSE BEGIN
     SXADDPAR, header, 'DOCLIP', 0b, '2nd level deglitching used'
  ENDELSE

  IF fixed_nt THEN $
     SXADDPAR, header, 'NTERMS', nterms, $
               "Number of terms in polynomial fit" $
  ELSE $
     SXADDPAR, header, 'NTSCALE', nt_scale, $
               "Time scale for nterms in poly fit"

  SXADDPAR, header, 'MINHITS', min_exposure, $
            "Min. exposure (sec) to be included in map"

  RETURN, header
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Read in parameters from previous run (stored as FITS file)
;; Does TOD re-ordering if needed
FUNCTION SMAP_ITERMAP_READDIAGS, diagparamsname, mapparam, $
                                 PARAMS_TODIDX=params_todidx, $
                                 SUCCESS=success, ERRMSG=errmsg
  COMPILE_OPT IDL2, HIDDEN
  success = 0b
  errmsg = ""


  IF FILE_TEST(diagparamsname, /READ) EQ 0 THEN BEGIN
     errmsg = "can't read input parameters file '"+diagparamsname+"'"
     RETURN, !VALUES.F_NAN
  ENDIF
  
;; Figure out which extension is which
  FITS_OPEN, diagparamsname, fcb, /NO_ABORT, MESSAGE=mssg
  FITS_CLOSE, fcb
  IF mssg NE '' THEN BEGIN
     errmsg = "Failed to read FITS param file: "+diagparamsname+": "+mssg
     RETURN, !VALUES.F_NAN
  ENDIF

  ;; See if we have old map params extension
  extmp = WHERE(fcb.extname EQ "mapparams",nmp)
  IF nmp EQ 1 THEN BEGIN
     has_oldmp = 1b
     old_mapparam = MRDFITS(diagparamsname, extmp[0], /SILENT,$
                            STATUS=mpstatus, /UNSIGNED)
     IF mpstatus NE 0 THEN BEGIN
        errmsg = "Unable to read old mapparams from param file: "+$
                 diagparamsname
        RETURN, !VALUES.F_NAN
     ENDIF

     ;;Try to match order of tods
     idx_old = mapparams_matchorder(old_mapparam, mapparam, $
                                    SUCCESS=matsuc, ERRMSG=materr)
     IF matsuc EQ 0 THEN BEGIN
        errmsg = "Unable to match TOD order from old param file: "+$
                 diagparamsname+": "+materr
        RETURN, !VALUES.F_NAN
     ENDIF
  ENDIF ELSE has_oldmp = 0b

  ;;Get headers
  RDFITS_STRUCT, diagparamsname, paramsstruct, /SILENT
  nterms_in = SXPAR(paramsstruct.hdr0, "NUMEXTEN") - 2
  IF has_oldmp THEN nterms_in -= 1                     ;;mapparam is one more
  nscans_in = SXPAR(paramsstruct.hdr1, "NAXIS1")       ;;number of TODs
  nchans_in = SXPAR(paramsstruct.hdr1, "NAXIS2")       ;;Number of bols
  niter_in  = SXPAR(paramsstruct.hdr1, "NAXIS3")       ;;Number of iterations
  IF niter_in EQ 0 THEN niter_in = 1
  
  ;; test tod_index
  IF KEYWORD_SET(params_todindex) THEN BEGIN
     IF N_ELEMENTS(params_todindex) NE mapparam.nscans THEN BEGIN
        errmsg = "wrong number of elements in TOD_INDEX"
        RETURN, !VALUES.F_NAN
     ENDIF
           
     IF MAX(params_todindex) GE nscans_in THEN BEGIN
        errmsg = "max element in TOD_INDEX too large for "+$
                 "parameters file"
        RETURN, !VALUES.F_NAN
     ENDIF
     IF has_oldmp THEN pt = idx_old[params_todindex] ELSE $
        pt = params_todindex
  ENDIF ELSE BEGIN
     IF has_oldmp THEN pt = idx_old ELSE $
        pt = INDGEN(mapparam.nscans)
  ENDELSE
        
  nterms = nterms_in

  retstruct = {dooffs:0b, dogain:0b, dowght:0b, doclip:0b, $
               nscans: mapparam.nscans, nchans: nchans_in, nterms: nterms, $
               ntermsbyscan: REPLICATE(nterms, mapparam.nscans),$
               offsets: FLTARR(mapparam.nscans, nchans_in, nterms), $
               variances: FLTARR(mapparam.nscans, nchans_in), $
               gains: FLTARR(nchans_in)}

  ;; read offsets
  FOR oi=0,nterms-1 DO $
     retstruct.offsets[*,*,oi] = paramsstruct.((3+oi)*2+1)[pt, *, niter_in - 1]

  ;; read variances
  retstruct.variances = paramsstruct.im1[pt, *, niter_in - 1]
        
  ;; read gains
  retstruct.gains = paramsstruct.im2[0, *, niter_in-1]

  success = 1b
  RETURN, retstruct

END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Main routine
PRO SMAP_ITERMAP, tods, mapparam, map250, map350, map500, $
                  ITERMAP_PARAMS=itermap_params, IFIXED250=ifixed250, $
                  IFIXED350=ifixed350, IFIXED500=ifixed500, $
                  SPARAMS250=sparams250, SPARAMS350=sparams350, $
                  SPARAMS500=sparams500, FIXEDPARAMS=fixedparams, $
                  FIXEDPARDIR=fixedpardir, BADBOLOS=badbolos, $
                  EXCLUDEMASK=excludemask, $
                  EXCLUDEWEIGHTMASK=excludeweightmask,$
                  SAVEMAPS=savemaps, SAVEPARAMS=saveparams, $
                  SAVEMAPDIR=savemapdir, SUCCESS=success, ERRMSG=errmsg, $
                  VERBOSE=verbose, EXNAME=exname, $
                  PARAMS_TODINDEX=params_todindex,$
                  NO250=no250, NO350=no350, NO500=no500, $
                  FIXEDDEGLITCH=fixeddeglitch, DEGLITCH250=deglitch250,$
                  DEGLITCH350=deglitch350, DEGLITCH500=deglitch500, $
                  STORE_PIXINFO=store_pixinfo
                  

  COMPILE_OPT IDL2

  ;; set up the error handling variables
  success = 0b
  errmsg = ''

  diagmapspre   = "ITERMAP_diagnostic_maps_"
  diagparamspre = "ITERMAP_diagnostic_params_"

  IF KEYWORD_SET(savemaps)      THEN dosavemaps   = 1B ELSE dosavemaps   = 0B
  IF KEYWORD_SET(saveparams)    THEN dosaveparams = 1B ELSE dosaveparams = 0B
  IF ~ KEYWORD_SET(savemapdir)  THEN savemapdir = !SMAP_MAPS

  ;; Figure out if we are using fixed parameters per band
  IF N_ELEMENTS(ifixed250) NE 0 THEN use_fixed_250 = 1b ELSE use_fixed_250 = 0b
  IF N_ELEMENTS(ifixed350) NE 0 THEN use_fixed_350 = 1b ELSE use_fixed_350 = 0b
  IF N_ELEMENTS(ifixed500) NE 0 THEN use_fixed_500 = 1b ELSE use_fixed_500 = 0b
  IF N_ELEMENTS(fixedparams) NE 0 THEN BEGIN
     use_fixed_250 = 1b
     use_fixed_350 = 1b
     use_fixed_500 = 1b
  ENDIF

  IF KEYWORD_SET(fixeddeglitch) THEN BEGIN
     IF ~ KEYWORD_SET(no250) && N_ELEMENTS(deglitch250) EQ 0 THEN BEGIN
        errmsg = "Must provide DEGLITCH250 if /FIXEDDEGLITCH is set "+$
                 " and making 250 micron map"
        GOTO, err_handler
     ENDIF
     IF ~ KEYWORD_SET(no350) && N_ELEMENTS(deglitch350) EQ 0 THEN BEGIN
        errmsg = "Must provide DEGLITCH350 if /FIXEDDEGLITCH is set "+$
                 " and making 350 micron map"
        GOTO, err_handler
     ENDIF
     IF ~ KEYWORD_SET(no500) && N_ELEMENTS(deglitch500) EQ 0 THEN BEGIN
        errmsg = "Must provide DEGLITCH500 if /FIXEDDEGLITCH is set "+$
                 " and making 500 micron map"
        GOTO, err_handler
     ENDIF
     MESSAGE,"Using previous deglitch info",/INF
  ENDIF

  ;;Setup mask for exclusion from polynomial fitting, variances, etc.
  IF N_ELEMENTS( excludemask ) NE 0 THEN BEGIN
     IF N_ELEMENTS( excludeweightmask ) NE 0 THEN $
        polyexcludemask = [excludemask, excludeweightmask ] ELSE $
           polyexcludemask = excludemask
  ENDIF

  savemapdir = ADDSLASH(savemapdir)

  ;; if we didn't get an exname, set it to null
  IF ~(KEYWORD_SET(exname)) THEN exname = ''

  ;; default parameter values
  niter        = 10
  first_offs   = 1
  first_gain   = 3
  first_wt     = 7
  first_clip   = 0
  grow_clip    = 3
  clipsigma    = 10.0
  nterms       = 1
  min_exposure = 0.1
  fixed_nt     = 0
  nt_scale     = 50.0 ;; ??? is this a good default?

  min_nterms = 1
  max_nterms = 10

  ;; minimum fraction of scan that must be good in order to use scan
  min_scanfrac = 0.3

  ;; read parameters from structure keyword
  IF KEYWORD_SET(itermap_params) THEN BEGIN
     IF TAG_EXIST(itermap_params, "niter") THEN $
        niter        = FIX(itermap_params.niter)
     IF TAG_EXIST(itermap_params, "first_offs") THEN $
        first_offs   = FIX(itermap_params.first_offs)
     IF TAG_EXIST(itermap_params, "first_gain") THEN $
        first_gain   = FIX(itermap_params.first_gain)
     IF TAG_EXIST(itermap_params, "first_wt") THEN $
        first_wt     = FIX(itermap_params.first_wt)
     IF TAG_EXIST(itermap_params, "first_clip") THEN $
        first_clip   = FIX(itermap_params.first_clip)
     IF TAG_EXIST(itermap_params, "clipsigma") THEN $
        clipsigma    = DOUBLE(itermap_params.clipsigma)
     IF TAG_EXIST(itermap_params, "grow_clip") THEN $
        grow_clip    = FIX(itermap_params.grow_clip)
     IF TAG_EXIST(itermap_params, "nterms") THEN $
        nterms       = FIX(itermap_params.nterms)
     IF TAG_EXIST(itermap_params, "min_exposure") THEN $
        min_exposure = FIX(itermap_params.min_exposure)
     IF TAG_EXIST(itermap_params, "fixed_nt") THEN $
        fixed_nt     = FIX(itermap_params.fixed_nt)
     IF TAG_EXIST(itermap_params, "nt_scale") && $
	itermap_params.nt_scale GT 0.0 THEN $
        nt_scale     = FLOAT(itermap_params.nt_scale)
  ENDIF

  IF first_clip NE 0 AND first_clip GT niter THEN BEGIN
     MESSAGE,"First_clip is larger than niter -- will not deglitch",/INF
     first_clip = 0
  ENDIF

  ;; read data (if necessary)
  
  ;; calculate astrometry (if data in memory)
  x_ptrs = PTRARR(mapparam.nscans)
  y_ptrs = PTRARR(mapparam.nscans)

  CASE SIZE(tods, /TNAME) OF 
     "POINTER": BEGIN
        first_tod = *tods[0]
        
        FOR iscan=0,mapparam.nscans-1 DO BEGIN

            ; check if pixel projection stored
            IF (*tods[iscan]).proj_stored THEN BEGIN

                ; error if STORE_PIXINFO not set
                IF NOT KEYWORD_SET(store_pixinfo) THEN BEGIN
                   errmsg = "tod structure has pixel projection stored, " + $
                            "but STORE_PIXINFO not set"
                   GOTO, err_handler
               ENDIF
            ENDIF ELSE BEGIN

                ; calculate projection
                SMAP_GETMAP_XY, *tods[iscan], map250, map350, map500, $
                                xvals, yvals

                IF KEYWORD_SET(store_pixinfo) THEN BEGIN
                    ; store inside tod structure (and set flag)
                    (*tods[iscan]).ra = xvals
                    (*tods[iscan]).dec = yvals
                    (*tods[iscan]).proj_stored = 1B
                ENDIF ELSE BEGIN
                    ; store externally
                    x_ptrs[iscan] = PTR_NEW(xvals)
                    y_ptrs[iscan] = PTR_NEW(yvals)
                ENDELSE
            ENDELSE
        ENDFOR
        
        IF TAG_EXIST(first_tod, 'mask_bits', /TOP_LEVEL) AND $
           N_ELEMENTS(excludemask) GT 0 THEN $
              def_mapmaskbits = $
           construct_mask_bitmask(excludemask, first_tod.mask_bits)        
     END
     "STRING": BEGIN
        ;; read first tod
        first_tod = SMAP_READTOD(tods[0], SUCCESS=rsuccess, ERRMSG=errmsg)
        IF rsuccess EQ 0 THEN BEGIN
           errmsg = "Error reading tod from "+tods[i]+": "+errmsg
           GOTO, err_handler
        ENDIF
        IF TAG_EXIST(first_tod, 'mask_bits', /TOP_LEVEL) AND $
           N_ELEMENTS(excludemask) GT 0 THEN $
              def_mapmaskbits = $
           construct_mask_bitmask(excludemask, first_tod.mask_bits)        
     END
     ELSE: BEGIN
        errmsg = "Unexpected TOD type in smap_make_maps_naivemap: "+tod_type
        GOTO, err_handler
     END
  ENDCASE

  ;; get lengths of each scan for nterms calculation (if not fixed,
  ;; otherwise use nterms).  These will be the same in all bands,
  ;; so if any are provided by the user, we can use that
  IF ~ (use_fixed_250 AND use_fixed_350 AND use_fixed_500) THEN BEGIN
     IF fixed_nt THEN BEGIN
        ntermsbyscan = REPLICATE(nterms, mapparam.nscans)
     ENDIF ELSE BEGIN
        ntermsbyscan = INTARR(mapparam.nscans)

        CASE SIZE(tods, /TNAME) OF 
           "POINTER": BEGIN
              FOR s=0,mapparam.nscans-1 DO BEGIN
                 scant = (*tods[s]).nsamps / (*tods[s]).sampfreq
                 ntermsbyscan[s] = ROUND(scant / nt_scale)
              ENDFOR
           END
           "STRING": BEGIN
              FOR s=0,mapparam.nscans-1 DO BEGIN
                 thistod = SMAP_READTOD(tods[s], SUCCESS=rsuccess, $
                                        ERRMSG=errmsg)
                 scant = thistod.nsamps / thistod.sampfreq
                 ntermsbyscan[s] = ROUND(scant / nt_scale)
              ENDFOR
           END
        ENDCASE
        
        ntermsbyscan = ntermsbyscan > min_nterms < max_nterms

        ;; maximum number of nterms (for declaration purposes)
        nterms = MAX(ntermsbyscan)
     ENDELSE
  ENDIF

  ;; loop over bands
  firstcoldone = 0b ;;Set to false the first actual map making pass
  
  FOR icol=0,2 DO BEGIN
     
     ;;Skip if asked to not do this band
     ;;continues not allowed in case statements
     IF icol EQ 0 && KEYWORD_SET(no250) THEN CONTINUE
     IF icol EQ 1 && KEYWORD_SET(no350) THEN CONTINUE
     IF icol EQ 2 && KEYWORD_SET(no500) THEN CONTINUE

     IF KEYWORD_SET(verbose) THEN $
        MESSAGE, "Performing iterative map making on band '" + $
                 mapparam.bands[icol] + "'.", /INF
     
     CASE icol OF
        0: BEGIN
           map = map250
           boloind = WHERE(STRMID(first_tod.chan,0,3) EQ $
                           mapparam.bands[0] AND $
                           first_tod.islight, nchans)
           IF KEYWORD_SET(fixeddeglitch) THEN $
              deglitchinfo = deglitch250
           use_fixed = use_fixed_250
           IF N_ELEMENTS(ifixed250) THEN scan_params = ifixed250 $
           ELSE DELVARX, scan_params
        END
        1: BEGIN
           map = map350
           boloind = WHERE(STRMID(first_tod.chan,0,3) EQ $
                           mapparam.bands[1] AND $
                           first_tod.islight, nchans)
           IF KEYWORD_SET(fixeddeglitch) THEN $
              deglitchinfo = deglitch350
           use_fixed = use_fixed_350
           IF N_ELEMENTS(ifixed350) THEN scan_params = ifixed350 $
           ELSE DELVARX, scan_params
        END
        2: BEGIN
           map = map500
           boloind = WHERE(STRMID(first_tod.chan,0,3) EQ $
                           mapparam.bands[2] AND $
                           first_tod.islight, nchans)
           IF KEYWORD_SET(fixeddeglitch) THEN $
              deglitchinfo = deglitch500
           use_fixed = use_fixed_500
           IF N_ELEMENTS(ifixed500) THEN scan_params = ifixed500 $
           ELSE DELVARX, scan_params
        END
     ENDCASE
     map.tod_excludemask = def_mapmaskbits

     IF use_fixed THEN BEGIN
        IF N_ELEMENTS(scan_params) EQ 0 THEN BEGIN
           ;; Not already available from user provided structure, so
           ;; we have to read them from a file (fixedparams)
           IF N_ELEMENTS(fixedparams) EQ 0 THEN BEGIN
              errmsg = "Can't figure out how to get fixed parameters"
              GOTO, err_handler
           ENDIF
           ;; Construct explicit filename
           IF KEYWORD_SET(fixedpardir) THEN $
              fixedmapdir = ADDSLASH(fixedpardir) $
           ELSE $
              fixedmapdir = savemapdir
           diagparamsname = fixedmapdir + diagparamspre + fixedparams + $
                            "_" + map.names + ".fits"
           
           scan_params = SMAP_ITERMAP_READDIAGS(diagparamsname, mapparam,$
                                                PARAMS_TODIDX=params_todidx,$
                                                SUCCESS=fsuccess, $
                                                ERRMSG=ferrmsg)
           IF fsuccess EQ 0 THEN BEGIN
              errmsg = "Error reading in previous parameters: "+ferrmsg
              GOTO, err_handler
           ENDIF

           ;; do offsets/gains/variance weighting? (0 means don't do it)
           IF first_offs NE 0 && $
              niter GE first_offs THEN scan_params.dooffs = 1b
           
           IF first_gain NE 0 && $
              niter GE first_gain THEN scan_params.dogain = 1b
           
           IF first_wt   NE 0 && $
              niter GE first_wt THEN scan_params.dowght = 1b
        ENDIF
        scan_params.doclip = 0b ;;shouldn't clip now, we aren't iterating

        ;; Anyways, one way or the other we have scan_params now, so
        ;; make map
        ITERMAP_MAKEMAP, mapparam, tods, x_ptrs, y_ptrs, $
                         boloind, map, scan_params.offsets, $
                         scan_params.gains, $
                         scan_params.variances, scan_params.dowght, $
                         scan_params.doclip, clipsigma, min_exposure, $
                         scan_params.ntermsbyscan, $
                         excludemask, min_scanfrac, grow_clip, $
                         FIXEDDEGLITCH=fixeddeglitch,$
                         DEGLITCHINFO=deglitchinfo, $
                         BADBOLOS=badbolos, SUCCESS=success, ERRMSG=errmsg, $
                         VERBOSE=verbose

        IF ~success THEN BEGIN
           MESSAGE, errmsg, /INF
           RETURN
        ENDIF

     ENDIF ELSE BEGIN           ;; end fixed parameters case
        ;;This is the 'normal' map making mode, where we are computing
        ;; the scan by scan parameters by hand

        ;; declare arrays to store map after each iteration
        IF dosavemaps THEN BEGIN
                                ; images
           imagecube = DBLARR(map.xsize, map.ysize, niter)
           errorcube = imagecube
           hitscube  = ULONARR(map.xsize, map.ysize, niter)
        ENDIF

        IF dosaveparams THEN BEGIN
           ;; parameters
           offscube  = DBLARR(mapparam.nscans, nchans, nterms, niter)
           varscube  = DBLARR(mapparam.nscans, nchans, niter)
           gainscube = DBLARR(1, nchans, niter)
        ENDIF

        ;; initialize flags
        dooffs = 0B
        dogain = 0B
        dowght = 0B
        doclip = 0B

        ;; initialize arrays
        ;; assume same bolometers are present in each scan (tested above)
        offsets   = REPLICATE(0.0, mapparam.nscans, nchans, nterms)
        variances = REPLICATE(1.0, mapparam.nscans, nchans)
        gains     = REPLICATE(1.0, nchans)
        
        ;; note: assume that maps are initialized to zero
        
        ;; loop over iterations
        FOR iter=0,niter-1 DO BEGIN
           
           IF KEYWORD_SET(verbose) THEN $
              MESSAGE,STRING(iter+1,niter,$
                             FORMAT='("On iteration ",I0," of ",I0)'),/INF
           
           ;; do offsets/gains/variance weighting? (0 means don't do it)
           IF first_offs NE 0 && $
              iter+1 GE first_offs THEN dooffs = 1B

           IF first_gain NE 0 && $
              iter+1 GE first_gain THEN dogain = 1B

           IF first_wt   NE 0 && $
              iter+1 GE first_wt   THEN dowght = 1B

           IF ~ KEYWORD_SET( fixeddeglitch ) && first_clip NE 0 && $
              iter+1 GE first_clip THEN doclip = 1B

           ;; calculate offsets, gains and variances
           ITERMAP_CALCPARAMS, mapparam, tods, x_ptrs, y_ptrs, $
                               boloind, map, dooffs, dogain, doclip, $
                               clipsigma, offsets, gains, variances, $
                               ntermsbyscan, polyexcludemask, min_scanfrac, $
                               grow_clip, FIXEDDEGLITCH=fixeddeglitch,$
                               DEGLITCHINFO=deglitchinfo, BADBOLOS=badbolos, $
                               VERBOSE=verbose, SUCCESS=cwsuccess,$
                               ERRMSG=cwerrmsg
           IF cwsuccess EQ 0 THEN BEGIN
              errmsg = "Error calculating map params: "+cwerrmsg
              GOTO, err_handler
           ENDIF

           ;; make map
           ITERMAP_MAKEMAP, mapparam, tods, x_ptrs, y_ptrs, $
                            boloind, map, offsets, gains, variances, dowght, $
                            doclip, clipsigma, min_exposure, ntermsbyscan, $
                            excludemask, min_scanfrac, grow_clip, $
                            FIXEDDEGLITCH=fixeddeglitch,$
                            DEGLITCHINFO=deglitchinfo, BADBOLOS=badbolos, $
                            SUCCESS=mksuccess, ERRMSG=errmsg, VERBOSE=verbose
           IF ~mksuccess THEN GOTO, err_handler

           ;; store map cubes, if requested
           IF dosavemaps THEN BEGIN
              imagecube[*,*,iter] = map.image
              errorcube[*,*,iter] = map.error
              hitscube[*,*,iter]  = map.exposure
           ENDIF

           IF dosaveparams THEN BEGIN
              offscube[*,*,*,iter] = offsets
              varscube[*,*,iter]  = variances
              gainscube[0,*,iter]  = gains
           ENDIF

        ENDFOR                  ; end loop over map iterations

        ;; Store last set of params
        scan_params = {dooffs: dooffs, dogain: dogain, dowght:dowght, $
                       doclip: doclip, nscans: mapparam.nscans, $
                       nchans: nchans, nterms: nterms,$
                       ntermsbyscan: ntermsbyscan,$
                       offsets: offsets, variances: variances,$
                       gains: gains}
        

        ;; write saved maps to file
        IF dosavemaps THEN BEGIN
           IF KEYWORD_SET(verbose) THEN MESSAGE, "Saving map cubes", /INF
           
           filename = savemapdir + diagmapspre + $
                      exname + "_" + map.names + ".fits"

           maphead = ITERMAP_MAKEHEADER(map, niter, first_offs, $
                                        first_gain, first_wt, fixed_nt, $
                                        nterms, nt_scale, min_exposure,$
                                        first_clip, clipsigma, grow_clip)
           
           MKHDR, mainhead, 0B, /EXTEND
           
           ;; load extension names
           SXADDPAR, mainhead, "NUMEXTEN", 3
           SXADDPAR, mainhead, "EXTEN01", "image   "
           SXADDPAR, mainhead, "EXTEN02", "error   "
           SXADDPAR, mainhead, "EXTEN03", "exposure"
           
           WRITEFITS, filename, 0B, mainhead

           SXADDPAR, maphead, "EXTNAME", "image   "
           ;;FXWRITE, filename, maphead, imagecube, /APPEND
           MWRFITS, imagecube, filename, maphead
           
           errtype = "PROPAGAT"

           SXADDPAR, maphead, "EXTNAME", "error   "
           SXADDPAR, maphead, "ERRTYPE", errtype
           ;;FXWRITE, filename, maphead, errorcube, /APPEND
           MWRFITS, errorcube, filename, maphead
           SXDELPAR, maphead, "ERRTYPE"
           
           SXADDPAR, maphead, "EXTNAME", "exposure"
           SXADDPAR, maphead, "BITPIX", 32
           ;;FXWRITE, filename, maphead, hitscube, /APPEND
           MWRFITS, hitscube, filename, maphead
        ENDIF

        IF dosaveparams THEN BEGIN
           IF KEYWORD_SET(verbose) THEN MESSAGE, "Saving param cubes", /INF
           
           filename = savemapdir + diagparamspre + $
                      ;exname + map.names + ".fits"
                      exname + "_" + map.names + ".fits"

           MKHDR, mainhead, 0B, /EXTEND
           
           ;; load extension names
           SXADDPAR, mainhead, "NUMEXTEN", nterms+3
           SXADDPAR, mainhead, "EXTEN01", "vars    "
           SXADDPAR, mainhead, "EXTEN02", "gains   "
           FOR k=0,nterms-1 DO $
              SXADDPAR, mainhead, "EXTEN"+STRING(k+3, FORMAT='(I02)'), $
                        "offs"+STRING(k, '(I02)')
           SXADDPAR, mainhead, "EXTEN"+STRING(nterms+3, FORMAT='(I02)'),$
                     "mapparams"

           ;; load bolo names
           FOR b=0,nchans-1 DO $
              SXADDPAR, mainhead, "BOLO"+STRING(b+1, FORMAT='(I03)'), $
                        first_tod.chan[boloind[b]]

           FXWRITE, filename, mainhead

           FXHMAKE, head1, varscube, /XTENSION
           SXADDPAR, head1, "EXTNAME", "vars"
           FXWRITE, filename, head1, varscube, /APPEND
           
           FXHMAKE, head2, gainscube, /XTENSION
           SXADDPAR, head2, "EXTNAME", "gains"
           FXWRITE, filename, head2, gainscube, /APPEND
           
           FXHMAKE, head3, REFORM(offscube[*,*,0,*]), /XTENSION
           FOR k=0,nterms-1 DO BEGIN
              SXADDPAR, head3, "EXTNAME", "offs"+STRING(k, '(I02)')
              FXWRITE, filename, head3, REFORM(offscube[*,*,k,*]), /APPEND
           ENDFOR

           FXBHMAKE, head4, N_TAGS(mapparam), "mapparams"
           MWRFITS, mapparam, filename, head4, /SILENT, STATUS=status
           IF status NE 0 THEN BEGIN
              errmsg = "Failed to write mapparams to parameter output file"
              GOTO, err_handler
           ENDIF
        ENDIF
     ENDELSE ;; end fixedparams else statement

     ;;We only need to set the output deglitch variables if we
     ;; did clipping.  If we didn't clip we either never computed
     ;; them -or- we were doing fixed deglitching, in which case
     ;; we would only be overwriting deglitch??? with itself,
     ;; which is not necessary
     CASE icol OF
        0: BEGIN
           map250 = TEMPORARY(map)
           IF scan_params.doclip THEN deglitch250 = TEMPORARY(deglitchinfo)
           sparams250 = scan_params
        END
        1: BEGIN
           map350 = TEMPORARY(map)
           IF scan_params.doclip THEN deglitch350 = TEMPORARY(deglitchinfo)
           sparams350 = scan_params
        END
        2: BEGIN
           map500 = TEMPORARY(map)
           IF scan_params.doclip THEN deglitch500 = TEMPORARY(deglitchinfo)
           sparams500 = scan_params
        END
     ENDCASE
     firstcoldone = 1b

  ENDFOR ;; end loop over bands

  ;; if we got this far, we wrote three
  ;; maps out and have nothing more to do
  success=1b
  PTR_FREE, x_ptrs, y_ptrs

  RETURN

  err_handler:
  success = 0b
  IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
  PTR_FREE, x_ptrs, y_ptrs
  RETURN


END
