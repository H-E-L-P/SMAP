;+
;NAME
; smap_tcdrift_correct
;CATEGORY
; SMAP pipeline
;PURPOSE
; To stich together contiguous sets of HerMES data, low pass filter them, and
; then regress out the part of the bolometer signal correlated with the 
; thermistors for each bolometer device.
;
;USAGE 
; After reading in data, do
; 
;  SMAP_TCDRIFT_CORRECT,tods,DRIFTS=drifts,EXCLUDEMASK=excludemask,$
;    /VERBOSE,SUCCESS=stc_success,ERRMSG=stc_errmsg,/NOROBUST,/TODMEAN
; 
; then proceed to further filtering and map making.
;
;INPUTS/OUTPUTS
; tods       A pointer array of SMAP style TODS -- see smap_read_and_filter.
;            The data is replaced on output by the filtered data.  This is 
;            both a mandatory input and output.  
;KEYWORDS
; max_gap_allowed   Floating point number giving the maximum gap between
;                   obsids allowed before considering as a new set of data 
;                   which requires separate splicing and filtering.  Units
;                   are s, default is 10.
; excludemask       Exclude mask to use when calculating correlations - 
;                   this affects which data will be fit between the 
;                   bolometers and thermistors.  Has default if user
;                   does not provide.
; verbose           SMAP-standard verbosity flag.
; success           SMAP-standard success flag (1=successful completion).
; errmsg            SMAP-standard error messaging.
; no250/no350/no500 Skip the respective band
; norobust          Use non-robust correlation fit always
; todmean           Subtract the mean from each TOD rather than each
;                    stitched together timeline; this restores the old
;                    behavior
; nocorr            Don't actually do the correction -- only of
;                    if you want to examine thermstrut, stitchinfo, or 
;                    breakinfo
; nofilter          Do not apply filtering -- this is probably not a
;                                             good idea
;OPTIONAL INPUTS
; userbreak         Array of structures ( {.band, .todidx, .breakpos} )
;                    giving desired locations for additional, user specified
;                    breaks. .band is 'PSW', 'PMW', 'PLW' or 'ALL'
;OPTIONAL OUTPUTS
; thermstruct       Contains stitched together thermistor timelines 
; stitchinfo        Information about how the timelines were stitched together
; breakinfo         Information about breaks in the scans
;MODIFICATION HISTORY:
; Initial Author: Mike Zemcov, Original version 0.1, Aug 2010
; Modified: Mike Zemcov, First production version 1.0, Feb 7 2011.      
; Modified: Alex Conley, use robust line fitting, July 6, 2011                  
; Modified: Alex Conley, Rewrite to be more memory efficient, change how
;                        scans are mean subtracted: April 25, 2012
;-

;;Utility routine for building break structures

FUNCTION smap_tcdrift_getbreakstruct, nbreaks, breakpos, breakpos_nogap,$
                                      ntods, endsamp, endsamp_nogap

  COMPILE_OPT IDL2, HIDDEN, STRICTARRSUBS
  breakstruct = {startidx: 0uL, endidx: 0uL, startidx_nogap: 0uL, $
                 endidx_nogap: 0uL}
  IF nbreaks EQ 0 THEN BEGIN
     breakinfo = REPLICATE( breakstruct, 3 )
     breakinfo.endidx = endsamp[ntods-1]
     breakinfo.endidx_nogap = endsamp_nogap[ntods-1]
  ENDIF ELSE BEGIN
     breakinfo = REPLICATE( breakstruct, nbreaks + 1)
     breakinfo[0:nbreaks-1].endidx = breakpos
     breakinfo[nbreaks].endidx = endsamp[ntods-1]
     breakinfo[1:nbreaks].startidx = breakinfo[0:nbreaks-1].endidx+1
     breakinfo[0:nbreaks-1].endidx_nogap = breakpos_nogap
     breakinfo[nbreaks].endidx_nogap = endsamp_nogap[ntods-1]
     breakinfo[1:nbreaks].startidx_nogap = $
        breakinfo[0:nbreaks-1].endidx_nogap+1
  ENDELSE
  RETURN,breakinfo
END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;This does three things: 1) figures out where in the combined
;; timestream the data from each TOD will go and 2) finds breaks
;; 3) finds gaps and figures out how many timesamples we will need
;; to stick in to fill said gaps

;;The information gleaned is returned in two structures:
;; 1) stitchinfo, which contains infromation about how to stitch
;;    all the timestreams together into an uber-timestream,
;;    including gap filling.
;; 2) breakinfo which states where breaks are in the uber-timestream
;;Since the user can specify breaks but not gaps, these two
;; types of information can be kept separate.  That is, data from
;; a tod is -always- inserted as a contiguous chunk into the
;; ubertimestream because gaps (which are later filled) always occur
;; at the edges of timestreams.
;; stitchinfo contains the start and end index into the ubertimestream
;;  for the data from each tod as .startsamp and .endsamp.  It also
;;  contains a sub-structure array (gapinfo) which gives the start and
;;  end index of each gap that will be filled later.
;; breakinfo is an array of structures holding the start and end index
;;  for each contiguous data section.

;;The user can specify additional break positions using USERBREAK.

PRO smap_tcdrift_getscaninfo, tods, stitchinfo, breakinfo, $
                              USERBREAK=userbreak,$
                              MAX_GAP_ALLOWED=max_gap_allowed,$
                              SUCCESS=success, ERRMSG=errmsg, $
                              VERBOSE=verbose

  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN
  success = 0b
  errmsg  = ''

  IF SIZE(tods[0],/TNAME) NE 'POINTER' THEN BEGIN
     errmsg = "Expected pointer array of TODs"
     RETURN
  ENDIF
  ntods = N_ELEMENTS(tods)
  IF ntods EQ 0 THEN BEGIN
     errmsg = "No TODs!"
     RETURN
  ENDIF

  ;;max_gap_allowed is in seconds
  IF N_ELEMENTS(max_gap_allowed) THEN maxgap = 10. ELSE $
     maxgap = FLOAT(max_gap_allowed)
  sampfreq = (*tods[0]).sampfreq
  sampstep = 1. / sampfreq ; in seconds
  scanrate = determine_scan_rate(tods,VERBOSE=verbose,$
                                 SUCCESS=scrsuccess,ERRMSG=screrrmsg)
  IF scrsuccess EQ 0b THEN BEGIN
     errmsg = "Error finding scan rate: "+screrrmsg
     RETURN
  ENDIF

 IF KEYWORD_SET( verbose ) THEN $
     MESSAGE,string(scanrate,format=$
                    "('Scan rate = ',F6.3,' arcsec / second')"),/INF,$
             LEVEL=-1

  ;;This is maxgap converted to samples (from seconds)
  maxsamp = CEIL( maxgap / sampstep )

  ;; Collect some basic information to get started
  totsamps     = 0
  nbolos       = (*tods[0]).nchans
  nsamps       = (*tods[0]).nsamps
  
  ;; Step 1: figure out where each TODs data will go in the
  ;; stitched-together scans
  startsamp = ULONARR(ntods) ;;these will be the index into the ubertimestream
  endsamp   = ULONARR(ntods) ;;of the elements from each TOD
  startsamp[0] = 0
  endsamp[0]   = nsamps-1
  FOR i=1, ntods-1 DO BEGIN
     IF (*tods[i]).nchans NE nbolos THEN BEGIN
        errmsg = "TOD from "+(*tods[i]).shortfile+" has unexpected number "+$
                 " of bolometers: "+STRING( (*tods[i]).nchans, FORMAT='(I0)')+$
                 " instead of "+STRING(nbolos,FORMAT='(I0)')
        RETURN
     ENDIF
     nsamps = (*tods[i]).nsamps
     IF nsamps EQ 0 THEN BEGIN
        errmsg = (*tods[i]).shortfile+" has no data!"
        RETURN
     ENDIF
     startsamp[i] = endsamp[i-1]+1
     endsamp[i]   = startsamp[i] + nsamps - 1
  ENDFOR
  IF KEYWORD_SET(verbose) THEN $
     MESSAGE,STRING(endsamp[ntods-1]+1,$
                    FORMAT='("Total number of samples: ",I0)'),/INF,LEVEL=-1
  
  startsamp_nogap = startsamp
  endsamp_nogap = endsamp

  ;;There are two types of things we find here: gaps, and breaks
  ;; Breaks are essentially big gaps that we will -not- try to fill.
  ;; gaps are small enough to be filled in.

  ;;Right now we assume breaks apply to all bands equally, which
  ;; will be the case unless the user adds some

  ;;If the timestep is longer than the maximum allowed gap, then
  ;; this is a break, and we record the position.  Basically, we
  ;; are looking for cases where we got some observations, went away
  ;; for a long period, then came back to the field.  If turnaround
  ;; data has been excluded, then this will find all the turnarounds
  ;; as gaps, and such data won't benefit from stitching together the
  ;; timestreams.

  ;;Step 2: look for breaks, store locations
  ;;Get timesamples
  samptime = DBLARR( endsamp[ntods-1]+1 )
  ;;Stitch together sampletime
  FOR j=0,ntods-1 DO BEGIN
     startidx = startsamp[j]
     endidx   = endsamp[j]
     samptime[startidx:endidx]=(*tods[j]).samptime
  ENDFOR

  ;;Find the timesteps
  dt = ABS(samptime[1:*]-samptime[0:*])

  ;;And find the breaks, storing their positions
  ;; relative to the original uber-timestream
  ;;Have to be careful about the no breaks case
  breakpos250 = WHERE(dt GT maxgap,nbreaks)
  breakpos350 = breakpos250 & breakpos500 = breakpos250

  ;;Add user breaks
  nuser = N_ELEMENTS(userbreak)
  IF nuser NE 0 THEN BEGIN
     IF ~ SIZE(userbreak,/TNAME) EQ 'STRUCT' THEN BEGIN
        errmsg = "USERBREAK not as expected"
        RETURN
     ENDIF
     
     IF nbreaks EQ 0 THEN BEGIN
        breakpos250 = [0uL]
        breakpos350 = [0uL]
        breakpos500 = [0uL]
     ENDIF

     ;;Do each band
     bands = ['PSW','PMW','PLW','ALL']
     FOR i=0, N_ELEMENTS(bands)-1 DO BEGIN
        wbrk = WHERE(STRUPCASE(userbreak.band) EQ bands[i], nbrk)
        IF nbrk NE 0 THEN BEGIN
           ;;Get indices and insert into breakpos
           userbreakpos = ULONARR(nbrk)
           FOR i=0,nbrk-1 DO $
              userbreakpos[i] = startsamp[ userbreak[wbrk[i]].todidx ] + $
              userbreak[wbrk[i]].breakpos
           CASE bands[i] OF
              'PSW' : breakpos250 = [breakpos250,TEMPORARY(userbreakpos)]
              'PMW' : breakpos350 = [breakpos350,TEMPORARY(userbreakpos)]
              'PLW' : breakpos500 = [breakpos500,TEMPORARY(userbreakpos)]
              'ALL' : BEGIN
                 breakpos250 = [breakpos250,userbreakpos]
                 breakpos350 = [breakpos350,userbreakpos]
                 breakpos500 = [breakpos500,TEMPORARY(userbreakpos)]
              END
           ENDCASE
        ENDIF
     ENDFOR
     breakpos250 = breakpos250[SORT(breakpos250)]
     breakpos350 = breakpos350[SORT(breakpos350)]
     breakpos500 = breakpos500[SORT(breakpos500)]
     IF nbreaks EQ 0 THEN BEGIN
        breakpos250 = breakpos250[1:*]
        breakpos350 = breakpos350[1:*]
        breakpos500 = breakpos500[1:*]
     ENDIF
     nbreaks250 = N_ELEMENTS(breakpos250)
     nbreaks350 = N_ELEMENTS(breakpos350)
     nbreaks500 = N_ELEMENTS(breakpos500)
  ENDIF ELSE BEGIN
     nbreaks250 = nbreaks
     nbreaks350 = nbreaks
     nbreaks500 = nbreaks
  ENDELSE

  ;;Keep track of locations before we add gap info
  breakpos250_nogap = breakpos250
  breakpos350_nogap = breakpos350
  breakpos500_nogap = breakpos500

  ;;For the future -- add ability for user to add their own gaps
  ;; here into whbreak

  ;;Step 3: look for gaps
  ;;Gaps == spacing larger than 2x sample step but not breaks
  gapind = WHERE(dt GE 2.d * sampstep AND dt LT maxgap, ngaps)
  IF KEYWORD_SET(verbose) THEN $
     MESSAGE,STRING(ngaps,format="('Number of gaps: ',I0)"),/INF,LEVEL=-1
 
  ;;Get structure to hold gap info
  ;;this stores indices into the uber timestream
  IF ngaps GT 0 THEN BEGIN
     ;;Convert timestep to number of samples it represents
     gap_samples = ROUND(dt * sampfreq)
     
     ;;We assume that all gaps happen at the end of TODs
     ;; since they are purely time gap based.  If a gap is -not- at
     ;; the end of a TOD, print a warning but then ignore that gap
     nsamps = (*tods[0]).nsamps
     gapinfo = REPLICATE({todidx: 0L, startidx:0uL, endidx:0uL}, ngaps)
     goodgap = BYTARR(ngaps)
     FOR i=0, ngaps-1 DO BEGIN
        ;;Figure out where each gap goes
        st = WHERE(gapind[i] EQ endsamp_nogap, ngp)
        IF ngp NE 1 THEN BEGIN
           ;; Figure out which tod it is
           todbad = MAX(WHERE(gapind[i] GT startsamp_nogap))
           errmsg = "Found gap not at end of timestream in TOD from file " + $
                    (*tods[todbad]).shortfile + "; ignoring"
           MESSAGE, errmsg, /INF
        ENDIF ELSE BEGIN
           goodgap[i] = 1b
           gapinfo[i].todidx = st
        ENDELSE
     ENDFOR
     wgoodgap = WHERE(goodgap, ngoodgap, NCOMPLEMENT=nbadgap)
     IF ngoodgap EQ 0 THEN BEGIN
        MESSAGE, "All gaps found in the middle of TODS -- ignoring", /INF
        ngaps = 0
        DELVARX, gapinfo
     ENDIF ELSE IF nbadgap NE 0 THEN BEGIN
        ngaps = ngoodgap
        gapinfo = gapinfo[TEMPORARY(wgoodgap)]
     ENDIF
     
     ;;Now, we have to insert spaces into the ubertimestream
     ;; to represent each gap.  We also have to update the
     ;; break positions if there are any
     FOR i=0, ngaps-1 DO BEGIN
        ;;The gap is at the end of gapinfo[i].todidx
        ;; so we need to insert space -before- the next one
        todidx = gapinfo[i].todidx
        IF todidx LT ntods-1 THEN BEGIN
           oridx = endsamp_nogap[todidx]
           gaplen = gap_samples[oridx]
           gapinfo[i].startidx = endsamp[todidx]+1
           gapinfo[i].endidx   = endsamp[todidx]+gaplen
           ;;Update the startsamp/endsamp above this insertion
           FOR j = todidx+1,ntods-1 DO BEGIN
              startsamp[j] += gaplen
              endsamp[j]   += gaplen
           ENDFOR
           ;;Update gap positions relative to timestream with gaps
           IF nbreaks250 GT 0 THEN BEGIN 
              wadj = WHERE( breakpos250_nogap GT oridx, nadj )
              IF nadj NE 0 THEN breakpos250[wadj] += gaplen
           ENDIF
           IF nbreaks350 GT 0 THEN BEGIN 
              wadj = WHERE( breakpos350_nogap GT oridx, nadj )
              IF nadj NE 0 THEN breakpos350[wadj] += gaplen
           ENDIF
           IF nbreaks500 GT 0 THEN BEGIN 
              wadj = WHERE( breakpos500_nogap GT oridx, nadj )
              IF nadj NE 0 THEN breakpos500[wadj] += gaplen
           ENDIF
        ENDIF
     ENDFOR

     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,STRING(endsamp[ntods-1]+1,$
                       FORMAT='("Gap-filled total number of samples: ",I0)'),$
                /INF,LEVEL=-1
  ENDIF
  nsamps = endsamp[ntods-1]+1
  nsamps_nogap = endsamp_nogap[ntods-1]+1

  ;;Construct breakinfo in each band, then stitch together
  ;;Note we create an entry even if there are no breaks spanning
  ;; the full timestream
  breakinfo250 = smap_tcdrift_getbreakstruct(nbreaks250,$
                                             TEMPORARY(breakpos250),$
                                             TEMPORARY(breakpos250_nogap),$
                                             ntods, endsamp, endsamp_nogap )
  breakinfo350 = smap_tcdrift_getbreakstruct(nbreaks350,$
                                             TEMPORARY(breakpos350),$
                                             TEMPORARY(breakpos350_nogap),$
                                             ntods, endsamp, endsamp_nogap )
  breakinfo500 = smap_tcdrift_getbreakstruct(nbreaks500,$
                                             TEMPORARY(breakpos500),$
                                             TEMPORARY(breakpos500_nogap),$
                                             ntods, endsamp, endsamp_nogap )
  breakinfo = { psw: TEMPORARY(breakinfo250),$
                pmw: TEMPORARY(breakinfo350),$
                plw: TEMPORARY(breakinfo500) }
                
     
  IF N_ELEMENTS(gapinfo) NE 0 THEN BEGIN
     stitchinfo = { maxgap: maxgap, sampfreq: sampfreq, sampstep: sampstep, $
                    scanrate: scanrate, ntods: ntods, $
                    startsamp: TEMPORARY(startsamp),$
                    endsamp: TEMPORARY(endsamp),$
                    startsamp_nogap: TEMPORARY(startsamp_nogap),$
                    endsamp_nogap: TEMPORARY(endsamp_nogap),$
                    gapinfo: TEMPORARY(gapinfo), nsamps: nsamps,$
                    nsamps_nogap: nsamps_nogap }
  ENDIF ELSE BEGIN
     stitchinfo = { maxgap: maxgap, sampfreq: sampfreq, sampstep: sampstep, $
                    scanrate: scanrate, ntods: ntods, $
                    startsamp: TEMPORARY(startsamp),$
                    endsamp: TEMPORARY(endsamp),$
                    startsamp_nogap: TEMPORARY(startsamp_nogap),$
                    endsamp_nogap: TEMPORARY(endsamp_nogap),$
                    nsamps: nsamps, nsamps_nogap: nsamps_nogap }
  ENDELSE


  success = 1b
  
END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION smap_tcdrift_getmaskbits, tods, excludemask, SUCCESS=success,$
                                   ERRMSG=errmsg
  COMPILE_OPT IDL2, HIDDEN
  success = 0b
  errmsg  = ''
  
  ntods = N_ELEMENTS(tods)
  IF ntods EQ 0 THEN BEGIN
     errmsg = "No tods present in smap_tcdrift_getmaskbits"
     RETURN,!VALUES.F_NAN
  ENDIF
  IF N_ELEMENTS(excludemask) EQ 0 THEN BEGIN
     errmsg = "No excludemask present in smap_tcdrift_getmaskbits"
     RETURN,!VALUES.F_NAN
  ENDIF

  maskbits = ULONARR( ntods )  
  FOR i=0,ntods-1 DO BEGIN
     IF TAG_EXIST( (*tods[i]), 'mask_bits', /TOP_LEVEL ) THEN BEGIN
        mskbts = construct_mask_bitmask( excludemask, $
                                         (*tods[i]).mask_bits,$
                                         SUCCESS=csuccess,$
                                         ERRMSG=cerrmsg )
        IF csuccess EQ 0 THEN BEGIN
           errmsg = "While making mask bits for "+$
                    (*tods[i]).shortfile+": "+cerrmsg
           RETURN,!VALUES.F_NAN
        ENDIF
        maskbits[i] = mskbts
     ENDIF ELSE BEGIN
        errmsg = "Mask bits not defined for tod: "+$
                 (*tods[i]).shortfile
        RETURN,!VALUES.F_NAN
     ENDELSE
  ENDFOR
  success = 1b
  RETURN,maskbits
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Stitches together timestreams and fills gaps for a user specified
;; bolometer.  Doesn't care about gaps at all
PRO smap_tcdrift_stitch, bol, tods, stitchinfo, ubertimestream, ubermask, $
                         MASKBITS=maskbits, SUCCESS=success, ERRMSG=errmsg,$
                         SEED=seed, NOGAP=nogap
  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN
  success = 0b
  errmsg  = ''

  IF SIZE(tods[0],/TNAME) NE 'POINTER' THEN BEGIN
     errmsg = "Expected pointer array of TODs"
     RETURN
  ENDIF

  ntods = N_ELEMENTS(tods)
  IF ntods EQ 0 THEN BEGIN
     errmsg = "No TODs!"
     RETURN
  ENDIF
  IF ntods NE stitchinfo.ntods THEN BEGIN
     errmsg = "Number of tods in stitchinfo doesn't match number of actual tods"
     RETURN
  ENDIF

  IF KEYWORD_SET(nogap) THEN nsamps = stitchinfo.nsamps_nogap ELSE $
     nsamps = stitchinfo.nsamps
  ubertimestream = DBLARR( nsamps )
  ubermask       = BYTARR( nsamps )

  has_maskbits   = N_ELEMENTS(maskbits) NE 0

  ;;Stitching step, also builds mask
  FOR j=0,ntods-1 DO BEGIN
     ;;Find the bolometer, -not- assuming they are in the same order
     wbolo = WHERE( (*tods[j]).chan EQ bol, nbolo )
     IF nbolo NE 1 THEN BEGIN
        errmsg = "Couldn't find "+bol+" bolometer in timestream for "+$
                 (*tods[j]).shortfile
        RETURN
     ENDIF
     wbolo = wbolo[0]

     ;; Place the scan data in the ubertimestream
     IF KEYWORD_SET( nogap ) THEN BEGIN
        uberstart = stitchinfo.startsamp_nogap[j]
        uberend   = stitchinfo.endsamp_nogap[j]
     ENDIF ELSE BEGIN
        uberstart = stitchinfo.startsamp[j]
        uberend   = stitchinfo.endsamp[j]
     ENDELSE
     ubertimestream[uberstart:uberend] = (*tods[j]).signal[wbolo,*]

     ;;Apply masking
     IF has_maskbits THEN BEGIN
        IF maskbits[j] NE 0 THEN BEGIN
           whmask = WHERE( ((*tods[j]).mask[wbolo,*] AND maskbits[j]) NE 0,$
                           count, NCOMPLEMENT=npass)
           
           IF npass EQ 0 THEN BEGIN
              ;;well then -- mask it all
              ubermask[uberstart:uberend] OR= 1b
           ENDIF ELSE IF count NE 0 THEN BEGIN
              ;;First -- we mask everything that actually failed
              ubermask[uberstart+whmask] OR= 1b
              ;;Then, if too large a fraction is masked (95%), toss the
              ;;whole scan
              currnsamp = uberend - uberstart + 1
              IF ( FLOAT(count)/currnsamp GT 0.95 ) THEN $
                 ubermask[uberstart:uberend] OR= 1b
           ENDIF
        ENDIF
     ENDIF
     ;;finiteness mask
     wnonfin = WHERE( ~FINITE(ubertimestream[uberstart:uberend]), nnonfin )
     IF nnonfin NE 0 THEN ubermask[uberstart+TEMPORARY(wnonfin)] OR= 1b
  ENDFOR ;;Loop over tods

  ;;Gap filling step.  Note that gaps can never occur at breaks,
  ;; so we don't have to worry about spanning a break in the fill
  IF ~KEYWORD_SET(nogap) AND $
     TAG_EXIST(stitchinfo,'gapinfo',/TOP_LEVEL) THEN BEGIN

     ;; Determine the noise level to add -- first, get band
     ;;hardwired noise values!
     band = STRUPCASE(STRMID( bol, 0, 3))
     IF (band EQ 'PSW') THEN n_level = 38.23d-3 $
     ELSE IF (band EQ 'PMW') THEN n_level = 37.86d-3 $
     ELSE IF (band EQ 'PLW') THEN n_level = 52.87d-3 $
     ELSE n_level = 0.0
     
     ;; this is for channels which are still calibrated in V
     IF STRMID( bol, 3, 1) EQ 'T' OR $
        STRMID( bol, 3, 2) EQ 'DP' OR $
        STRMID( bol, 3, 1) EQ 'R' THEN BEGIN
        ;; this is a generalization but is close enough in 
        ;; all the data which has been tested.
        n_level /= 800000.0
     ENDIF
              
     ngaps = N_ELEMENTS(stitchinfo.gapinfo)
     FOR i=0,ngaps-1 DO BEGIN
        
        ;; Get the mean levels for both ends of the gap-filling data
        ;; from ubertimestream
        ;;the gap happens -after- this tod
        gaptod   = stitchinfo.gapinfo[i].todidx 
        gapstart = stitchinfo.gapinfo[i].startidx
        gapend   = stitchinfo.gapinfo[i].endidx
        
        IF gaptod GE stitchinfo.ntods-1 THEN BEGIN
           ;;also shouldn't be possible
           errmsg = "Can't find mean level after gap at end of ubertimestream"
           RETURN
        ENDIF
        
        ;;We can take data from the ubertimestream here
        ;;Take the previous 300 samples or the whole thing if smaller
        ;;First do the tod before the gap
        ;;Note we -ignore- the mask here, since we want
        ;; the interpolation to be smoothish even if there are pixels
        ;; we don't like on the end
        uberstart = stitchinfo.startsamp[gaptod]
        uberend = stitchinfo.endsamp[gaptod]
        IF (uberend-uberstart+1 GT 300) THEN uberstart = uberend-300+1
        mean_lo = MEAN(ubertimestream[uberstart:uberend],/NAN )

        ;;And tod above
        uberend = stitchinfo.endsamp[gaptod+1]
        uberstart = stitchinfo.startsamp[gaptod+1]
        IF (uberend-uberstart+1 GT 300) THEN uberend = uberstart+300-1
        mean_up = MEAN(ubertimestream[uberstart:uberend],/NAN )

        ;;Note that these being non-finite is allowable!  This can
        ;; happen if the data on both sides of the gap is bad, in which
        ;; case it is ok if we don't know how to fill
        ;;We use a special mask value (2) for gaps
        ngap     = gapend - gapstart + 1
        ubermask[gapstart:gapend] OR= 2b  ;;mask the artificial data always
        IF ~ FINITE(mean_lo) THEN BEGIN
           IF ~ FINITE(mean_up) THEN BEGIN
              ubertimestream[gapstart:gapend] = !VALUES.D_NAN
           ENDIF ELSE BEGIN
              ubertimestream[gapstart:gapend] = $
                 n_level*RANDOMN(seed,ngap,/DOUBLE)+mean_up
           ENDELSE
        ENDIF ELSE IF ~ FINITE(mean_up) THEN BEGIN
           ;;But mean_lo is okay
              ubertimestream[gapstart:gapend] = $
                 n_level*RANDOMN(seed,ngap,/DOUBLE)+mean_lo
        ENDIF ELSE BEGIN
           ;;Should be the majority case -- both good
           ;;Fit a line between them
           fsamps  = INDGEN( ngap )
           slope   = (mean_up - mean_lo) / ngap
           filler  = DOUBLE(slope * fsamps + mean_lo) + $
                     RANDOMN( seed, ngap, /DOUBLE ) * n_level
           ubertimestream[gapstart:gapend] = TEMPORARY(filler)
        ENDELSE
     ENDFOR
  ENDIF
  success = 1b
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO smap_tcdrift_filter, bol, timestream, mask, stitchinfo, breakinfo, $
                         VERBOSE=verbose, SUCCESS=success, ERRMSG=errmsg
  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN
  success = 0b
  errmsg  = ''

  band = STRUPCASE(STRMID(bol,0,3))
  CASE band OF 
     'PSW' : cbreakinfo = breakinfo.psw
     'PMW' : cbreakinfo = breakinfo.pmw
     'PLW' : cbreakinfo = breakinfo.plw
  ENDCASE
  ;;The filtering is done on a per-break basis
  nbreaks = N_ELEMENTS(cbreakinfo)
  IF nbreaks EQ 0 THEN BEGIN
     ;;Should be at least one element (the whole thing) even if there are no
     ;; actual breaks per se
     errmsg = "Breakinfo is empty"
     RETURN
  ENDIF
  
  ;;Look for bad data, do something about it.
  ;;We fill these in with random noise for filtering, then
  ;; reset them afterwards
  ;;It turns out that using the mask to flag out samples to replace
  ;; is a -very bad- idea unless you do something smarter about
  ;; filling.  The problem is that there are large swaths without
  ;; unflagged data, so replacing them wholesale does really odd
  ;; things to the filtering.
  ;wbad = WHERE( mask NE 0, nbad, NCOMPLEMENT=ngood )
  wbad = WHERE( ~ FINITE(timestream), nbad, NCOMPLEMENT=ngood )
  IF ngood EQ 0 THEN BEGIN
     IF KEYWORD_SET( verbose ) THEN $
        MESSAGE,"No good samples for bol: "+bol+" -- skipping",/INF,$
                LEVEL=-1
     success = 1b
     RETURN
  ENDIF
  IF nbad NE 0 THEN BEGIN
     ;; This could be a lot more sophisticated, I'm sure.
     ;; It would be nice to use some sort of local mean
     ;; but we then have to be careful of gaps of a few
     ;; bad values in a row
     mn               = MEAN( timestream, /NAN )
     sig              = STDDEV( timestream, /NAN )
     badvals          = timestream[wbad]
     timestream[wbad] = mn + sig*RANDOMN(seed, nbad )
  ENDIF

  FOR i=0,nbreaks-1 DO BEGIN
     losamp = cbreakinfo[i].startidx
     hisamp = cbreakinfo[i].endidx
   
     ;; copy out for filtering subset
     temptimestream = REFORM(timestream[losamp:hisamp])
     
     ;; filter
     smap_lpfilter,temptimestream,stitchinfo.scanrate,stitchinfo.sampstep,$
                   ANGSCALE=60.d, VERBOSE=verbose,SUCCESS=lpsuccess,$
                   ERRMSG=lperrmsg
     IF lpsuccess EQ 0 THEN BEGIN
        errmsg = "Error applying low-pass filter: "+lperrmsg
        RETURN
     ENDIF

     ;; copy back in
     timestream[losamp:hisamp] = TEMPORARY(temptimestream)
  ENDFOR

  ;;Restore any bad values to ubertimestream
  IF nbad NE 0 THEN $
     timestream[TEMPORARY(wbad)] = TEMPORARY(badvals)
  success = 1b
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;This dumps the modified timestreams back into tods
PRO smap_tcdrift_thermcorr, tods, bol, stitchinfo, breakinfo,$
                            thermstruct, timestream, mask,$
                            timestream_nogap, mask_nogap,$
                            NOROBUST=norobust, NOTHERMMASK=nothermmask,$
                            SUCCESS=success, ERRMSG=errmsg, TODMEAN=todmean

  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN
  success = 0b
  errmsg  = ''
        
  IF SIZE(tods[0],/TNAME) NE 'POINTER' THEN BEGIN
     errmsg = "Expected pointer array of TODs"
     RETURN
  ENDIF

  band = STRUPCASE(STRMID(bol,0,3))

  CASE band OF 
     'PSW' : cbreakinfo = breakinfo.psw
     'PMW' : cbreakinfo = breakinfo.pmw
     'PLW' : cbreakinfo = breakinfo.plw
  ENDCASE

  ;; find the thermistors in thermstruct
  wT1 = WHERE( thermstruct.thermlist EQ band + 'T1', nbolo )
  IF nbolo NE 1 THEN BEGIN
     errmsg = "Couldn't find "+band+"T1 thermistor in therm struct."
     RETURN
  ENDIF
  wT2 = WHERE( thermstruct.thermlist EQ band + 'T2', nbolo )
  IF nbolo NE 1 THEN BEGIN
     errmsg = "Couldn't find "+band+"T2 thermistor in therm struct"
     RETURN
  ENDIF
     
  ntods = N_ELEMENTS(tods)
  IF ntods EQ 0 THEN BEGIN
     errmsg = "No TODS"
     RETURN
  ENDIF
  nbreaks = N_ELEMENTS(cbreakinfo)
  IF nbreaks EQ 0 THEN BEGIN
     errmsg = "No break info"
     RETURN
  ENDIF

  ;;will be set to 1 where no corr possible relative to original data
  nocorr = BYTARR(stitchinfo.nsamps_nogap) 

  ;;Two steps: 1) preform the correction on a per-break basis
  ;; 2) stitch the data back into the tods
  FOR i = 0, nbreaks-1 DO BEGIN
     ;;Index into filtered and gap filled timestream
     losamp = cbreakinfo[i].startidx
     hisamp = cbreakinfo[i].endidx
     nsamp = hisamp-losamp+1uL

     ;;Without gaps for correction
     losamp_nogap = cbreakinfo[i].startidx_nogap
     hisamp_nogap = cbreakinfo[i].endidx_nogap
     nsamp_nogap = hisamp_nogap - losamp_nogap + 1uL

     ;; strip out the time streams of interest
     signal = timestream[losamp:hisamp]
     sigmask= mask[losamp:hisamp] ;;1 where bad, 0 where good, 2 where gap
     therm1 = thermstruct.thermsignal[losamp:hisamp,wT1]
     therm2 = thermstruct.thermsignal[losamp:hisamp,wT2]
     t1mask = thermstruct.thermmask[losamp:hisamp,wT1]
     t2mask = thermstruct.thermmask[losamp:hisamp,wT2]

     ;;See if there are any good thermistor points -- if not, we can
     ;; skip some steps!
     t1ngood= thermstruct.ngood[wT1[0]]
     t2ngood= thermstruct.ngood[wT2[0]]

     ;; find where the good data lives in them -- data which
     ;; is finite and not masked
     wkeep_sig = WHERE( FINITE(signal) AND (sigmask EQ 0), $
                        ykbol,COMPLEMENT=nkeep_bol,NCOMPLEMENT=nksig )
     IF t1ngood NE 0 THEN $
        wkeep_AT1 = WHERE( FINITE(therm1) AND (t1mask EQ 0), $
                           ykAT1,COMPLEMENT=nkeep_AT1,NCOMPLEMENT=nkAT1 )
     IF t2ngood NE 0 THEN $
     wkeep_AT2 = WHERE( FINITE(therm2) AND (t2mask EQ 0),$
                        ykAT2,COMPLEMENT=nkeep_AT2,NCOMPLEMENT=nkAT2 )
        
     ;; make the tracking (ie the thermistor interpolant) arrays
     IF t1ngood NE 0 THEN tc_tracker1 = DBLARR(nsamp)
     IF t2ngood NE 0 THEN tc_tracker2 = DBLARR(nsamp)

     ;; need to keep track of samples we've visited for later
     IF t1ngood NE 0 THEN tc_hit1     = BYTARR(nsamp)
     IF t2ngood NE 0 THEN tc_hit2     = BYTARR(nsamp)
       
     ;;Do the fit for therm1
     ;;First, we need the union of the masks
     has_therm1 = 0b
     IF t1ngood NE 0 THEN BEGIN
        wfit1 = WHERE( FINITE(signal) AND (sigmask EQ 0) AND $
                       FINITE(therm1) AND (TEMPORARY(t1mask) EQ 0), $
                       ykfit1,COMPLEMENT=nkeep_fit,NCOMPLEMENT=nkfit )
        
        ;; if we have 3 or more points then fit - using linfit here as 
        ;; it's faster - and store the result 
        IF (ykfit1 GT 2) THEN BEGIN
           IF ykfit1 GT 10 AND ~ KEYWORD_SET(norobust) THEN BEGIN
              scalfit1 = ROBUST_LINEFIT( therm1[wfit1], signal[wfit1] )
              IF N_ELEMENTS(scalfit1) EQ 1 THEN BEGIN
                 scalfit1 = LINFIT(therm1[wfit1],signal[wfit1] )
              ENDIF
           ENDIF ELSE BEGIN
              scalfit1 = LINFIT(therm1[wfit1],signal[wfit1] )
           ENDELSE
           ;;Note we don't just use YFIT here because wfit1 != wkeep_AT1
           tc_tracker1[wkeep_AT1] = $
              scalfit1[1] * therm1[wkeep_AT1] + scalfit1[0]
           tc_hit1[wkeep_AT1] = 1
           has_therm1 = 1b
        ENDIF ;;note has_therm1 will stay 0 if there aren't enough points
     ENDIF
        
     ;; ditto for therm2
     has_therm2 = 0b
     IF t2ngood NE 0 THEN BEGIN
        wfit2 = WHERE( FINITE(signal) AND (sigmask EQ 0) AND $
                       FINITE(therm2) AND (TEMPORARY(t2mask) EQ 0),$
                       ykfit2,COMPLEMENT=nkeep_fit,NCOMPLEMENT=nkfit )
        IF (ykfit2 GT 2) THEN BEGIN
           IF ykfit2 GT 10 AND ~ KEYWORD_SET(norobust) THEN BEGIN
              scalfit2 = ROBUST_LINEFIT( therm2[wfit2], signal[wfit2] )
              IF N_ELEMENTS(scalfit2) EQ 1 THEN BEGIN
                 scalfit2 = LINFIT(therm2[wfit2],signal[wfit2] )
              ENDIF
           ENDIF ELSE BEGIN
              scalfit2 = LINFIT(therm2[wfit2],signal[wfit2] )
           ENDELSE
           tc_tracker2[wkeep_AT2] = $
              scalfit2[1] * therm2[wkeep_AT2] + scalfit2[0]
           tc_hit2[wkeep_AT2] = 1
           has_therm2 = 1b
        ENDIF
     ENDIF
     
     ;;If we have neither therm, we can't do anything
     ;; although we label this a 'success'.
     IF ~ (has_therm1 OR has_therm2) THEN BEGIN
        nocorr[losamp_nogap:hisamp_nogap] = 1b
        CONTINUE ;;maybe the next break will be okay
     ENDIF

     ;;Now, interpolate into stuff that wasn't in wkeep_AT[12]
     IF has_therm1 THEN BEGIN
        wcorr1 = WHERE(tc_hit1,count,COMPLEMENT=ncorr,NCOMPLEMENT=nount)
        IF count GT 0 THEN $
           tc_tracker1 = INTERPOL(tc_tracker1[wcorr1],wcorr1,$
                                  FINDGEN(hisamp-losamp+1))
     ENDIF
     ;; ditto for number 2
     IF has_therm2 THEN BEGIN
        wcorr2 = WHERE(tc_hit2,count,COMPLEMENT=ncorr,NCOMPLEMENT=nount)
        IF count GT 0 THEN $
           tc_tracker2 = INTERPOL(tc_tracker2[wcorr2],wcorr2,$
                                  FINDGEN(hisamp-losamp+1))
     END

     ;; Now comes a little tomfoolery - I'm worried that therm1 and 
     ;; therm2 don't mutually track one another all that well *SO* 
     ;; came up with following algorithm.  First, take the abs 
     ;; difference between them.  If more than half of 
     ;; the points lie more than 10 mK different from one another, 
     ;; then take the one with more points -- sort of.  Therm1 seems
     ;; more reliable, so we take therm1 unless therm2 has at least 50%
     ;; more accepted points 
     IF has_therm1 AND has_therm2 THEN BEGIN
        tcdiff = ABS(tc_tracker1 - tc_tracker2)
        whbig = WHERE(tcdiff GT 0.01,countdiff)
        IF ( (FLOAT(countdiff) / N_ELEMENTS(tcdiff)) LT 0.5) AND $
           MAX(tcdiff) LT 0.015 THEN BEGIN
           ;;Average the two because they agree pretty well
           tc_tracker = 0.5*(TEMPORARY(tc_tracker1) + TEMPORARY(tc_tracker2))
        ENDIF ELSE BEGIN
           ;;Aw crap, they don't agree.  So choose one as discussed
           ;; above
           IF ykAT1 GE 0.5*ykAT2 THEN BEGIN
              tc_tracker = TEMPORARY(tc_tracker1)
           ENDIF ELSE BEGIN
              tc_tracker = TEMPORARY(tc_tracker2)
           ENDELSE
        ENDELSE
     ENDIF ELSE BEGIN
        ;;Well, only one had any points, so use that
        IF has_therm1 THEN tc_tracker = TEMPORARY(tc_tracker1)
        IF has_therm2 THEN tc_tracker = TEMPORARY(tc_tracker2)
     ENDELSE
     
     ;;Now, we want to apply the correction to the ubertimestream.
     ;; We correct the original (nogap) data, not the filtered and gap
     ;; filled version, since we don't want to apply all that low pass
     ;; filtering to our data.  The approach here is to
     ;; cut down tc_tracker to remove the gaps -- fortunately this
     ;; is easy because we set a special mask in the gaps (2b),
     ;; so it's quite easy to remove them
     wkp = WHERE( (sigmask AND 2b) EQ 0, nkp, NCOMPLEMENT=ntoss )
     IF nkp EQ 0 THEN BEGIN
        errmsg = "No non-gap samples found"
        RETURN
     ENDIF
     IF nkp NE nsamp_nogap THEN BEGIN
        errmsg = "Found unexpected number of no-gap samples"
        RETURN
     ENDIF
     IF ntoss NE 0 THEN tc_tracker = tc_tracker[ TEMPORARY(wkp) ]

     ;;And, finally, actually apply the correction
     timestream_nogap[losamp_nogap:hisamp_nogap] -= TEMPORARY(tc_tracker)

     ;;Apply mean subtraction to entire segment unless we are doing it
     ;; on a per-tod basis later
     IF ~ KEYWORD_SET( todmean ) THEN BEGIN
        wkp = WHERE( mask_nogap[losamp_nogap:hisamp_nogap] EQ 0, nkp, $
                     NCOMPLEMENT=ncmask )
        IF nkp EQ 0 OR ncmask EQ 0 THEN BEGIN
           mnval = MEAN(timestream_nogap[losamp_nogap:hisamp_nogap],/NAN)
        ENDIF ELSE mnval = MEAN(timestream_nogap[losamp_nogap+TEMPORARY(wkp)],$
                                /NAN)
        timestream_nogap[losamp_nogap:hisamp_nogap] -= mnval
     ENDIF
     
  ENDFOR ;;loop over breaks
  

  ;;Now stitch back into TODs, possibly mean subtracting (depending
  ;; on /TODMEAN), and adding masking where correction didn't work
  FOR i=0,stitchinfo.ntods-1 DO BEGIN
     ;;indices into uber-timestreams, etc.
     startsamp = stitchinfo.startsamp[i]
     endsamp   = stitchinfo.endsamp[i]
     startsamp_nogap = stitchinfo.startsamp_nogap[i]
     endsamp_nogap   = stitchinfo.endsamp_nogap[i]

     ;;Figure out where the bolometer is in this tod
     bolidx = WHERE( (*tods[i]).chan EQ bol, nbol )
     IF nbol NE 1 THEN BEGIN
        errmsg = "Error locating bolometer: "+bol
        RETURN
     ENDIF
     bolidx = bolidx[0]

     ;;Mask samples that we couldn't correct
     IF N_ELEMENTS(nothermmask) NE 0 THEN BEGIN
        wbad = WHERE( nocorr[startsamp_nogap:endsamp_nogap] NE 0, nbad, $
                      NCOMPLEMENT=ngood )
        IF nbad NE 0 THEN BEGIN
           ;;Looks like we couldn't therm corr at least some samples
           ;;Figure out what mask bit to set based on value of nothermmask
           IF ~ TAG_EXIST( (*tods[i]), 'mask_bits', /TOP_LEVEL ) THEN BEGIN
              errmsg = "No mask information available for nothermmask"
              RETURN
           ENDIF
           nothermbit = construct_mask_bitmask( nothermmask, $
                                                (*tods[i]).mask_bits,$
                                                SUCCESS=csuccess,$
                                                ERRMSG=cerrmsg )
           IF csuccess EQ 0 THEN BEGIN
              errmsg = "While making mask bits for "+$
                       (*tods[i]).shortfile+": "+cerrmsg+" for user mask "+$
                       nothermmask
              RETURN
           ENDIF

           IF ngood EQ 0 THEN BEGIN
              ;;All data from this tod/bol could not be corrected
              (*tods[i]).mask[bolidx,*] OR= nothermbit
              CONTINUE ;;leave data in original tod alone, move to next tod
           ENDIF ELSE BEGIN
              ;;Some bits couldn't be corrected
              (*tods[i]).mask[bolidx,wbad] OR= nothermbit
              mask_nogap[startsamp+wbad] OR= 1b
           ENDELSE
        ENDIF
     ENDIF

     ;;Mean correct on a per-tod basis if necessary
     ;;Note we do make use of the mask
     IF KEYWORD_SET(todmean) THEN BEGIN
        wgood = WHERE( mask_nogap[startsamp_nogap:endsamp_nogap] EQ 0, ngood, $
                       NCOMPLEMENT=nbad )
        IF ngood EQ 0 THEN BEGIN
           ;;do it anyways
           mnval = MEAN( timestream_nogap[startsamp_nogap:endsamp_nogap],/NAN )
        ENDIF ELSE IF nbad NE 0 THEN BEGIN
           mnval = MEAN( timestream_nogap[startsamp_nogap+TEMPORARY(wgood)], $
                         /NAN )
        ENDIF ELSE $
           mnval = MEAN( timestream_nogap[startsamp_nogap:endsamp_nogap],/NAN )

        IF FINITE(mnval) THEN $
           timestream_nogap[startsamp_nogap:endsamp_nogap] -= mnval
     ENDIF

     ;;Stitch into TOD
     (*tods[i]).signal[bolidx,*] = $
        timestream_nogap[startsamp_nogap:endsamp_nogap]
     
  ENDFOR

  success = 1b
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION smap_tcdrift_gettherm, tods, stitchinfo, breakinfo, $
                                MASKBITS=maskbits, NOFILTER=nofilter,$
                                SUCCESS=success, ERRMSG=errmsg, $
                                VERBOSE=verbose, NO250=no250, NO350=no350, $
                                NO500=no500, SEED=seed
  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN
  success = 0b
  errmsg  = ''

  IF SIZE(tods[0],/TNAME) NE 'POINTER' THEN BEGIN
     errmsg = "Expected pointer array of TODs"
     RETURN, !VALUES.F_NAN
  ENDIF

  ntods = N_ELEMENTS(tods)
  IF ntods EQ 0 THEN BEGIN
     errmsg = "No TODs in smap_tcdrift_gettherm"
     RETURN,!VALUES.F_NAN
  ENDIF

  ntotsamps = stitchinfo.endsamp[ntods-1]+1
  IF ntotsamps EQ 0 THEN BEGIN
     errmsg = "No samples to stitch in smap_tcdrift_gettherm"
     RETURN,!VALUES.F_NAN
  ENDIF

  ntherms = 0
  thermlist = ['']
  IF ~ KEYWORD_SET(no250) THEN BEGIN
     ntherms +=2
     thermlist = [thermlist,'PSWT1','PSWT2']
  ENDIF
  IF ~ KEYWORD_SET(no350) THEN BEGIN
     ntherms +=2
     thermlist = [thermlist,'PMWT1','PMWT2']
  ENDIF
  IF ~ KEYWORD_SET(no500) THEN BEGIN
     ntherms +=2
     thermlist = [thermlist,'PLWT1','PLWT2']
  ENDIF
  IF ntherms EQ 0 THEN BEGIN
     errmsg = "No therms to compute"
     RETURN,!VALUES.F_NAN
  ENDIF
  thermlist = thermlist[1:*]

  ;;Prepare output structure
  thermstruct = { ntherms: ntherms, thermlist: thermlist,$
                  thermsignal: DBLARR(ntotsamps,ntherms),$
                  thermmask: ULONARR(ntotsamps,ntherms),$
                  ngood: ULONARR(ntherms) }

  ;;Now, process each bolometer
  FOR ibol = 0, ntherms-1 DO BEGIN
     bol = thermstruct.thermlist[ibol]

     ;;First stitch and gap fill
     smap_tcdrift_stitch, bol, tods, stitchinfo, timestream, mask, $
                          MASKBITS=maskbits, SUCCESS=stitchsuccess, $
                          ERRMSG=stitcherrmsg, SEED=seed
     IF stitchsuccess EQ 0 THEN BEGIN
        errmsg = "Error stitching timestreams: "+stitcherrmsg
        RETURN,!VALUES.F_NAN
     ENDIF
     st = WHERE( mask EQ 0, ngood )
     thermstruct.ngood[ibol] = ngood ;;efficient for later
     thermstruct.thermmask[*,ibol] = mask

     ;;Then filter
     IF ~ KEYWORD_SET( nofilter ) THEN BEGIN
        smap_tcdrift_filter, bol, timestream, TEMPORARY(mask), $
                             stitchinfo, breakinfo,$
                             SUCCESS=filtsuccess, $
                             ERRMSG=filterrmsg, VERBOSE=verbose
        IF filtsuccess EQ 0 THEN BEGIN
           errmsg = "Error filtering: "+filterrmsg
           RETURN,!VALUES.F_NAN
        ENDIF
     ENDIF

     thermstruct.thermsignal[*,ibol] = TEMPORARY(timestream)

  ENDFOR

  success = 1b
  RETURN,thermstruct
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION smap_tcdrift_getbollist, tods, NO250=no250, NO350=no350,$
                                  NO500=no500, SUCCESS=success, $
                                  ERRMSG=errmsg

  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN
  success = 0b
  errmsg  = ''

  IF N_ELEMENTS(tods) EQ 0 THEN BEGIN
     errmsg = "No TODs in smap_tcdrift_getbollist"
     RETURN,''
  ENDIF

  IF SIZE(tods[0],/TNAME) NE 'POINTER' THEN BEGIN
     errmsg = "Expected pointer array of TODs"
     RETURN,''
  ENDIF

  wbolos = WHERE( (*tods[0]).islight, nbolos )
  IF nbolos EQ 0 THEN BEGIN
     errmsg = "No bolometers seeing sky to correct"
     RETURN,''
  ENDIF


  ;;This assumes that the first tod has all the light bolometers
  bollist = (*tods[0]).chan[wbolos]

  IF KEYWORD_SET( no250 ) THEN BEGIN
     band = STRUPCASE(STRMID(bollist,0,3))
     wkeep = WHERE(band NE 'PSW',nkeep,NCOMPLEMENT=ntoss)
     IF nkeep EQ 0 THEN BEGIN
        errmsg = "No scans to correct after no250 cut"
        RETURN,''
     ENDIF
     IF ntoss NE 0 THEN BEGIN
        nbolos = nkeep
        bollist = bollist[wkeep]
     ENDIF
  ENDIF

  IF KEYWORD_SET( no350 ) THEN BEGIN
     band = STRUPCASE(STRMID(bollist,0,3))
     wkeep = WHERE(band NE 'PMW',nkeep,NCOMPLEMENT=ntoss)
     IF nkeep EQ 0 THEN BEGIN
        errmsg = "No scans to correct after no350 cut"
        RETURN,''
     ENDIF
     IF ntoss NE 0 THEN BEGIN
        nbolos = nkeep
        bollist = bollist[wkeep]
     ENDIF
  ENDIF

  IF KEYWORD_SET( no500 ) THEN BEGIN
     band = STRUPCASE(STRMID(bollist,0,3))
     wkeep = WHERE(band NE 'PLW',nkeep,NCOMPLEMENT=ntoss)
     IF nkeep EQ 0 THEN BEGIN
        errmsg = "No scans to correct after no500 cut"
        RETURN,''
     ENDIF
     IF ntoss NE 0 THEN BEGIN
        nbolos = nkeep
        bollist = bollist[wkeep]
     ENDIF
  ENDIF

  success = 1b
  RETURN, bollist
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO smap_tcdrift_correct, tods, MAX_GAP_ALLOWED=max_gap_allowed,$
                          EXCLUDEMASK=excludemask,VERBOSE=verbose, $
                          SUCCESS=success, ERRMSG=errmsg, NO250=no250,$
                          NO350=no350, NO500=no500, NOROBUST=norobust,$
                          NOTHERMMASK=nothermmask, TODMEAN=todmean,$
                          THERMSTRUCT=thermstruct, STITCHINFO=stitchinfo,$
                          BREAKINFO=breakinfo, NOCORR=nocorr, SEED=seed,$
                          USERBREAK=userbreak, NOFILTER=nofilter
  
  COMPILE_OPT IDL2, STRICTARRSUBS

  ;; error handling setup
  success = 0b
  errmsg  = ''

  IF KEYWORD_SET(no250) AND KEYWORD_SET(no350) AND KEYWORD_SET(no500) THEN BEGIN
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,"Skipping tcdrift, since NO250, NO350, and NO500 all set",/INF
     RETURN
  ENDIF

  ;; check that we actually got an input
  ntods = N_ELEMENTS(tods)
  IF ntods EQ 0 THEN BEGIN
     errmsg = "No TODs provided!"
     GOTO, err_handler
  ENDIF

  ;; check we got a pointer in the modern SMAP style
  IF SIZE( tods, /TNAME ) NE 'POINTER' THEN BEGIN
     errmsg = "Was expecting an array of pointers to TODs, got: "+$
              SIZE(tods,/TNAME)
     GOTO, err_handler
  ENDIF
  
  IF N_ELEMENTS(nothermmask) EQ 0 THEN nothermmask = "maskNoSMAPThermcorr"

  ;;Default excludemask
  IF ~KEYWORD_SET(excludemask) THEN BEGIN
     ;;Note -- note quite the same as createmap_defparams
     ;; missing maskZeroVelocity and maskSlew, since those are useful
     ;; also missing maskNoSMAPThermcorr, since it's set by this routine
     i_excludemask = ["maskMaster","maskTruncated",$
                      "maskUncorrectedTrunctation","maskDead",$
                      "maskAdcLatch","maskNoRespData","maskTSignalHdv",$
                      "maskGlitchL1Detected","maskGlitchL1NotRemoved",$
                      "maskGlitchL2Detected","maskGlitchL2NotRemoved",$
                      "maskJumpThermistorsDarksSignal",$
                      "maskNoThermistorAvailable","maskManual"]
     IF KEYWORD_SET( verbose ) THEN $
        MESSAGE,'WARNING: Excludemask was not input so using default set, ' + $
                'these may differ from your mapping masks!',/INFORMATIONAL
  ENDIF ELSE i_excludemask = excludemask

  IF N_ELEMENTS(max_gap_allowed) EQ 0 THEN BEGIN
     ;; this assumes the pre-drift correction 1/f knee of 100mHz.
     max_gap_allowed = 10. 
  ENDIF
  
  ;;;;;;; end io checking

  IF KEYWORD_SET(verbose) THEN MESSAGE,$
     string(ntods,format="('Number of TODS provided: ',I0)"),/INF

  ;;;;;;; Prepare to stitch the timestreams together
  ;; Upon return: startsamp tells us where the data from each
  ;; sample where begin, endsamp where it will end, gapsamp[j]
  ;; tells us how many fake timesamples we have to insert before
  ;; the jth timestream to fill small gaps, and breaknum tells
  ;; us which set of contiguous data each tod belongs to
  
  smap_tcdrift_getscaninfo, tods, stitchinfo, breakinfo, $
                            MAX_GAP_ALLOWED=max_gap_allowed,$
                            USERBREAK=userbreak,$
                            SUCCESS=gapfindsuccess, $
                            ERRMSG=gaperrmsg, VERBOSE=verbose 
  IF gapfindsuccess EQ 0 THEN BEGIN
     errmsg = "Error preparing to stitch timestreams "+gaperrmsg
     GOTO,err_handler
  ENDIF
  
  ;;Figure out maskbits we will use
  maskbits = smap_tcdrift_getmaskbits( tods, i_excludemask, $
                                       SUCCESS=msksuccess, ERRMSG=mskerrmsg )
  IF msksuccess EQ 0 THEN BEGIN
     errmsg = "Error building mask bits: "+mskerrmsg
     GOTO,err_handler
  ENDIF

  ;;Now: stitch and filter together the thermistor data, since we
  ;; will need that for all others.  This stiches and low-pass filters
  IF KEYWORD_SET(verbose) THEN $
     MESSAGE,"Processing thermistor data",/INF
  thermstruct = smap_tcdrift_gettherm(tods, stitchinfo, breakinfo,$
                                      MASKBITS=maskbits, $
                                      SUCCESS=thermsuccess, $
                                      ERRMSG=thermerrmsg,$
                                      VERBOSE=verbose, SEED=seed,$
                                      NOFILTER=nofilter,$
                                      NO250=no250, NO350=no350, NO500=no500)
  IF thermsuccess EQ 0 THEN BEGIN
     errmsg = "Error forming thermistor timestreams: "+thermerrmsg
     GOTO,err_handler
  ENDIF

  ;;Early return
  IF KEYWORD_SET(nocorr) THEN BEGIN
     success = 1b
     RETURN
  ENDIF

  ;;This is the main loop: for each light bolometer, stitch it
  ;; together, fill gaps, filter it, then apply the thermistor
  ;; correction
  ;;First, figure out what bolometers we want to use
  bollist = smap_tcdrift_getbollist( tods, SUCCESS=bolsuccess, $
                                     ERRMSG=bolerrmsg, NO250=no250,$
                                     NO350=no350, NO500=no500 )
  IF bolsuccess EQ 0 THEN BEGIN
     errmsg = "Error getting bolometer list: "+bolerrmsg
     GOTO, err_handler
  ENDIF
  nbolos = N_ELEMENTS(bollist)

  FOR ibol=0,nbolos-1 DO BEGIN
     bol = bollist[ibol]
     IF KEYWORD_SET(verbose) THEN BEGIN
        fmt = "('Correcting bolometer ',I3,' of ',I3,' [',F5.1,'%]: ',A0)"
        pct = 100*FLOAT(ibol+1)/nbolos
        MESSAGE,STRING(ibol+1,nbolos,pct,bol,FORMAT=fmt),/INF
     ENDIF

     ;;Stitch
     smap_tcdrift_stitch, bol, tods, stitchinfo,$
                          timestream, mask, MASKBITS=maskbits, $
                          SUCCESS=stitchsuccess, ERRMSG=sticherrmsg, $
                          SEED=seed
     IF stitchsuccess EQ 0 THEN BEGIN
        errmsg = "Error stitching timestreams "+stitcherrmsg
        GOTO,err_handler
     ENDIF

     ;;Filter
     IF ~ KEYWORD_SET( nofilter ) THEN BEGIN
        smap_tcdrift_filter, bol, timestream, mask, stitchinfo, breakinfo,$
                             SUCCESS=filtsuccess, $
                             ERRMSG=filterrmsg, VERBOSE=verbose
        IF filtsuccess EQ 0 THEN BEGIN
           errmsg = "Error filtering: "+filterrmsg
           GOTO, err_handler
        ENDIF
     ENDIF

     ;;Get data without gap fill
     smap_tcdrift_stitch, bol, tods, stitchinfo,$
                          timestream_nogap, mask_nogap, MASKBITS=maskbits, $
                          SUCCESS=stitchsuccess, ERRMSG=sticherrmsg, $
                          SEED=seed, /NOGAP
     IF stitchsuccess EQ 0 THEN BEGIN
        errmsg = "Error stitching timestreams without gaps "+stitcherrmsg
        GOTO,err_handler
     ENDIF

     ;;Correct and store back in tod
     smap_tcdrift_thermcorr, tods, bol, stitchinfo, breakinfo, $
                             thermstruct, TEMPORARY(timestream),$
                             TEMPORARY(mask), TEMPORARY(timestream_nogap),$
                             TEMPORARY(mask_nogap), NOROBUST=norobust,$
                             NOTHERMMASK=nothermmask, TODMEAN=todmean,$
                             SUCCESS=thermsuccess,$
                             ERRMSG=thermerrmsg
     IF thermsuccess EQ 0 THEN BEGIN
        errmsg = "Error applying thermistor correction: "+thermerrmsg
        GOTO, err_handler
     ENDIF

  END
   
  ;; if we made it this far we're in the clear - yay!
  success = 1b
  RETURN
  
  ;; error handling
  err_handler:
  IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
  RETURN

END

