;+
;NAME
; simulate_scan
;PURPOSE
; To rescan a user-provided map, replacing the TOD data
; with the scan of the map
;USAGE
; simulate_scan, tods, maps
;ARGUMENTS
; tods      Pointer array of TODs from smap_read_and_filter.  Modified
;            on output
; maps      Pointer array of SMAP maps to scan.  All bands are
;            modified, so if you don't provide the map in a given band,
;            you will get back a blank (or noise only) scan
;OPTIONAL INPUTS
; noise     Noise per sample, assumed Gaussian, in Jy.  The defaults are
;            0.0384/0.0417/0.0491 Jy/beam.  If your maps already
;            have noise in them, you should probably set this to zero.
; fknee     Knee frequency for 1/f noise [in Hz], which is where 1/f and
;            white noise have equal power.  0 by default, which
;            means no 1/f noise is included.  If your noise level is
;            zero, you also don't get any 1/f noise.  This can be
;            either a single value (applied to all bands) or a
;            different value for each band.  The SPIRE pre-flight goal
;            was 0.1 Hz.
; falpha    Alpha for 1/f noise -- the PSD is 1/f^alpha,
;            in Jy^2/Hz, so the usual meaning of 1/f noise is alpha=2,
;            the default.
; seed      The seed for the random number generator
;KEYWORDS
; verbose   Print informational messages as the code runs.
;NOTES
; If 1/f noise is included, the tods are grouped (to avoid gaps), then
;  a single long timestream for each group and band is generated and
;  added. So this ignores any response variations, and assumes
;  all the bolometers on a given array (PSW, PMW, PLW) have perfectly
;  correlated 1/f noise, but that they aren't correlated between bands.
; Note that setting fknee to a single value and noise to different
;  values for each band means you get different levels of 1/f noise
;  in each.
; You can achieve equal 1/f power with fknee=[0.048, 0.0414, 0.0267]
;  for the default settings (alpha=2, default white noise).
; This is not compatible with using STORE_PIXINFO when reading in the TODs.
;MODIFICATION HISTORY
; Author: Alex Conley, Feb 2010
;-

;; Group tods by time
FUNCTION simulate_scan_group, tods
  COMPILE_OPT IDL2, HIDDEN

  max_time_gap = 500.0 ;;seconds

  ;; Get min/max time for each
  ntods = N_ELEMENTS(tods)
  IF ntods EQ 0 THEN MESSAGE, "No TODS"

  retstr = {ngroups: 1, group: UINTARR(ntods),$
            nsamps: ULONARR(ntods), delta: FLTARR(ntods),$
            mintime: DBLARR(ntods), maxtime: DBLARR(ntods)}
  FOR i=0, ntods-1 DO BEGIN
     retstr.mintime[i] = MIN((*tods[i]).samptime, MAX=mx)
     retstr.maxtime[i] = mx
     retstr.delta[i] = (*tods[i]).samptime[1] - (*tods[i]).samptime[0]
     retstr.nsamps[i] = (*tods[i]).nsamps
  ENDFOR
  IF ntods EQ 1 THEN RETURN, retstr

  ;; Now group.  Note that we do not assume the tods are in order
  ;; However, we -do- assume that any change in sampling frequency
  ;; will be coupled with a big gap
  sort_idx = SORT(retstr.mintime + retstr.maxtime)
  groupnum = 0
  prev_maxtime = retstr.maxtime[sort_idx[0]]
  FOR i = 1, ntods-1 DO BEGIN
     idx = sort_idx[i]
     curr_mintime = retstr.mintime[idx]
     IF curr_mintime - prev_maxtime GT max_time_gap THEN groupnum += 1
     retstr.group[idx] = groupnum
     prev_maxtime = retstr.maxtime[idx]
  ENDFOR
  retstr.ngroups = groupnum + 1

  RETURN, retstr
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO simulate_scan_fnoise, tods, band, fconst, nsamps, delta, $
                          wgroup, group_nsamps, falpha, seed, SUCCESS=success,$
                          ERRMSG=errmsg
  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN
  success = 0b
  errmsg = ""

  f_smoothlen = 5 ;; Smoothing length for adding 1/f noise to therms
  wband = WHERE(band EQ ['PSW', 'PMW', 'PLW'], nband)
  IF nband EQ 0 THEN BEGIN
     errmsg = "Unknown band: " + band
     RETURN
  ENDIF
  n_level = [38.23d-3, 37.86d-3, 52.87d-3] ;; Hardwired no-sky noise levels

  ;; Generate noise
  fnoise = generate_fnoise(nsamps, delta, fconst, $
                           ALPHA=falpha, SEED=seed, ERRMSG=errmsg, $
                           SUCCESS=fsuccess)
  IF fsuccess EQ 0 THEN RETURN

  ;; Stitch into data
  prev_maxidx = 0
  maxidx = group_nsamps[0]
  FOR j=0, N_ELEMENTS(wgroup)-1 DO BEGIN
     tod_idx = wgroup[j]
     wt = where(STRMID((*tods[tod_idx]).chan, 0, 3) EQ band $
                AND (*tods[tod_idx]).islight, nt)
     curr_noise = fnoise[prev_maxidx:prev_maxidx + group_nsamps[j] - 1]
     nnoise = N_ELEMENTS(curr_noise)
     prev_maxidx += group_nsamps[j]

     ;; Add to all light channels
     IF nt NE 0 THEN $
        (*tods[tod_idx]).signal[wt, *] += curr_noise ## REPLICATE(1.0, nt)

     ;; Add to thermistors, with some smoothing, additional noise
     wt1 = WHERE((*tods[tod_idx]).chan EQ band+'T1', nt1)
     wt2 = WHERE((*tods[tod_idx]).chan EQ band+'T2', nt2)
     IF nt1 + nt2 GT 0 THEN BEGIN
        IF nt1 GT 1 OR nt2 GT 1 THEN BEGIN
           errmsg = "Found too many thermistors"
           RETURN
        ENDIF
        curr_noise = SMOOTH(curr_noise, f_smoothlen)
        IF nt1 EQ 1 THEN $
           (*tods[tod_idx]).signal[wt1[0], *] = $
           curr_noise + n_level[wband[0]] * RANDOMN(seed, nnoise)
        IF nt2 EQ 2 THEN $
           (*tods[tod_idx]).signal[wt2[0], *] = $
           curr_noise + n_level[wband[0]] * RANDOMN(seed, nnoise)
     ENDIF
  ENDFOR

  success = 1b
END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO simulate_scan, tods, maps, NOISE=noise, VERBOSE=verbose,$
                   ERRMSG=errmsg, SUCCESS=success, SEED=seed,$
                   FKNEE=fknee, FALPHA=falpha
  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg  = ''

;<<<<<<< .mine
;  IF keyword_set(noise) then begin
;     nnoise = N_ELEMENTS(noise)
;     IF noise[0] EQ -1 THEN BEGIN
;	inoise = [43.13,49.97,77.63]*1e-3;/([1.1619,1.5916,1.46926]*1.15)
;     ENDIF ELSE IF nnoise EQ 1 THEN BEGIN
;	inoise = REPLICATE(noise[0],3)
;     ENDIF ELSE IF nnoise EQ 3 THEN inoise=noise ELSE BEGIN
;	errmsg = "Don't know how to handle noise vector of length "+$
;	   STRING(nnoise)
;	GOTO,err_handler
;     ENDELSE
;  ENDIF ELSE inoise = 0.;REPLICATE(0.,3)
;=======
  f_smoothlen = 5 ;; Smoothing length for adding 1/f noise to therms

  nnoise = N_ELEMENTS(noise)
  IF nnoise EQ 0 THEN BEGIN
     ;; Default value tuned to match actual maps with fknee=0.060 Hz
     inoise = [38.4, 41.7, 49.1] * 1e-3 ;; in Jy/beam
  ENDIF ELSE IF nnoise EQ 1 THEN BEGIN
     inoise = REPLICATE(noise[0], 3)
  ENDIF ELSE IF nnoise EQ 3 THEN inoise=noise ELSE BEGIN
     errmsg = "Don't know how to handle noise vector of length "+$
              STRING(nnoise)
     GOTO,err_handler
  ENDELSE
;>>>>>>> .r1119

  nfknee = N_ELEMENTS(fknee)
  IF nfknee EQ 0 THEN BEGIN
     ifknee = [0.0, 0, 0]
  ENDIF ELSE IF nfknee EQ 1 THEN BEGIN
     ifknee = REPLICATE(fknee[0], 3)
  ENDIF ELSE IF nfknee EQ 3 THEN ifknee=fknee ELSE BEGIN
     errmsg = "Don't know how to handle knee vector of length "+$
              STRING(fknee)
     GOTO,err_handler
  ENDELSE

  ntods = N_ELEMENTS(tods)
  IF ntods EQ 0 THEN BEGIN
     errmsg = "No tods passed in"
     GOTO, err_handler
  ENDIF
  IF SIZE(tods,/TNAME) NE 'POINTER' THEN BEGIN
     errmsg = "Expected pointer of TODs"
     GOTO, err_handler
  ENDIF

  nmaps = N_ELEMENTS(maps)
  IF nmaps EQ 0 THEN BEGIN
     errmsg = "No maps passed in"
     GOTO, err_handler
  ENDIF
  IF SIZE(maps,/TNAME) NE 'POINTER' THEN BEGIN
     errmsg = "Expected pointer of maps"
     GOTO,err_handler
  ENDIF

  ;;Figure out which bands we are doing
  bands = STRARR(nmaps)
  FOR i=0, nmaps - 1 DO bands[i] = (*maps[i]).names
  w250 = WHERE(bands EQ 'PSW', n250)
  w350 = WHERE(bands EQ 'PMW', n350)
  w500 = WHERE(bands EQ 'PLW', n500)

  IF n250 + n350 + n500 EQ 0 THEN BEGIN
     errmsg = "Didn't recognize the bands of any of the maps"
     GOTO, err_handler
  ENDIF

  ;; Add map signal and white noise
  FOR i=0,ntods-1 DO BEGIN
     IF KEYWORD_SET(verbose) THEN BEGIN
        msg = STRING(i, ntods - 1, 100.0 * i/(ntods - 1.0),$
                     FORMAT='("Processing ",I0," of ",I0," [",F5.1,"%]")')
        MESSAGE,msg,/INF
     ENDIF

     DELVARX, tout

     ;; Only scan the light bolometers
     wt250 = where(STRMID((*tods[i]).chan, 0, 3) EQ 'PSW' $
                   AND (*tods[i]).islight, nt250)
     wt350 = where(STRMID((*tods[i]).chan, 0, 3) EQ 'PMW' $
                   AND (*tods[i]).islight, nt350)
     wt500 = where(STRMID((*tods[i]).chan, 0, 3) EQ 'PLW' $
                   AND (*tods[i]).islight, nt500)
     IF (nt250 + nt350 + nt500 EQ 0) THEN BEGIN
        errmsg = "No bands found!"
        GOTO, err_handler
     ENDIF

     ;; Loop over each maps in each band, adding signal
     ;; This builds up tout, which is a lightweighted tod containing
     ;; the scanned map
     IF n250 NE 0 AND nt250 NE 0 THEN BEGIN
        IF n250 GT 0 THEN FOR j=0, n250-1 DO BEGIN
           get_timestream_from_map, *tods[i], *maps[w250[j]], tout,$
                                    SUCCESS=get_success, ERRMSG=errmsg
           IF get_success EQ 0 THEN BEGIN
              errmsg = "While making PSW timestream: " + STRING(j) + " " $
                       + errmsg
              GOTO, err_handler
           ENDIF
        ENDFOR
     ENDIF

     IF n350 NE 0 AND nt350 NE 0 THEN BEGIN
        IF n350 GT 0 THEN FOR j=0, n350-1 DO BEGIN
           get_timestream_from_map, *tods[i], *maps[w350[j]], tout,$
                                    SUCCESS=get_success, ERRMSG=errmsg
           IF get_success EQ 0 THEN BEGIN
              errmsg = "While making MSW timestream: " + STRING(j) + " " $
                       + errmsg
              GOTO, err_handler
           ENDIF
        ENDFOR
     ENDIF

     IF n500 NE 0 AND nt500 NE 0 THEN BEGIN
        IF n500 GT 0 THEN FOR j=0, n500-1 DO BEGIN
           get_timestream_from_map, *tods[i], *maps[w500[j]], tout,$
                                    SUCCESS=get_success, ERRMSG=errmsg
           IF get_success EQ 0 THEN BEGIN
              errmsg = "While making PlW timestream: " + STRING(j) + " " $
                       + errmsg
              GOTO, err_handler
           ENDIF
        ENDFOR
     ENDIF

     ;;Now replace the signal in the output
     ;;This is a bit messy because tout doesn't have all the same
     ;; channels (things that don't see sky are not included)
     in_chans = (*tods[i]).chan
     wpresent = WHERE_ARRAY(in_chans, tout.chan, npresent)
     IF npresent NE tout.nchan THEN BEGIN
        wmissing = MISSING(minuend.chan, subtrahend.chan)
        errmsg = "Missing channels in input tod found in simulated one: "+$
                 STRJOIN(tout.chan[wmissing], ',')
        GOTO, err_handler
     ENDIF
     min_sort = SORT(in_chans)
     wunsort = WHERE(in_chans NE in_chans[min_sort], nunsort)
     IF nunsort EQ 0 THEN BEGIN
        ;;Already checked they are all present, so no need to verify output
        idxs = VALUE_LOCATE( in_chans, tout.chan )
     ENDIF ELSE BEGIN
        ;;So, sort by hand.  Sigh
        idxs = min_sort[VALUE_LOCATE(in_chans[min_sort], tout.chan)]
     ENDELSE
     (*tods[i]).signal[idxs, *] = tout.signal

     ;;Now add white noise
     in_bands = STRUPCASE(STRMID(in_chans[idxs], 0, 3))
     IF inoise[0] NE 0.0 THEN BEGIN 
        w250n = WHERE(in_bands  EQ 'PSW', n250n)
        ;; light channels only
        IF n250n NE 0 THEN BEGIN 
           snoise = inoise[0] * RANDOMN(seed, n250n, (*tods[i]).nsamps)
           (*tods[i]).signal[idxs[w250n], *] += TEMPORARY(snoise)
        ENDIF
     ENDIF
     IF inoise[1] NE 0.0 THEN BEGIN
        w350n = WHERE(in_bands  EQ 'PMW', n350n)
        IF n350n NE 0 THEN BEGIN
           snoise = inoise[1] * RANDOMN(seed, n350n, (*tods[i]).nsamps)
           (*tods[i]).signal[idxs[w350n], *] += TEMPORARY(snoise)
        ENDIF
     ENDIF
     IF inoise[2] NE 0.0 THEN BEGIN
        w500n = WHERE(in_bands  EQ 'PLW', n500n)
        IF n500n NE 0 THEN BEGIN
           snoise = inoise[2] * RANDOMN(seed, n500n, (*tods[i]).nsamps)
           (*tods[i]).signal[idxs[w500n], *] += TEMPORARY(snoise)
        ENDIF
     ENDIF
  END
  
  ;; Add 1/f noise.  Note that the 1/f between different channels
  ;; is assumed uncorrelated, which is certainly not true.  But since
  ;; be build the map in each band independently, it should be ok.
  IF MAX(ifknee * inoise) GT 0.0 THEN BEGIN
     do_fpsw = (ifknee[0] GT 0.0) AND (inoise[0] GT 0.0) AND (n250 NE 0)
     do_fpmw = (ifknee[1] GT 0.0) AND (inoise[1] GT 0.0) AND (n350 NE 0)
     do_fplw = (ifknee[2] GT 0.0) AND (inoise[2] GT 0.0) AND (n500 NE 0)

     IF N_ELEMENTS(falpha) EQ 0 THEN falpha=2.0

     fmt = '(" Adding ",I0,"um 1/f noise with ",F0.2," Hz knee")'
     IF do_fpsw THEN MESSAGE, STRING(250, ifknee[0], FORMAT=fmt), /INF
     IF do_fpmw THEN MESSAGE, STRING(350, ifknee[1], FORMAT=fmt), /INF
     IF do_fplw THEN MESSAGE, STRING(500, ifknee[2], FORMAT=fmt), /INF

     ;; Okay -- we need to group the TODS into continguous blocks
     ;; that don't have overly large time gaps between them.
     ;; Otherwise we would have to generate an impossibly long
     ;; timestreams
     group_info = simulate_scan_group(tods)
     
     ;; Now we go through each group
     FOR i=0, group_info.ngroups-1 DO BEGIN
        wgroup = WHERE(group_info.group EQ i, ngroup)
        IF ngroup EQ 0 THEN MESSAGE, "Grouping logic error"

        group_mintime = group_info.mintime[wgroup]
        wgroup = wgroup[SORT(group_mintime)] ;;Get them in time order
        group_mintime = group_info.mintime[wgroup]
        group_maxtime = group_info.maxtime[wgroup]
        group_delta = MEAN(group_info.delta[wgroup])
        group_nsamps = group_info.nsamps[wgroup]
        group_totsamps = TOTAL(group_nsamps, /PRESERVE)
        
        ;; one for each band
        fconsts = 2 * group_delta * inoise^2 * ifknee^falpha
        
        ;; Now do each band.  Note we ignore the small gaps
        ;; between samples -- not perfect, but easy
        IF do_fpsw THEN BEGIN
           simulate_scan_fnoise, tods, 'PSW', fconsts[0], group_totsamps,$
                                 group_delta, wgroup, group_nsamps,$
                                 falpha, seed, SUCCESS=fsuccess,$
                                 ERRMSG=errmsg
           IF fsuccess EQ 0 THEN GOTO, err_handler
        ENDIF ELSE BEGIN
           ;; Zero out any thermistors
           FOR j=0, ngroup-1 DO BEGIN
              tod_idx = wgroup[j]
              wpswt1 = WHERE((*tods[tod_idx]).chan EQ 'PSWT1', npswt1)
              wpswt2 = WHERE((*tods[tod_idx]).chan EQ 'PSWT2', npswt2)
              IF npswt1 NE 0 THEN (*tods[tod_idx]).signal[wpswt1, *] = 0.0
              IF npswt2 NE 0 THEN (*tods[tod_idx]).signal[wpswt2, *] = 0.0
           ENDFOR
        ENDELSE

        IF do_fpmw THEN BEGIN
           simulate_scan_fnoise, tods, 'PMW', fconsts[1], group_totsamps,$
                                 group_delta, wgroup, group_nsamps,$
                                 falpha, seed, SUCCESS=fsuccess,$
                                 ERRMSG=errmsg
           IF fsuccess EQ 0 THEN GOTO, err_handler
        ENDIF ELSE BEGIN
           FOR j=0, ngroup-1 DO BEGIN
              tod_idx = wgroup[j]
              wpmwt1 = WHERE((*tods[tod_idx]).chan EQ 'PMWT1', npmwt1)
              wpmwt2 = WHERE((*tods[tod_idx]).chan EQ 'PMWT2', npmwt2)
              IF npmwt1 NE 0 THEN (*tods[tod_idx]).signal[wpmwt1, *] = 0.0
              IF npmwt2 NE 0 THEN (*tods[tod_idx]).signal[wpmwt2, *] = 0.0
           ENDFOR
        ENDELSE

        IF do_fplw THEN BEGIN
           simulate_scan_fnoise, tods, 'PLW', fconsts[2], group_totsamps,$
                                 group_delta, wgroup, group_nsamps,$
                                 falpha, seed, SUCCESS=fsuccess,$
                                 ERRMSG=errmsg
           IF fsuccess EQ 0 THEN GOTO, err_handler
        ENDIF ELSE BEGIN
           FOR j=0, ngroup-1 DO BEGIN
              tod_idx = wgroup[j]
              wplwt1 = WHERE((*tods[tod_idx]).chan EQ 'PLWT1', nplwt1)
              wplwt2 = WHERE((*tods[tod_idx]).chan EQ 'PLWT2', nplwt2)
              IF nplwt1 NE 0 THEN (*tods[tod_idx]).signal[wplwt1, *] = 0.0
              IF nplwt2 NE 0 THEN (*tods[tod_idx]).signal[wplwt2, *] = 0.0
           ENDFOR
        ENDELSE
     ENDFOR
  ENDIF

  success = 1b
  RETURN

err_handler:
  IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
  RETURN

END
