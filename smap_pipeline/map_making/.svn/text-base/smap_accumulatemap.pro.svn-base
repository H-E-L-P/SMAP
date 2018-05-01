;+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  function smap_accumulatemap.pro
;;  Aug 30, 2009
;;  Mike Zemcov
;;  This procedure takes an smap tod structure, a set of 3 color map
;;  structures, and a mapparam structure and makes a map with them.
;;  Inputs: tod: standard smap tod structure filled with data
;;          map250: initialized 250 micron smap map structure; may already
;;            contain data
;;          map350: initialized 350 micron smap map structure; may already
;;            contain data
;;          map500: initialized 500 micron smap map structure; may already
;;            contain data
;;          mapparam: a standard smap map parameter structure
;;            corresponding to these data
;;          badbolos: a list of bolometers not to use
;;          usebolos: a list of bolometers yes to use (for making single-
;;            bolometer maps -- one bolometer per array)
;;  Options: weightedmap = should I report back scan variance weighted
;;            map or just super naive map (1 = use scan variance)
;;           no250/no350/no500 = Don't make maps in this band;
;;            note you still need to provide the dummy output argument.
;;           useglobal = Flag to reject timelines with a variance more
;;                      than 10x the variance of all bolometers in
;;                      that band in this TOD
;;           mapmaskbits = List of bits in the mask that indicate the
;;                          data should not be used.  Can be
;;                          constructed from construct_mask_bitmask
;;                          and the mask_bits field of tod
;;           verbose = verbosity flag, 0 = silent
;;           success = success flag, 1=successful, 0=not
;;           errmsg = if error, string containing error message 
;;  Outputs = none
;;
;;
;;  Modified: Timothy Ellsworth Bowers 1/31/10
;;            Added functionality for creating single-bolometer maps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-

PRO SMAP_ACCUMULATEMAP_INNER, tod, wbols, map, weightmap, xvals, yvals, $
                              WEIGHTED=weighted, GLOBALVAR=globalvar, $
                              USEGLOBAL=useglobal, MAPMASKBITS=mapmaskbits, $
                              BADBOLOS=badbolos
  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN

  nbols = N_ELEMENTS(wbols)
  IF nbols EQ 0 THEN RETURN

  IF N_ELEMENTS( mapmaskbits ) NE 0 && MIN(mapmaskbits) NE 0 && $
     TAG_EXIST(tod,'mask',/TOP_LEVEL) THEN do_mask=1b ELSE $
        do_mask=0b

  timestep = 1.0 / tod.sampfreq

  ;;Only 'light' bolomters should be passed in
  FOR i=0, nbols-1 DO BEGIN
     c_signal = REFORM(tod.signal[wbols[i],*])
     ;;Masking code
     IF do_mask THEN BEGIN
        wkeep = WHERE( FINITE(c_signal) AND $
                       (REFORM(tod.mask[wbols[i],*]) AND mapmaskbits) EQ 0, $
                       nkeep,NCOMPLEMENT=nnokeep )

     ENDIF ELSE wkeep = WHERE( FINITE(c_signal), nkeep, NCOMPLEMENT=nnokeep )

     IF KEYWORD_SET(badbolos) THEN $
        IF TOTAL(tod.chan[wbols[i]] EQ badbolos) GT 0 THEN nkeep = 0
         

     IF nkeep GE 2 THEN BEGIN ;;we need 2 to get the variance
        IF nnokeep NE 0 THEN BEGIN
           c_signal = c_signal[wkeep]
           c_thisx  = REFORM(xvals[wbols[i],wkeep])
           c_thisy  = REFORM(yvals[wbols[i],wkeep])
        ENDIF ELSE BEGIN
           c_thisx  = REFORM(xvals[wbols[i],*])
           c_thisy  = REFORM(yvals[wbols[i],*])
        ENDELSE

        scanvar = VARIANCE( c_signal, /NAN )
        iscanvar = 1.0d0 / scanvar

        ;;Global variance cut
        IF KEYWORD_SET( useglobal ) && FINITE(globalvar) AND $
           scanvar GT 10.0*globalvar THEN CONTINUE
        
        ;;Remove pixels outside the mask region
        wbad = WHERE( c_thisx LT 0 OR c_thisy LT 0 OR $
                      (~FINITE(c_thisx)) OR (~FINITE(c_thisy)), nbad,$
                      COMPLEMENT=wgood, NCOMPLEMENT=ngood )
        IF nbad NE 0 THEN BEGIN
           ;;MESSAGE,STRING(nbad,FORMAT='(I0," pixels outside map")'),/INF
           IF ngood EQ 0 THEN CONTINUE
           c_signal = c_signal[wgood]
           c_thisx  = c_thisx[wgood]
           c_thisy  = c_thisy[wgood]
        ENDIF
        
        ;;Now add to map
        ;;In general, we will have many hits going into
        ;; the same pixel, so the simple (and -very- fast)
        ;;  tempmap[c_thisx,c_thisy] += signal
        ;; doesn't work.
        ;;So, we can either do this in a loop 
        ;; or get fancy with Histogram.  Unfortunately, the
        ;; single histogram method is actually slower because
        ;; the number of pixels hit in each pass is fairly small.
        ;;The two histogram approach is better, but starts to
        ;; cross that scary complexity line.
        ;;Really, we should do this in c...
        ;;In any case, note that the maps are weighted either way.
        ;; If the weightmap is present, they get weighted by
        ;; the scan variance.  If not they get weighted by
        ;; the inverse of the sample frequency.  This is so we can combine
        ;; parallel and non-parallel mode observations correctly.
        ;;Note we store the variance in map.error right now
        IF KEYWORD_SET( weighted ) THEN weightval = iscanvar $
        ELSE weightval = timestep
        w_signal       = c_signal * weightval
        errval         = scanvar  * weightval^2
        FOR idx=0,N_ELEMENTS(c_thisx)-1 DO BEGIN
           cx = c_thisx[idx] & cy = c_thisy[idx]
           map.exposure[cx,cy] += timestep
           map.image[cx,cy] += w_signal[idx]
           map.error[cx,cy] += errval
           weightmap[cx,cy] += weightval
        ENDFOR
     ENDIF        
  ENDFOR
  RETURN

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO SMAP_ACCUMULATEMAP,tod,map250,wt250,map350,wt350,map500,wt500,mapparam,$
                       WEIGHTEDMAP=weimap,VERBOSE=verbose,SUCCESS=success,$
                       ERRMSG=errmsg,USEGLOBAL=useglobal,MAPMASKBITS=mapmaskbits,$
                       BADBOLOS=badbolos, USEBOLOS=usebolos,$
                       NO250=no250, NO350=no350, NO500=no500

  COMPILE_OPT IDL2

  ; set up the error handling variables
  success = 0b
  errmsg = ''

  ; set up verbosity
  IF ~(KEYWORD_SET(verbose)) THEN verbose = 0b

  ; if you didn't tell me about variance weighting, assume
  ; you want the dumbest possible thing
  IF ~(KEYWORD_SET(weimap)) THEN weimap = 0b

  ;;Get the x/y positions
  smap_getmap_xy, tod, map250, map350, map500, xvals, yvals

  IF ~ KEYWORD_SET( no250 ) THEN BEGIN
     ;;========================================================
     ;; Functionality for creating single-bolometer maps -- PSW
     ;;========================================================
     IF KEYWORD_SET(usebolos) THEN BEGIN
        psw_ind = WHERE(STRMID(usebolos,0,3) EQ 'PSW',n_found)
        IF n_found GT 0 THEN BEGIN
           psw_bolo = usebolos[psw_ind]
           w250 = WHERE(tod.chan EQ $
                        string(psw_bolo,format="(A0)"), n250)
        ENDIF ELSE n250 = 0
     ENDIF ELSE BEGIN
        ;;========================================================
        ;; If not single-bolometer maps, then get the list of light 
        ;; bolometers in each channel and build the map
        ;;========================================================
        w250 = WHERE(STRMID(tod.chan,0,3) EQ 'PSW' AND $
                     tod.islight,n250)
     ENDELSE
     
     IF n250 GE 2 THEN globalvar = VARIANCE(tod.signal[w250,*],/NAN) ELSE $
        globalvar = !VALUES.D_NAN
     IF n250 GE 1 THEN $
        smap_accumulatemap_inner, tod, w250, map250, wt250, xvals, yvals,$
                                  WEIGHTED=weimap,$
                                  GLOBALVAR=globalvar, USEGLOBAL=useglobal,$
                                  MAPMASKBITS=mapmaskbits,BADBOLOS=badbolos
  ENDIF

  IF ~ KEYWORD_SET( no350 ) THEN BEGIN
     ;;========================================================
     ;; Functionality for creating single-bolometer maps - PMW
     ;;========================================================
     IF KEYWORD_SET(usebolos) THEN BEGIN
        pmw_ind = WHERE(STRMID(usebolos,0,3) EQ 'PMW',n_found)
        IF n_found GT 0 THEN BEGIN
           pmw_bolo = usebolos[pmw_ind]
           w350 = WHERE(tod.chan EQ $
                        string(pmw_bolo,format="(A0)"), n350)
        ENDIF ELSE n350 = 0
     ENDIF ELSE BEGIN
        ;;========================================================
        ;; If not single-bolometer maps, then get the list of light 
        ;; bolometers in each channel and build the map
        ;;========================================================
        w350 = WHERE(STRMID(tod.chan,0,3) EQ 'PMW' AND $
                     tod.islight, n350)
     ENDELSE
     
     IF n350 GE 2 THEN globalvar = VARIANCE(tod.signal[w350,*],/NAN) ELSE $
        globalvar = !VALUES.D_NAN
     IF n350 GE 1 THEN $
        smap_accumulatemap_inner, tod, w350, map350, wt350, xvals, yvals,$
                                  WEIGHTED=weimap, GLOBALVAR=globalvar, $
                                  USEGLOBAL=useglobal, MAPMASKBITS=mapmaskbits, $
                                  BADBOLOS=badbolos
  ENDIF
  
  IF ~ KEYWORD_SET( no500 ) THEN BEGIN
     ;;========================================================
     ;; Functionality for creating single-bolometer maps - PLW
     ;;========================================================
     IF KEYWORD_SET(usebolos) THEN BEGIN
        plw_ind = WHERE(STRMID(usebolos,0,3) EQ 'PLW',n_found)
        IF n_found GT 0 THEN BEGIN
           plw_bolo = usebolos[plw_ind]
           w500 = WHERE(tod.chan EQ $
                        string(plw_bolo,format="(A0)"), n500)
        ENDIF ELSE n500 = 0
     ENDIF ELSE BEGIN
        ;;========================================================
        ;; If not single-bolometer maps, then get the list of light 
        ;; bolometers in each channel and build the map
        ;;========================================================
        w500 = WHERE(STRMID(tod.chan,0,3) EQ 'PLW' AND $
                     tod.islight, n500)
     ENDELSE
     
     IF n500 GE 2 THEN globalvar = VARIANCE(tod.signal[w500,*],/NAN) ELSE $
        globalvar = !VALUES.D_NAN
     IF n500 GE 1 THEN $
        smap_accumulatemap_inner, tod, w500, map500, wt500, xvals, yvals,$
                                  WEIGHTED=weimap, GLOBALVAR=globalvar, $
                                  USEGLOBAL=useglobal, MAPMASKBITS=mapmaskbits,$
                                  BADBOLOS=badbolos
  ENDIF
  success = 1b
  RETURN

  err_handler:
  IF verbose THEN MESSAGE,errmsg,/INF
  success = 0b
  RETURN

END
