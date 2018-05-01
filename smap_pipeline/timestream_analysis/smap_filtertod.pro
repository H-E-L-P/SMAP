;+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  pro smap_filtertod.pro
;;  Aug 30, 2009
;;  Mike Zemcov
;;  This function takes smap formatted single scan tod and a filter
;;  flag as input and filters the data - it's basically meant
;;                                       to be just a front end 
;;  Inputs: tod = smap formatted data structure to be filtered
;;          filt = Which filter you want:
;;                  'n' : None
;;                  'm' : Median
;;                  'w' : Weiner
;;                  'p?' : Polynomial where ? is the order (i.e., 'p1')
;;                 Note there is no way to apply multiple filters
;;  Options: verbose = verbosity flag, 0 = silent
;;           success = success flag, 1=successful, 0=not
;;           errmsg = if error, string containing error message 
;;  Outputs = none
;;  Changelog: v1.0, MZ, Aug 30, 2009, orignal version implementing
;;   only 'n' and 'm' filters.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-
PRO SMAP_FILTERTOD,tod,filt,$
                   OUTPOLYS=outpolys,$
                   EXCLUDEMASK=excludemask,$
                   VERBOSE=verbose,$
                   SUCCESS=success,$
                   ERRMSG=errmsg

  COMPILE_OPT IDL2

  ; set up the error handling variables
  success = 0b
  errmsg = ''

  ; set up verbosity
  IF NOT(KEYWORD_SET(verbose)) THEN verbose = 0b

  ; strip out first character of the name
  filt_type = STRMID(filt,0,1)


  IF TAG_EXIST( tod, 'mask_bits', /TOP_LEVEL ) THEN BEGIN
     mapmaskbits = construct_mask_bitmask( excludeMask, $
                                           tod.mask_bits )
  ENDIF ELSE DELVARX,mapmaskbits

  ; check what filt flag we've been given
  CASE filt_type OF
     ; if n filter requested, don't do anything.
     'n': BEGIN
        ; tell me what's up if I want to know
        IF KEYWORD_SET(verbose) THEN $
           MESSAGE,'No filter requested!  Continuing...',/INFORMATIONAL
     END
     ; if m filter requested, do a median filter
     'm': BEGIN
        ; tell me what's up if I want to know
        IF KEYWORD_SET(verbose) THEN $
           MESSAGE,'Median filter requested.  Filtering...',/INFORMATIONAL
        ; loop through channels applying the filter
        FOR ichan=0,tod.nchans-1 DO BEGIN
           ; find the finite elements of these data and apply the mask
           IF N_ELEMENTS(mapmaskbits) NE 0 THEN BEGIN
              whfinite = WHERE( FINITE( tod.signal[ichan,*] ) AND $
                                ( (tod.mask[ichan,*] AND mapmaskbits) EQ 0 ), $
                                count)
           ENDIF ELSE BEGIN
              whfinite = WHERE( FINITE( tod.signal[ichan,*] ), count)
           ENDELSE
           ; if there are finite, unmasked elements
           IF count GT 0 THEN BEGIN
              ; make median
              mymed = MEDIAN(tod.signal[ichan,whfinite])
              ; subtract it
              tod.signal[ichan,*] = REFORM(tod.signal[ichan,*]) - mymed
              ; if the data says the whole scan is nan, kill 
              ; it just to be sure
           ENDIF ELSE tod.signal[ichan,*] = !VALUES.F_NAN
        ENDFOR
     END
     'p': BEGIN
        polyord = FIX(STRMID(filt,1))
        outpolys = REMOVE_TIMESTREAM_POLY(tod,BADMASK=mapmaskbits,$
                                          POLY_ORDER=polyord,$
                                          VERBOSE=verbose,SUCCESS=rtp_success,$
                                          ERRMSG=rtp_errmsg)

        IF ~rtp_success THEN BEGIN
           errmsg = 'REMOVE_TIMESTREAM_POLY kicked up error: ' + rtp_errmsg
           IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
           RETURN
        ENDIF
     END
     'w' : BEGIN
        SMAP_WFILTER, tod, SUCCESS=wsuccess, ERRMSG=errmsg
        IF wsuccess EQ 0 THEN BEGIN
           IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
           RETURN
        ENDIF
     END
  ENDCASE

  ; ok, we've applyed the fitler and we can go now.
  success=1b

  ; the filtered data is passed back implicitly as this is a pro
  RETURN

END
