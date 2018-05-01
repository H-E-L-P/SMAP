;+
;NAME
; make_noise_tod
;PURPOSE
; To replace the signal in an input TOD with noise
; having specified properties.
;USAGE
; noise_tod = make_noise_tod( tod, psd_freq, psd [, SEED=
;                              BOLOMETERSENS=, /ADDONEOVERF,
;                              ONEOVERFNORM=, ONEOVERFALPHA= ] )
;INPUTS
; tod             The input tod from smap_readtod
; psd_freq        The frequency of the noise spectrum in 1/[time units]
; psd             The PSD of the noise in [signal units]^2 [time
;                  units].  This must be non-negative.
;RETURNS
; A copy of the input tod with the signal replaced by noise.  If the
; code can identify bad channels, these will be ignored unless
; /FILLBAD is set.  The mean signal value is zero in each channel.
;KEYWORDS
; addoneoverf     Add 1/f-like noise to the timestreams, assumed to
;                  be perfectly correlated between all channels.
; fillbad         Also fill dead channels with noise.
; verbose         Print informational status messages as the code runs.
;OPTIONAL INPUTS
; seed            The seed for the random number generator.
; bolometersens   A structure of arrays with tags .CHAN and
;                  .SENSITIVITY giving the sensitivity for each
;                  channel.  The noise timestream is multiplied by
;                  this before being stuck in each channel.  Channels
;                  which are not found are given sensitivity one.
; oneoverfnorm    Normalization of 1/f-like noise; see generate_fnoise.
;                  You must provide this is /ADDONEOVERF is set.
; oneoverfalpha   The power law coefficient of the 1/f-like noise,
;                  which is really oneoverfnorm/f^alpha in the same
;                  units as the psd.  See generate_fnoise.
;OPTIONAL OUTPUTS
; success         1b if the code succeeded, 0b if it did not.
; errmsg          An error message explaining why the code failed if
;                  it did, '' if it succeeded.
;MODEL
;  The model is that the psd_freq/psd noise is uncorrelated between
;  channels, and the 1/f-like noise is perfectly correlated.
;MODIFICATION HISTORY
; Author: Alex Conley, Oct. 2009
;-

FUNCTION make_noise_tod, tod, psd_freq, psd, SEED=seed, $
                         BOLOMETERSENS=bolometersens, ADDONEOVERF=addoneoverf,$
                         ONEOVERFNORM=oneoverfnorm, $
                         ONEOVERFALPHA=oneoverfalpha, FILLBAD=fillbad,$
                         VERBOSE=verbose, SUCCESS=success, ERRMSG=errmsg
  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg  = ''

  ;;Input checks
  IF SIZE( tod, /TNAME ) NE 'STRUCT' THEN BEGIN
     errmsg = "Input TOD is not a structure"
     GOTO, err_handler
  ENDIF
  tags_needed = ['sampfreq','chan','signal','mask','nsamps']
  wpresent = WHERE_ARRAY( TAG_NAMES(tod), tags_needed, npresent )
  IF npresent NE N_ELEMENTS(tags_needed) THEN BEGIN
     wmissing = MISSING( TAG_NAMES(tod), tags_needed, npresent )
     errmsg   = "Missing some needed tags in input TOD: "+$
                STRJOIN(tags_needed[wmissing],',')
     GOTO, err_handler
  ENDIF

  nfreq = N_ELEMENTS( psd_freq )
  IF nfreq EQ 0 THEN BEGIN
     errmsg = "Psd freq has no entries"
     GOTO, err_handler
  ENDIF
  IF N_ELEMENTS( psd ) NE nfreq THEN BEGIN
     errmsg = "psd not same length as psd_freq"
     GOTO, err_handler
  ENDIF
  min_psd = MIN( psd )
  IF min_psd LT 0.0 OR ~ FINITE( min_psd ) THEN BEGIN
     errmsg = "PSD must be positive at all frequencies"
     GOTO, err_handler
  ENDIF

  ;;Figure out min/max frequencies we need
  df = tod.sampfreq / DOUBLE( tod.nsamps ) ;;frequency step
  IF KEYWORD_SET(verbose) THEN BEGIN
     max_f = MAX( psd_freq, MIN=min_f )
     IF min_f GT df THEN $
        MESSAGE,"WARNING: Input PSD doesn't cover min required freq;"+$
                " extrapolation will be used",/INF
     IF max_f LT df*(tod.nsamps/2+1) THEN $
        MESSAGE,"WARNING: Input PSD doesn't cover max required freq;"+$
                " extrapolation will be used",/INF
  ENDIF

  do_oneoverf = KEYWORD_SET( addoneoverf )
  IF do_oneoverf THEN BEGIN
     IF N_ELEMENTS( oneoverfnorm ) THEN BEGIN
        errmsg = "Must provide ONEOVERFNORM if /ADDONEOVERF set"
        GOTO, err_handler
     ENDIF
     IF oneoverfnorm LT 0.0 THEN BEGIN
        errmsg = "oneoverfnorm must be positive"
        GOTO, err_handler
     ENDIF
     ;;If the normalization is zero, then there's no need
     ;; to actually add any
     IF oneoverfnorm EQ 0 THEN do_oneoverf = 0b
  ENDIF
  
  ;;Handler bolometer sensitivity
  IF N_ELEMENTS( bolometersens ) NE 0 THEN BEGIN
     IF SIZE( bolometersens, /TNAME ) NE 'STRUCT' THEN BEGIN
        errmsg = "Bolometersens passed in but not structure"
        GOTO, err_handler
     ENDIF
     ;;Make sure it has the tags we need
     IF ~ TAG_EXIST( bolomometersens, 'chan', /TOP_LEVEL ) THEN BEGIN
        errmsg = "Bolometersens doesn't have .chan tag"
        GOTO, err_handler
     ENDIF
     IF ~ TAG_EXIST( bolomometersens, 'sensitivity', /TOP_LEVEL ) THEN BEGIN
        errmsg = "Bolometersens doesn't have .chan tag"
        GOTO, err_handler
     ENDIF

     do_sens = 1b

     ;;Build an internal array in the same order as tod.chan
     ;; giving the sensitivities
     isens = DBLARR( tod.nchans )
     bol_chans = STRUPCASE( bolometersens.chan )
     bol_sens  = bolometersens.sensitivity
     wpresent1 = WHERE_ARRAY( tod.chans, bol_chans, npresent1 )

     ;;First, eliminate any channels not found in the tod, as there
     ;; is no point in keeping those
     IF npresent1 NE N_ELEMENTS( bol_chans ) THEN BEGIN
        IF KEYWORD_SET( verbose ) THEN BEGIN
           wmissing = MISSING( tod.chans, bol_chans )
           MESSAGE,"Some bolometers in bolometersens not found in tod: "+$
                   STRJOIN(bol_chans[wmissing],','),/INF
        ENDIF
        IF npresent1 NE 0 THEN BEGIN
           bol_chans = bol_chans[wpresent1]
           bol_sens  = bol_sens[wpresent]
        ENDIF ELSE DELVARX,bol_chans,bol_sens
        DELVARX, wmissing
     ENDIF
     DELVARX, wpresent1

     ;;Now add on any missing bolometer sensitivities with default
     ;; value (1)
     IF N_ELEMENTS(bol_chans) NE 0 THEN BEGIN
        wmissing = MISSING( bol_chans, tod.chans, nmissing )
        IF nmissing NE 0 THEN BEGIN
           IF KEYWORD_SET( verbose ) THEN $
              MESSAGE,"Some channels in TOD not found in bolometersens: "+$
                      STRJOIN( tod.chans[wmissing],','),/INF
           bol_chans = [bol_chans, tod.chans[wmissing]]
           bol_sens  = [bol_sens, REPLICATE(1.0,nmissing)]
        ENDIF
     ENDIF ELSE do_sens = 0b ;;No user specified values are useful, so skip

     ;;Now match the order into chans
     IF do_sens THEN BEGIN
        mord = MATCH_ORDER( tod.chans, bol_chans )
        bol_chans = bol_chans[mord]
        bol_sens  = bol_sens[mord]
        DELVARX,mord
        
        ;;Error check
        wbad = WHERE( tod.chans NE bol_chans, nbad )
        IF nbad NE 0 THEN MESSAGE,"You found a bug"
        DELVARX, bol_chans
     ENDIF
  ENDIF ELSE do_sens = 0b

  ;;Find dead channels so we can skip them
  skip_chans = 0b
  IF ~ KEYWORD_SET(fillbad) && TAG_EXIST(tod,'mask_bits',/TOP_LEVEL) THEN BEGIN
     wdeadchan = WHERE( STRUPCASE(tod.mask_bits.name) EQ 'MASKDEAD', $
                        ndeadchan )
     IF ndeadchan EQ 1 THEN BEGIN
        dead_maskbit = tod.mask_bits[wdeadchan].bits
        skip_chans=1b
     ENDIF
  ENDIF

  ;;Make 1/f-like (perfectly correlated noise) if needed
  IF do_oneoverf THEN BEGIN
     fnoise = generate_fnoise( tod.nsamps, 1.0/tod.sampfreq, $
                               oneoverfnorm, ALPHA=oneoverfalpha,$
                               SEED=seed, SUCCESS=fsuccess, ERRMSG=errmsg )
     IF fsuccess EQ 0b THEN BEGIN
        errmsg = "While making 1/f-like correlated noise: "+errmsg
        GOTO, err_handler
     ENDIF
  ENDIF

  ;;Main loop for uncorrelated noise
  tod_output = tod
  FOR i=0, tod.nchans-1 DO BEGIN
     ;;Figure out if this channel is dead
     ;; The dead channel bit should be set for all timesamples
     IF skip_chans THEN $
        IF (dead_maskbit AND tod.mask[i,0]) NE 0 THEN CONTINUE

     ;;Make uncorrelated noise
     channoise = generate_psd_noise( tod.nsamps, 1.0/tod.sampfreq,$
                                     psd_freq, psd, SEED=seed,$
                                     /NOPOSCHECK,/NOWARN, SPLINE=spline,$
                                     LSQUADRATIC=lsquadratic,$
                                     SUCCESS=esuccess, ERRMSG=errmsg )
     IF esuccess EQ 0b THEN BEGIN
        errmsg = "While making uncorrelated noise for channel: "+$
                 tod.chans[i]+": "+errmsg
        GOTO, err_handler
     ENDIF

     IF do_oneoverf THEN channoise += fnoise
     IF do_sens THEN channoise *= bol_sens[i]
     tod_output.signal[i,*] = TEMPORARY(channoise)
  ENDFOR

  success = 1b
  RETURN,tod_output

err_handler:
  IF KEYWORD_SET( verbose ) THEN MESSAGE,errmsg,/INF
  RETURN,!VALUES.F_NAN

END


