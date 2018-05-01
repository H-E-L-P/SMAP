;+
;NAME
; generate_psd_noise
;PURPOSE
; To generate simulated noise following a specified PSD
;USAGE
; signal = generate_psd_noise( ndata, dt, psd_freq, psd )
;INPUTS
; ndata         Number of data points
; dt            Timestep
; psd_freq      Frequency array for PSD, in 1/[units of dt]
; psd           Tabulated value of PSD in [signal units]^2 [units of
;                dt].  The PSD -must- be non-negative at all
;                                     frequencies.
;RETURNS
; A simulated timestream having the specified PSD and zero mean.
;OPTIONAL INPUTS
; seed          Random phase seed
;KEYWORDS
; noposcheck    Don't check to make sure the PSD is positive
; nowarn        Don't issue warnings
; spline        Use a spline instead of linear interpolation;
;                see the INTERPOL documentation
; lsquadratic   Use a least-squares quadratic fit instead of linear
;                interpolation; see the INTERPOL documentation
;OPTIONAL OUTPUTS
; success       1b if the code succeeded, 0b if it did not.
; errmsg        An error message explaining why the code failed if
;                it did, '' if it succeeded.
;NOTES
; INTERPOL is used to do the interpolation.
; Typically, psd_freq will be in Hz, and psd will be in Jy^2/Hz.
;  The output signal will then be in Jy.
; To understand the normalization, you need to read the documentation
;  for auto_correlate.
;MODIFICATION HISTORY
; Author: Alex Conley, Oct 27, 2009
;-

FUNCTION generate_psd_noise, ndata, dt, psd_freq, psd, $
                             SPLINE=spline, LSQUADRATIC=lsquadratic,$
                             SEED=seed, NOPOSCHECK=noposcheck, NOWARN=nowarn,$
                             SUCCESS=success, ERRMSG=errmsg, VERBOSE=verbose
  COMPILE_OPT IDL2, STRICTARRSUBS
  
  success = 0b
  errmsg = ''

  ;;Input tests
  IF ndata LT 0 THEN BEGIN
     errmsg = "Invalid ndata: "+STRING(ndata)
     GOTO, err_handler
  ENDIF
  IF dt LT 0 THEN BEGIN 
     errmsg = "Invalid dt: "+STRING(dt)
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

  ;;Make sure PSD is non-negative
  IF ~ KEYWORD_SET( noposcheck ) THEN BEGIN
     min_psd = MIN( psd )
     IF min_psd LT 0.0 OR ~ FINITE( min_psd ) THEN BEGIN
        errmsg = "PSD must be positive at all frequencies"
        GOTO, err_handler
     ENDIF
  ENDIF

  df = 1.0 / DOUBLE( ndata * dt )
  ;;Warn user about extrapolation
  IF ~ KEYWORD_SET( nowarn ) THEN BEGIN
     max_f = MAX( psd_freq, MIN=min_f )
     IF min_f GT df THEN $
        MESSAGE,"WARNING: Input PSD doesn't cover min required freq;"+$
                " extrapolation will be used",/INF
     IF max_f LT df*(ndata/2+1) THEN $
        MESSAGE,"WARNING: Input PSD doesn't cover max required freq;"+$
                " extrapolation will be used",/INF
  ENDIF

  IF ndata MOD 2 EQ 0 THEN BEGIN
     val = DBLARR(ndata/2 + 1)
     freq = df*FINDGEN( N_ELEMENTS(val) )
     prefac = 1.0 / SQRT( 2.0*ndata*dt )
     val[0] = 0.0
     val[1:*] = prefac * SQRT(INTERPOL( psd, psd_freq, freq[1:*],$
                                        SPLINE=spline, $
                                        LSQUADRATIC=lsquadratic ))
     DELVARX,freq
     phase = 2*!PI*RANDOMU(seed,ndata/2-1)
     fdat = dcomplexarr(ndata)
     fdat[0] = val[0]
     fdat[1:ndata/2-1] = DCOMPLEX( val[1:ndata/2-1]*COS(phase),$
                                   val[1:ndata/2-1]*SIN(phase) )
     fdat[ndata/2] = val[ndata/2]
     DELVARX,val
     fdat[ndata/2+1:*] = CONJ( REVERSE( fdat[1:ndata/2-1] ) )
     retval = REAL_PART(FFT( TEMPORARY(fdat),/INVERSE))
  ENDIF ELSE BEGIN
     ;;non power of 2
     ndm1o2 = (ndata-1)/2
     val = DBLARR(ndm1o2 + 1)
     freq = df*FINDGEN( N_ELEMENTS(val) )
     prefac = 1.0 / SQRT(2.0*ndata*dt)
     val[0] = 0.0
     val[1:*] = prefac* SQRT(INTERPOL( psd, psd_freq, freq[1:*],$
                                       SPLINE=spline, LSQUADRATIC=lsquadratic))
     DELVARX,freq
     phase = 2*!PI*RANDOMU(seed,ndm1o2)
     fdat = dcomplexarr(ndata)
     fdat[0] = val[0]
     fdat[1:ndm1o2] = DCOMPLEX( val[1:ndm1o2]*COS(phase),$
                                val[1:ndm1o2]*SIN(phase) )
     DELVARX,val
     fdat[ndm1o2+1:*] = CONJ( REVERSE( fdat[1:ndm1o2] ) )
     retval = REAL_PART(FFT( TEMPORARY(fdat),/INVERSE))
  ENDELSE
  success = 1b
  RETURN,retval

err_handler:
  IF KEYWORD_SET( verbose ) THEN MESSAGE,errmsg,/INF
  RETURN,!VALUES.F_NAN
END
