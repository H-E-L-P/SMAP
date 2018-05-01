;+
;NAME
; generate_fnoise
;PURPOSE
; To generate simulated 1/f noise
;USAGE
; signal = generate_fnoise( ndata, dt, const, [ALPHA= ])
;INPUTS
; ndata         Number of data points
; dt            Timestep
; const         Model parameter
;OPTIONAL INPUTS
; alpha         Model parameter (def: 2)
; seed          Random phase seed
;RETURNS
; A simulated 1/f timestream with mean zero
;OPTIONAL OUTPUTS
; success       1b if the code succeeded, 0b if it did not.
; errmsg        An error message explaining why the code failed if
;                it did, '' if it succeeded.
;MODEL
; The model is that the PSD of the data is const/f^(alpha),
;  where the PSD is normalized as in auto_correlate.  Note that
;  this is squared relative to what most people expect, because
;  auto_correlate returns PSDs in unit^2 Hz^-1 (i.e., Jy^2/Hz).
; To understand the normalization, you need to read the documentation
;  for auto_correlate.
;MODIFICATION HISTORY
; Author: Alex Conley, July 2, 2009
;-

FUNCTION generate_fnoise, ndata, dt, const, ALPHA=alpha, SEED=seed,$
                          SUCCESS=success, ERRMSG=errmsg, VERBOSE=verbose
  COMPILE_OPT IDL2, STRICTARRSUBS
  
  success = 0b
  errmsg = ''

  ;;Input tests
  IF ndata LT 0 THEN BEGIN
     errmsg = "Invalid ndata: "+STRING(ndata)
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.D_NAN
  ENDIF
  IF dt LT 0 THEN BEGIN 
     errmsg = "Invalid dt: "+STRING(dt)
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.D_NAN
  ENDIF
  IF const LT 0 THEN BEGIN
     errmsg="Invalid const: "+STRING(const)
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.D_NAN
  ENDIF
  IF N_ELEMENTS(alpha) EQ 0 THEN alpha = 2.0
  IF alpha LE 0 THEN BEGIN
     errmsg = "Alpha must be positive: "+STRING(alpha)
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.D_NAN
  ENDIF

  IF ndata MOD 2 EQ 0 THEN BEGIN
     df = 1.0/DOUBLE(ndata*dt)
     val = DBLARR(ndata/2 + 1)
     freq = df*FINDGEN( N_ELEMENTS(val) )
     ;;Normalization -- mysterious looking, but not that bad
     prefac = SQRT( const / ( 2.0*ndata*dt ) )
     val[0] = 0                 ;mean, set to zero
     val[1:*] = prefac*freq[1:*]^(-alpha/2.0)
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
     df = 1.0/DOUBLE(ndata*dt)
     ndm1o2 = (ndata-1)/2
     val = DBLARR(ndm1o2 + 1)
     freq = df*FINDGEN( N_ELEMENTS(val) )
     prefac = SQRT( const / (2.0*ndata*dt) )
     val[0] = 0                 ;mean, set to zero
     val[1:*] = prefac*freq[1:*]^(-alpha/2.0)
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
END
