;+
;NAME
; auto_correlate
;PURPOSE
; Form the auto correlation of a signal using fourier transforms,
; returning the power spectrum
;USAGE
;  psd = auto_correlate(data,nchunk)
;INPUTS
;  data             The data to form the PSD of
;  nchunk           The chunk size to use.  The transform is faster
;                    if this is a power of 2.  The maximum frequency 
;                    returned is 1/(2*delta) if this is even, and
;                    (n/2-1/2)/(n*delta) if this is odd, where
;                    delta is the time step for the data.
;                    The frequency step is 1/(nchunk*delta).
;                    Note that this code doesn't know the value
;                    of delta, so you have to apply that yourself.
;RETURNS
;  The one sided power spectral density for nchunk/2+1 frequencies.
;  The normalization is described below.  You must multiply by
;  delta to get the desired normalization.
;OPTIONAL OUTPUTS
;  frequencies      The frequencies of the returned PSD in 1/delta
;                    units.
;KEYWORDS
;  poly_remove      Remove a low polynomial from the data before
;                    transforming.
;  usehamming       Use a Hamming window function.  This turns on data
;                    overlapping.
;  usehanning       Use a Hanning window function.  This turns on data
;                    overlapping.
;  nooverlap        Don't overlap data segments.
;  verbose          Run in verbose mode
;OPTIONAL INPUTS
;  poly_order       Order of polynomial if poly_remove is set.  Must
;                    be from 1 to 6.  The default is 5.
;  discard_thresh   Threshold before warning the user that a certain
;                    fraction have been removed in the poly
;                    subtraction.  Def: 0.1 (10%)
;NORMALIZATION
;  There are a semi-infinite number of conventions for normalizing the
;   PSD.  The one here is defined as follows:  Let T = nchunk*delta,
;   and let the function the data is drawn from be f.  Then if the
;   returned variable is PSD, then 
;       \int_{0}^{f_c} PSD * delta df = 1/T \int_{0}^{T} f^2 dt
;   where f_c is the nyquist frequency (1/(2*delta)).  Once the returned
;   variable has been multiplied by delta, then the PSD is in units of
;   (the units of f)^2 Hz^{-1}.  You may want to take the square root,
;   as PSD is guaranteed to be positive.  However, this spoils the
;   symmetry with the cross_spectra (see cross_spectra.pro).
;  Don't forget to multiply by delta!
;  This means that the total of the returned variable should be
;   approximately equal to the total squared of the input variable
;   (before multiplying the returned value by delta, and assuming no
;   polynomial removal, etc.).
;  Translated from integrals to sums, this means that
;   \sum_{i=1}^{N/2+1} PSD*delta*f_c/(N/2+1) = 1/N \sum_{i=1}^{N} f^2
;  This convention has the rather nice property that the mean value
;   stays roughly constant as you increase nchunk.
;NOTES
;  Robust polynomial fits are removed for /POLY_REMOVE
;  If the window functions are turned on, the segments are overlapped
;   by half their length.  This presumes the S/N of your output PSD
;   is limited by your data size.  If you have a lot of data, you
;   might want to set /nooverlap.  There is no way to overlap without
;   using a window function.
;MODIFICATINO HISTORY
; Author: Alex Conley, Feb 2009.
;-

FUNCTION auto_correlate, data, nchunk, POLY_REMOVE=poly_remove,$
                         POLY_ORDER=poly_order, USEHANNING=usehanning,$
                         USEHAMMING=usehamming, FREQUENCIES=frequencies,$
                         NOOVERLAP=nooverlap, VERBOSE=verbose, $
                         DISCARD_THRESH=discard_thresh, SUCCESS=success,$
                         ERRMSG=errmsg

  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg  = ''

  IF N_ELEMENTS( discard_thresh ) EQ 0 THEN discard_thresh = 0.1

  chunktype = SIZE(nchunk,/TNAME)
  IF chunktype NE 'INT' AND chunktype NE 'BYTE' AND $
     chunktype NE 'LONG' THEN BEGIN
     errmsg = "ERROR in auto_correlate: chunktype is not byte/int/long"
     RETURN,!VALUES.F_NAN
  ENDIF
  
  ;;Check for power of 2 size in nchunk
  ;;This is for the sake of efficiency
  IF KEYWORD_SET(verbose) AND ~( (nchunk and (nchunk-1)) EQ 0) THEN $
     MESSAGE,"WARNING in auto_correlate: chunktype is not a power of 2",/INF
  nchunko2 = LONG(nchunk) / 2  ;;n/2-1/2 if odd
  
  invnchunk = 1.0 / DOUBLE(nchunk)

  IF KEYWORD_SET(usehanning) AND KEYWORD_SET(usehamming) THEN BEGIN
     errmsg="ERROR in auto_correlate: choose hanning or hamming, not both"
     RETURN,!VALUES.F_NAN
  ENDIF
  IF KEYWORD_SET( poly_remove ) THEN BEGIN
     IF N_ELEMENTS( poly_order ) EQ 0 THEN poly_order = 5
     IF poly_order LT 1 THEN BEGIN
        errmsg = "ERROR in auto_correlate: poly order < 1"
        RETURN,!VALUES.F_NAN
     ENDIF
     IF poly_order GT 6 THEN BEGIN 
        errmsg="ERROR in auto_correlate: poly order > 6"
        RETURN,!VALUES.F_NAN
     ENDIF
  ENDIF
  
  IF KEYWORD_SET(usehanning) OR KEYWORD_SET(usehamming) THEN overlap=1b ELSE $
     overlap=0b
  IF KEYWORD_SET(nooverlap) THEN overlap=0b
  IF KEYWORD_SET(usehanning) THEN wind = HANNING(nchunk,/DOUBLE)
  IF KEYWORD_SET(usehamming) THEN wind = HANNING(nchunk,/DOUBLE,ALPHA=0.54)
  
  powerspec = DBLARR(nchunko2+1) ;;Return variable
  
  IF KEYWORD_SET(verbose) THEN $
     MESSAGE,STRING(nchunk,FORMAT='("Building power spectrum with ",I6,'+$
                    '" data points at a time")'),/INF
  
  IF KEYWORD_SET(verbose) THEN BEGIN
     IF overlap THEN MESSAGE," Using half-overlapped data segments",/INF
     IF KEYWORD_SET(usehamming) THEN $
        MESSAGE," Using Hamming window function",/INF
     IF KEYWORD_SET(usehanning) THEN $
        MESSAGE," Using Hanning window function",/INF
  ENDIF
  
  ndata = N_ELEMENTS(data)
  IF overlap THEN BEGIN   
     numchunks = 2 * (ndata/nchunk) - 1
     maxindx = (numchunks+1)*nchunko2
     IF KEYWORD_SET(verbose) AND (ndata NE maxindx) THEN $
        MESSAGE,STRING(ndata-maxindx,ndata,100.0*FLOAT(ndata-maxindx)/ndata,$
                       FORMAT='(" Not using last ",I5," elements of ",I6,'+$
                       '" [",F5.2,"%]")'),/INF
  ENDIF ELSE BEGIN
     numchunks = ndata/nchunk
     maxindx = numchunks*nchunk
     IF KEYWORD_SET(verbose) AND (ndata NE maxindx) THEN $
        MESSAGE,STRING(ndata-maxindx,ndata,100.0*FLOAT(ndata-maxindx)/ndata,$
                       FORMAT='(" Not using last ",I5," elements of ",I6,'+$
                       '" [",F5.2,"%]")'),/INF
  ENDELSE
  
  ;;Get normalization factors
  IF KEYWORD_SET(usehamming) OR KEYWORD_SET(usehanning) THEN BEGIN
     invsumw2 = 1.0 / TOTAL( wind^2 ) ;;Window function normalization
  ENDIF ELSE invsumw2 = invnchunk     ;;If no window function, wind=1
  invnumchunks = 1.0 / DOUBLE(numchunks)
  prefac = 2.0 * nchunk * invsumw2 * invnumchunks
  
  mnvals = DBLARR(numchunks)
  FOR i=0, numchunks-1 DO BEGIN
     IF overlap THEN BEGIN
        idx_lower = i*nchunko2
        idx_upper = idx_lower + nchunk - 1
     ENDIF ELSE BEGIN
        idx_lower = i*nchunk
        idx_upper = idx_lower + nchunk - 1
     ENDELSE
     curr_sample = data[idx_lower:idx_upper]
     
     IF KEYWORD_SET( poly_remove ) THEN BEGIN
        coeffs = ROBUST_POLY_FIT(FINDGEN(nchunk),curr_sample,poly_order,yfit)
        curr_sample -= yfit
     ENDIF
     
     IF KEYWORD_SET(usehamming) OR KEYWORD_SET(usehanning) THEN $
        curr_sample *= wind
     
     ;;The IDL FFT normalization for the forward transform is
     ;; H_n = 1/N \sum_{k} h_k exp( -2 pi i k n / N )
     ;;This differs by a 1/N from the FFTW and numrec normalizations
     ft = FFT(curr_sample,/DOUBLE)
     
     ;;Note that we are deliberately ignoring half of the FFT
     ;;because we are taking advantage of the symmetry of the FFT
     ;; of the input real data ( H(-f) = CONJ(H(f)).  This is why
     ;; the middle chunk has a factor of 2 relative to the first and
     ;; last element.  The result must be non-negative.
     IF KEYWORD_SET( poly_remove ) THEN $
        powerspec[0] += 0.5*prefac*REAL_PART(ft[0])^2
     
     IF nchunk MOD 2 EQ 0 THEN BEGIN
        ;;Even length
        powerspec[1:nchunko2-1] += prefac*REAL_PART( ft[1:nchunko2-1] * $
                                                     CONJ(ft[1:nchunko2-1]) )
        powerspec[nchunko2] += 0.5*prefac*REAL_PART(ft[nchunko2])^2
     ENDIF ELSE BEGIN
        ;;Odd length
        powerspec[1:nchunko2] += prefac*REAL_PART(ft[1:nchunko2] * $
                                                  CONJ(ft[1:nchunko2]) )
     ENDELSE
     
  ENDFOR
  ;;It's more accurate not to do this in chunks.
  IF ~ KEYWORD_SET( poly_remove ) THEN powerspec[0] = (MEAN(data,/DOUBLE))^2
  
  ;;Convert to normalization convention
  ;;The calculated convention above corresponds to 
  ;; Sum_{i=0}^{N/2+1} PSD = 1/N sum_{i=0}^{N} data^2
  ;;Instead, we want (arbitrary decision, here)
  ;; \int_{0}_{f_c} PSD df = 1/T \int_{0}^{T} f^2
  ;;where f is the function describing data, f_c is the nyquist
  ;;frequency, and T is the total time used
  ;;
  ;;Translated into sums, this is equivalent to
  ;; {f_c \over N/2+1} \sum_{i=1}^{N/2+1} PSD = {1 \over N} \sum_{i=1}^{N} f^2
  ;;so we multiply the PSD by 2*(N/2+1)*delta_t
  ;; except we don't know delta_t, so we leave that to the user
  powerspec *= 2.0*(nchunko2+1)
  
  IF ARG_PRESENT(frequencies) THEN $
     frequencies = FINDGEN(nchunko2+1)/nchunk

  success=1b
  RETURN,powerspec
  
END
