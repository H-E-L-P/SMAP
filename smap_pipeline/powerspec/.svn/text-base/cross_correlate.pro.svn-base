;+
;NAME
; cross_correlate
;PURPOSE
; Form the cross correlation of two signals using fourier transforms,
; returning the power spectrum
;USAGE
;  psd = cross_correlate(data1,data2,nchunk)
;INPUTS
;  data1            The first data stream
;  data2            The second data stream
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
;  The normalization convention is described below. You must multiply by
;  delta to get the desired normalization.
;OPTIONAL OUTPUTS
;  frequencies      The frequencies of the returned PSD in 1/delta
;                    units.
;KEYWORDS
;  poly_remove      Remove a low polynomial from the data before
;                    tranfsorming.
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
;                    fraction have been removed in the mean/poly
;                    subtraction.  Def: 0.1 (10%)
;NORMALIZATION
;  This uses the same normalization convention as auto_correlate.
;  Specifically, if f corresponds to data1 and g to data2 (assumed
;  real), and the returned variable is CPSD
;    \int_{0}^{f_c} CPSD * delta df = 1/T \int_{0}^{T} f*g dt
;  So, if f and g have units of mJy, then CSD * delta has units
;   of mJy^2 Hz^-1.  This is not required to be positive, so taking
;   the square root is not a good idea.  
;  Don't forget to multiply by delta!
;NOTES
;  If the window functions are turned on, the segments are overlapped
;   by half their length.  This presumes the S/N of your output PSD
;   is limited by your data size.  If you have a lot of data, you
;   might want to set /nooverlap.
;  It is assumed that data1 and data2 have the same sampling interval.
;MODIFICATINO HISTORY
; Author: Alex Conley, April 1 2009.
;-

FUNCTION cross_correlate, data1, data2, nchunk, POLY_REMOVE=poly_remove,$
                          POLY_ORDER=poly_order, USEHANNING=usehanning,$
                          USEHAMMING=usehamming, FREQUENCIES=frequencies,$
                          SUCCESS=success, ERRMSG=errmsg,$
                          NOOVERLAP=nooverlap, VERBOSE=verbose,$
                          DISCARD_THRESH=discard_thresh

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
     MESSAGE,"WARNING in cross_correlate: chunktype is not a power of 2",/INF
  nchunko2 = LONG(nchunk) / 2

  IF N_ELEMENTS(data1) NE N_ELEMENTS(data2) THEN BEGIN
     errmsg = "ERROR in cross_correlate: data segments not same size"
     RETURN,!VALUES.F_NAN
  ENDIF

  invnchunk = 1.0 / DOUBLE(nchunk)

  IF KEYWORD_SET(usehanning) AND KEYWORD_SET(usehamming) THEN BEGIN
     errmsg = "ERROR in cross_correlate: choose hanning or hamming, not both"
     RETURN, !VALUES.F_NAN
  ENDIF
  IF KEYWORD_SET( poly_remove ) THEN BEGIN
     IF N_ELEMENTS( poly_order ) EQ 0 THEN poly_order = 5
     IF poly_order LT 1 THEN BEGIN
        errmsg="ERROR in cross_correlate: poly order < 1"
        RETURN, !VALUES.F_NAN
     ENDIF
     IF poly_order GT 6 THEN BEGIN
        errmsg = "ERROR in cross_correlate: poly order > 6"
        RETURN, !VALUES.F_NAN
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
  
  ndata = N_ELEMENTS(data1)
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
  
  FOR i=0, numchunks-1 DO BEGIN
     IF overlap THEN BEGIN
        idx_lower = i*nchunko2
        idx_upper = idx_lower + nchunk - 1
     ENDIF ELSE BEGIN
        idx_lower = i*nchunk
        idx_upper = idx_lower + nchunk - 1
     ENDELSE
     
     curr_sample1 = data1[idx_lower:idx_upper]
     curr_sample2 = data2[idx_lower:idx_upper]
     
     IF KEYWORD_SET( poly_remove ) THEN BEGIN
        coeffs1 = ROBUST_POLY_FIT(FINDGEN(nchunk),curr_sample1,poly_order,yfit)
        curr_sample1 -= yfit
        coeffs2 = ROBUST_POLY_FIT(FINDGEN(nchunk),curr_sample2,poly_order,yfit)
        curr_sample2 -= yfit
     ENDIF 
     
     IF KEYWORD_SET(usehamming) OR KEYWORD_SET(usehanning) THEN BEGIN
        curr_sample1 *= wind
        curr_sample2 *= wind
     ENDIF
     
     ft1 = FFT(curr_sample1)
     ft2 = FFT(curr_sample2)
     
     ;;We take advantage of the symmetry of the FFT of real data to
     ;; throw away half of the FFTs here.
     ;; For i=1:N/2-1 the expression for the cross-PSD is
     ;;   ft1(f) ft2*(f) + ft1(-f) ft2*(-f)  (where * is conjugation).
     ;; Since ft1(-f) = ft1*(f) for real data, this transforms to
     ;;   ft1(f) ft2*(f) + ft1*(f) ft2(f) which is therefore real
     ;; and furthermore is equal to 2*REAL( ft1(f) ft2*(f) )
     ;;Note that it does -not- have to be non-negative!
     IF KEYWORD_SET(poly_remove) THEN $
        powerspec[0] += 0.5 * prefac*REAL_PART(ft1[0])*REAL_PART(ft2[0])

     IF nchunk MOD 2 EQ 0 THEN BEGIN
        ;;Even length
        powerspec[1:nchunko2-1] += prefac*REAL_PART( ft1[1:nchunko2-1] * $
                                                     CONJ(ft2[1:nchunko2-1]) )
        powerspec[nchunko2] += 0.5 * prefac*REAL_PART(ft1[nchunko2]) * $
                               REAL_PART(ft2[nchunko2])
     ENDIF ELSE BEGIN
        ;;Odd length
        powerspec[1:nchunko2] += prefac*REAL_PART(ft1[1:nchunko2] * $
                                                  CONJ(ft2[1:nchunko2]) )
     ENDELSE
     
  ENDFOR
  ;;It's more accurate not to do the mean in chunks
  IF ~ KEYWORD_SET( poly_remove ) THEN powerspec[0] = $
     MEAN(data1,/DOUBLE) * MEAN(data2,/DOUBLE)
  
  ;;Convert to normalization convention
  ;; See comments in auto_correlate
  powerspec *= 2.0*(nchunko2+1)
  
  IF ARG_PRESENT(frequencies) THEN $
     frequencies = FINDGEN(nchunko2+1)/nchunk
  
  success = 1b
  RETURN,powerspec
  
END
