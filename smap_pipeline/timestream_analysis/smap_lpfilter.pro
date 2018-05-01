;+
;Name:
;      SMAP_LPFILTER
;
;PURPOSE
;      Low pass filters a stitched together smap time stream using a
;      Butterworth filter with adjustable cutoff wavenumber.
;USAGE
;      smap_lpfilter, ubertimestream
;
;INPUTS/OUTPUTS
;      ubertimestream   - Stitched-together timestreams for a single bolometer
;
;RETURNS
;      The filtered timestream (OVERWRITES!)
;
;KEYWORDS
;      verbose     - Output informational messages
;      meanrestore - Make the mean before and after filtering the same
;
;OPTIONAL OUTPUTS
;      success     - 1b on success, 0b on failure
;      errmsg      - On failure, some explanation of what happened
;
;NOTES 
;
;
;MODIFICATION HISTORY
;   Author: Mike Zemcov, May 13, 2010
;-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Log base 2 (from: http://www.dfanning.com/tips/log_base2.html)
FUNCTION ALOG2, x
  
  RETURN, ALOG(x) / ALOG(2.d)

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO SMAP_LPFILTER, timestream, scanspeed, sampstep, $
                   MEANSUBTRACT=meansubtract,ANGSCALE=angscale,$
                   VERBOSE=verbose, SUCCESS=success, ERRMSG=errmsg

  ;; set up
  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg  = ''

  ;; assume a default for this parameter if necessary
  IF NOT(KEYWORD_SET(angscale)) THEN BEGIN
     angscale = 6.d
     IF KEYWORD_SET(verbose) THEN MESSAGE,$
        'Filter angular scale not set, setting to ' + $
        STRCOMPRESS(STRING(angscale,FORMAT='(2I)'),/REMOVE_ALL),/INFORMATIONAL
  ENDIF
  
  ;; find the length of the time stream
  nts = N_ELEMENTS( timestream )
  
  ;; Pad timestream with 0's out to a power of 2 for speed of the FFT
  ninput = 2 ^ CEIL( ALOG2( double(nts) ) )
  ;; this makes sure that we have enough points that post-filter ringing 
  ;; isn't going to get into our filtered time stream
  IF ninput - nts LT 1e3 THEN ninput = 2 ^ CEIL( ALOG2( double(nts) )+1.)

  ;;          'zero' pad the front and back of the array; this could
  ;;          be done in such a way as to ensure that the derivatives
  ;;          match but that's probably overkill
  input = dblarr(ninput)
  ;; make the middle be the real time stream
  input[(ninput - nts) / 2L: (ninput + nts) / 2L -1L] = timestream
  ;;              make the beginning of the pad match the values near
  ;;              the beginning of the time stream
  input[0:(ninput - nts) / 2L - 1L] = $
     REPLICATE(MEAN(timestream[0L:99L]),(ninput - nts) / 2L)
  ;;                                              and make the back of
  ;;                                              the pad match the
  ;;                                              values near the end
  ;;                                              of the time stream

  input[(ninput + nts) / 2L: ninput-1L] = $
     REPLICATE(MEAN(timestream[nts-100L:nts-1L]),$
               ROUND(FLOAT(ninput - nts) / 2.,/L64))

  ;; this does mean subtraction if for some reason we think that's necessary
  IF KEYWORD_SET(meansubtract) THEN inputm = MEAN(input) ELSE inputm = 0.0

  ; subtract off our set mean
  input = input - inputm

  ;; Get the FFT of the signal
  ft_sig = FFT( input, /DOUBLE )
  
  ;; Build the filter as a mask
  size_ft = N_ELEMENTS(ft_sig)
  filter = dcomplexarr( size_ft ) + COMPLEX(0., /DOUBLE)
  
  ;; Calculate the wavenumber for a scale of 6' -- set ft_sig
  ;;   to zero for all k < k_cr, being careful about the wrapping
  ;;   nature of the frequencies
  k_cr = long( (sampstep * scanspeed * size_ft) / (angscale * 60.d ) )
  
  IF (0) THEN BEGIN
     mydrop = COS(!pi * 0.5 * FINDGEN(k_cr) / k_cr)
     mydrop = EXP(-10. * (FINDGEN(k_cr) / (k_cr))^2)

     filter[0L: k_cr + 1L]                         = COMPLEX (1., /DOUBLE)
     filter[k_cr+2L:2 * k_cr + 1L]                 = COMPLEX(mydrop,/DOUBLE)
     filter[size_ft - 2*k_cr: size_ft - k_cr - 1L] = $
        COMPLEX(REVERSE(mydrop),/DOUBLE)
     filter[size_ft - k_cr: size_ft - 1L]          = COMPLEX (1., /DOUBLE)
  ENDIF

  ; construct the lp filter
  filter = COMPLEX( butterworth_long(size_ft,cutoff=k_cr,order=1), /DOUBLE)

  ;; Apply the filter and reverse transform to get filtered signal 
  output = REAL_PART( FFT( ft_sig * filter, /INVERSE, /DOUBLE ) )

  ;; Extract the timestream part of the 'output' array
  timestream = output[(ninput - nts) / 2L: (ninput + nts) / 2L -1L] + inputm  
  
  success = 1b
  RETURN
END
