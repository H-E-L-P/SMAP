;+
; NAME:
;       DETERMINE_SCAN_RATE
;
; CATEGORY:
;       SMAP pipeline
;
; PURPOSE:
;       To determine the scan rate of a set of observations from a set
;       of TODS in "/s
;
; USAGE:
;       scanrate = determine_scan_rate, tods [, SUCCESS=success, ERRMSG=errmsg ]
;
; INPUTS / OUTPUTS:
;       tods     -  A pointer array of SMAP style TODS -- see
;                  smap_read_and_filter. 
;       scanrate -  The output scan rate in "/s
;
; KEYWORDS:
;       alltods  - Calculate the scan rate as the mean of all
;                  individual TOD rates.  If not set, then the
;                  returned scan rate is that of the first TOD
;
; MODIFICATION HISTORY:
; Initial Author: Timothy Ellsworth Bowers, March 2010
;-
FUNCTION determine_scan_rate, tods, ALLTODS=alltods, VERBOSE=verbose, $
                              SUCCESS=success, ERRMSG=errmsg
  
  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg  = ''
  
  ntods = N_ELEMENTS(tods)
  IF ntods EQ 0 THEN BEGIN
     errmsg = "No TODs provided!"
     GOTO, err_handler
  ENDIF
  
  IF SIZE( tods, /TNAME ) NE 'POINTER' THEN BEGIN
     errmsg = "Was expecting an array of pointers to TODs, got: "+$
              SIZE(tods,/TNAME)
     GOTO, err_handler
  ENDIF
  
  IF ~ KEYWORD_SET(alltods) THEN ntods = 1
  
  ;; Array to hold individually-caluclated scan rates
  tod_rate = dblarr(ntods)
  
  ;; Loop over all TODS to get the best median scan rate
  FOR jj=0L, ntods - 1 DO BEGIN
     
     ;; Extract the sampling frequency from the TOD
     freq = (*tods[jj]).sampfreq
     
     ;; From this TOD, use GCIRC to determine the Great Circle
     ;; distance (in arcsec) between each set of RAs and decs.  NOTE:
     ;; GCIRC requires the RAs to be in HOURS -- divide by 15
     GCIRC, 1, (*tods[jj]).ra[*,0:*] / 15.d, (*tods[jj]).dec[*,0:*],$
            (*tods[jj]).ra[*,1:*] / 15.d, (*tods[jj]).dec[*,1:*], distances
     
     
     ;; RA & Dec shifts per time sample * sampling frequency 
     ;;    delta = distances * freq
     
     tod_rate[jj]  = MEDIAN( distances * freq,  /EVEN)
     
  ENDFOR
  
  ;; Median again to get the overall scan rate
  scanrate = MEAN( tod_rate, /NAN)
  
  success = 1b
  RETURN, scanrate
  
  err_handler:
  IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
  RETURN,0.d
  
END
