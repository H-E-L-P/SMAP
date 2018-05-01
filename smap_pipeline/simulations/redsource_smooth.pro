
;+
;NAME
; redsource_smooth
;USAGE
; redsource_smooth, basemap250, basemap350, basemap500, smoothmap250,$
;                   smoothmap350, smoothmap500, BRUTE=brute, VERBOSE=verbose
;PURPOSE
; Generates smoothed versions of the base maps
;-

PRO redsource_smooth, basemap250, basemap350, basemap500,$
                      smoothmap250, smoothmap350, smoothmap500,$
                      BRUTE=brute, VERBOSE=verbose, NOPEAK=nopeak
  COMPILE_OPT IDL2, HIDDEN

  smoothmap250 = basemap250
  IF KEYWORD_SET(verbose) THEN MESSAGE," Smoothing 250 micron map",/INF
  SMAP_REDSOURCE_SMOOTH, smoothmap250, BRUTE=brute, NOPEAK=nopeak
  smoothmap350 = basemap350
  IF KEYWORD_SET(verbose) THEN MESSAGE," Smoothing 350 micron map",/INF
  SMAP_REDSOURCE_SMOOTH, smoothmap350, BRUTE=brute, NOPEAK=nopeak
  smoothmap500 = basemap500
  IF KEYWORD_SET(verbose) THEN MESSAGE," Smoothing 500 micron map",/INF
  SMAP_REDSOURCE_SMOOTH, smoothmap500, BRUTE=brute, NOPEAK=nopeak
  
END
