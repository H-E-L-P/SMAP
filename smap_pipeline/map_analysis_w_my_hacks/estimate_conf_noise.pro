;+
;NAME
; estimate_conf_noise
;PURPOSE
; Estimates confusion and instrumental noise using Louis's
; technique for a set of SMAP maps
;USAGE
; estimate_conf_noise, mapnames, band
;INPUTS
; mapnames    Names of maps, fed to read_smap_fitsmap
; band        The band to use
;OPTIONAL INPUTS
; dir         Directory to look for maps in
; maxhits     Maximum number of hits to allow (def: 5000)
; minhits     Minimum number of hits to require -- can be a vector
;              specifying a different value for each map (def: 5)
; expbin      Size of bins in exposure hits (def: 1)
; sigclip     Sigma clipping value for variance (def: 100)
; minpix      Minimum number of pixels to comput variance from --
;              i.e., if there are fewer pixels in the bin than this,
;              don't use it (def: 15)
;KEYWORDS
; weightvar   Try to weight the variances by how many measures went
;              into each value (experimental)
; robust      Use robust_linefit instead of linfit.  Not available
;              when weightvar is used
; noplot      Don't plot results
;OPTIONAL OUTPUTS
; instnoise   Returns instrument noise in mJy n^1/2 / beam
; confnoise   Returns confusion noise in mJy/beam
;NOTES
; Also understands plotting keywords.
; Robust and sigma clipping turn out to be a bad idea, so you
; probably shouldn't use them.
;AUTHOR
; Alex Conley, adapted from code by Louis Levenson
;-

PRO estimate_conf_noise, mapnames, band, DIR=dir, MAXHITS=maxhits, $
                         MINHITS=minhits, EXPBIN=expbin, SIGCLIP=sigclip,$
                         MINPIX=minpix, WEIGHTVAR=weightvar, $
                         ROBUST=robust, _EXTRA = _ex, INSTNOISE=instnoise,$
                         CONFNOISE=confnoise, NOPLOT=noplot
  COMPILE_OPT IDL2, STRICTARRSUBS
  
  IF N_ELEMENTS(maxhits) EQ 0 THEN maxhits = 5000
  IF N_ELEMENTS(expbin) EQ 0 THEN expbin = 1
  IF N_ELEMENTS(sigclip) EQ 0 THEN sigclip = 100.0
  IF N_ELEMENTS(minpix) EQ 0 THEN minpix = 15

  nmaps = N_ELEMENTS(mapnames)

  IF N_ELEMENTS( minhits ) EQ 0 THEN iminhits = REPLICATE(5,nmaps) $
     ELSE IF N_ELEMENTS(minhits) EQ 1 THEN iminhits=REPLICATE(minhits,nmaps) $
     ELSE IF N_ELEMENTS(minhits) EQ nmaps THEN iminhits = minhits ELSE $
        MESSAGE,"Minhits unexpected length"

  ;;Prepare the data
  FOR i=0,nmaps-1 DO BEGIN
     MESSAGE,"Processing: "+mapnames[i],/INF
     map = read_smap_fitsmap( mapnames[i], band, DIR=dir, /SILENT, $
                              SUCCESS=succ, ERRMSG=errmsg )
     IF succ EQ 0 THEN MESSAGE,errmsg
     
     wkeep = WHERE( map.exposure GT iminhits[i] AND $
                    map.exposure LT maxhits AND $
                    FINITE(map.image), nkeep )
     IF nkeep LT 100 THEN MESSAGE,"Insufficient pixels to keep!"
     
     exp = map.exposure[wkeep]
     im  = map.image[wkeep]
     hpix = HISTOGRAM( exp, BIN=expbin, REV=R )
     nhits     = DBLARR(N_ELEMENTS(hpix))
     variances = DBLARR(N_ELEMENTS(hpix))
     IF KEYWORD_SET(weightvar) THEN weights = DBLARR(N_ELEMENTS(hpix))
     FOR j=0, N_ELEMENTS(hpix)-1 DO IF hpix[j] GT minpix THEN BEGIN
        idx = R[ R[j]:R[j+1]-1 ]
        
        ;;Get variance using outlier rejection
        RESISTANT_MEAN, im[idx], sigclip, mnval, sig_mean, nrej, /DOUBLE,$
                        GOODVEC=goodvals
        variances[j] = sig_mean * sig_mean * (hpix[j]-nrej)
        nhits[j] = MEAN( exp[idx[goodvals]] )
        IF KEYWORD_SET( weightvar ) THEN weights[j] = N_ELEMENTS(goodvals)
     ENDIF
     
     wkeep = WHERE( variances GT 0.0, nkeep )
     IF nkeep EQ 0 THEN MESSAGE,"All pixels rejected!"
     variances = variances[wkeep]
     nhits     = nhits[wkeep]
     IF KEYWORD_SET(weightvar) THEN weights=weights[wkeep]

     ;;Append
     IF N_ELEMENTS( all_var ) EQ 0 THEN all_var = TEMPORARY(variances) ELSE $
        all_var = [all_var, TEMPORARY(variances)]
     IF N_ELEMENTS( all_hits ) EQ 0 THEN all_hits = TEMPORARY(nhits) ELSE $
        all_hits = [all_hits, TEMPORARY(nhits)]
     IF KEYWORD_SET( weightvar ) THEN BEGIN
        IF N_ELEMENTS( all_weights ) EQ 0 THEN $
           all_weights = TEMPORARY(weights) ELSE $
              all_weights = [all_weights, TEMPORARY(weights)]
     ENDIF
  ENDFOR

  ;;And fit
  IF ~ KEYWORD_SET(weightvar) THEN BEGIN
     ;;Unweighted fit
     IF KEYWORD_SET( robust ) THEN BEGIN
        fitpar = ROBUST_LINEFIT( 1.0/all_hits, all_var)
     ENDIF ELSE $
        fitpar = LINFIT( 1.0/all_hits, all_var )
  ENDIF ELSE BEGIN
     ;;The variance of the variance estimate (!) is proportional to
     ;;1/n, where n is the number of elements in the sample
     fitpar = LINFIT(1.0/all_hits, all_var, /DOUBLE, $
                     MEASURE=1.0/SQRT(all_weights))
  ENDELSE
  instnoise = SQRT(fitpar[1])*1000
  confnoise = SQRT(fitpar[0])*1000
  PRINT,"The combined instrument noise is: ",instnoise,$
        ' mJy/beam /sqrt(samples)'
  PRINT,"The confusion noise is: ",confnoise,$
        ' mJy/beam'

  IF ~ KEYWORD_SET( noplot ) THEN BEGIN
     PLOT,1.0/all_hits,all_var,PSYM=1,_EXTRA=_ex,$
          YTITLE='Variance [(Jy/beam)^2]',XTITLE='1/hits',$
          XTHICK=3,YTHICK=3
     OPLOT,!X.CRANGE,fitpar[0]+fitpar[1]*!X.CRANGE,COLOR=color,$
           THICK=3,_EXTRA=_ex
  ENDIF

END
