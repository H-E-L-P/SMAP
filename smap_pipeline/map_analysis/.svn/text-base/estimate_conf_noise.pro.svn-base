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
; maexp       Maximum exposure to allow (def: 100)
; minexp      Minimum exposure to require -- can be a vector
;              specifying a different value for each map (def: 0.1)
; expbin      Size of bins in seconds (def: 0.1)
; sigclip     Sigma clipping value for variance (def: don't use)
; minpix      Minimum number of pixels to comput variance from --
;              i.e., if there are fewer pixels in the bin than this,
;              don't use it (def: 25)
; psfile      Direct the plot to this file rather than to the screen
;KEYWORDS
; weightvar   Try to weight the variances by how many measures went
;              into each value (experimental)
; robust      Use robust_linefit instead of linfit.  Not available
;              when weightvar is used
; noplot      Don't plot results
; usemask     Respect the map mask info, if present
;OPTIONAL OUTPUTS
; instnoise   Returns instrument noise in mJy s^{1/2} / beam
; confnoise   Returns confusion noise in mJy/beam
;NOTES
; Robust and sigma clipping turn out to be a bad idea, so you
; probably shouldn't use them.
;AUTHOR
; Alex Conley, adapted from code by Louis Levenson
;-

PRO estimate_conf_noise, mapnames, band, DIR=dir, MAXEXP=maxexp, $
                         MINEXP=minexp, EXPBIN=expbin, SIGCLIP=sigclip,$
                         MINPIX=minpix, WEIGHTVAR=weightvar, $
                         ROBUST=robust, _EXTRA = _ex, INSTNOISE=instnoise,$
                         CONFNOISE=confnoise, NOPLOT=noplot, USEMASK=usemask,$
                         VERBOSE=verbose, PLOTSYM=plotsym,$
                         COLORS=colors, PLOTVAR=p, SYM_SIZE=sym_size,$
                         LAMBDA=lambda, PSFILE=psfile, FILENAMEIN=filenamein
  COMPILE_OPT IDL2, STRICTARRSUBS
  
  IF N_ELEMENTS(expbin) EQ 0 THEN expbin = 0.1
  IF N_ELEMENTS(sigclip) EQ 0 THEN sigclip = !VALUES.F_NAN
  IF N_ELEMENTS(minpix) EQ 0 THEN minpix = 25
  IF N_ELEMENTS(sym_size) EQ 0 THEN sym_size=1.5
  nmaps = N_ELEMENTS(mapnames)

  IF N_ELEMENTS(plotsym) NE 0 && N_ELEMENTS(plotsym) LT nmaps THEN $
     MESSAGE,"Plotsym -- if present -- must be same size as number of maps"

  IF N_ELEMENTS( minexp ) EQ 0 THEN iminexp = REPLICATE(0.1,nmaps) $
     ELSE IF N_ELEMENTS(minexp) EQ 1 THEN iminexp=REPLICATE(minexp,nmaps) $
     ELSE IF N_ELEMENTS(minexp) GE nmaps THEN iminexp = minexp ELSE $
        MESSAGE,"Minexp unexpected length"

  IF N_ELEMENTS(maxexp) EQ 0 THEN imaxexp = REPLICATE(100.0,nmaps) $
     ELSE IF N_ELEMENTS(maxexp) EQ 1 THEN imaxexp = REPLICATE(maxexp,nmaps) $
     ELSE IF N_ELEMENTS(maxexp) GE nmaps THEN imaxexp = maxexp ELSE $
        MESSAGE,"Maxexp unexpected length"


  ;;Prepare the data
  idxarr = LONARR(nmaps) ;;these two keep track of which exp belong
  idxtoparr = LONARR(nmaps) ;;to which map
  FOR i=0,nmaps-1 DO BEGIN
     MESSAGE,"Processing: "+mapnames[i],/INF
     map = read_smap_fitsmap( mapnames[i], band, DIR=dir, /SILENT, $
                              SUCCESS=succ, ERRMSG=errmsg, /ALLOW,$
                              LAMBDA=lambda, FILENAMEIN=filenamein )
     IF succ EQ 0 THEN MESSAGE,errmsg
     
     IF KEYWORD_SET( usemask ) && map.has_mask THEN BEGIN
        wkeep = WHERE( map.exposure GT iminexp[i] AND $
                       map.exposure LT imaxexp[i] AND map.mask EQ 0 AND $
                       FINITE(map.image), nkeep )
     ENDIF ELSE BEGIN
        wkeep = WHERE( map.exposure GT iminexp[i] AND $
                       map.exposure LT imaxexp[i] AND $
                       FINITE(map.image), nkeep )
     ENDELSE
     IF nkeep LT 100 THEN MESSAGE,"Insufficient pixels to keep!"
     
     exp       = map.exposure[wkeep]
     im        = map.image[wkeep]
     hpix      = HISTOGRAM( exp, BIN=expbin, REV=R )
     nexp      = DBLARR(N_ELEMENTS(hpix))
     variances = DBLARR(N_ELEMENTS(hpix))

     IF KEYWORD_SET(verbose) THEN BEGIN
        minval = MIN(exp,MAX=maxval)
        MESSAGE,STRING(minval,maxval,$
                       FORMAT='(3X,"Exp range: ",F0.2," to ",F0.2)'),/INF
     ENDIF
     
     IF KEYWORD_SET(weightvar) THEN weights = DBLARR(N_ELEMENTS(hpix))
     FOR j=0, N_ELEMENTS(hpix)-1 DO IF hpix[j] GT minpix THEN BEGIN
        idx = R[ R[j]:R[j+1]-1 ]
        
        ;;Get variance using outlier rejection
        IF FINITE(sigclip) THEN BEGIN
           RESISTANT_MEAN, im[idx], sigclip, mnval, sig_mean, nrej, /DOUBLE,$
                           GOODVEC=goodvals
           variances[j] = sig_mean * sig_mean * (hpix[j]-nrej)
           nexp[j] = MEAN( exp[idx[goodvals]] )
           IF KEYWORD_SET( weightvar ) THEN weights[j] = N_ELEMENTS(goodvals)
        ENDIF ELSE BEGIN
           variances[j] = STDEV(im[idx])^2
           nexp[j]      = MEAN( exp[idx] )
           IF KEYWORD_SET( weightvar ) THEN weights[j] = N_ELEMENTS(idx)
        ENDELSE

     ENDIF
     
     wkeep = WHERE( variances GT 0.0, nkeep )
     IF nkeep EQ 0 THEN MESSAGE,"All pixels rejected!"
     variances = variances[wkeep]
     nexp     = nexp[wkeep]
     IF KEYWORD_SET(weightvar) THEN weights=weights[wkeep]

     ;;Append
     idxarr[i] = N_ELEMENTS(all_exp)
     idxtoparr[i] = idxarr[i] + nkeep
     IF N_ELEMENTS( all_var ) EQ 0 THEN all_var = TEMPORARY(variances) ELSE $
        all_var = [all_var, TEMPORARY(variances)]
     IF N_ELEMENTS( all_exp ) EQ 0 THEN all_exp = TEMPORARY(nexp) ELSE $
        all_exp = [all_exp, TEMPORARY(nexp)]
     IF KEYWORD_SET( weightvar ) THEN BEGIN
        IF N_ELEMENTS( all_weights ) EQ 0 THEN $
           all_weights = TEMPORARY(weights) ELSE $
              all_weights = [all_weights, TEMPORARY(weights)]
     ENDIF

  ENDFOR ;;loop over maps

  ;;And fit
  IF ~ KEYWORD_SET(weightvar) THEN BEGIN
     all_weights = REPLICATE(1.0,N_ELEMENTS(all_var))
     ;;Unweighted fit
     IF KEYWORD_SET( robust ) THEN BEGIN
        fitpar = ROBUST_LINEFIT( 1.0/all_exp, all_var)
     ENDIF ELSE $
        fitpar = LINFIT( 1.0/all_exp, all_var )
  ENDIF ELSE BEGIN
     ;;The variance of the variance estimate (!) is proportional to
     ;;1/n, where n is the number of elements in the sample
     fitpar = LINFIT(1.0/all_exp, all_var, /DOUBLE, $
                     MEASURE=1.0/SQRT(all_weights))
  ENDELSE
  instnoise = SQRT(fitpar[1])*1000
  confnoise = SQRT(fitpar[0])*1000
  PRINT,"The map RMS is: ",1000.*SQRT(TOTAL(all_var/all_weights)/$
                                TOTAL(1.0/all_weights)),$
        " mJy/beam."
  PRINT,"The combined instrument noise is: ",instnoise,$
        ' mJy/beam /sqrt(seconds)'
  PRINT,"The confusion noise is: ",confnoise,$
        ' mJy/beam'

  IF ~ KEYWORD_SET( noplot ) THEN BEGIN
     xvals = 1.0/all_exp
     xmin = MIN(xvals,MAX=xmax,/NAN)
     ymin = MIN(all_var, MAX=ymax, /NAN)
     xtitle='1/seconds'

     p = PLOT(xvals,all_var,XRANGE=[0.9*xmin,1.1*xmax],$
              YRANGE=[0.9*ymin,1.1*ymax],$
              XSTYLE=1,YSTYLE=1,YTITLE='Variance [(Jy/beam)^2]',$
              XTITLE=xtitle, XTHICK=3,YTHICK=3, /NODATA,$
              COLOR='black',POSITION=[0.18,0.1,0.95,0.90],TITLE=band,$
              BUFFER=KEYWORD_SET(psfile), _EXTRA=_ex)

     ;;plot indiv data points
     FOR i=0,nmaps-1 DO BEGIN
        cxvals = xvals[idxarr[i]:idxtoparr[i]-1]
        cyvals = all_var[idxarr[i]:idxtoparr[i]-1]
        wfin = WHERE( FINITE(cxvals) AND FINITE(cyvals), nfin )
        IF nfin EQ 0 THEN CONTINUE

        IF N_ELEMENTS(colors) EQ 0 THEN cclr = 'blue' ELSE $
           cclr = colors[i]
        IF N_ELEMENTS(plotsym) EQ 0 THEN cpsym = 'dot' ELSE $
           cpsym = plotsym[i]
        st = PLOT(cxvals[wfin],cyvals[wfin],COLOR=cclr,$
                  OVERPLOT=p,SYMBOL=cpsym,SYM_SIZE=sym_size,LINESTYLE='',$
                  /SYM_FILLED)
     ENDFOR

     ;;overplot fit
     linplot = (xmax-xmin)*FINDGEN(1000)/999.0+xmin
     st = PLOT(linplot,fitpar[0]+fitpar[1]*linplot,COLOR=color,$
               THICK=3,OVERPLOT=p)
     IF N_ELEMENTS(psfile) NE 0 THEN $
        p.Save,psfile

  ENDIF

END
