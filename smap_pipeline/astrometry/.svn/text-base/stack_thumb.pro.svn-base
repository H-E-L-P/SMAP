;+
; STACK_THUMB, sigmap, errmap, hitmap, header, srcra, srcdec, $
;              thumbsize, sigthumb, errthumb, headthumb, $
;              HITTHUMB=hitthumb, GOODRAD=goodrad, GOODFRAC=goodfrac, $
;              SUBPIXFACT=subpixfact, NSRCS=nsrcs
;
; Create thumbnail image by stacking map on source catalog. Sources
; from (srcra, srcdec) are used in the stack if the number of good
; pixels in the map surrounding the source exceeds a threshhold. This
; is determined by the params goodrad and goodfrac: if the number of
; good pixels within goodrad of the source is greater than goodfrac *
; pi r^2, source is used. Can optionally subsample the map resolution.
;
; INPUTS:
;   sigmap:     signal map
;   errmap:     error map
;   hitmap:     hits map
;   header:     map header
;   srcra:      ra of stacking sources [deg]
;   srcdec:     dec of stacking sources [deg]
;   thumbsize:  size (in pixels) of output thumbnail images (for
;                both X and Y)
;
; OPTIONAL INPUTS:
;   goodrad:    radius for circle centered on source for determining
;               whether to use source [5 pixels] (input)
;   goodfrac:   threshhold for number of good pixels within source
;               [0.75] (input)
;   subpixfact: pixel sub-sampling factor (integer >= 1) (input);
;
; OUTPUTS:
;   sigthumb:   signal thumbnail image
;   errthumb:   error thumbnail image
;   headthumb:  header for thumbnail image
;
; OPTIONAL OUTPUTS
;   nsrcs:      number of sources used in stack (output)
;   hitthumb:   number of stacked pixels in each pixel (output)
;
; Created by gmarsden 2011-02-10
;
; CHANGELOG:
;   20110318,GM: add goodrad/goodfrac options
;   20110711,GM: change cenpix output to headthumb
;   20111122,GM: fix cut on hits map (now time) and add HITTHUMB keyword
;-


FUNCTION ST_GOODPIX, mask, xx, yy, goodrad, goodfrac, NGOOD=niii

  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN
  ;; find which pixels (xx,yy) meet criteria: goodfrac of pixels within
  ;; goodrad of (xx,yy) are in mask (where mask EQ 1) 

  ;; if goodrad LT 1, this is simply a test of the central pixel

  nx = N_ELEMENTS(mask[*,0])
  ny = N_ELEMENTS(mask[0,*])
  
  ;; find pixels that lie in mask region
  iii = WHERE(xx GE 0.0 AND xx LT nx AND $
              yy GE 0.0 AND yy LT ny, niii)
  
  ;; find where (remaining) pixels where mask EQ 1B
  IF niii GT 0 THEN BEGIN
     
     ;; if goodrad GE 1, loop over sources, else use WHERE
     IF goodrad GE 1 THEN BEGIN
        ;; mask of circle width goodrad centered on (0,0)
        minimask = BYTARR(nx, ny)
        minimask[WHERE(DIST(nx, ny) LE goodrad, nmini)] = 1B
        
        ;; flag for whether to keep source iii
        iiiflag = BYTARR(niii)
        
        ;; loop over sources
        FOR i=0L,niii-1 DO BEGIN
           
           ;; shift circular minimask to pixel, and with mask
           npix = TOTAL(mask AND SHIFT(minimask, xx[iii[i]], yy[iii[i]]))
           
           IF FLOAT(npix) / nmini GE goodfrac THEN iiiflag[i] = 1B
        ENDFOR

        ;; find sources that have fraction of central pixels in mask
        jjj = WHERE(iiiflag EQ 1B, njjj)
     ENDIF ELSE $
        jjj = WHERE(mask[xx[iii],yy[iii]] EQ 1B, njjj)
     
     IF njjj GT 0 THEN $
        iii = iii[jjj] $
     ELSE $
        iii = -1
     
     niii = njjj
  ENDIF
  
  RETURN, iii
  
END


PRO STACK_THUMB, sigmap, errmap, hitmap, header, srcra, srcdec, thumbsize, $
                 sigthumb, errthumb, headthumb, HITTHUMB=hitthumb, $
                 GOODRAD=goodrad, GOODFRAC=goodfrac, $
                 SUBPIXFACT=subpixfact, NSRCS=nsrcs

  COMPILE_OPT IDL2, STRICTARRSUBS
  ;; do subsampling?
  dosubpix = 0B
  IF KEYWORD_SET(subpixfact) && subpixfact GT 1 THEN dosubpix = 1B $
  ELSE subpixfact = 1
  
  IF NOT KEYWORD_SET(goodrad)  THEN goodrad = 5
  IF NOT KEYWORD_SET(goodfrac) THEN goodfrac = 0.75
  
  ;; extra pixel around border for shifting (when subsampling)
  nthumborig = thumbsize + 2
  nthumbsub  = nthumborig * subpixfact
  
  ;; accumulator maps
  sigthumb = FLTARR(nthumbsub, nthumbsub)
  errthumb = sigthumb
  hitthumb = LONARR(nthumbsub, nthumbsub)
  
  ;; projection
  ADXY, header, srcra, srcdec, srcxx, srcyy
  
  ;; map size
  s = SIZE(sigmap, /DIM)
  nx = s[0]
  ny = s[1]
  
  ;; mask where data are finite
  mskmap = BYTARR(nx, ny)
  mskmap[WHERE(hitmap GT 0.0 AND FINITE(sigmap) NE 0)] = 1B

 ;; find sources in good map region
  goodsrcind = ST_GOODPIX(mskmap, ROUND(srcxx), ROUND(srcyy), $
                          goodrad, goodfrac, NGOOD=ngood)
  
  ;; central pixel in original and sub-sampled maps
  cenpixorig = ROUND((nthumborig - 1) / 2.0)
  cenpixsub = ROUND((nthumbsub - 1) / 2.0)
  
  ;; pixel indices for thumbnail images
  mxx = REPLICATE(1, nthumbsub) ## FINDGEN(nthumbsub)
  myy = FINDGEN(nthumbsub) ## REPLICATE(1, nthumbsub)
  
  IF ngood GT 0 THEN BEGIN
     ;; loop over sources
     FOR i=0L,ngood-1 DO BEGIN
        
        ;; full-size single source maps
        subthumbsig = FLTARR(nthumborig, nthumborig)
        subthumberr = subthumbsig
        subthumbmask = BYTARR(nthumborig, nthumborig)
        
        ;; find box surrounding source 
        mxl = ROUND(srcxx[goodsrcind[i]] - cenpixorig)
        mxh = mxl + nthumborig - 1
        myl = ROUND(srcyy[goodsrcind[i]] - cenpixorig)
        myh = myl + nthumborig - 1
        
        ;; check for map edge
        mxoff = ABS(mxl < 0)
        mxl = mxl > 0
        mxh = mxh < (nx - 1)
        nmx = mxh - mxl + 1
        
        myoff = ABS(myl < 0)
        myl = myl > 0
        myh = myh < (ny - 1)
        nmy = myh - myl + 1
        
        ;; extract thumbnails
        HEXTRACT, sigmap, header, thmap, thhead, mxl, mxh, myl, myh, /SIL
        HEXTRACT, errmap, header, therr, thhead, mxl, mxh, myl, myh, /SIL
        HEXTRACT, mskmap, header, thmsk, thhead, mxl, mxh, myl, myh, /SIL
        
        ;; copy onto (empty) submap
        ;; (this step is necessary if we are near the map edge)
        subthumbsig[mxoff:mxoff+nmx-1,myoff:myoff+nmy-1] = thmap
        subthumberr[mxoff:mxoff+nmx-1,myoff:myoff+nmy-1] = therr
        subthumbmask[mxoff:mxoff+nmx-1,myoff:myoff+nmy-1] = thmsk

        ;; rebin thumbnail if subpix requested
        IF dosubpix THEN BEGIN
           ;; expand
           subthumbsig = REBIN(subthumbsig, nthumbsub, nthumbsub, /SAMP)
           subthumberr = REBIN(subthumberr, nthumbsub, nthumbsub, /SAMP)
           subthumbmask = REBIN(subthumbmask, nthumbsub, nthumbsub, /SAMP)
           
           ;; calculate amount to shift
           ;; offset (0.5 * subpixfact - 1) needed due to change
           ;; coordinates during rebinning
           origcoordx = srcxx[goodsrcind[i]] - (mxl - mxoff)
           subcoordx = origcoordx * subpixfact + 0.5 * (subpixfact - 1)
           nsubpixshiftx = ROUND(subcoordx) - cenpixsub
           
           origcoordy = srcyy[goodsrcind[i]] - (myl - myoff)
           subcoordy = origcoordy * subpixfact + 0.5 * (subpixfact - 1)
           nsubpixshifty = ROUND(subcoordy) - cenpixsub
           
           subthumbsig = SHIFT(subthumbsig, -nsubpixshiftx, -nsubpixshifty)
           subthumberr = SHIFT(subthumberr, -nsubpixshiftx, -nsubpixshifty)
           subthumbmask = SHIFT(subthumbmask, -nsubpixshiftx, $
                                -nsubpixshifty)
        ENDIF 
        
        ;; indices to good pixels in thumbnail
        mind = ST_GOODPIX(subthumbmask, mxx, myy, 0)
        
        ;; accumulate good pixels
        thiserr = subthumberr[mind]^2
        sigthumb[mind] += subthumbsig[mind] / thiserr
        errthumb[mind] += 1.0 / thiserr
        hitthumb[mind] += 1
        
     ENDFOR
  ENDIF

  ;; clip border (width subpixfact)
  sigthumb = sigthumb[subpixfact:(thumbsize+1)*subpixfact-1, $
                      subpixfact:(thumbsize+1)*subpixfact-1]
  errthumb = errthumb[subpixfact:(thumbsize+1)*subpixfact-1, $
                      subpixfact:(thumbsize+1)*subpixfact-1]
  hitthumb = hitthumb[subpixfact:(thumbsize+1)*subpixfact-1, $
                      subpixfact:(thumbsize+1)*subpixfact-1]
  
  ;; calculate signal and error thumbnails from accumulators
  sigthumb /= errthumb
  errthumb = 1.0 / SQRT(errthumb)
  
  ;; outputs
  nsrcs = ngood
  cenpix = ROUND((thumbsize * subpixfact - 1) / 2.0)
  
  ;; header for thumbnail
  headthumb = header
  SXADDPAR, headthumb, 'NAXIS1', thumbsize * subpixfact
  SXADDPAR, headthumb, 'NAXIS2', thumbsize * subpixfact
  SXADDPAR, headthumb, 'CRPIX1', cenpix + 1
  SXADDPAR, headthumb, 'CRPIX2', cenpix + 1
  ;; Note we -don't- put it at zero-zero, since that could
  ;; cause wrapping headaches
  SXADDPAR, headthumb, 'CRVAL1', 15.0
  SXADDPAR, headthumb, 'CRVAL2', 15.0
  SXADDPAR, headthumb, 'CD1_1', SXPAR(headthumb, 'CD1_1') / subpixfact
  SXADDPAR, headthumb, 'CD1_2', SXPAR(headthumb, 'CD1_2') / subpixfact
  SXADDPAR, headthumb, 'CD2_1', SXPAR(headthumb, 'CD2_1') / subpixfact
  SXADDPAR, headthumb, 'CD2_2', SXPAR(headthumb, 'CD2_2') / subpixfact
  
END
