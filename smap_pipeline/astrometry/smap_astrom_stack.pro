;+
; SMAP_ASTROM_STACK, mapfile, srcra, srcdec, thumbsize, $
;                    sigthumb, errthumb, params, shift, width, theta, nsrcs, $
;                    GOODRAD=goodrad, GOODFRAC=goodfrac, $
;                    SUBPIXFACT=subpixfact, SNR=snr, PARERRS=parerrs, $
;                    DOBGSUB=dobgsub, BGPARAMS=bgparams, BGRAD=bgrad
;
; Stack map on external source catalog, fit elliptical Gaussian and
; return parameters. Input is filename of smap-style fits image.
;
; NOTE: the output shift is in map coordinates, that is positive shift
; in X means west and in Y means north. These offsets should be
; *subtracted* from the pointing solution (in the tangent plane) to correct
; the shift.
;
; INPUTS:
;   mapfile:    path to map file (in smap fits format)
;   srcra:      ra of stacking sources [deg]
;   srcdec:     dec of stacking sources [deg]
;   thumbsize:  size (in pixels) of output thumbnail images (for both X and Y)
;
; OUTPUTS:
;   sigthumb:   signal thumbnail image
;   errthumb:   error thumbnail image
;   params:     7-element parameter array of fitted Gaussian in pixels
;               (offset, scale, sig1, sig2, x0, y0, theta)
;   shift:      2-element array of offsets from central pixel in arcsec
;   width:      2-element array of Gaussian widths in arcsec
;   theta:      angle of rotation of Gaussian ellipse [radians]
;   nsrcs:      number of sources used in stack
;
; KEYWORDS:
;   goodrad:    see stack_thumb.pro (input)
;   goodfrac:   see stack_thumb.pro (input)
;   subpixfact: pixel sub-sampling factor (integer >= 1) (input)
;   snr:        if set, perform fit on SNR map instead of signal map (input)
;   parerrs:    1-sigma errors on fitted parameters (output)
;   dobgsub:    perform background subtraction on stacked image before fitting 
;               (input)
;   bgrad:      radius at center of image to exclude in bg fit (input) [10 pix]
;   bgparams:   parameters of background fit [offset, slopex, slopey] (output)
;
; Created by gmarsden 2011-02-10
;
; CHANGELOG:
;   20110318,GM: add goodrad and goodfrac keywords
;   20110712,GM: change shift calculation to use header
;   20120403,GM: switch to MPFIT2DPEAK instead of GAUSS2DFIT
;                use error map in fit
;                add output keyword PARERRS for returning param errors
;   20120522,GM: extract relevant submap before stacking
;   20120523,GM: add options for background subtraction (linear only)
;-

PRO SMAP_ASTROM_STACK, mapfile, srcra, srcdec, thumbsize, $
                       sigthumb, errthumb, params, shift, width, $
                       theta, nsrcs, GOODRAD=goodrad, GOODFRAC=goodfrac, $
                       SUBPIXFACT=subpixfact, SNR=snr, PARERRS=parerrs, $
                       DOBGSUB=dobgsub, BGRAD=bgrad, BGPARAMS=bgparams

  COMPILE_OPT IDL2, STRICTARRSUBS
  ;; default exclusion radius for background fit
  IF NOT KEYWORD_SET(bgrad) THEN bgrad = 10
  bgorder = 1                   ; can option-ize this if we want

  ;; read map file
  RDFITS_STRUCT, mapfile, mapstruct, /SILENT

  sigmap = mapstruct.im1
  errmap = mapstruct.im2
  hitmap = mapstruct.im3
  header = mapstruct.hdr1
  
  ;; CROP IMAGE
  sz = SIZE(sigmap, /DIM)
  nx = sz[0]
  ny = sz[1]

  ;; indices to valid data
  goodind = WHERE(hitmap GT 0.0, ngood)
  xgood = goodind MOD nx
  ygood = goodind / nx

  x0 = MIN(xgood, MAX=x1)
  y0 = MIN(ygood, MAX=y1)

  IF NOT (x0 EQ 0 AND x1 EQ nx-1 AND y0 EQ 0 AND y1 EQ ny-1) THEN BEGIN
     origheader = header
     HEXTRACT, sigmap, header, x0, x1, y0, y1
     header = origheader
     HEXTRACT, errmap, header, x0, x1, y0, y1
     header = origheader
     HEXTRACT, hitmap, header, x0, x1, y0, y1
  ENDIF

  STACK_THUMB, sigmap, errmap, hitmap, header, srcra, srcdec, thumbsize, $
               sigthumb, errthumb, headthumb, $
               GOODRAD=goodrad, GOODFRAC=goodfrac, $
               SUBPIXFACT=subpixfact, NSRCS=nsrcs
    
  IF KEYWORD_SET(snr) THEN BEGIN
     fitmap = sigthumb / errthumb 
     fitsig = REPLICATE(1.0, thumbsize, thumbsize)
  ENDIF ELSE BEGIN
     fitmap = sigthumb
     fitsig = errthumb
  ENDELSE

  IF KEYWORD_SET(dobgsub) THEN BEGIN
     tx0 = SXPAR(headthumb, 'CRPIX1') - 1
     ty0 = SXPAR(headthumb, 'CRPIX2') - 1
     
     ;; get pixels in surrounding region of map
     bgfitind = WHERE(SHIFT(DIST(thumbsize, thumbsize), tx0, ty0) GT bgrad,$
                      nbgfitpix)

     ;; do linear fit
     xx = REBIN(FINDGEN(thumbsize), thumbsize, thumbsize)
     yy = REBIN(FINDGEN(1, thumbsize), thumbsize, thumbsize)

     bgpars = POLYFIT2D(xx[bgfitind], yy[bgfitind], fitmap[bgfitind], $
                        bgorder, ZERROR=fitsig[bgfitind])
     bgfitmap = bgpars[0] + bgpars[1] * yy + bgpars[2] * xx

     fitmap -= bgfitmap
  ENDIF

  fit = MPFIT2DPEAK(fitmap, params, /TILT, ERROR=fitsig, SIGMA=parerrs)

  ;; outputs        
  crv1 = SXPAR(headthumb, 'CRVAL1')
  crv2 = SXPAR(headthumb, 'CRVAL2')
  XYAD, headthumb, params[4], params[5], shiftra, shiftdec
  shift = [shiftra - crv1, shiftdec - crv2] * 3600.0

  ;; convert width (in pixels) to arcsec

  ;; throw warning if pixels are not square
  EXTAST, headthumb, astr
  pix1 = SQRT(astr.cd[0,0]^2 + astr.cd[0,1]^2)
  pix2 = SQRT(astr.cd[1,0]^2 + astr.cd[1,1]^2)
  
  IF pix1 NE pix2 THEN $
     MESSAGE, /INF, "Warning: map pixels aren't square: 'width' "+$
              "parameter is not meaningful"

  width = params[2:3] * pix1 * 3600.0
  theta = params[6]
    
END

