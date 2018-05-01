;+
; NAME
;  smap_simplephot
; PURPOSE
;  Simple photometry for a fixed list of positions
; USAGE
;  fluxes = smap_simplephot( map, ra, dec, FWHM=, NPIX=, ERR=, /USEMASK )
; INPUTS
;  map         Map to perform photometry on
;  ra          RA positions of sources
;  dec         DEC positions of sources
; KEYWORDS
;  usemask     Check for non-zero mask information, excluding any
;               pixels where the mask is non-present
;  xypos       Interpret ra/dec as x/y position directly
; OUTPUTS
;  fluxes      Measured flux densities, in whatever units the map
;               is in (probably Jy)
; OPTIONAL INPUTS
;  bgrnd       Background map to subtract off before doing photometry
;  fwhm        FWHM of beam in arcsec.  Otherwise taken from map band info
;  npix        Number of pixels to use in fit around position;
;               basically, the npix by npix box around the position
;               is used. (def: 3)
; OPTIONAL OUTPUTS
;  err         Errors on fluxes.  Ignored if map doesn't have error information
; MODIFICATION HISTORY
;  Author: Alex Conley, October 26, 2011
;-

FUNCTION smap_simplephot, map, ra, dec, FWHM=fwhm, NPIX=npix, ERR=err,$
                          XYPOS=xypos, USEMASK=usemask, BGRND=bgrnd

  COMPILE_OPT IDL2, STRICTARRSUBS

  ;;input check and defaults
  IF N_ELEMENTS(map) EQ 0 THEN MESSAGE,"No map provided"
  IF SIZE(map,/TNAME) NE 'STRUCT' THEN MESSAGE,"Input map not expected type"
  nra = N_ELEMENTS(ra)
  IF nra EQ 0 THEN MESSAGE,"No ra values provided"
  IF nra NE N_ELEMENTS(dec) THEN $
     MESSAGE,"Number of ras don't match number of decs"
  IF N_ELEMENTS(fwhm) EQ 0 THEN BEGIN
     CASE map.names OF
        'PSW' : i_fwhm = 17.8
        'PMW' : i_fwhm = 24.0
        'PLW' : i_fwhm = 35.3
        ELSE : MESSAGE,"Unknown band: "+map.names
     ENDCASE
  ENDIF ELSE i_fwhm = fwhm
  IF i_fwhm LE 0 THEN MESSAGE,"Invalid (non-positive) fwhm"
  IF i_fwhm / map.pixscale GT MIN([ map.xsize, map.ysize] ) THEN $
     MESSAGE,"Specified FWHM is larger than map!"
  IF N_ELEMENTS(npix) EQ 0 THEN i_npix = 3 ELSE i_npix = npix
  IF i_npix LE 0 OR i_npix GT MIN([map.xsize, map.ysize]) THEN $
     MESSAGE,"Invalid npix"
  IF N_ELEMENTS(bgrnd) NE 0 THEN BEGIN
     has_bg = 1b
     szbg = SIZE(bgrnd)
     IF szbg[0] NE 2 THEN MESSAGE,"Input background not 2D"
     IF szbg[1] NE map.xsize THEN MESSAGE,"BG doesn't match map xsize"
     IF szbg[2] NE map.ysize THEN MESSAGE,"BG doesn't match map xsize"
  ENDIF ELSE has_bg = 0b

  IF KEYWORD_SET( xypos ) THEN BEGIN
     x = ra
     y = dec
  ENDIF ELSE BEGIN
     ;;Convert ra/dec to positions
     AD2XY, ra, dec, map.astrometry, x, y
  ENDELSE

  minx = MIN(x,MAX=maxx)
  miny = MIN(y,MAX=maxy)
  hfsz = i_npix/2
  IF minx LT hfsz OR maxx GT map.xsize-hfsz-1 THEN BEGIN
     fmt = '("Invalid x input position -- idx ",I0,'+$
           '" too close to edge at position ",F0.2," allowed ",I0)'
     IF minx LT hfsz THEN BEGIN
        st = MIN(x, wmin)
        MESSAGE, STRING(wmin, minx, hfsz, FORMAT=fmt)
     ENDIF ELSE BEGIN
        st = MAX(x, wmax)
        MESSAGE, STRING(wmax, maxx, map.xsize-hfsz-1, FORMAT=fmt)
     ENDELSE
  ENDIF
  IF miny LT hfsz OR maxy GT map.ysize-hfsz-1 THEN BEGIN
     fmt = '("Invalid y input position -- idx ",I0,'+$
           '" too close to edge at position ",F0.2," allowed ",I0)'
     IF miny LT hfsz THEN BEGIN
        st = MIN(y, wmin)
        MESSAGE, STRING(wmin, miny, hfsz, FORMAT=fmt)
     ENDIF ELSE BEGIN
        st = MAX(y, wmax)
        MESSAGE, STRING(wmax, maxy, map.ysize-hfsz-1, FORMAT=fmt)
     ENDELSE
  ENDIF

  ;;Get beam we will use.  Note this will be oversampled,
  ;; and we will integrate by hand to get what we want for each
  ;; object
  oversample = 7 ;; amount we will oversample
  bmnpix = i_npix + 4
  IF bmnpix MOD 2 EQ 0 THEN bmnpix += 1
  genpix = bmnpix * oversample
  bmpixscale = map.pixscale/oversample
  bm = GET_SPIRE_BEAM('PSW',bmpixscale,genpix,genpix,$
                      FWHM=i_fwhm, OVERSAMP=1, /SILENT)
  bmcent = genpix/2
  ngrabhf = (i_npix*oversample)/2

  IF KEYWORD_SET(usemask) && map.has_mask THEN use_mask_info = 1b ELSE $
     use_mask_info = 0b

  ;;Main phot loop
  fluxarr = FLTARR(nra)
  IF map.has_error THEN err = FLTARR(nra)
  FOR i=0,nra-1 DO BEGIN
     ;;Extract subpixel array we will actually fit
     minxidx = ROUND(x[i])-hfsz
     maxxidx = minxidx + i_npix - 1
     minyidx = ROUND(y[i]-hfsz)
     maxyidx = minyidx + i_npix - 1
     subarr  = map.image[minxidx:maxxidx,minyidx:maxyidx]

     IF has_bg THEN $
        subarr -= bgrnd[minxidx:maxxidx,minyidx:maxyidx]

     IF map.has_error THEN BEGIN
        vararr = map.error[minxidx:maxxidx,minyidx:maxyidx]^2
        has_error = 1b
     ENDIF ELSE has_error = 0b

     IF use_mask_info THEN BEGIN
        marr = map.mask[minxidx:maxxidx,minyidx:maxyidx]
        IF has_error THEN BEGIN
           wgoodsub = WHERE( FINITE(subarr) AND FINITE(vararr) AND $
                             marr EQ 0, ngoodsub, $
                             NCOMPLEMENT=nbadsub )
        ENDIF ELSE BEGIN
           wgoodsub = WHERE( FINITE(subarr) AND marr EQ 0, ngoodsub, $
                             NCOMPLEMENT=nbadsub )
        ENDELSE
     ENDIF ELSE BEGIN
        IF has_error THEN BEGIN
           wgoodsub = WHERE( FINITE(subarr) AND FINITE(vararr), $
                             ngoodsub, NCOMPLEMENT=nbadsub )
        ENDIF ELSE BEGIN
           wgoodsub = WHERE( FINITE(subarr), ngoodsub, NCOMPLEMENT=nbadsub )
        ENDELSE
     ENDELSE
     IF ngoodsub EQ 0 THEN BEGIN
        fluxarr[i] = !VALUES.F_NAN
        CONTINUE
     ENDIF
     IF nbadsub NE 0 THEN BEGIN
        subarr = subarr[wgoodsub]
        IF has_error THEN vararr = vararr[wgoodsub]
     ENDIF

     ;;Now things get tricky -- we want to grab and sum the
     ;; PSF, taking into account that there may be an offset 
     ;; (the source doesn't have to be in an image pixel centre)
     xoffset = 0.5 - (x[i] - FLOOR(x[i])) ;;in map pixels
     yoffset = 0.5 - (y[i] - FLOOR(y[i])) ;;in map pixels
     ;;convert from flt map pixels to bm pixels
     xoffset = ROUND(oversample*xoffset)
     yoffset = ROUND(oversample*yoffset)

     ;;compute what part of beam to grab, including offset
     xcpos = bmcent - xoffset
     ycpos = bmcent - yoffset
     minxidx2 = xcpos - ngrabhf
     maxxidx2 = xcpos + ngrabhf
     minyidx2 = ycpos - ngrabhf
     maxyidx2 = ycpos + ngrabhf
     bmpiece = bm[ minxidx2:maxxidx2, minyidx2:maxyidx2 ]

     ;;rebin to map pixel scale
     bmpiece = REBIN( bmpiece, i_npix, i_npix )

     IF nbadsub NE 0 THEN bmpiece = bmpiece[wgoodsub]

     ;;Now, compute scaling
     IF has_error THEN BEGIN
        varterm = 1.0/TOTAL(bmpiece^2/vararr)
        fluxarr[i] = TOTAL( subarr*bmpiece / vararr ) * varterm
        err[i] = SQRT( varterm )
     ENDIF ELSE BEGIN
        ;;Even scaling, no errors
        fluxarr[i] = TOTAL( subarr*bmpiece )/ TOTAL( bmpiece^2 )
     ENDELSE
  ENDFOR

  RETURN,fluxarr
        

END
