PRO EXAMINE_IMAGE, image, header,  MINV=minv, MAXV=maxv, CUT=cut, WIND=wind, $
                   XSIZE=xsize, YSIZE=ysize, ZOOMFACT=zoomfact

;+
; USAGE: EXAMINE_IMAGE, image, header,  MINV=minv, MAXV=maxv, CUT=cut, $
;                       WIND=wind, XSIZE=xsize, YSIZE=ysize, ZOOMFACT=zoomfact
;
; Displays a FITS image at 1:1 resolution by dividing up the region
; into boxes that fit into display window. Allows for some of data.
;
; Options while examining image:
;
;   [N]ext:   proceed to next image
;   [C]oords: return ra,dec coordinates of map click
;   [Z]oom:   return coords AND display zoomed image in sub-windo
;   [Q]uit:   abort routine, return to MAIN
;
; PARAMETERS:
;   image:    data image
;   header:   corresponding map header
;
; OPTIONAL INPUTS:
;   minv:     minimum map value for color clipping
;   maxv:     maximum map value for color clipping
;   cut:      value between 0 and 1 for choosing color clipping range
;             chooses limits by taking central cut fraction of pixels
;             honors minv and maxv (takes cut on pixels in range)
;             can be slow for large maps
;   wind:     routine uses two windows, wind and wind+1. Default: 1.
;   xsize:    X size of main plotting window. Default: 1200.
;   ysize:    Y size of main plottinw window. Default: 960.
;   zoomfact: factor to blow up zoom region. Default: 4.
;
; CREATED BY:
;   gmarsden@phas.ubc.ca 2012-07-31
;-

IF NOT KEYWORD_SET(minv) THEN minv = -!VALUES.F_INFINITY
IF NOT KEYWORD_SET(maxv) THEN maxv = !VALUES.F_INFINITY

IF NOT KEYWORD_SET(wind) THEN wind = 1 ; default window
IF NOT KEYWORD_SET(xsize) THEN xsize = 1200
IF NOT KEYWORD_SET(ysize) THEN ysize = 960

IF NOT KEYWORD_SET(zoomfact) THEN zoomfact = 4.0

finiteind = WHERE(FINITE(image), nfinite)
IF nfinite EQ 0 THEN $
   MESSAGE, "No finite values in image!"

thismin = minv > MIN(image[finiteind])
thismax = maxv < MAX(image[finiteind])


IF KEYWORD_SET(cut) THEN BEGIN
   IF cut LE 0.0 OR cut GT 1.0 THEN BEGIN
      MESSAGE, "Keyword 'CUT' must be in range (0,1]", /INF
   ENDIF ELSE BEGIN
      sortind = SORT(image[finiteind])
      thismin = image[finiteind[sortind[(1.0 - cut)/2.0 * nfinite]]]
      thismax = image[finiteind[sortind[(1.0 + cut)/2.0 * nfinite]]]
      PRINT, "Setting limits (" + STRTRIM(STRING(thismin), 2) + $
             ", " + STRTRIM(STRING(thismax), 2) + ")"
   ENDELSE
ENDIF


WINDOW, wind
WINDOW, wind+1, XSIZE=xsize, YSIZE=ysize

sz = SIZE(image, /DIM)
mnx = sz[0]
mny = sz[1]

npx = CEIL( FLOAT(mnx) / xsize)
npy = CEIL( FLOAT(mny) / ysize)

; offset in X,Y
IF npx EQ 0 THEN dxp = 0 $
ELSE dpx = xsize - (FLOAT(npx) * xsize - mnx) / (npx - 1)

IF npy EQ 0 THEN dyp = 0 $
ELSE dpy = ysize - (FLOAT(npy) * ysize - mny) / (npy - 1)

PRINT, "Overlap is " + $
       STRTRIM(STRING((1.0 - FLOAT(mnx) / (npx * xsize)) * 100, $
                      FORM='(F4.1)'), 2) + "% in X, " + $
       STRTRIM(STRING((1.0 - FLOAT(mny) / (npy * ysize)) * 100, $
                      FORM='(F4.1)'), 2) +  "% in Y"

FOR i=0,npx-1 DO BEGIN

   xl = ROUND(i * dpx)
   xh = xl + xsize < mnx ; for when npx == 1

   FOR j=0,npy-1 DO BEGIN
      
      yl = ROUND(j * dpy)
      yh = yl + ysize < mny ; for when npy == 1

      HEXTRACT, image, header, thismap, thishead, xl, xh-1, yl, yh-1, /SIL

      find = WHERE(FINITE(thismap), nf)
      IF nf EQ 0 THEN action = "Skipping empty" ELSE action = "Plotting"

      PRINT, action + " submap X: " + $
             STRTRIM(STRING(i+1),2) + "/" + STRTRIM(STRING(npx),2) + $
             ", Y: " + $
             STRTRIM(STRING(j+1),2) + "/" + STRTRIM(STRING(npy),2)

      IF nf EQ 0 THEN CONTINUE

      WSET, wind
      TVSCALE, image, /NOINT, MINV=thismin, MAXV=thismax
      PLOT, [0,mnx], [0,mny], /NOD, POS=[0,0,1,1], XS=5, YS=5, /NOERASE
      OPLOT, [xl, xh, xh, xl, xl], [yl, yl, yh, yh, yl], COL=255

      WSET, wind+1
      TV, BYTSCL(thismap, MIN=thismin, MAX=thismax)

      ; get coordinates?
      ans = ""
      WHILE 1 DO BEGIN
         READ, "[N]ext [C]oords [Z]oom [Q]uit? [n]: ", ans

         char = STRLOWCASE(STRMID(ans, 0, 1))

         IF char EQ 'q' THEN RETURN
         IF ans EQ "" OR char EQ 'n' THEN BREAK

         IF char NE 'c' AND char NE 'z' THEN BEGIN
            PRINT, "Choice not understood"
            CONTINUE
         ENDIF

         PRINT, "Click on map..."
         CURSOR, xclick, yclick, WAIT=4, /DEVICE

         XYAD, thishead, xclick, yclick, thisra, thisdec
         PRINT, thisra, thisdec

         IF char EQ 'c' THEN CONTINUE

         ; else zoom
         WSET, wind
         wxs = !D.X_SIZE
         wys = !D.Y_SIZE

         nzxo = ROUND(wxs / zoomfact)
         nzyo = ROUND(wys / zoomfact)

         zoomimage = FLTARR(nzxo, nzyo)

         ; coords into image
         zixl = xl + xclick - nzxo / 2 
         zixh = zixl + nzxo

         ziyl = yl + yclick - nzyo / 2
         ziyh = ziyl + nzyo

         ; check for out-of-range
         zixo = - (zixl < 0)
         zinx = (zixh < mnx) - zixl + zixo

         ziyo = - (ziyl < 0)
         ziny = (ziyh < mny) - ziyl + ziyo

         zoomimage[zixo:zixo+zinx-1,ziyo:ziyo+ziny-1] = $
            image[zixl-zixo:zixl-zixo+zinx-1,ziyl-ziyo:ziyl-ziyo+ziny-1]

         WSET, wind
         pos = [0,0,1,1]
         TVSCALE, zoomimage, /KEEP, /NOINT, /ERASE, $
                  MINV=thismin, MAXV=thismax, POS=pos
         PLOT, [zixo,zixo+zinx], [ziyo,ziyo+ziny], XS=5, YS=5, /NODATA, /NOER, $
               POS=pos

         ; crosshair
         chspace = 2
         chlen = 6
         chthick=2

         zxc = nzxo / 2 - zixo
         zyc = nzyo / 2 - ziyo
         OPLOT, [1,1]*zxc, zyc-[chspace,chspace+chlen], COL=255, THICK=chthick
         OPLOT, [1,1]*zxc, zyc+[chspace,chspace+chlen], COL=255, THICK=chthick
         OPLOT, zxc-[chspace,chspace+chlen], [1,1]*zyc, COL=255, THICK=chthick
         OPLOT, zxc+[chspace,chspace+chlen], [1,1]*zyc, COL=255, THICK=chthick
         
         WSET, wind+1
      ENDWHILE

   ENDFOR
ENDFOR

END
