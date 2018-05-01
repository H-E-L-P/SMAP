PRO SMAP_GETMAPBOUNDARY, astro, minra, maxra, mindec, maxdec, $
                         xlo, xhi, ylo, yhi,$
                         LATPOLE=latpole, LONGPOLE=longpole,$
                         NPOINTS=npoints
;+
;
; SMAP_GETMAPBOUNDARY, astro, minra, maxra, mindec, maxdec, $
;                      xlo, xhi, ylo, yhi,$
;                      LATPOLE=latpole, LONGPOLE=longpole
;
; Given map pixel size, tangent point and maximum extents of data on
; sky, determine size of map needed to contain all data points. This
; is not simply a rectangle in sky space because of tangent plan
; distortions. 
;
; INPUTS: (all in degrees)
;
;   astro:   astrometry structure
;   minra:   smallest RA value in data set
;   maxra:   largest RA value in data set
;   mindec:  smallest Dec value in data set
;   maxdec:  largest Dec value in data set
;
; OPTIONAL INPUTS
;   npoints: Number of points along each edge (def: 1000)
;   latpole/longpole  Latpole and longpole to use in FITS astrometric
;                      solution
; OUTPUTS:
;
;   xlo: lowest pixel value in X dimension
;   xhi: highest pixel value in X dimension
;   ylo: lowest pixel value in Y dimension
;   yhi: highest pixel value in Y dimension
;
; NOTE: output pixels values are based on a projection with the
; tangent point at (0,0). Values should be shifted accordingly.
;
; Created 2010-04-13 (gmarsden@phas.ubc.ca)
;
; CHANGELOG:
;
; 20120720 (gm): pass in astro structure instead of params
;
;-

  COMPILE_OPT IDL2
; number of points along each each
  IF N_ELEMENTS(npoints) EQ 0 THEN npoints = 1000

; build rectangular boundary in sky coords
  rabound = [FINDGEN(npoints) / npoints * (maxra - minra) + minra, $
             REPLICATE(maxra, npoints), $
             FINDGEN(npoints) / npoints * (minra - maxra) + maxra, $
             REPLICATE(minra, npoints)]

  decbound = [REPLICATE(mindec, npoints), $
              FINDGEN(npoints) / npoints * (maxdec - mindec) + mindec, $
              REPLICATE(maxdec, npoints), $
              FINDGEN(npoints) / npoints * (mindec - maxdec) + maxdec]

  AD2XY, rabound, decbound, astro, xxbound, yybound

  xlo = MIN(xxbound, MAX=xhi)
  ylo = MIN(yybound, MAX=yhi)

END
