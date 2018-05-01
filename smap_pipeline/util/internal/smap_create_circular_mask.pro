;+
;NAME
; smap_create_circular_mask
;PURPOSE
; Make a circular mask
;USAGE
; mask = smap_create_circular_mask( radius, pixsize, npix )
;INPUTS
; radius     Radius within which to set the mask
; pixsize    Size of pixels in same units as radius
; npix       Number of pixels per side.  Should be odd.
;RETURNS
; A npix by npix bytarr set to 1b inside the specified radius.
;MODIFICATION HISTORY
; Author: Alex Conley, Oct 2009
;-
FUNCTION smap_create_circular_mask, radius, pixsize, npix
  COMPILE_OPT IDL2, STRICTARRSUBS
  
  IF npix LE 0 THEN RETURN,0b
  IF radius LE 0 OR pixsize LE 0 THEN RETURN,BYTARR(npix,npix)
  IF npix MOD 2 EQ 0 THEN i_npix = npix+1 ELSE i_npix = npix

  dist_circle, dist, i_npix, /DOUBLE
  dist *= pixsize ;;dist in arcsec
  apert = BYTARR( i_npix, i_npix )
  wrad = WHERE( TEMPORARY(dist) LE radius, nrad )
  IF nrad GT 0 THEN apert[wrad]=1b
  RETURN,apert
END
