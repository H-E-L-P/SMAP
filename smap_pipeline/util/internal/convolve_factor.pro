;+
;NAME
; convolve_factor
;PURPOSE
; Convolve an image by a factorized kernel
;USAGE
; convimage = convolve_factor( image, kernel, /NORMALIZE, /NAN, /WRAP )
;INPUTS
; image        Image to convolve (2D)
; kernel       Kernel factor (actual kernel is kernel ## kernel)
;KEYWORDS
; normalize    Normalize image by sum of absolute 2d kernel
; finite       Check for non-finite values, including in normalization
; wrap         Wrap the convolution around the edges.  Otherwise zero pad
;RETURNS
; Convolved image
;NOTE
; Not all combinations of the keywords are allowed.  Right now the
; only combinations are: normalize+nan+wrap, normalize+nan,
; nan+wrap, wrap, and none.
;NOTES
; In most cases this should give identical results to
;  CONVOL( image, kernel ## kernel, /CENTER )
; with /NORMALIZE being (almost) the same, /WRAP being like
; /EDGE_WRAP, not setting /WRAP like /EDGE_ZERO, and /NAN like /NAN.
; However, the no-wrap, /NORMALIZE behavior is not quite the same;
; CONVOL will include kernel bits that are off the edge in the
; normalization, CONVOL_FACTOR will not (which, frankly, makes more
; sense for astronomical applications).  That is, CONVOL will
; effectively apodize the edges, CONVOL factor won't.
;MODIFICATION HISTORY
; Author: Alex Conley, April 2012
;-

FUNCTION convolve_factor, image, kernel, NORMALIZE=normalize, NAN=nan,$
                          WRAP=wrap
  COMPILE_OPT IDL2, STRICTARRSUBS
  ON_ERROR,2
  
  ;;input checks
  IF N_ELEMENTS(image) EQ 0 THEN MESSAGE,"No input image"
  IF N_ELEMENTS(kernel) EQ 0 THEN MESSAGE,"No input kernel"

  sz = SIZE(image)
  nx = LONG( sz[1] )
  ny = LONG( sz[2] )
  nk = LONG( N_ELEMENTS(kernel) )
  imcopy = DOUBLE( image )
  kercopy = DOUBLE( kernel )
  IF KEYWORD_SET( normalize ) THEN argnorm = 1L ELSE argnorm=0L
  IF KEYWORD_SET( nan ) THEN argnan = 1L ELSE argnan=0L
  IF KEYWORD_SET( wrap ) THEN argwrap = 1L ELSE argwrap=0L
  status = CALL_EXTERNAL( SMAP_GET_CALLEXT(),$
                          'smap_convolve_factor_extern',$
                          nx,ny,imcopy,nk,kercopy,argnorm,argnan,argwrap,$
                          VALUE=[0,0,0,0,0,0,0,0],/CDECL )
  IF status NE 0 THEN MESSAGE,"Error doing convolution in C call"
  RETURN,imcopy

END
  
