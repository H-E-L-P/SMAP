;+
;NAME
; get_region_mask
;PURPOSE
; Return a mask showing which points are inside or
; outside a specified simple polygon.
;USAGE
; mask = get_region_mask( nx, ny, xpoly, ypoly )
;INPUTS
; nx, ny         Size of mask to output
; xpoly, ypoly   Verticies of polygon.  Should all be unique,
;                 and should define a simple polygon (i.e., one
;                 that doesn't cross itself.
;RETURNS
; A mask that is 0 for everything outside the polygon, and
; 1 for everything inside or on the border of the polygon.
;MODIFICATION HISTORY
; Author: Alex Conley, Nov 2, 2009
;-

FUNCTION get_region_mask, nx, ny, xpoly, ypoly
  COMPILE_OPT IDL2, STRICTARRSUBS
  ON_ERROR,2 

  IF nx LE 0 THEN MESSAGE,"nx must be positive"
  IF ny LE 0 THEN MESSAGE,"ny must be positive"
  nxv = N_ELEMENTS(xpoly) & nyv = N_ELEMENTS(ypoly)
  IF nxv EQ 0 THEN MESSAGE,"No x values for polynomial"
  IF nxv LT 2 THEN MESSAGE,"Need at least 2 points in poly"
  IF nxv NE nyv THEN MESSAGE,"xpoly and ypoly not the same length"
  wbad = WHERE( (~ FINITE(nxv)) OR (~ FINITE(nyv)),nbad)
  IF nbad NE 0 THEN MESSAGE,"Non-finite vertex values"

  outarr = BYTARR(nx,ny)
  xvals = FINDGEN(nx) # REPLICATE(1.0,ny)
  yvals = REPLICATE(1.0,nx) # FINDGEN(ny)
  windings = ABS( polywind(xvals[*],yvals[*],xpoly,ypoly,/NOCHECK) )
  mwind = MAX(windings)
  IF mwind GT 1 THEN MESSAGE,"Non simple polygon entered"
  win = WHERE( windings EQ 1, nin )
  IF nin NE 0 THEN outarr[win] = 1b

  RETURN,outarr
END
