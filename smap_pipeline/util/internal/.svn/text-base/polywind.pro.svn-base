;+
;NAME
; polywind
;PURPOSE
; Computes the winding number of a polynomial
; around a point.
;USAGE
; wind = polywind( x, y, xvertex, yvertex )
;INPUTS
; x,y                X and Y positions of point
; xvertex, yvertex   X and Y positions of verticies.  All points
;                     should be unique (i.e., don't include the first
;                     point twice).
;RETURNS
; The polynomial winding number.  For simple polygons, this is 1
;  for points inside a CCW polygon, -1 a CW one, and 0 outside.
;  For non-simple polygons, anything is possible.
;NOTES
; See Numerical Recipies 3 chap. 21 for details.
;MODIFICATION HISTORY
; Author: Alex Conley, Nov 2009
;-

FUNCTION polywind, x, y, xvertex, yvertex, NOCHECK=nocheck
  COMPILE_OPT IDL2, STRICTARRSUBS

  ON_ERROR,2  ;;return to caller on error

  ;;Input checks
  nx = N_ELEMENTS(x) & ny = N_ELEMENTS(y)
  nxv = N_ELEMENTS(xvertex) & nyv = N_ELEMENTS(yvertex)
  IF ~ KEYWORD_SET( nocheck ) THEN BEGIN
     IF nx EQ 0 THEN MESSAGE,"No x values for point"
     IF nx NE ny THEN MESSAGE,"x and y not the same length"
     wbad = WHERE( (~ FINITE(nx)) OR (~ FINITE(ny)),nbad)
     IF nbad NE 0 THEN MESSAGE,"Non-finite x/y values"
     IF nxv EQ 0 THEN MESSAGE,"No x values for polynomial"
     IF nxv LT 2 THEN MESSAGE,"Need at least 2 points in poly"
     IF nxv NE nyv THEN MESSAGE,"xvertex and yvertex not the same length"
     wbad = WHERE( (~ FINITE(nxv)) OR (~ FINITE(nyv)),nbad)
     IF nbad NE 0 THEN MESSAGE,"Non-finite vertex values"
  ENDIF

  ;;Use C call to make this fast
  xvertex_work = DOUBLE(xvertex)
  yvertex_work = DOUBLE(yvertex)
  xwork = DOUBLE(x)
  ywork = DOUBLE(y)
  winds = LONARR(nx)
  status = CALL_EXTERNAL(SMAP_GET_CALLEXT(), $
                         'smap_polywind_extern', nxv, xvertex_work, $
                         yvertex_work, nx, xwork, ywork, winds,$
                         VALUE=[0,0,0,0,0,0,0],/CDECL)
        
  RETURN,winds

END
