;+
;NAME
; loggen
;PURPOSE
; Create an array of n logarithmically spaced points between two
; limits
;USAGE
;  points = loggen(minval, maxval, npoints)
;INPUTS
;  minval   Smallest point.  Must be > 0
;  maxval   Largest point.  Must be  > 0
;  npoints  Number of points.  Must be > 0
;KEYWORDS
; double    Return a double array rather than float
; linear    Force the array to be linearly spaced
;RETURNS
; The array of points
;MODIFICATION HISTORY
; Author:  Edward Chapin, echapin@phas.ubc.ca
;-

FUNCTION loggen, minval, maxval, npoints, DOUBLE=double, LINEAR=linear
  COMPILE_OPT IDL2
 
  IF npoints LE 0 THEN MESSAGE,"npoints must be positive"
  IF minval LE 0 THEN MESSAGE,"minval must be positive"
  IF maxval LE 0 THEN MESSAGE,"maxval must be positive"

  IF KEYWORD_SET(double) THEN points = DINDGEN(npoints)/(npoints-1.0) $
  ELSE points = FINDGEN(npoints)/(npoints-1.0)

  IF KEYWORD_SET(linear) then $
    RETURN, (maxval - minval)*points + minval $
  ELSE $
    RETURN, EXP( (ALOG(maxval/minval))*points + ALOG(minval) )
END
