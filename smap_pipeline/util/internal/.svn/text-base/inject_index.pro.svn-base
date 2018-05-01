;+
; NAME
;  inject_index
; PURPOSE
;  To do the index arithmetic needed to figure out how to inject
;  one 2D array into another at a given position
; USAGE
;  indices = inject_index(map, object, xpos, ypos[, SUCCESS=success])
; INPUTS
;  map          Thing to inject object into.  Must be 2D.
;  object       Thing to inject into map.  Must be 2D, smaller than
;                map, and of odd extent along each dimension
;  xpos         Desired x position of central pixel of object.
;  ypos         Desired y position of central pixel of object.
; RETURNS
;   A structure giving which indices to use for both map and object
;   so that, for example, object can be added to map using
;    s = inject_index(map, object, xpos, ypos)
;    map[s.mx0:s.mx1, s.my0:s.my1] += object[s.ox0:s.ox1, s.oy0:s.oy1]
;   Returns -1 if there is no insertion (e.g., xpos, ypos are too far
;   off the edge to be interesting)
; OPTIONAL OUTPUTS
;   success     1b if injection is possible, 0b if not (so -1 was returned)
; MODIFICATION HISTORY
;  Author: A. Coley, January 2015
;-
FUNCTION inject_index, map, object, xpos, ypos, SUCCESS=success
  COMPILE_OPT IDL2, STRICTARRSUBS

  success = 0b
  
  szim = SIZE(map)
  IF szim[0] NE 2 THEN MESSAGE, "Input map is not 2D"
  xsize = szim[1]
  ysize = szim[2]

  szobj = SIZE(object)
  IF szobj[0] NE 2 THEN MESSAGE, "Input object is not 2D"
  xosize = szobj[1]
  yosize = szobj[2]
  IF xosize MOD 2 EQ 0 THEN MESSAGE, "Input object not of odd x extent"
  IF yosize MOD 2 EQ 0 THEN MESSAGE, "Input object not of odd y extent"

  IF xosize GE xsize OR yosize GE ysize THEN $
     MESSAGE, "Object is larger in at least one dimension than map"
  
  xosizehf = xosize / 2
  yosizehf = yosize / 2

  ;; Make sure positions are integers
  xpos = ROUND(xpos)
  ypos = ROUND(ypos)
  
  ;; Check for too far off edge
  IF xpos LT -xosizehf OR xpos GE xsize + xosizehf THEN RETURN, -1
  IF ypos LT -yosizehf OR ypos GE ysize + yosizehf THEN RETURN, -1

  ;; Primary case -- index arithmetic maybe needed
  minxedge = xpos - xosizehf 
  maxxedge = xpos + xosizehf
  minyedge = ypos - yosizehf 
  maxyedge = ypos + yosizehf
  xbot = 0 & xtop = xosize - 1
  ybot = 0 & ytop = yosize - 1
  IF minxedge LT 0 THEN xbot = ABS(minxedge)
  IF minyedge LT 0 THEN ybot = ABS(minyedge)
  IF maxxedge GE xsize THEN xtop -= (maxxedge - xsize + 1)
  IF maxyedge GE ysize THEN ytop -= (maxyedge - ysize + 1)
  xbidx = 0 > (xpos - xosizehf)
  ybidx = 0 > (ypos - yosizehf)
  xtidx = (xsize - 1) < (xpos + xosizehf)
  ytidx = (ysize - 1) < (ypos + yosizehf)

  success = 1b
  RETURN, {mx0: xbidx, mx1: xtidx, my0: ybidx, my1: ytidx,$
           ox0: xbot, ox1: xtop, oy0: ybot, oy1: ytop}
END


