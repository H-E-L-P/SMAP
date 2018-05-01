;+
;NAME
; smap_getmap_xy
;PURPOSE
; To get the nearest pixel x/y values for a timestream
; given a map structure
;USAGE
; smap_getmap_xy, tod, map250, map350, map500, xvals, yvals
;INPUTS
; tod             The time-ordered data product (see smap_getlevel1)
; map250/350/500  The SMAP map structure for each band.  Set to a
;                  non-existant variable to skip.
;OUTPUTS
; xvals           The x positions corresponding to the order of
;                  .signal in tod.  -1 for values outside the map.
; yvals           The y positions
;NOTES
; Only bolometers that see sky have positions calculated
;MODIFICATION HISTORY
; Author: Alex Conley, Nov 17, 2009
;-

PRO smap_getmap_xy_inner, ra, dec, map, x, y
  COMPILE_OPT IDL2, HIDDEN

  AD2XY, ra, dec, map.astrometry, thisx, thisy

  ;;Remove pixels out of range, or with other
  ;; issues
  wbad = WHERE( thisx LE -0.5 OR thisy LE -0.5 OR $
                thisx GE map.xsize-0.5 OR $
                thisy GE map.ysize-0.5 OR $
                (~FINITE(thisx)) OR (~FINITE(thisy)), nbad,$
                COMPLEMENT=wgood, NCOMPLEMENT=ngood )
  x = ROUND(thisx)
  y = ROUND(thisy)
  IF nbad EQ 0 THEN RETURN
  x[wbad] = -1
  y[wbad] = -1
  RETURN
END


PRO smap_getmap_xy, tod, map250, map350, map500, xvals, yvals
  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg  = ''

  xvals = REPLICATE(-1, tod.nchans, tod.nsamps )
  yvals = xvals

  IF N_ELEMENTS(map250) NE 0 THEN BEGIN
     w250 = WHERE(STRMID(tod.chan,0,3) EQ 'PSW' AND $
                  tod.islight,n250)
     IF n250 NE 0 THEN BEGIN
        smap_getmap_xy_inner, tod.ra[w250,*], tod.dec[w250,*],$
                              map250, cx, cy
        xvals[w250,*] = TEMPORARY(cx)
        yvals[w250,*] = TEMPORARY(cy)
     ENDIF
  ENDIF
  IF N_ELEMENTS(map350) NE 0 THEN BEGIN
     w350 = WHERE(STRMID(tod.chan,0,3) EQ 'PMW' AND $
                  tod.islight,n350)
     IF n350 NE 0 THEN BEGIN
        smap_getmap_xy_inner, tod.ra[w350,*], tod.dec[w350,*],$
                              map350, cx, cy
        xvals[w350,*] = TEMPORARY(cx)
        yvals[w350,*] = TEMPORARY(cy)
     ENDIF
  ENDIF
  IF N_ELEMENTS(map500) NE 0 THEN BEGIN
     w500 = WHERE(STRMID(tod.chan,0,3) EQ 'PLW' AND $
                  tod.islight,n500)
     IF n500 NE 0 THEN BEGIN
        smap_getmap_xy_inner, tod.ra[w500,*], tod.dec[w500,*],$
                              map500, cx, cy
        xvals[w500,*] = TEMPORARY(cx)
        yvals[w500,*] = TEMPORARY(cy)
     ENDIF
  ENDIF
  RETURN
END
