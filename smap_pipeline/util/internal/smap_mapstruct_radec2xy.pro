;+
;NAME
; smap_mapstruct_radec2xy
;PURPOSE
; Convert ra,dec to x,y for a SMAP map structure
;USAGE
; smap_mapstruct_radec2xy, ra, dec, map_struct, x, y
;INPUTS
; ra, dec     RA and DEC in decimal degrees.
; map_struct  smap map structure (see get_smap_mapstruct)
;OUTPUTS
; x,y         Pixel coordinates on map
;MODIFICATION HISTORY
; Author: Alex Conley, May 2009
;-

PRO smap_mapstruct_radec2xy, ra, dec, map_struct, x, y
         
  IF SIZE(map_struct,/TNAME) NE 'STRUCT' THEN $
     MESSAGE,"Map struct is not a structure!"
  IF ~ TAG_EXIST( map_struct, 'astrometry', /TOP_LEVEL ) THEN $
     MESSAGE,"Map struct doesn't contain astrometry info"
  astr = map_struct.astrometry

  ;;The following is stolen from adxy
  case strmid( astr.ctype[0], 5,3) of
     'GSS': gsssadxy, astr, ra, dec, x, y ;HST Guide star astrometry
     else:  ad2xy, ra, dec, astr, x, y    ;All other cases
  endcase
END
