;+
;NAME
; smap_mapstruct_xy2radec
;PURPOSE
; Convert x,y to ra,dec for a SMAP map structure
;USAGE
; smap_mapstruct_xy2radec, x, y, map_struct, ra, dec
;INPUTS
; x,y         Pixel coordinates on map
; map_struct  smap map structure (see get_smap_mapstruct)
;OUTPUTS
; ra, dec     RA and DEC in decimal degrees (unless one of
;              the keywords are set below)
;KEYWORDS
; ecliptic    Output is Ecliptic longitude and latitude
; galactic    Output is Galactic longitude and latitude
;MODIFICATION HISTORY
; Author: Alex Conley, May 2009
;-

PRO smap_mapstruct_xy2radec, x, y, map_struct, ra, dec,$
                             GALACTIC=galactic, ECLIPTIC=ecliptic
         
  IF SIZE(map_struct,/TNAME) NE 'STRUCT' THEN $
     MESSAGE,"Map struct is not a structure!"
  IF ~ TAG_EXIST( map_struct, 'astrometry', /TOP_LEVEL ) THEN $
     MESSAGE,"Map struct doesn't contain astrometry info"
  astr = map_struct.astrometry

  ;;The following is stolen from xyad
  case strmid(astr.ctype[0],5,3)  of 
        'GSS': gsssxyad, astr, x, y, a, d
         else: xy2ad, x, y, astr, a, d
  endcase
  titname = strmid(astr.ctype,0,4)
  if (titname[0] EQ 'DEC-') or (titname[0] EQ 'ELAT') or $
          (titname[0] EQ 'GLAT') then titname = rotate(titname,2)

  if keyword_set(GALACTIC) then begin
      case titname[0] of 
      'RA--': euler, a,d, select=1
      'ELON': euler, a,d, select=5
      else:
      endcase
  endif else if keyword_set(ECLIPTIC) then begin 
      case titname[0] of 
      'RA--': euler, a, d, select=3
      'GLON': euler, a,d, select=6
      else: 
      endcase
  endif else begin
      case titname[0] of 
      'ELON': euler, a, d, select=4
      'GLON': euler, a,d, select=2
      else: 
      endcase
  endelse

  ra = a
  dec = d
END
