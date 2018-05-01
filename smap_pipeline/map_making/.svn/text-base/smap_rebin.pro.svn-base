;+
;NAME
; smap_rebin
;PURPOSE
; To create a rebinned version of a smap fitsmap
;USAGE
; newmap = smap_rebin(map,rebinfac)
;INPUTS
; map      SMAP fits map structure
; rebinfac Amount to rebin by.  Must be odd, integral, and divide the
;           the map size cleanly in both dimensions
;RETURNS
; newmap   New, rebinned, map
;NOTES
; The new map will -not- have error or mask information
;MODIFICATION HISTORY
; Author: Alex Conley, June 2011
;-

FUNCTION smap_rebin, map, rebinfac
  COMPILE_OPT IDL2

  IF N_ELEMENTS(map) EQ 0 THEN MESSAGE,"No input map"
  IF SIZE(map,/TNAME) NE 'STRUCT' THEN MESSAGE,"Was expecting SMAP fits map"
  IF rebinfac LE 0 THEN MESSAGE,"Size is invalid"
  IF rebinfac - ROUND(rebinfac) NE 0 THEN MESSAGE,"Rebin size must be integral"
  irebinfac = ROUND(rebinfac) ;;Make integer
  IF irebinfac MOD 2 NE 1 THEN MESSAGE,"Rebin fac must be odd"
  IF map.xsize MOD irebinfac NE 0 THEN $
     MESSAGE,"Rebin fac doesn't evenly divide x size of map"
  IF map.ysize MOD irebinfac NE 0 THEN $
     MESSAGE,"Rebin fac doesn't evenly divide y size of map"

  xnewsize = map.xsize / irebinfac
  ynewsize = map.ysize / irebinfac

  castr = map.astrometry
  castr.crpix /= irebinfac
  castr.cd *= irebinfac

  getmap_succ = 0b
  mapstruct = get_smap_mapstruct(NPIXX=xnewsize, NPIXY=ynewsize, $
                                 BAND=map.names, /NOMASK,$
                                 ASTROMETRY=TEMPORARY(castr),$
                                 NOEXP=~map.has_exposure, $
                                 NOERR=~map.has_error, /SILENT,$
                                 ERRMSG=errmsg, SUCCESS=getmap_succ,$
                                 /NO_ABORT)
  IF ~ getmap_succ THEN MESSAGE,errmsg

  ;;Rebin averages, we will need sum for exposure
  renorm = irebinfac*irebinfac 
  mapstruct.image = REBIN(map.image, xnewsize, ynewsize )
  IF mapstruct.has_exposure THEN $
     mapstruct.exposure = renorm*REBIN(map.exposure, xnewsize, ynewsize )
  IF map.has_error THEN $
     mapstruct.error = REBIN(map.error, xnewsize, ynewsize )/irebinfac
  mapstruct.pixscale = map.pixscale*irebinfac
  
  RETURN,mapstruct
END
