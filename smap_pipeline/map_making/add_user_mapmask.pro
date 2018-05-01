;+
;NAME
; add_user_mapmask
;PURPOSE
; To add user specified mask information to a SMAP structure
;USAGE
; add_user_mapmask, mapstruct, masklist, MASKVAL=maskval,$
;                   MASKDIR=maskdir, ANDPLUS=andplus,$
;                   OBJMASK=objmask, MINEXP=minexp, MAXEXP=maxexp,$
;                   RADIUSSCALE=radiusscale
;INPUTS
; mapstruct      SMAP map structure to modify, setting places
;                 where the user specifies to be 0 where the data
;                 is good, and MASKVAL where it shouldn't.  This must
;                 have the mask extension
; masklist       List of masks to use.  If prefaced with a +,
;                 then points inside the polygon specified by the list
;                 should be kept, - to exclude them.  If neither is
;                 present, + is assumed
;OPTIONAL INPUTS
; maskval         Value to set mask to where excluded (def: 1)
; maskdir         Place to look for masks in
; objmask         Bright object mask (also uses maskdir)
; minexp/maxexp   Also masks out regions where the exposure time
;                  falls outside this range.
; radiusscale     Scaling to apply to radius of object mask
;KEYWORDS
; andplus        And together all of the + masks, rather than oring.
;                 This means the final region must satisfy all of the
;                 + masks to pass.
; verbose        Print status messages as it runs
;NOTES
; The format of the mask files are a list of pairs of numbers
; specifying the vertices of a polygon outlining the mask.  See
; the get_region_mask documentation.  The pairs of numbers should
; be ra/dec in decimal form.
; The object mask is also lists of numbers, but in a nod to NED,
; should be in HMS and DMS : format.
;MODIFICATION HISTORY
; Author: Alex Conley, July 2011
;-

PRO add_user_mapmask, mapstruct, masklist, MASKVAL=maskval,$
                      MASKDIR=maskdir, ANDPLUS=andplus, VERBOSE=verbose,$
                      OBJMASK=objmask, MINEXP=minexp, MAXEXP=maxexp,$
                      RADIUSSCALE=radiusscale
  COMPILE_OPT IDL2

  IF SIZE(mapstruct,/TNAME) NE 'STRUCT' THEN $
     MESSAGE,"Input mapstruct not a structure"
  IF ~mapstruct.has_mask THEN $
     MESSAGE,"Input mask struct has no mask present"

  mapstruct.mask OR= $
     construct_user_mapmask(mapstruct, masklist, MASKVAL=maskval, $
                            MASKDIR=maskdir, ANDPLUS=andplus, $
                            VERBOSE=verbose, OBJMASK=objmask, $
                            MINEXP=minexp, MAXEXP=maxexp, $
                            RADIUSSCALE=radiusscale)

END
