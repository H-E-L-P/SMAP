;+
;NAME
; construct_user_mapmask
;PURPOSE
; To build a mask from a list of input files
;USAGE
; mask = construct_user_mapmask(mapstruct, masklist, MASKVAL=maskval,$
;                               MASKDIR=maskdir, ANDPLUS=andplus, $
;                               OBJMASK=objmask, MINEXP=minexp, $
;                               MAXEXP=maxexp, RADIUSSCALE=radiusscale)
;INPUTS
; mapstruct      SMAP structure that size and astrometry information
;                 are taken from.
; masklist       List of masks to use.  If prefaced with a +,
;                 then points inside the polygon specified by the list
;                 should be kept, - to exclude them.  If neither is
;                 present, + is assumed
;RETURNS
;                A mask set to 0 where the point passes and maskval
;                 where it doesn't.
;OPTIONAL INPUTS
; maskval         Value to set mask to where excluded (def: 1)
; maskdir         Place to look for masks in
; objmask         Bright object mask (also uses maskdir)
; minexp/maxexp   Also masks out regions where the exposure time
;                  falls outside this range.
; radiusscale     Scaling to apply to user object masks
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
; Author: Alex Conley
;-

FUNCTION construct_user_mapmask, mapstruct, masklist, MASKVAL=maskval,$
                                 MASKDIR=maskdir, ANDPLUS=andplus, $
                                 VERBOSE=verbose, OBJMASK=objmask, $
                                 MINEXP=minexp, MAXEXP=maxexp, $
                                 RADIUSSCALE=radiusscale
  COMPILE_OPT IDL2

  IF SIZE(mapstruct,/TNAME) NE 'STRUCT' THEN $
     MESSAGE,"Input mapstruct not a structure"
  IF N_ELEMENTS(masklist) EQ 0 THEN $
     MESSAGE,"User provided no masks"
  IF N_ELEMENTS(maskval) EQ 0 THEN i_maskval = 1uL ELSE $
     i_maskval = ULONG(maskval)
  IF i_maskval EQ 0 THEN $
     MESSAGE,"WARNING: mask value is 0 -- will match unmasked specification",$
             /INF

  nmasks = N_ELEMENTS(masklist)
  
  ;;Figure out which are positives, and which are negatives
  fchar = STRMID(masklist,0,1)
  wpos  = WHERE(fchar NE '-', npos, COMPLEMENT=wneg, NCOMPLEMENT=nneg)

  mask = ULONARR(mapstruct.xsize, mapstruct.ysize)

  IF npos GT 0 THEN BEGIN
     posmasks = STRARR(npos)
     FOR i=0,npos-1 DO BEGIN
        ;;strip off + if needed
        IF fchar[wpos[i]] EQ '+' THEN mname = STRMID(masklist[wpos[i]], 1) ELSE $
           mname = masklist[wpos[i]]
        IF N_ELEMENTS(maskdir) NE 0 THEN mname = addslash(maskdir) + mname
        posmasks[i] = TEMPORARY(mname)
     ENDFOR
  ENDIF

  IF nneg NE 0 THEN BEGIN
     negmasks = STRARR(nneg)
     FOR i=0,nneg-1 DO BEGIN
        mname = STRMID(masklist[wneg[i]], 1) ;;strip off -
        IF N_ELEMENTS(maskdir) NE 0 THEN mname = addslash(maskdir) + mname
        negmasks[i] = TEMPORARY(mname)
     ENDFOR
  ENDIF
  
  ;;Start the actual computation -- keep anything already masked
  xsize = mapstruct.xsize
  ysize = mapstruct.ysize
  rmask = BYTARR(xsize, ysize) ;;working mask to later incorporate
  IF KEYWORD_SET(andplus) THEN rmask[*] = 1b
  ;;After this, rmask will be 1 everywhere we want to keep, and 0
  ;; where we don't (the opposite of the final product!)
  FOR i=0,npos-1 DO BEGIN
     ;;Read file
     IF ~ FILE_TEST(posmasks[i],/READ) THEN $
        MESSAGE, "Unable to read in: " + posmasks[i]
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE, "Doing positive mask: " + posmasks[i],/INF
     READCOL, posmasks[i], ra, dec, /SILENT, FORMAT='(F,F)'
     IF N_ELEMENTS(ra) EQ 0 THEN $
        MESSAGE, "Positive mask: " + posmasks[i] + " had no information"
     ;;Convert to x/y
     AD2XY, ra, dec, mapstruct.astrometry, xpoly, ypoly
     ;;This will be 1 for everything inside the polygon -- i.e., that
     ;; we want to keep
     curr_rmask = get_region_mask(xsize, ysize, xpoly, ypoly)
     IF KEYWORD_SET(andplus) THEN rmask AND= TEMPORARY(curr_rmask) ELSE $
        rmask OR= TEMPORARY(curr_rmask)
  ENDFOR

  ;;Now do negative masks
  FOR i=0,nneg-1 DO BEGIN
     IF ~ FILE_TEST(negmasks[i],/READ) THEN $
        MESSAGE, "Unable to read in: " + negmasks[i]
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE, "Doing negative mask: " + negmasks[i], /INF
     READCOL, negmasks[i], ra, dec, /SILENT, FORMAT='(F,F)'
     IF N_ELEMENTS(ra) EQ 0 THEN $
        MESSAGE, "Negative mask: " + negmasks[i] + " had no information"
     AD2XY, ra, dec, mapstruct.astrometry, xpoly, ypoly
     ;;This will be 0 for everything inside the polygon -- i.e., that
     ;; we want to remove
     curr_rmask = ~ get_region_mask(xsize, ysize, xpoly, ypoly)
     rmask AND= TEMPORARY(curr_rmask)
  ENDFOR

  ;; Make user mask.  Note we have to reverse it
  IF N_ELEMENTS(objmask) NE 0 THEN BEGIN
     IF KEYWORD_SET(verbose) THEN MESSAGE, " Building object mask"
     rmask AND= ~ CONSTRUCT_USER_OBJECT_MASK(objmask, mapstruct.astrometry, $
                                             mapstruct.xsize, mapstruct.ysize, $
                                             mapstruct.pixscale, $
                                             MASKDIR=maskdir, $
                                             RADIUSSCALE=radiusscale)
  ENDIF

  IF N_ELEMENTS(minexp) NE 0 THEN BEGIN
     IF ~ mapstruct.has_exposure THEN $
        MESSAGE,"Map has no exposure information, needed for MINEXP="
     wbad = WHERE(mapstruct.exposure LT minexp, nbad)
     IF nbad NE 0 THEN rmask[wbad] = 0b
  ENDIF
  IF N_ELEMENTS(maxexp) NE 0 THEN BEGIN
     IF ~ mapstruct.has_exposure THEN $
        MESSAGE,"Map has no exposure information, needed for MAXEXP="
     wbad = WHERE(mapstruct.exposure GT maxexp, nbad)
     IF nbad NE 0 THEN rmask[wbad] = 0b
  ENDIF

  ;;Now put this into the final mask
  wmask = WHERE(TEMPORARY(rmask) EQ 0, nmask) ;;things to exclude
  IF nmask NE 0 THEN mask[wmask] OR= i_maskval
  
  RETURN, mask
END
