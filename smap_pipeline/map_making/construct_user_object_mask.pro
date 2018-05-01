;+
;NAME
; construct_user_object_mask
;PURPOSE
; Construct object based circular masks
;USAGE
; mask = construct_user_object_mask(objmask, astrometry, xsize,$
;                                   ysize, pixscale, MASKDIR=, VERBOSE=)
;INPUTS
; objmask     Name of file containing ra dec mask_radius of masks,
;              where ra and dec are hms and dms strings, and the
;              mask radius is in arcmin.
; astrometry  Astrometry structure
; xsize       X size of mask
; ysize       Y size of mask
; pixscale    Pixel scale in arcsec
;OPTIONAL INPUTS
; maskdir     Directory to look for objmask in.
; radiusscale Scaling to apply to radius of mask
;RETURNS
; xsize by ysize ulong array with 1 where the mask is set, 0 otherwise
;-

FUNCTION construct_user_object_mask, objmask, astrometry, xsize, ysize,$
                                     pixscale, MASKDIR=maskdir, $
                                     RADIUSSCALE=radiusscale
  COMPILE_OPT IDL2, STRICTARRSUBS
  IF N_ELEMENTS(maskdir) NE 0 THEN $
     objmaskfl = addslash(maskdir)+objmask ELSE $
        objmaskfl = objmask
  IF ~ FILE_TEST(objmaskfl,/READ) THEN $
     MESSAGE,"Couldn't read object mask file: "+objmaskfl
  READCOL, objmaskfl, rao, deco, maskrad, FORMAT='(A,A,F)',/SILENT

  rao  = RA_TO_RADEG(rao)
  deco = DEC_TO_DECDEG(deco)
  AD2XY, rao, deco, astrometry, xo, yo
  wmask = WHERE(xo GE 0 AND xo LT xsize AND yo GE 0 AND yo LT ysize,$
                nmask)

  IF N_ELEMENTS(radiusscale) NE 0 THEN BEGIN
     IF radiusscale LE 0.0 THEN MESSAGE, "Invalid raidusscale (non-positive)"
     IF ABS(radiusscale - 1.0) GT 1e-2 THEN maskrad *= FLOAT(radiusscale)
  ENDIF
  
  retmask = ULONARR(xsize, ysize)
  IF nmask NE 0 THEN BEGIN
     maskrad = maskrad[wmask]
     xo = xo[wmask]
     yo = yo[wmask]

     FOR j=0, nmask-1 DO BEGIN 
        msize = maskrad[j] * 60.0
        npix = CEIL(2.2 * msize / pixscale)
        IF npix MOD 2 EQ 0 THEN npix += 1
        nphalf = npix/2
        ;;Set to one in circle
        cmask = smap_create_circular_mask(msize, pixscale, npix)
        
        s = inject_index(retmask, cmask, xo[j], yo[j], SUCCESS=isuccess)
        IF isuccess THEN retmask[s.mx0:s.mx1, s.my0:s.my1] OR= $
           cmask[s.ox0:s.ox1, s.oy0:s.oy1]
     ENDFOR
  ENDIF
  RETURN, retmask
END
