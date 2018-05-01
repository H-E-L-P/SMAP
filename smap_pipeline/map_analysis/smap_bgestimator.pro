;+
; NAME:
;     smap_bgestimator
; PURPOSE:
;     Estimate residual background in input sky map.  Like
;     bg_estimator2, but with some bits done in compiled code
; EXPLANATION:
;     This is intended as a subroutine of ps_extractor, but 
;     can be used independentsly  
;
; CALLING SEQUENCE:
;     bgim250=smap_bgestimator(image,maskimage, patch)
;
; INPUTS:
;     image   - input image for background estimation
;     maskimage   - input coverage or mask image for background estimation
;                    0 where you want the image to be used
;     patch - single scalar size of subimage used for background estimation 
;
; OPTIONAL INPUTS:
;     NONE
;
; OPTIONAL KEYWORD INPUTS:
;     nosmooth - Don't apply final filter_image smoothing step
;
; RETURNS:
;     bgimage - background image
;
; EXAMPLE:
;	make a background image from an image called im250 using a 
;       32x32 subimages and put into bgim250:
;        IDL> bgim250=bg_estimator2(im250, 32)
;       
; PROCEDURES USED:
;       READFITS(), AVG, SORT, PS_OPEN, PS_CLOSE
;-

FUNCTION smap_bgestimator, image, maskimage, patch, NABSDEV=nabsdev,$
                           NOSMOOTH=nosmooth,VERBOSE=verbose
  COMPILE_OPT IDL2

  IF patch LE 0 THEN MESSAGE,"Invalid patch size (non-positive)"

  IF N_ELEMENTS(nabsdev) EQ 0 THEN i_nabsdev = 3.0d0 ELSE $
     i_nabsdev = DOUBLE(nabsdev)
  IF i_nabsdev LE 0.0 THEN MESSAGE,"Invalid nabsdev (non-positive)"

; MAKE BACKGROUND MAP
  imsize=size(image,/dimensions)
  bgimage=fltarr(size(image,/dimensions))

; ADJUST PATCH SIZE TO IMAGE SIZE FOR SMALL IMAGES
; SOMETHING LIKE:
  IF imsize[0] lt patch THEN ipatch=imsize[0] ELSE ipatch=patch
  IF imsize[1] lt patch THEN jpatch=imsize[1] ELSE jpatch=patch

; BG ESTIMATOR INTERPRETS THE MASK IMAGE AS A COVERAGE MAP
; THIS REVERSES THE MASK IMAGE TO INDICATE GOOD PIXELS WITH 1b
  covimage=maskimage
  cov = WHERE(maskimage EQ 0,ngood, COMPLEMENT=nocov, NCOMPLEMENT=nbad)
  IF ngood NE 0 THEN covimage[cov]=1b
  IF nbad NE 0 THEN covimage[nocov]=0b

; LOOP OVER SUBIMAGES
  nsubim1 = CEIL( imsize[0]/(1.0*ipatch) )
  nsubim2 = CEIL( imsize[1]/(1.0*jpatch) )

  subimcheck = nsubim1/4

  FOR i=0, nsubim1-1 DO BEGIN
     lowidx1 = i*ipatch
     upidx1  = (lowidx1 + ipatch) < (imsize[0]-1)
     size1   = upidx1 - lowidx1 + 1
     FOR j=0, nsubim2-1 DO BEGIN
        lowidx2 = j*jpatch
        upidx2  = (lowidx2 + jpatch) < (imsize[1]-1)
        size2   = upidx2 - lowidx2 + 1

        subim  = image[lowidx1:upidx1,lowidx2:upidx2]
        subcov = covimage[lowidx1:upidx1,lowidx2:upidx2]

        ;;Subcov is one where pixels are good, zero where bad
        goodpix = WHERE( subcov NE 0 AND FINITE(subim), $
                         ngoodpix, NCOMPLEMENT=nbadpix )

        IF ngoodpix GT 2 THEN BEGIN
           subim   = DOUBLE( subim[goodpix] )
           working = subim
           nimage  = LONG( ngoodpix )
           med     = 0.0d0
           nmed    = 0L
           status = CALL_EXTERNAL( SMAP_GET_CALLEXT(), $
                                   'smap_bgestimator_extern', $
                                   subim, working, nimage, i_nabsdev, $
                                   med, nmed, VALUE=[0,0,0,0,0,0],/CDECL)
           IF status EQ 0 THEN $
              bgimage[lowidx1:upidx1,lowidx2:upidx2] = med
        ENDIF
     ENDFOR

     IF KEYWORD_SET(verbose) && i GT 0 AND (i MOD subimcheck EQ 0) THEN $
        MESSAGE,STRING(100.0*i/nsubim1,FORMAT='(" Completed ",F0.1,"%")'),$
                /INF
     
  ENDFOR

; SMOOTH THE BACKGROUND IMAGE in a box 
  IF ~ KEYWORD_SET( nosmooth ) THEN BEGIN
     smoothsize = ROUND(2*patch) < (imsize[0] < imsize[1])
     IF KEYWORD_SET( verbose ) THEN $
        MESSAGE,STRING(smoothsize,FORMAT='(" smoothing with size ",I0)'),/INF
     bgimage=filter_image(bgimage,SMOOTH=smoothsize)
  ENDIF

; CRUDE COVERAGE MASK
  IF nbad NE 0 THEN bgimage[nocov]=0.

  return, bgimage
END
