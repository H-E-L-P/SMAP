function bg_estimator2, image, maskimage, patch, VERBOSE=verbose

;+
; NAME:
;     bg_estimator
; PURPOSE:
;     Estimate residual background in input sky map 
; EXPLANATION:
;     This is intended as a subroutine of ps_extractor, but 
;     can be used independentsly  
;
; CALLING SEQUENCE:
;     bgim250=bg_estimator2(image,covimage, patch)
;
; INPUTS:
;     image   - input image for background estimation
;     covimage   - input coverage or mask image for background estimation
;		values should be zero for no coverage, non-zero for coverage
;     patch - single scalar size of subimage used for background estimation 
;
; OPTIONAL INPUTS:
;     NONE
;
; OPTIONAL KEYWORD INPUTS:
;     NONE
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
  On_error,2                    ;Return to caller
  compile_opt idl2

; MAKE BACKGROUND MAP
  imsize=size(image,/dimensions)
  bgimage=fltarr(size(image,/dimensions))

; ADJUST PATCH SIZE TO IMAGE SIZE FOR SMALL IMAGES
; SOMETHING LIKE:
  IF imsize[0] lt patch THEN ipatch=imsize[0]
  IF imsize[1] lt patch THEN jpatch=imsize[1]

; MAKE ADJUSTABLE PATCH SIZES FOR ARRAY EDGES
  ipatch=patch
  jpatch=patch

; BG ESTIMATOR INTERPRETS THE MASK IMAGE AS A COVERAGE MAP
; THIS REVERSES THE MASK IMAGE TO INDICATE GOOD PIXELS WITH 1b
  covimage=maskimage
  cov = WHERE(maskimage EQ 0,ngood, COMPLEMENT=nocov, NCOMPLEMENT=nbad)
  IF ngood NE 0 THEN covimage[cov]=1b
  IF nbad NE 0 THEN covimage[nocov]=0b

; LOOP OVER SUBIMAGES
  FOR i=000,imsize[0]-patch-1,8 DO BEGIN
     FOR j=000,imsize[1]-patch-1,8 DO BEGIN
        ;; PRINT PROGRESS
        IF i gt imsize[0]/4-4 and i lt imsize[0]/4+4 and j eq 0 THEN $
           print, '25% Completed'
        IF i gt imsize[0]/2-4 and i lt imsize[0]/2+4 and j eq 0 THEN $
           print, '50% Completed'
        IF i gt imsize[0]*3/4-4 and i lt imsize[0]*3/4+4 and j eq 0 THEN $
           print, '75% Completed'

; ESTIMATE BACKGROUND IN PATCH
        IF (i lt imsize[0]-patch-8-1) and (j lt imsize[1]-patch-8-1) THEN BEGIN
           subim=image[i:i+patch,j:j+patch]  
           subcov=covimage[i:i+patch,j:j+patch]  
           ipatch=patch
           jpatch=patch
        ENDIF 
        IF (i ge imsize[0]-patch-8-1) and (j lt imsize[1]-patch-8-1)  THEN BEGIN
;print, 'Using expanded sub-image size at array edge'
           subim=image[i:imsize[0]-1,j:j+patch]  
           subcov=covimage[i:imsize[0]-1,j:j+patch]  
           ipatch=imsize[0]-1-i
           jpatch=patch
        ENDIF
        IF (i lt imsize[0]-patch-8-1) and (j ge imsize[1]-patch-8-1)  THEN BEGIN
;print, 'Using expanded sub-image size at array edge'
           subim=image[i:i+patch,j:imsize[1]-1]  
           subcov=covimage[i:i+patch,j:imsize[1]-1]  
           ipatch=patch
           jpatch=imsize[1]-1-j
        ENDIF
        IF (i ge imsize[0]-patch-8-1) and (j ge imsize[1]-patch-8-1)  THEN BEGIN
;print, 'Using expanded sub-image size at array edge'
           subim=image[i:imsize[0]-1,j:imsize[1]-1]  
           subcov=covimage[i:imsize[0]-1,j:imsize[1]-1]  
           ipatch=imsize[0]-1-i
           jpatch=imsize[1]-1-j
        ENDIF

        ;;Subcov is one where pixels are good, zero where bad
        goodpix = WHERE( subcov NE 0 AND FINITE(subim), $
                         ngoodpix, NCOMPLEMENT=nbadpix,$
                         COMPLEMENT=badpix )
        
        IF nbadpix NE 0 THEN subim[badpix] = !VALUES.F_NAN
        IF ngoodpix GT 2 THEN BEGIN ;;Require at least 3 points
           subav=avg(subim,/nan)                                   
           subsd=stddev(subim,/nan)
           IF KEYWORD_SET( verbose ) THEN $
              PRINT,'Initial subimage mean and standard deviation are: ',$
                    subav,subsd
           source=where(abs(subim-subav) gt 2.*subsd,nsource)
           count=0
           WHILE nsource NE 0 DO BEGIN
              IF KEYWORD_SET(verbose) THEN $
                 print,'In iteration',count, ' there are ',n_elements(source),$
                       ' pixels more than 3 sigma above the background.'
              count+=1   
              subim[where(abs(subim-subav) gt 2.*subsd)]=!VALUES.F_NAN
              subav=avg(subim,/nan)                                   
              subsd=stddev(subim,/nan)                                
              source=where(abs(subim-subav) gt 2.*subsd,nsource)
           ENDWHILE
           IF KEYWORD_SET( verbose ) THEN BEGIN
              print,'Final subimage mean and standard deviation are: ',$
                    subav,subsd
              print,'It took ', count, ' iterations to remove all ',$
                    'pixels 2 sigma from the mean.
           ENDIF

           ;; FILL THE BACKGROUND IMAGE WITH THE MEDIAN VALUE
           IF i eq 0 and j eq 0 THEN bgimage[i:i+patch,j:j+patch]=median(subim)

           IF i ne 0 or j ne 0 THEN BEGIN
              FOR l=0,ipatch DO BEGIN
                 FOR m=0,jpatch DO BEGIN
                    IF bgimage[i+l,j+m] eq 0 and covimage[i+l,j+m] ne 0 THEN $
                       bgimage[i+l,j+m]=median(subim)
                    IF bgimage[i+l,j+m] ne 0 THEN $
                       bgimage[i+l,j+m]=(bgimage[i+l,j+m]+median(subim))/2.

                 ENDFOR
              ENDFOR
;bgimage[i:i+patch/2,j:j+patch/2]=(bgimage[i:i+patch/2,j:j+patch/2]+median(subim))/2.
;bgimage[i+patch/2+1:i+patch,j+patch/2+1:j+patch]=median(subim)
;bgimage[i:i+patch,j:j+patch]=median(subim)
           ENDIF

        ENDIF
     ENDFOR
  ENDFOR



; SMOOTH THE BACKGROUND IMAGE in a box 16x16 pixels
  bgimage=filter_image(bgimage,SMOOTH=32,/ITER)

; CRUDE COVERAGE MASK
  IF nbad NE 0 THEN bgimage[nocov]=0.

  return, bgimage
END
