;+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  function smap_finalizemap.pro
;;  Sept. 14, 2009
;;  Mike Zemcov
;;  This procedure takes a three color set of smap map structures
;;   after scan-wise binning and converts them into the correct thing
;;   that was measured on the sky.
;;  Inputs: map250: initialized 250 micron smap map structure; may already
;;           contain data
;;          map350: initialized 350 micron smap map structure; may already
;;           contain data
;;          map500: initialized 500 micron smap map structure; may already
;;           contain data
;;          mapparam: a standard smap map parameter structure
;;           corresponding to these data
;;          no250/no350/no500: Don't make a map in this band; note you
;;           still need to pass in the dummy argument (i.e., map250)
;;  Options: verbose = verbosity flag, 0 = silent
;;           success = success flag, 1=successful, 0=not
;;           errmsg = if error, string containing error message 
;;  Outputs = none
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-
PRO SMAP_FINALIZEMAP,map250,wt250,map350,wt350,map500,wt500,WEIGHTEDMAP=weimap,$
                     VERBOSE=verbose,SUCCESS=success,ERRMSG=errmsg,$
                     NO250=no250, NO350=no350, NO500=no500
  
    COMPILE_OPT IDL2

  ; set up the error handling variables
  success = 0b
  errmsg = ''

  ; set up verbosity
  IF ~(KEYWORD_SET(verbose)) THEN verbose = 0b
  ; if you didn't tell me about variance weighting, assume
  ; you want the dumbest possible thing
  IF ~(KEYWORD_SET(weimap)) THEN weimap = 0b

  ; tell me what we're up to
  IF KEYWORD_SET(verbose) THEN MESSAGE,'Finalizing maps...',/INF

  IF ~ KEYWORD_SET(no250) THEN BEGIN
     whmap = WHERE(map250.exposure GT 0,countmap, COMPLEMENT=wnmap, $
                   NCOMPLEMENT=nnmap)
     IF (countmap LT 1) THEN BEGIN
        errmsg = '250 micron map does not have any hits.'
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN
     END ELSE BEGIN
        map250.image[whmap] /= wt250[whmap]
        map250.error[whmap] /= wt250[whmap]^2
        map250.error[whmap] = SQRT( map250.error[whmap] ) ;;to sigma from var
     ENDELSE
     IF nnmap NE 0 THEN map250.image[wnmap] = !VALUES.D_NAN
  ENDIF

  IF ~ KEYWORD_SET(no350) THEN BEGIN 
     whmap = WHERE(map350.exposure GT 0,countmap,COMPLEMENT=wnmap, $
                   NCOMPLEMENT=nnmap)
     IF (countmap LT 1) THEN BEGIN
        errmsg = '350 micron map does not have any hits.'
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN
     END ELSE BEGIN
        map350.image[whmap] /= wt350[whmap]
        map350.error[whmap] /= wt350[whmap]^2
        map350.error[whmap] = SQRT( map350.error[whmap] )
     ENDELSE
     IF nnmap NE 0 THEN map350.image[wnmap] = !VALUES.D_NAN
  ENDIF

  IF ~ KEYWORD_SET(no500) THEN BEGIN
     whmap = WHERE(map500.exposure GT 0,countmap,COMPLEMENT=wnmap, $
                   NCOMPLEMENT=nnmap)
     IF (countmap LT 1) THEN BEGIN
        errmsg = '500 micron map does not have any hits.'
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN
     END ELSE BEGIN
        map500.image[whmap] /= wt500[whmap]
        map500.error[whmap] /= wt500[whmap]^2
        map500.error[whmap] = SQRT( map500.error[whmap] )
     ENDELSE
     IF nnmap NE 0 THEN map500.image[wnmap] = !VALUES.D_NAN
  ENDIF

  success=1b 
  RETURN

END
