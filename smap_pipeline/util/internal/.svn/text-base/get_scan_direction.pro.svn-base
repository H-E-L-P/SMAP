;+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  function get_scan_direction.pro
;;  Sept 14, 2009
;;  Mike Zemcov
;;  This function takes an smap tod structure and figures out what
;;  scan direction it was taken in.
;;
;;  Inputs: tod = smap tod with ra and dec info
;;  Outputs: direction = one of the following:
;;                       0 = failed to find scan direction
;;                       1 = moving in the direction of +ra,+dec 
;;                       2 = moving in the direction of -ra,+dec
;;                       3 = moving in the direction of -ra,-dec
;;                       4 = moving in the direction of +ra,-dec   
;;  Options: scanangles = (optional) floating angle at which the scan
;;                         was performed.
;;           verbose = verbose messaging, 0 = silent
;;           success = the success flag, =1 if success, =0 if not
;;           errmsg = if error, this holds a string explaining the
;;           error
;;  Changelog: v1.0, MZ, Sept 14, 2009 - orignal
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-
PRO GET_SCAN_DIRECTION,tod,inderivs,$
                       SCANANGLES=scanang,$
                       VERBOSE=verbose,$
                       SUCCESS=success,$
                       ERRMSG=errmsg

  COMPILE_OPT IDL2

  ; set up the error handling variables
  success = 0b
  direction = 0
  errmsg = ''

  ; set up verbosity
  IF ~(KEYWORD_SET(verbose)) THEN verbose = 0b
  IF KEYWORD_SET(verbose) THEN silent=0 ELSE silent=1
  
  ; make arrays to hold the derivatives
  posderivs = DBLARR(tod.nchans,2)

  ; loop through all bolos and compute the sign of their derivatives
  FOR id=0,tod.nchans-1 DO BEGIN
     raderivtemp = DERIV(tod.ra[id,*])
     decderivtemp = DERIV(tod.dec[id,*])
     posderivs[id,0] = MEAN(raderivtemp)
     posderivs[id,1] = MEAN(decderivtemp)
  ENDFOR
  
  thesederivs = [MEAN(posderivs[*,0]),MEAN(posderivs[*,1])]

  IF N_ELEMENTS(inderivs) EQ 0 THEN BEGIN
     inderivs = [thesederivs,1]
     scanang = ACOS((TRANSPOSE(thesederivs)#[1,0]) / $
                    (SQRT(TOTAL(thesederivs^2)) * $
                     SQRT(TOTAL([1,0]^2)))) * 180. / !PI 
  ENDIF ELSE BEGIN
     thisang = ACOS((TRANSPOSE(thesederivs)#inderivs[0:1]) / $
                    (SQRT(TOTAL(thesederivs^2)) * $
                     SQRT(TOTAL(inderivs[0:1]^2)))) * 180. / !PI 
     scanang = ACOS((TRANSPOSE(thesederivs)#[1,0]) / $
                    (SQRT(TOTAL(thesederivs^2)) * $
                     SQRT(TOTAL([1,0]^2)))) * 180. / !PI 
     IF thisang GT 90 THEN thisang = thisang - 180.
     IF ABS(thisang) LT 30. THEN inderivs[2] = 1
     IF ABS(thisang) GT 60. THEN inderivs[2] = 2

  ENDELSE


 
  ; and we're out
  success = 1b
  RETURN

END
