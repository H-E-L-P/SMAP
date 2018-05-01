;+
; NAME:
;   construct_turnaround_mask
; CATEGORY
;   Herschel SPIRE SMAP pipeline
; PURPOSE:
;   Add low velocity and slew mask bits to the TOD.
; CALLING SEQUENCE:
;   construct_turnaround_mask,tod
; INPUTS/OUTPUTS:
;   tod = standard SMAP tod.  Note that bits will be added to the mask
;          denoting turnaround and fast slew timesamples
; KEYWORDS
;   verbose = verbose output
; OUTPUTS:
;   A modified tod.mask which you can use to determine when the telescope 
;    was stopped.
; OPTIONAL OUTPUTS
;   success      1b if it succeeded, 0 if not
; NOTES
;
; MODIFICATION HISTORY:
;   Author: Michael Zemcov, Jan 2010
;           Michael Zemcov, Apr 2011, Made maskZeroVelocity flag 
;                                     dynamic and reinstituted this 
;                                     functionality in smap_getlevel1.pro
;           Michael Zemcov, May 2011, Added optional slew masking.
;-

PRO construct_turnaround_mask, tod, $
                               SPEEDCUT=speedcut,$
                               SUCCESS=success,$
                               ERRMSG=errmsg,$
                               VERBOSE=verbose

  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  IF ~(N_ELEMENTS(verbose)) THEN verbose=0 ELSE verbose = verbose


  ;; check input - if nothing's set then assume default
  ;;Anything slower than this is flagged as maskZeroVelocity
  IF ~N_ELEMENTS(speedcut) THEN speedcut = 10. ; arcsec/s

  ;; check we're not close to the nominal scan speed
  IF speedcut GT 29. THEN BEGIN
     errmsg = 'Proposed speed cut is within 1 arcsec/s of nominal scan speed.'
     GOTO,err_handler
  ENDIF 

  ;; check we're not close to zero
  IF speedcut LT 1. THEN BEGIN
     errmsg = 'Proposed speed cut is within 1 arcsec/s of zero.'
     GOTO,err_handler
  ENDIF 

  ;; this is to get it from arcsec/s to degrees/sample, which is the native unit
  speedcut_scal = speedcut / (3600. * tod.sampfreq)

  ;; find the bit values for the mask we want to make
  retvalpos = STRCMP('maskZeroVelocity',tod.mask_bits.name)
  whpl = WHERE(retvalpos EQ 1,countpos)
  IF countpos LE 0 THEN BEGIN
     errmsg = 'Did not find maskZeroVelocity in mask bit definitions.'
     GOTO,err_handler
  ENDIF 
  retval = tod.mask_bits[whpl].bits

  ;; this is for slew
  retvalpos = STRCMP('maskSlew',tod.mask_bits.name)
  whpl = WHERE(retvalpos EQ 1,countpos)
  IF countpos LE 0 THEN BEGIN
     errmsg = 'Did not find maskSlew in mask bit definitions.'
     GOTO,err_handler
  ENDIF 
  slewval = tod.mask_bits[whpl].bits

  ;; first, compute the derivative of the coordinate changes 
  ;; in a representative channel in the tod - which one is 
  ;; arbitrary as if hipe did its job it shouldn't affect the 
  ;; algorithm so I pick zero. 
  ;; thisderiv will then be in deg per timesample
  thisderiv = REFORM(SQRT((DERIV(tod.ra[0,*]) * COS(!DTOR * tod.dec[0,*]))^2 + $
                          DERIV(tod.dec[0,*])^2))

  ;; set the slew value - first find the nominal scan speed
  wfin = WHERE( FINITE(thisderiv), nfin, NCOMPLEMENT=nnonfin )
  IF nfin EQ 0 THEN BEGIN
     errmsg = 'All scan rates non-finite'
     GOTO, err_handler
  ENDIF
  IF nnonfin NE 0 THEN thismed = MEDIAN(thisderiv[wfin]) ELSE $
     thismed = MEDIAN(thisderiv)
  thismed *= tod.sampfreq * 3600.0 ;;to arcsec/sec
  ;;The +2 to 30/60 is to give us a little headroom
  IF ABS(thismed - 30.) LT ABS(thismed - 60.) THEN BEGIN
     slewthres = (30. + 2.) / (3600. * tod.sampfreq)
  ENDIF ELSE BEGIN
     slewthres = (60. + 2.) / (3600. * tod.sampfreq)
  ENDELSE

  ; find the places which are slower than the threshold
  whthres = WHERE(thisderiv LT speedcut_scal,nthres)

  ; ok, if there are instances of slow scanning...
  IF (nthres GT 0) THEN BEGIN
     ; ... loop through the channels...
     FOR imask=0L,tod.nchans-1 DO BEGIN
        ; ... appending the new mask to the old mask.
        tod.mask[imask,whthres] OR= retval
     ENDFOR
  ENDIF

  ; find the places which are faster than the threshold
  whthres = WHERE(thisderiv GT slewthres,nthres)  
  ; ok, if there are instances of slow scanning...
  IF (nthres GT 0) THEN BEGIN
     ; ... loop through the channels...
     FOR imask=0L,tod.nchans-1 DO BEGIN
        ; ... appending the new mask to the old mask.
        tod.mask[imask,whthres] OR= slewval
     ENDFOR
  ENDIF

  ; if we made it to here we're done.
  success = 1b
  RETURN

err_handler:
  IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
  RETURN

END
