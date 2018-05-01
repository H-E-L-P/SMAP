;+
;NAME
; mask_noresponse_channels
;PURPOSE
; Sets the mask bits on channels where the signal is unvarying.
;USAGE
; mask_noresponse_channels, tod, MASKNAME=, THRESHVAL=, SUCCESS=, ERRMSG=
;INPUTS
; tod          The TOD, which is modified on output.  Can be a pointer
;               or a struct
;OPTIONAL INPUTS
; threshval    Minimum amount of variation below which the scan is
;               flagged, in Jy (def: 0.0001)
; maskname     Which mask bit to set (def: "maskManual")
;MODIFICATION HISTORY
; Author: Alex Conley, May 2012
;-

PRO mask_noresponse_channels, tod, MASKNAME=maskname, SUCCESS=success, $
                              ERRMSG=errmsg, VERBOSE=verbose, $
                              THRESHVAL=threshval
  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg  = ''

  IF N_ELEMENTS(tod) EQ 0 THEN BEGIN
     errmsg = "No TOD passed in"
     GOTO, err_handler
  END
  IF SIZE(tod,/TNAME) EQ 'POINTER' THEN use_ptr = 1b $
     ELSE IF SIZE(tod,/TNAME) EQ 'STRUCT' THEN use_ptr = 0b $
     ELSE BEGIN
     errmsg = "TOD is neither a pointer nor a structure"
     GOTO,err_handler
  ENDELSE
  IF N_ELEMENTS(maskname) EQ 0 THEN maskname = "maskUser"
  IF N_ELEMENTS(threshval) EQ 0 THEN threshval = 0.0001
  IF threshval LE 0.0 THEN BEGIN
     errmsg = "Invalid (non-positive) threshold value"
     GOTO,err_handler
  ENDIF

  IF use_ptr THEN BEGIN
     IF ~ TAG_EXIST( *tod, 'mask_bits', /TOP_LEVEL ) THEN BEGIN
        errmsg = "TOD doesn't have mask bit information"
        GOTO,err_handler
     ENDIF
     mskbt = construct_mask_bitmask( maskname, $
                                     (*tod).mask_bits,$
                                     SUCCESS=csuccess,$
                                     ERRMSG=cerrmsg )
     IF ~ csuccess THEN BEGIN
        errmsg = cerrmsg
        GOTO, err_handler
     ENDIF

     wbolos = WHERE( (*tod).islight, nbolos )
     IF nbolos EQ 0 THEN BEGIN
        errmsg = "No light bolomters"
        GOTO, err_handler
     ENDIF
     
     FOR i=0,nbolos-1 DO BEGIN 
        minval = MIN((*tod).signal[wbolos[i],*],MAX=maxval,/NAN)
        IF ~ FINITE(minval) || (ABS(maxval-minval) LT threshval) THEN $
           (*tod).mask[wbolos[i],*] OR= mskbt
     ENDFOR
  ENDIF ELSE BEGIN
     IF ~ TAG_EXIST( tod, 'mask_bits', /TOP_LEVEL ) THEN BEGIN
        errmsg = "TOD doesn't have mask bit information"
        GOTO,err_handler
     ENDIF
     mskbt = construct_mask_bitmask( maskname, $
                                     tod.mask_bits,$
                                     SUCCESS=csuccess,$
                                     ERRMSG=cerrmsg )
     IF ~ csuccess THEN BEGIN
        errmsg = cerrmsg
        GOTO, err_handler
     ENDIF

     wbolos = WHERE( tod.islight, nbolos )
     IF nbolos EQ 0 THEN BEGIN
        errmsg = "No light bolomters"
        GOTO, err_handler
     ENDIF
     
     FOR i=0,nbolos-1 DO BEGIN 
        minval = MIN(tod.signal[wbolos[i],*],MAX=maxval,/NAN)
        IF ~ FINITE(minval) || (ABS(maxval-minval) LT threshval) THEN $
           tod.mask[wbolos[i],*] OR= mskbt
     ENDFOR
  ENDELSE

  success = 1b
  RETURN

err_handler:
  IF KEYWORD_SET( verbose ) THEN MESSAGE,"Encountered error: "+errmsg
  RETURN
END
