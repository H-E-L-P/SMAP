;+
;NAME
; remove_timestream_poly
;PURPOSE
; Removes a polynomial from a L1 timestream, also
; returning the coefficients
;CATEGORY
; Herschel SPIRE timestream
;USAGE
; coeff_info = remove_timestream_poly( data )
;INPUT/OUTPUTS
; data              L1 data read into a timestream by smap_getlevel1.
;                    Modified on output.
;RETURNS
; A structure holding information about the polynomial removed from
;  each channel.  The structure is of the form:
;   .T0             Zero of time used
;   .ORDER          Order of the polynomial
;   .CHAN           The names of the channels (same order as data)
;   .CHAN_SUCCESS   1b if this channel was subtracted, 0b if not
;   .COEFFS         An array of coeffs for each channel.
; The removed polynomial can be reconstructed using:
;    POLY( data.samptim - coeff_info.t0, coeff_info[idxbol,*] )
; where idxbol is the index of the bolometer in question unless
; poly_order is 0, in which case you just subtract the only value.
;OPTIONAL INPUTS
; badmask         Mask values to exclude from the fits.
;                  The default is 1, usually the maskMaster bit
; poly_order      Degree of the polynomial ( 0 to 6 ).  Def: 5
; sigma_cut       Cut sigma if poly_order = 0 (i.e., mean subtraction
;                  only). The default is 3.
;OPTIONAL OUTPUTS
; success        1 if it worked, 0 if it didn't
; errmsg         An error message if a problem was encountered
;ROUTINES CALLED
; SMAP_ROBUST_POLY_FIT, RESISTANT_MEAN
;MODIFICATION HISTORY
; Author: Alex Conley, Aug 26th, 2009
;-

FUNCTION remove_timestream_poly, data, BADMASK=badmask, POLY_ORDER=poly_order,$
                                 SIGMA_CUT=sigma_cut, VERBOSE=verbose, $
                                 SUCCESS=success, ERRMSG=errmsg
  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg  = ''

  ;;input checks
  IF N_ELEMENTS( data ) EQ 0 THEN BEGIN
     errmsg = "No input data"
     GOTO, err_handling
  ENDIF
  IF SIZE( data, /TNAME ) NE 'STRUCT' THEN BEGIN
     errmsg = "Input data is not structure"
     GOTO, err_handling
  ENDIF
  IF N_ELEMENTS( poly_order ) EQ 0 THEN poly_order = 3
  IF poly_order LT 0 OR poly_order GT 6 THEN BEGIN
     errmsg = "Invalid poly_order: "+STRING(poly_order)
     GOTO, err_handling
  ENDIF
  IF N_ELEMENTS( badmask ) EQ 0 THEN int_badmask = 1uL ELSE $
     int_badmask = badmask
  IF int_badmask NE 0 THEN checking_mask = 1b ELSE checking_mask=0b
  IF N_ELEMENTS( sigma_cut ) EQ 0 THEN sigma_cut = 3.0
  IF poly_order EQ 0 AND sigma_cut LE 0.0 THEN BEGIN
     errmsg = "Invalid sigma_cut for 0th order poly removal"
     GOTO, err_handling
  ENDIF
  tags_required = ['SAMPTIME','CHAN','SIGNAL']
  IF checking_mask THEN tags_required = [tags_required,'MASK']
  FOR i=0, N_ELEMENTS(tags_required)-1 DO BEGIN
     IF ~ TAG_EXIST( data, tags_required[i], /TOP_LEVEL ) THEN BEGIN
        errmsg = "Couldn't find required tag: "+tags_required[i]
        GOTO, err_handling
     ENDIF
  ENDFOR

  nchannels = N_ELEMENTS( data.chan )
  t0 = MEAN( data.samptime)
  IF ~ FINITE(t0) THEN BEGIN
     errmsg = "Couldn't find finite sampletimes"
     GOTO, err_handling
  ENDIF
  timevals = data.samptime - t0
  coeff_info = { t0: t0, chan: data.chan, order: poly_order, $
                 chan_success: BYTARR(nchannels),$
                 coeffs: DBLARR( nchannels, poly_order+1 ) }
  FOR i = 0, nchannels-1 DO BEGIN
     IF checking_mask THEN BEGIN
        wuse = WHERE( FINITE( data.signal[i,*] ) AND $
                      ( (data.mask[i,*] AND int_badmask) EQ 0 ), nuse,$
                      NCOMPLEMENT=nbad)
     ENDIF ELSE BEGIN
        ;;Get some unmasked NaNs from SPS sometimes, so check
        wuse = WHERE( FINITE( data.signal[i,*] ), nuse,$
                      NCOMPLEMENT=nbad)
     ENDELSE
     IF nuse LT (poly_order+2)*(poly_order+1) THEN BEGIN
        IF KEYWORD_SET(verbose) THEN $
           MESSAGE," Skipping "+data.chan[i]+" as insufficient good points",/INF
        CONTINUE
     ENDIF
     
     ;;Poly fit
     IF poly_order EQ 0 THEN BEGIN
        ;;Mean subtraction only
        IF nbad NE 0 THEN BEGIN
           RESISTANT_MEAN, data.signal[i,wuse], sigma_cut, mn, sm, numrej
        ENDIF ELSE BEGIN
           RESISTANT_MEAN, data.signal[i,*], sigma_cut, mn, sm, numrej
        ENDELSE
        IF numrej EQ nuse OR ~ FINITE( mn ) THEN BEGIN
           IF KEYWORD_SET(verbose) THEN $
              MESSAGE,"Unsuccessful fit to cahnnel: "+data.chan[i],/INF
           CONTINUE
        ENDIF
        coeffs = [ mn ]
     ENDIF ELSE BEGIN
        IF nbad NE 0 THEN BEGIN
           coeffs = SMAP_ROBUST_POLY_FIT(timevals[wuse], data.signal[i,wuse],$
                                         poly_order, /DOUBLE )
        ENDIF ELSE BEGIN
           coeffs = SMAP_ROBUST_POLY_FIT(timevals, data.signal[i,*],$
                                         poly_order, /DOUBLE )
        ENDELSE
        IF N_ELEMENTS( coeffs ) EQ 1 AND coeffs[0] EQ 0.0 THEN BEGIN
           IF KEYWORD_SET(verbose) THEN $
              MESSAGE,"Unsuccessful fit to cahnnel: "+data.chan[i],/INF
           CONTINUE
        ENDIF
        IF N_ELEMENTS(coeffs) GT poly_order+1 THEN BEGIN
           IF KEYWORD_SET(verbose) THEN $
              MESSAGE,"Poor fit to cahnnel: "+data.chan[i]+"; skipping",/INF
           CONTINUE
        ENDIF
     ENDELSE

     ;;Ok, it worked
     ;;Subtract.  Don't use YFIT from robust_poly so we subtract from
     ;; masked points too
     data.signal[i,*] -= POLY( timevals, coeffs )
     coeff_info.chan_success[i] = 1b
     coeff_info.coeffs[i,*] = coeffs
  ENDFOR

  wgood = WHERE( coeff_info.chan_success, ngood )
  IF ngood EQ 0 THEN BEGIN
     errmsg = "No channels successfully fit"
     GOTO, err_handling
  ENDIF
  IF KEYWORD_SET( verbose ) THEN $
     MESSAGE,STRING(ngood,nchannels,$
                    FORMAT='(I0," of ",I0," channels poly subtracted")'),/INF

  success = 1b
  RETURN, coeff_info

  err_handling: 
  IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
  RETURN, !VALUES.F_NAN
  
END
