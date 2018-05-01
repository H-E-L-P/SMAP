;+
;NAME
; subtract_timestream
;PURPOSE
; Subtracts one timestream from another.  Typically, the subtrahend
; is output from something like get_timestream_from_map, and
; the minuend is real data from smap_getlevel1 or the like.
;USAGE
; subtract_timestream, minuend, subtrahend
;ARGUMENTS
; minuend                 The timestream to subtract from.  Modified
;                          on output.
; subtrahend              The thing to subtract from minuend.
;KEYWORDS
; verbose                 Runs in verbose mode
; masksources             Code is being used to mask sources, not
;                          really subtract.  In this case, the
;                          subtrahend will be non-finite where there
;                          is a mask.  The master bit will be set at those
;                          locations.
;OPTIONAL INPUTS
;  maskthresh             Threshold to add mask bit saying that a source has
;                          been subtracted from the specified position.  This
;                          is in Jy.  If 0 or negative, mask bits are
;                          not added.
;  newmaskbit             New mask bit to add for masked pixels.  Must
;                          be provided if maskthresh is used, and be a power
;                          of 2.  It is up to the caller to add this
;                          to the header if this is re-written.
;                          Ignored if masksources set.
;OPTIONAL OUTPUTS
; success                 1b on success, 0b on failure
; errmsg                  Error message for failure
;MODIFICATION HISTORY
; Author: Alex Conley, Sep 1, 2009
;-

PRO subtract_timestream, minuend, subtrahend, ADD=add, VERBOSE=verbose,$
                         MASKTHRESH=maskthresh, NEWMASKBIT=newmaskbit,$ 
                         MASKSOURCES=masksources, SUCCESS=success, $
                         ERRMSG=errmsg
  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg  = ''

  ;;Input checks
  IF N_ELEMENTS( minuend ) EQ 0 THEN BEGIN
     errmsg = "Minuend not provided"
     GOTO, err_handler
  ENDIF
  IF SIZE( minuend, /TNAME ) NE 'STRUCT' THEN BEGIN
     errmsg = "Minuend is not structure"
     GOTO, err_handler
  ENDIF
  tags_required = ['SHORTFILE','NSAMPS','CHAN','SIGNAL']
  IF N_ELEMENTS(maskthresh) NE 0 && maskthresh GT 0.0 THEN $
     tags_required = [tags_required,'MASK']
  wpresent = WHERE_ARRAY( TAG_NAMES(minuend), tags_required, npresent )
  IF npresent NE N_ELEMENTS(tags_required) THEN BEGIN
     wmissing = MISSING( TAG_NAMES(minuend), tags_required, nmissing )
     errmsg = "Missing tags in minuend structure: "+$
              STRJOIN(tags_required[wmissing],',')
     GOTO, err_handler
  ENDIF
  IF N_ELEMENTS( subtrahend ) EQ 0 THEN BEGIN
     errmsg = "Subtrahend not provided"
     GOTO, err_handler
  ENDIF
  IF SIZE( subtrahend, /TNAME ) NE 'STRUCT' THEN BEGIN
     errmsg = "Subtrahend is not structure"
     GOTO, err_handler
  ENDIF
  tags_required = ['SHORTFILE','NSAMPS','NCHAN','CHAN','SIGNAL']
  wpresent = WHERE_ARRAY( TAG_NAMES(subtrahend), tags_required, npresent )
  IF npresent NE N_ELEMENTS(tags_required) THEN BEGIN
     wmissing = MISSING( TAG_NAMES(subtrahend), tags_required, nmissing )
     errmsg = "Missing tags in subtrahend structure: "+$
              STRJOIN(tags_required[wmissing],',')
     GOTO, err_handler
  ENDIF
  IF N_ELEMENTS(maskthresh) NE 0 && maskthresh GT 0.0 THEN BEGIN
     IF N_ELEMENTS( newmaskbit ) EQ 0 THEN BEGIN
        errmsg = "New mask bit must be provided if masking is to be applied"
        GOTO, err_handler
     ENDIF
     ;;Check it's a power of 2
     IF ( newmaskbit AND (newmaskbit-1) ) NE 0 THEN BEGIN
        errmsg = "Newmaskbit is not a power of 2, which is required"
        GOTO, err_handler
     ENDIF
  ENDIF


  IF minuend.shortfile NE subtrahend.shortfile THEN BEGIN
     errmsg = "Minuend and subtrahend represent different input files: "+$
              minuend.shortfile+" - "+subtrahend.shortfile
     GOTO, err_handler
  ENDIF

  IF minuend.nsamps NE subtrahend.nsamps THEN BEGIN
     errmsg = "Minuend and subtrahend have different numbers of samples: "+$
              STRING(minuend.nsamps,subtrahend.nsamps,FORMAT='(I0,"-",I0)')
     GOTO, err_handler
  ENDIF

  ;;Make sure every channel in subtrahend can be found in minuend
  wpresent = WHERE_ARRAY( minuend.chan, subtrahend.chan, npresent )
  IF npresent NE subtrahend.nchan THEN BEGIN
     wmissing = MISSING( minuend.chan, subtrahend.chan )
     errmsg = "Missing channels in minuend found in subtrahend: "+$
              STRJOIN( subtrahend.chan[wmissing],',')
     GOTO, err_handler
  ENDIF

  ;;Get index into minuend of channels in subtrahend
  ;;Hopefully, minuend is sorted and we can bisect to it
  ;;Should do check to see if they are already in the same order,
  ;; which would be very fast
  min_sort = SORT( minuend.chan )
  wunsort = WHERE( minuend.chan NE minuend.chan[min_sort], nunsort )
  IF nunsort EQ 0 THEN BEGIN
     ;;Already checked they are all present, so no need to verify output
     idxs = VALUE_LOCATE( minuend.chan, subtrahend.chan )
  ENDIF ELSE BEGIN
     ;;So, sort by hand.  Sigh
     idxs = min_sort[ VALUE_LOCATE( minuend.chan[min_sort], subtrahend.chan ) ]
  ENDELSE

  IF KEYWORD_SET( masksources ) THEN BEGIN
     ;;It would be nice not to use a for loop here, but the
     ;; re-ordering makes the simple non-loop solution fail miserably
     IF TAG_EXIST( minuend, 'mask_bits', /TOP_LEVEL ) THEN BEGIN
        master_bit = construct_mask_bitmask('maskMaster',minuend.mask_bits,$
                                            /REQUIRE,SUCCESS=mb_success)
     ENDIF ELSE mb_success=0b
     FOR i=0, subtrahend.nchan-1 DO BEGIN
        wmask = WHERE( ~ FINITE( subtrahend.signal[i,*] ), nmask )
        IF nmask NE 0 THEN BEGIN
           minuend.signal[idxs[i],wmask] = !VALUES.D_NAN
           IF mb_success THEN minuend.mask[idxs[i],wmask] = master_bit
        ENDIF
     ENDFOR
  ENDIF ELSE BEGIN
     ;;Do the subtraction
     IF KEYWORD_SET(add) THEN $
        minuend.signal[ idxs, * ] += subtrahend.signal ELSE $
           minuend.signal[ idxs, * ] -= subtrahend.signal
     ;;And the mask
     IF N_ELEMENTS(maskthresh) NE 0 && maskthresh GT 0.0 THEN BEGIN
        ;;Figure out which fluxes exceed mask threshold
        ;;There's probably some fancy non-for loop way to do this, but
        ;; this works.
        FOR i=0, subtrahend.nchan-1 DO BEGIN
           wmask = WHERE( subtrahend.signal[i,*] GE maskthresh OR $
                          ( ~FINITE(subtrahend.signal[i,*]) ), nmask )
           IF nmask NE 0 THEN BEGIN
              minuend.mask[idxs[i],wmask] = newmaskbit
           ENDIF
        ENDFOR
     ENDIF
  ENDELSE

  success = 1b
  RETURN

err_handler:
  IF KEYWORD_SET( verbose ) THEN MESSAGE,errmsg,/INF
  RETURN

END
