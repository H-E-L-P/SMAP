;+
; NAME:
;   construct_manual_mask
; CATEGORY
;   Herschel SPIRE SMAP pipeline
; PURPOSE:
;   Found in general processing that some scans have a variety of blemishes 
;   in them which are not caught by HIPE or SMAP processing - this function 
;   provides the ability to mask those scans.
; CALLING SEQUENCE:
;   construct_manual_mask,tod,badobsid
; INPUTS:
;   tod = standard SMAP tod.
;   badobsid = pointer array to string arrays, each of which is 
;              3 (or 5) elements.  For a 
;              given mask, array is of the form [band,obsid,bbid], for 
;              example ['PSW','1342186109','a103002c'].  To match file 
;              naming convention, obsid is in dec and bbid is in hex, 
;              all strings. Wildcards are supported.  For example:
;              ['PSWA*','134218610[7-8]','a10300?c']. Optional sample 
;              extension allows for addional params [startsamp,
;              nsamp], the starting sample and number of samples to mask
; KEYWORDS
;   success = success flag (1=success)
;   verbose = verbose output
;   errmsg = informational error messages on failure.
; OUTPUTS:
;   A modified tod.mask which contains manually masked scans.
; OPTIONAL OUTPUTS
;   success      1b if it succeeded, 0 if not
;   errmsg       informational error messages on failure.
; NOTES
;
; MODIFICATION HISTORY:
;   Author: Michael Zemcov, Apr 2011, original version.
;
;   2012-07-24 (GM): extend badobsid to allow optional sample masking
;                    requires change of format to badobsid structure
;
;-

PRO construct_manual_mask, tod, $                    
                           badobsid,$
                           SUCCESS=success,$
                           ERRMSG=errmsg,$
                           VERBOSE=verbose

  ;; standard init
  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  IF ~(N_ELEMENTS(verbose)) THEN verbose=0 ELSE verbose = verbose

  ;; find instances where the current obsid and bad obsids overlap
  ;; have to do a little string massaging to make sure STRMATCH will
  ;; work
  ;;We do this as a loop because we want to match wildcards, so the
  ;; search string must be the second string.  But STRMATCH only
  ;; supports single search strings
  
  nbadspecs = N_ELEMENTS(badobsid)

  ;; with new badobsid format (as of 2012-07-24), need more complex indexing
  this_badobsid = STRARR(nbadspecs)
  this_badbbid  = STRARR(nbadspecs)
  this_badbolo  = STRARR(nbadspecs)
  this_badssamp = REPLICATE(-1L, nbadspecs)
  this_badnsamp = REPLICATE(-1L, nbadspecs)

  FOR i=0,nbadspecs-1 DO BEGIN
     this_badbolo[i]  = (*badobsid[i])[0]
     this_badobsid[i] = (*badobsid[i])[1]
     this_badbbid[i]  = (*badobsid[i])[2]

     IF N_ELEMENTS(*badobsid[i]) EQ 5 THEN BEGIN
        this_badssamp[i] = (*badobsid[i])[3]
        this_badnsamp[i] = (*badobsid[i])[4]
     ENDIF
  ENDFOR

  mtbadobsid = BYTARR(nbadspecs)
  FOR i=0,nbadspecs-1 DO $
     mtbadobsid[i] = STRMATCH(STRCOMPRESS(tod.obsid,/REMOVE_ALL),$
                              STRCOMPRESS(this_badobsid[i],/REMOVE_ALL))
  whbadobsid = WHERE(mtbadobsid,countobsid)

  ;; find instances where the current bbid and bad bbids overlap
  ;; have to do a little string massaging to make sure STRMATCH will work
  thisbbid  = STRCOMPRESS(STRLOWCASE(TO_HEX(tod.bbid)),/REMOVE_ALL)
  matchbbid = STRCOMPRESS(STRLOWCASE(this_badbbid),/REMOVE_ALL)
  mtbadbbid = BYTARR(nbadspecs)
  FOR i=0,nbadspecs-1 DO $
     mtbadbbid[i] = STRMATCH(thisbbid,matchbbid[i])
  whbadbbid = WHERE(mtbadbbid,countbbid)

  ;; if we've found both obsids and bbids that match, look more closely.
  IF countobsid GT 0 AND countbbid GT 0 THEN BEGIN
     whbad = WHERE( mtbadobsid AND mtbadbbid, countbad )
     ;;If we have matches, then mask
     IF countbad NE 0 THEN BEGIN

        ;;Looks like we have a match, so find the mask bit
        retvalpos = STRCMP('maskManual',tod.mask_bits.name)
        whpl = WHERE(retvalpos EQ 1,countpos)
        IF countpos EQ 0 THEN BEGIN
           errmsg = "Didn't find any manual mask bits in "+tod.shortfile
           GOTO,err_handler
        ENDIF
        IF countpos GT 1 THEN BEGIN
           errmsg = "Found more than one manual mask bit in "+tod.shortfile
           GOTO,err_handler
        ENDIF
        mbit = tod.mask_bits[whpl[0]].bits

        FOR idx=0, countbad-1 DO BEGIN
           ;; useful informational message that something's happening
           IF KEYWORD_SET(verbose) THEN BEGIN
              mystring = 'Manually masking scan in ' + $
                         this_badbolo[whbad[idx]] + $
                         ' at obsid ' + this_badobsid[whbad[idx]] + $
                         ', bbid ' + this_badbbid[whbad[idx]] + '.'
              MESSAGE,mystring,/INFORMATIONAL
           ENDIF

           ;; create band search string to find only light bolometers
           bolstr = this_badbolo[whbad[idx]]
           IF STRLEN(bolstr) EQ 3 THEN bolstr += '[A-J]*'
           ;; now find band matches
           whband = WHERE(STRMATCH(tod.chan,bolstr),countband)
           ;; just make sure we hit some devices!
           ;; add the mask bit to the mask for the entire scan
           IF countband GT 0 THEN BEGIN
              ;; check for sample masking
              s0 = this_badssamp[whbad[idx]]
              ns = this_badnsamp[whbad[idx]]
              IF s0 GE 0 AND ns GE 0 THEN BEGIN
                 tind = LINDGEN(tod.nsamps)
                 thismaskind = WHERE(tind GE s0 AND tind LT s0+ns, thisnmask)
                 IF thisnmask GT 0 THEN $
                    FOR b=0,countband-1 DO $
                       tod.mask[whband[b],thismaskind] OR= mbit
              ENDIF ELSE $
                 tod.mask[whband,*] OR= mbit
           ENDIF
        ENDFOR
     ENDIF

  ENDIF

  ;; if we made it to here we're done.
  success = 1b
  RETURN

  ;; some error handling
err_handler:
  IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
  RETURN


END
