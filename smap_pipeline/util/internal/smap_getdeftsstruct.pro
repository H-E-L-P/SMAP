;+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  function smap_getdeftsstruct.pro
;;
;;  This function takes in the size of a time stream data structure
;;   in the form (nsamps, nchans, ntherms) and returns the data
;;   structure initialized to default values (ie 0's).
;;
;;  Inputs: nsamps = integer number of samples to make structure hold
;;          nchans = number of bolo channels to make structure hold
;;          ntherms = number of thermometry channels to make structure
;;                     hold
;;  Outputs: dataout = structure into which data can be stored.
;;  Options: verbose = logical of whether to be verbose or not
;;           success = logical success flag, 0 if failure, 1 if
;;                      success
;;           noquality = Don't include room for quality information
;;           notherm = Don't include room for temperature information
;;           errmsg = string message associated with error, if succes
;;                     is 0
;;  Changelog: Aug 20, 2009, MZ, v1.0
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-
FUNCTION SMAP_GETDEFTSSTRUCT,nsamps,nchans,ntherms,$
                             NOTHERM=notherm,NOQUALITY=noquality,$
                             VERBOSE=verbose,SUCCESS=success,ERRMSG=errmsg

  COMPILE_OPT IDL2

  ; set some default values
  success = 0b
  errmsg = ''

  ; check for verbosity, make is false if not specified
  IF NOT(KEYWORD_SET(verbose)) THEN verbose = 0b

  ; check nsamps makes some sense
  IF nsamps EQ 0 THEN BEGIN
     errmsg = "Number of samples equals zero!"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF

  ; check nchans makes some sense
  IF nchans EQ 0 THEN BEGIN
     errmsg = "Number of channels equals zero!  Expect 288."
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF

  ; check ntherms makes some sense
  IF ntherms EQ 0 AND ~ KEYWORD_SET( notherm ) THEN BEGIN
     errmsg = "Number of thermometers equals zero!  Expect 6."
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF



  ; ok, if we got to here we can make the struct
  ; no uL allowed at mrdfits doesn't know about it
  dataout = {progenitorfile:'',$
             shortfile:"",$
             dateobs: "",$
             bbid:0uL,$
             obsid:0uL,$
             object:'',$
             nsamps:LONG(nsamps),$
             nchans:LONG(nchans),$
             sampfreq: !VALUES.D_NAN,$
             samptime:DBLARR(nsamps),$
             chan:STRARR(nchans),$
             islight:BYTARR(nchans),$
             signal:DBLARR(nchans,nsamps),$
             ra:DBLARR(nchans,nsamps),$
             dec:DBLARR(nchans,nsamps),$
             masktime:DBLARR(nsamps),$
             mask:ULONARR(nchans,nsamps),$
             proj_stored:0B}

  IF ~ KEYWORD_SET( noquality ) THEN $
     dataout = CREATE_STRUCT(dataout, 'adcerrors', DBLARR(nchans),$
                             'truncation', DBLARR(nchans))
  IF ~ KEYWORD_SET( notherm ) THEN $
     dataout = CREATE_STRUCT(dataout,'ntherms', LONG(ntherms),$
                             'temperaturetime', DBLARR(nsamps),$
                             'temperature', DBLARR(ntherms,nsamps),$
                             'therm',STRARR(ntherms))


  ; get me outta here.
  success = 1b
  RETURN,dataout

END
  
