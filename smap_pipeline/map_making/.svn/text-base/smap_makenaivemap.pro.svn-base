;+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  function smap_makenaivemap.pro
;;  Aug 26, 2009
;;  Mike Zemcov
;;  This function takes in an obsid, converts it to smap data, 
;;   and makes maps out of it.
;;  Inputs: none
;;  Outputs: none
;;  Options: dir = Directory to look for files in
;;           filelist = List of files to process
;;           obsid = the string hex obsid identifier to process
;;           filt = the string filter flag (default: 'm')
;;           exname = extended file name to append to the output files
;;           sps = data is from SPS, and hence is missing some info
;;           verbose = verbose messaging, 0 = silent
;;           success = the success flag, =1 if success, =0 if not
;;           errmsg = if error, this holds a string explaining the
;;                    error
;;  Notes:
;;    There are several ways to specify where to look for files:
;;       1) Set !SMAP_DATA to point at the data directory
;;       2) Set DIR to point at the data directory
;;       3) Specifically provide a list of files with FILELIST
;;  Changelog: v1.0, MZ, Aug 26, 2009 - orignal
;;             v1.1, MZ, Aug 30, 2009 - moved things around to
;;              accomodate per scan filtering
;;             v1.2, MZ, Sept. 14, 2009 - implemented some
;;              improvements, including better input file handling,
;;              correct astrometry, and added smap_finalizemap.pro
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-
PRO SMAP_MAKENAIVEMAP,FILT=filt,$
                      PIXEL_SIZE=pixsize,$
                      FILELIST=filelist, SPS=sps,$
                      DIR=dir, EXNAME=exname,$
                      VERBOSE=verbose,$
                      SUCCESS=success,$
                      ERRMSG=errmsg

  COMPILE_OPT IDL2

  ; set up the error handling variables
  success = 0b
  errmsg = ''

  ; set up verbosity
  IF ~(KEYWORD_SET(verbose)) THEN verbose = 0b
  IF KEYWORD_SET(verbose) THEN silent=0 ELSE silent=1

  ; DEFINE THE FILTER
  IF ~KEYWORD_SET(filt) THEN BEGIN
     filt = 'm'
     msg = 'FILT not set, assuming ' + filt
     IF KEYWORD_SET(verbose) THEN MESSAGE,msg,/INF
  ENDIF

  ; contains pixsize and band names; this needs to be made more general
  defparams = SMAP_GETDEFPARAMS()
  ; error check that it at least gave back something
  IF N_ELEMENTS(defparams) EQ 0 THEN BEGIN
     errmsg='SMAP_GETDEFPARAMS did not return anything!'
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,errmsg,/INF
     RETURN
  ENDIF

  IF ~KEYWORD_SET(pixsize) THEN pixsize = defparams.pixsize
     

  ; ok, if we don't have a filelist in hand, then we should call 
  ; smap_setupmap and get one
  ; NOTE filelist is necessarily a fully qualified path - if 
  ; one is generating a filelist by hand one has to adhere to the
  ; convention or this will puke
  IF N_ELEMENTS(filelist) EQ 0 THEN BEGIN
     ; get the map setup information.  
     mapparam = SMAP_SETUPMAP(PIXSIZE=pixsize,EXNAME=exname,$
                              MAP250=map250,MAP350=map350,$
                              MAP500=map500,DIR=dir,FILELIST=filelist,$
                              ERRMSG=ssm_errmsg,VERBOSE=verbose,$
                              SUCCESS=ssm_success,SPS=sps) 
     IF ~ ssm_success THEN BEGIN
        errmsg = 'SMAP_SETUPMAP kicked up an error: ' + ssm_errmsg
        IF KEYWORD_SET(verbose) THEN $
           MESSAGE,errmsg,/INF
        RETURN
     ENDIF
  ENDIF ELSE BEGIN
     ; if filelist already exists, check it's in the right format
     fl_success = CHECK_FILELIST(filelist)
     IF ~ fl_success THEN BEGIN
        errmsg = 'CHECK_FILELIST kicked up an error: ' + ssm_errmsg
        IF KEYWORD_SET(verbose) THEN $
           MESSAGE,errmsg,/INF
        RETURN
     ENDIF
     ; get the mapparam structure which we need
     com = addslash(!SMAP_DATA) + '*mapparam_idltod.fits'
     mapparamfile = FILE_SEARCH(com,/FULLY_QUALIFY_PATH)
     mapparam = MRDFITS(mapparamfile,1)
 ENDELSE

  ; ok, this is the loop through scans
  FOR iscan=0,mapparam.nscans-1 DO BEGIN
     ; tell me what scan we're on if I'd like to know
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,'On scan ' + STRCOMPRESS(STRING(iscan+1),/REMOVE_ALL) + $
                ' of ' + STRCOMPRESS(STRING(mapparam.nscans),$
                                     /REMOVE_ALL),/INF

     ; get the scan tod on which to operate 
     tod=MRDFITS(filelist[iscan],1)     

     ; filter the tod
     ; error check
     sft_success = 1
     IF ~sft_success THEN BEGIN
        errmsg='SMAP_FILTERTOD returned with error: ' + sft_errmsg
        IF KEYWORD_SET(verbose) THEN $
           MESSAGE,errmsg,/INF
        RETURN
     ENDIF

     ;;setup weight maps
     IF N_ELEMENTS(map250) NE 0 THEN wt250 = DBLARR(map250.xsize,map250.ysize)
     IF N_ELEMENTS(map350) NE 0 THEN wt350 = DBLARR(map350.xsize,map350.ysize)
     IF N_ELEMENTS(map500) NE 0 THEN wt500 = DBLARR(map500.xsize,map500.ysize)

     ; accumulate it to the map
     SMAP_ACCUMULATEMAP,tod,map250,wt250,map350,wt350,map500,wt500,mapparam,$
                        VERBOSE=verbose,SUCCESS=sam_success,ERRMSG=sam_errmsg
     ; error check
     IF ~sam_success THEN BEGIN
        errmsg='SMAP_ACCUMULATEMAP returned with error: ' + sam_errmsg
        IF KEYWORD_SET(verbose) THEN $
           MESSAGE,errmsg,/INF
        RETURN
     ENDIF

     ; get rid of tod so it doesn't confuse us somehow
     DELVARX,tod

  ENDFOR

  ;; at this point we need to renormalize our accumulated maps - this
  ;; wrapper program takes care of that.  It also goes from variance
  ;; to error.
  SMAP_FINALIZEMAP,map250,wt250,map350,wt350,map500,wt500,$
                   VERBOSE=verbose,SUCCESS=sfm_success,ERRMSG=sfm_errmsg
  ; error check
  IF ~sfm_success THEN BEGIN
     errmsg='SMAP_FINALIZEMAP returned with error: ' + sfm_errmsg
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,errmsg,/INF
     RETURN
  ENDIF
  
  ;;now we have properly normalized maps
  ; write out the three maps - this function is just a 
  ; wrapper for smap_write_fits.pro
  IF ~SMAP_WRITE3COLORS(mapparam.obsids_short,map250,map350,map500,$
                        SILENT=silent,ERRMSG=swc_errmsg, EXNAME=exname) $
  THEN BEGIN
     errmsg = 'SMAP_WRITE3COLORS kicked up an error: ' + swc_errmsg
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,errmsg,/INF
     RETURN
  ENDIF

  ; if we got this far, we wrote three
  ; maps out and have nothing more to do
  success=1b
  RETURN

END
