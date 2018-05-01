;+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  function smap_makeFouriercombinedmap.pro
;;  Sept 14, 2009
;;  Mike Zemcov
;;  This function takes a list of files, converts it to smap data, 
;;   and makes Fourier combined maps out of it.
;;  Inputs: none
;;  Outputs: none
;;  Options: filt = the string filter flag
;;           pixel_size = a fltarr(3) of pixel sizes in degrees
;;           filelist = an (optional) string list of files to analyze
;;           dir = the directory to look in for files if
;;                 filelist isn't specified
;;           exname = extended file name
;;           verbose = verbose messaging, 0 = silent
;;           success = the success flag, =1 if success, =0 if not
;;           errmsg = if error, this holds a string explaining the
;;           error
;;  Changelog: v1.0, MZ, Sept 14, 2009 - orignal
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-
PRO SMAP_MAKEFOURIERCOMBINEDMAP,FILT=filt,$
                                PIXEL_SIZE=pixsize,$
                                FILELIST=filelist,$
                                DIR=dir, $
                                EXNAME=exname,$
                                SPS=sps,$
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
     filt = 'p0'
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
                              SPS=sps,ERRMSG=ssm_errmsg,VERBOSE=verbose,$
                              SUCCESS=ssm_success) 
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
     
     ;;And the file summary info
     fileinfo = PARSE_TIMESTREAM_DETINFO(filelist,SUCCESS=ptd_success,$
                                         ERRMSG=ptd_errmsg,$
                                         NOTEMPERATURE=sps,SIGNAL=sps)
     IF ptd_success EQ 0 THEN BEGIN
        errmsg = "ERROR from parse_timestream_detinfo: " + errmsg
        RETURN
     ENDIF
 ENDELSE

  ;; for the case of the Fourier mapper, make copies of the maps so
  ;; that we can divide up into different scan directions
  map250_Ad = map250
  map350_Ad = map350
  map500_Ad = map500
  map250_Bd = map250
  map350_Bd = map350
  map500_Bd = map500
  map250_Ap = map250
  map350_Ap = map350
  map500_Ap = map500
  map250_Bp = map250
  map350_Bp = map350
  map500_Bp = map500
  DELVARX,map250,map350,map500

  scanangs = FLTARR(mapparam.nscans)

  ; ok, this is the loop through scans
  FOR iscan=0,mapparam.nscans-1 DO BEGIN
     ; tell me what scan we're on if I'd like to know
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,'On scan ' + STRCOMPRESS(STRING(iscan+1),/REMOVE_ALL) + $
                ' of ' + STRCOMPRESS(STRING(mapparam.nscans),$
                                     /REMOVE_ALL),/INF

     ; get the scan tod on which to operate 
     tod=MRDFITS(filelist[iscan],1)     

     ; find the scan direction of this thing
     GET_SCAN_DIRECTION,tod,gsd_out,SCANANGLES=tempscan,VERBOSE=verbose,$
                        SUCCESS=gsd_success,ERRMSG=gsd_errmsg
     IF ~gsd_success THEN BEGIN
        errmsg='GET_SCAN_DIRECTION returned with error: ' + gsd_errmsg
        IF KEYWORD_SET(verbose) THEN $
           MESSAGE,errmsg,/INF
        RETURN
     ENDIF

     scanangs[iscan] = tempscan

     ; if this scandir is 1 or 3 then we can dump in A map
     IF (FIX(gsd_out[2]) EQ 1) THEN BEGIN ;MOD 2 EQ 1) THEN BEGIN
        ;; filter the tod
        SMAP_FILTERTOD,tod,filt,OUTPOLYS=outpolys_A,VERBOSE=verbose,$
                       SUCCESS=sft_success,ERRMSG=sft_errmsg
        ; error check
        IF ~sft_success THEN BEGIN
           errmsg='SMAP_FILTERTOD returned with error: ' + sft_errmsg
           IF KEYWORD_SET(verbose) THEN $
              MESSAGE,errmsg,/INF
           RETURN
        ENDIF

        ;; accumulate data to the map
        SMAP_ACCUMULATEMAP,tod,map250_Ad,map350_Ad,map500_Ad,mapparam,$
                           VERBOSE=verbose,SUCCESS=sam_success,ERRMSG=sam_errmsg
        ;; error check
        IF ~sam_success THEN BEGIN
           errmsg='SMAP_ACCUMULATEMAP returned with error: ' + sam_errmsg
           IF KEYWORD_SET(verbose) THEN $
              MESSAGE,errmsg,/INF
           RETURN
        ENDIF

        ;; accumulate polynomial to map
        SMAP_ACCUMULATEPOLY,tod,outpolys_A,$
                            map250_Ap,map350_Ap,map500_Ap,mapparam,$
                            VERBOSE=verbose,$
                            SUCCESS=sap_success,ERRMSG=sap_errmsg
        ;; error check
        IF ~sap_success THEN BEGIN
           errmsg='SMAP_ACCUMULATEPOLY returned with error: ' + sap_errmsg
           IF KEYWORD_SET(verbose) THEN $
              MESSAGE,errmsg,/INF
;          RETURN
        ENDIF
     ENDIF
     ; if this scandir is 2 or 4 then we can dump in B map
     IF (FIX(gsd_out[2]) EQ 2) THEN BEGIN ;MOD 2 EQ 0) THEN BEGIN

        ;; filter the tod
        SMAP_FILTERTOD,tod,filt,OUTPOLYS=outpolys_B,VERBOSE=verbose,$
                       SUCCESS=sft_success,ERRMSG=sft_errmsg
        ;; error check
        IF ~sft_success THEN BEGIN
           errmsg='SMAP_FILTERTOD returned with error: ' + sft_errmsg
           IF KEYWORD_SET(verbose) THEN $
              MESSAGE,errmsg,/INF
           RETURN
        ENDIF

        ;; accumulate it to the map
        SMAP_ACCUMULATEMAP,tod,map250_Bd,map350_Bd,map500_Bd,mapparam,$
                           VERBOSE=verbose,SUCCESS=sam_success,ERRMSG=sam_errmsg
        ;; error check
        IF ~sam_success THEN BEGIN
           errmsg='SMAP_ACCUMULATEMAP returned with error: ' + sam_errmsg
           IF KEYWORD_SET(verbose) THEN $
              MESSAGE,errmsg,/INF
           RETURN
        ENDIF

        ;; accumulate polynomial to map
        SMAP_ACCUMULATEPOLY,tod,outpolys_B,$
                            map250_Bp,map350_Bp,map500_Bp,mapparam,$
                            VERBOSE=verbose,$
                            SUCCESS=sap_success,ERRMSG=sap_errmsg
        ;; error check
        IF ~sap_success THEN BEGIN
           errmsg='SMAP_ACCUMULATEPOLY returned with error: ' + sap_errmsg
           IF KEYWORD_SET(verbose) THEN $
              MESSAGE,errmsg,/INF
           RETURN
        ENDIF
     ENDIF
     
     ;; get rid of tod so it doesn't confuse us somehow
     DELVARX,tod

  ENDFOR

  ;; at this point we need to renormalize our accumulated A maps - this
  ;; wrapper program takes care of that
  SMAP_FINALIZEMAP,map250_Ad,map350_Ad,map500_Ad,$
                   VERBOSE=verbose,SUCCESS=sfm_success,ERRMSG=sfm_errmsg
  ; error check
  IF ~sfm_success THEN BEGIN
     errmsg='SMAP_FINALIZEMAP returned with error: ' + sfm_errmsg
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,errmsg,/INF
     RETURN
  ENDIF
  
  ;; at this point we need to renormalize our accumulated B maps - this
  ;; wrapper program takes care of that
  SMAP_FINALIZEMAP,map250_Bd,map350_Bd,map500_Bd,$
                   VERBOSE=verbose,SUCCESS=sfm_success,ERRMSG=sfm_errmsg
  ; error check
  IF ~sfm_success THEN BEGIN
     errmsg='SMAP_FINALIZEMAP returned with error: ' + sfm_errmsg
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,errmsg,/INF
     RETURN
  ENDIF

  SMAP_FINALIZEMAP,map250_Ap,map350_Ap,map500_Ap,$
                   VERBOSE=verbose,SUCCESS=sfm_success,ERRMSG=sfm_errmsg
  ; error check
  IF ~sfm_success THEN BEGIN
     errmsg='SMAP_FINALIZEMAP returned with error: ' + sfm_errmsg
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,errmsg,/INF
     RETURN
  ENDIF
  
  ;; at this point we need to renormalize our accumulated B maps - this
  ;; wrapper program takes care of that
  SMAP_FINALIZEMAP,map250_Bp,map350_Bp,map500_Bp,$
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
  exname='Ad'
  IF ~SMAP_WRITE3COLORS(mapparam.obsids_short,map250_Ad,map350_Ad,map500_Ad,$
                        SILENT=silent,ERRMSG=swc_errmsg, EXNAME=exname) $
  THEN BEGIN
     errmsg = 'SMAP_WRITE3COLORS kicked up an error: ' + swc_errmsg
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,errmsg,/INF
     RETURN
  ENDIF

  exname='Bd'
  IF ~SMAP_WRITE3COLORS(mapparam.obsids_short,map250_Bd,map350_Bd,map500_Bd,$
                        SILENT=silent,ERRMSG=swc_errmsg, EXNAME=exname) $
  THEN BEGIN
     errmsg = 'SMAP_WRITE3COLORS kicked up an error: ' + swc_errmsg
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,errmsg,/INF
     RETURN
  ENDIF

  exname='Ap'
  IF ~SMAP_WRITE3COLORS(mapparam.obsids_short,map250_Ap,map350_Ap,map500_Ap,$
                        SILENT=silent,ERRMSG=swc_errmsg, EXNAME=exname) $
  THEN BEGIN
     errmsg = 'SMAP_WRITE3COLORS kicked up an error: ' + swc_errmsg
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,errmsg,/INF
     RETURN
  ENDIF

  exname='Bp'
  IF ~SMAP_WRITE3COLORS(mapparam.obsids_short,map250_Bp,map350_Bp,map500_Bp,$
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
