;+
;NAME 
; MAKE_NOISELESS_SIM
;PURPOSE
; Generates a set of noiseless sims for eg computing map making 
;  transfer functions.
;USAGE
; MAKE_NOISELESS_SIM,NSIM=100,FILT='p1'
;INPUTS
; OSIM = the number of the first sim (def: 0)
; NSIM = number of sims to perform (def:1)
; FILT = filter to apply to TOD (def:'m')
; PIXEL_SIZE = size of pixels to use (def: 6")
; DIR = directory of files to read (def: !SMAP_HIPE)
; INFILES = list of files to read (def: all in DIR)
; EXNAME = extended naming
; SCANDOWN = lowest scan number to use (def: 1)
; SCANUP = highest scan number to use (def: end of dir listing)
; VERBOSE = verbose flag (def:0)
; BADBOLS = list of badbolometers (def: 'PSWD15','PSWC12','PSWG8',
;            'PSWG11','PSWA10','PSWA11','PSWA13')
; ITERMAP_PARAMS = list of itermap parameters to give (def: first_gain:0)
; EXCLUDEMASK = list of flags to exclude in map making 
;               (def: "maskUncorrectedTruncation","maskDead","maskNoisy",
;               "maskSlow","maskZeroVelocity")
; VERBOSE = verbosity (def: 0)
;OPTIONAL OUTPUTS
; SUCCESS = success flaf (1 if ran through to end)
; ERRMSG = errmsg if failed
;MODIFICATION HISTORY
; Author: Mike Zemcov, Feb 2010
;-

PRO MAKE_NOISELESS_SIM,$
   OSIM=osim,NSIM=nsim,$
   FILT=filt,PIXEL_SIZE=pixel_size,$
   DIR=dir, INFILES=infiles, EXNAME=exname,$
   SCANDOWN=scandown,SCANUP=scanup,BADBOLOS=badbols,$
   ITERMAP_PARAMS=itermap_params, EXCLUDEMASK=excludemask, $$ 
   VERBOSE=verbose,SUCCESS=success,ERRMSG=errmsg
  
  COMPILE_OPT IDL2

  ;; set up the error handling variables
  success = 0b
  errmsg = ''
  
  IF N_ELEMENTS(exname) EQ 0 THEN exname=""
  
  ;; set up verbosity
  IF ~(KEYWORD_SET(verbose)) THEN verbose = 0b
  IF KEYWORD_SET(verbose) THEN silent=0 ELSE silent=1

  IF N_ELEMENTS(excludemask) EQ 0 THEN $
     excludeMask = ["maskUncorrectedTruncation","maskDead",$
                    "maskNoisy","maskSlow","maskZeroVelocity"] ELSE BEGIN
     IF SIZE( excludemask, /TNAME ) NE 'STRING' THEN BEGIN
        errmsg = "Didn't get expected string type for excludemask, but "+$
                 SIZE( excludemask, /TNAME )
        GOTO, err_handler
     ENDIF
  ENDELSE
  
  ;; set the list of bad bolometers
  IF N_ELEMENTS(badbols) EQ 0 THEN BEGIN
     badbols = ['PSWD15','PSWC12','PSWG8','PSWG11','PSWA10','PSWA11','PSWA13']
     IF SIZE( badbols, /TNAME ) NE 'STRING' THEN BEGIN
        errmsg = "Didn't get expected string type for badbolos, but "+$
                 SIZE( badbols, /TNAME )
        GOTO, err_handler
     ENDIF
  ENDIF

  ;; set list of itermap parameters
  IF N_ELEMENTS(itermap_params) EQ 0 THEN itermap_params = {first_gain:0}

  ;; DEFINE THE FILTER
  IF ~KEYWORD_SET(filt) THEN BEGIN
     filt = 'm'
     msg = 'WARNING: FILT not set, assuming ' + filt
     IF KEYWORD_SET(verbose) THEN MESSAGE,msg,/INF
  ENDIF

  ;; contains pixsize and band names; this needs to be made more general
  defparams = SMAP_GETDEFPARAMS(PIXSIZE=pixel_size)
  ;; error check that it at least gave back something
  IF N_ELEMENTS(defparams) EQ 0 THEN BEGIN
     errmsg='SMAP_GETDEFPARAMS did not return anything!'
     GOTO, err_handler
  ENDIF
  
  ; set up the pixel size
  IF ~KEYWORD_SET(pixel_size) THEN pixel_size = defparams.pixsize
  
  IF ~KEYWORD_SET(osim) THEN osim = 0

  ; check the nsims requested
  IF ~KEYWORD_SET(nsim) THEN BEGIN
     nsim = 1
     msg = 'WARNING: NSIM not set, assuming ' + $
           STRCOMPRESS(STRING(nsim),/REMOVE_ALL)
     IF KEYWORD_SET(verbose) THEN MESSAGE,msg,/INF
  ENDIF

  ;;Now setup the map 
  ;;This gets back mapparam, filelist, and fileinfo
  mapparam = SMAP_SETUPMAP(PIXSIZE=pixel_size,EXNAME=exname,$
                           MAP250=map250,MAP350=map350,$
                           MAP500=map500,SPS=sps,DIR=dir,$
                           ERRMSG=ssm_errmsg,VERBOSE=verbose,$
                           SCANDOWN=scandown,SCANUP=scanup,$
                           SUCCESS=ssm_success,FILEINFO=fileinfo,$
                           INFILES=infiles)
  IF ~ ssm_success THEN BEGIN
     errmsg = 'SMAP_SETUPMAP kicked up an error: ' + ssm_errmsg
     GOTO, err_handler
  ENDIF
  
  ;; ok, now we have empty maps and the tod - 
  ;; we can now sim an input map and sample off it, then remap
  
  IF KEYWORD_SET(verbose) THEN $
     MESSAGE,'Set up complete, beginning sim run...',/INFORMATIONAL

  ;; the big for loop
  FOR isim=osim,osim + nsim - 1 DO BEGIN
     
     opmap250 = map250
     opmap350 = map350
     opmap500 = map500

     ; make the sim number to 6 digits so we can make lots
     thissim = STRCOMPRESS(STRING(isim + 1,FORMAT='(I06)'),/REMOVE_ALL)
    
     ;; tell me what number I'm on so I know you're working
     ;; NOTE: most of the verbosity is turned off here, even when the 
     ;; verbose flag is set - this is to cut down needless output so 
     ;; we can see when this sim counter is incrementing
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,'On sim ' + STRCOMPRESS(STRING(isim + 1),/REMOVE_ALL) + $
                ' of ' + STRCOMPRESS(STRING(nsim),/REMOVE_ALL),/INFORMATIONAL

     ; make the sky model
     simmap250 = simple_skymodel(m,opmap250,/convolve,/verbose)
     simmap350 = simple_skymodel(m,opmap350,/convolve,/verbose)
     simmap500 = simple_skymodel(m,opmap500,/convolve,/verbose)

     ; write out the input - will be used later
     IF ~SMAP_WRITE3COLORS(mapparam.obsid,simmap250,simmap350,simmap500,$
                           /SILENT,ERRMSG=swc_errmsg, $
                           EXNAME=exname+'_simmap_' + thissim) $
     THEN BEGIN
        errmsg = 'SMAP_WRITE3COLORS kicked up an error: ' + swc_errmsg
        GOTO, err_handler
     ENDIF

     ; make a ptrarr to the input maps
     simmap_ptrs = PTRARR(3)
     simmap_ptrs[0] = PTR_NEW( TEMPORARY(simmap250))
     simmap_ptrs[1] = PTR_NEW( TEMPORARY(simmap350))
     simmap_ptrs[2] = PTR_NEW( TEMPORARY(simmap500))
          
     ; read data into ptr array
     tod_ptrs = PTRARR(mapparam.nscans)
     FOR iscan=0,mapparam.nscans-1 DO BEGIN
        tod = SMAP_READTOD(mapparam.todnames[iscan], ERRMSG=errmsg, $
                           SUCCESS=tod_rdsuccess)
        IF tod_rdsuccess EQ 0 THEN GOTO, err_handler
        
        tod_ptrs[iscan] = PTR_NEW( TEMPORARY( tod ) )
     ENDFOR
     
     ;; ok, now have the data read in, so we want to overwrite 
     ;; those with samples from the fake map 
     SIMULATE_SCAN,tod_ptrs,simmap_ptrs,NOISE=0.0

     ;; don't need the map pointers any more
     PTR_FREE, simmap_ptrs
     
     ;; do the filtering - this is written in a scan-wise way 
     ;; so we have to give it individual scans
     FOR iscan=0,mapparam.nscans-1 DO BEGIN
        SMAP_FILTERTOD,*tod_ptrs[iscan],filt,EXCLUDEMASK=excludemask,$
                       VERBOSE=0,SUCCESS=sft_success,ERRMSG=sft_errmsg     
        ;; error check
        sft_success = 1b
        IF ~sft_success THEN BEGIN
           errmsg='SMAP_FILTERTOD returned with error: ' + sft_errmsg
           RETURN
        ENDIF
     ENDFOR

     ; tell me I got to the itermap part
     MESSAGE,"Building iter map",/INF

     ;; make the itermap
     SMAP_ITERMAP, tod_ptrs, mapparam, opmap250, opmap350, opmap500,$
                   SUCCESS=success, ERRMSG=errmsg, $
                   VERBOSE=0, SAVEMAPDIR=!SMAP_MAPS, BADBOLOS=badbols,$
                   ITERMAP_PARAMS=itermap_params, EXCLUDEMASK=excludemask, $
                   /PROPERROR
     
     ; don't need tod anymore
     PTR_FREE, tod_ptrs 

     ; error check
     IF success EQ 0b THEN BEGIN
        errmsg = "SMAP_ITERMAP return with error: "+errmsg
        GOTO, err_handler
     ENDIF

     ; and write out output for later processing
     IF ~SMAP_WRITE3COLORS(mapparam.obsid,opmap250,opmap350,opmap500,$
                           /SILENT,ERRMSG=swc_errmsg, $
                           EXNAME=exname+'_procmap_' + thissim) $
     THEN BEGIN
        errmsg = 'SMAP_WRITE3COLORS kicked up an error: ' + swc_errmsg
        GOTO, err_handler
     ENDIF
     
     ; end of nsims loop
  ENDFOR

  ; and we're out
  success = 1b
  RETURN

  err_handler:
  IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF

END
