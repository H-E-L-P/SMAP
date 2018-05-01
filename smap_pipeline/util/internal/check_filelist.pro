;+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  function check_filelist.pro
;;  Sept 11, 2009
;;  Mike Zemcov
;;  This function takes a list of files and makes sure they
;;   exist and are readable.
;;
;;  Inputs: filelist = string array of filenames
;;  Outputs: 1 if filelist is sane, 0 if there's a problem
;;  Options: verbose = verbose messaging, 0 = silent
;;           errmsg = if error, this holds a string explaining the
;;           error
;;  Changelog: v1.0, MZ, Sept 11, 2009 - orignal
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-
FUNCTION check_filelist,filelist,$
                        VERBOSE=verbose,$
                        ERRMSG=errmsg

  COMPILE_OPT IDL2

  ;; set up the error handling variables
  errmsg = ''

  ;; set up verbosity
  IF ~(KEYWORD_SET(verbose)) THEN verbose = 0b
  IF KEYWORD_SET(verbose) THEN silent=0b ELSE silent=1b

  ;; let's check that filelist is at least in the correct 
  ;; format and the files in the list exist
  IF N_ELEMENTS(filelist) LT 1 THEN BEGIN
     errmsg = "No input files specified!"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,0b
  ENDIF
  IF SIZE(filelist,/TNAME) NE 'STRING' THEN BEGIN
     errmsg = "Input file list is not of string type"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,0b
  ENDIF
  IF N_ELEMENTS(filelist) EQ 1 && filelist EQ '' THEN BEGIN
     errmsg = "Passed null file list"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,0b
  ENDIF
  ;; check all files are readable
  w_exist = WHERE(FILE_TEST(filelist), nexist, COMPLEMENT=w_noexist,$
                  NCOMPLEMENT=n_noexist)
  IF nexist EQ 0 THEN BEGIN
     errmsg = "None of input files exist"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,0b
  ENDIF
  IF n_noexist NE 0 THEN $
     errmsg = "Some of the input files do not exist: "+$
              STRJOIN(filelist[w_noexist],',')+ " "
  w_read = WHERE(FILE_TEST(filelist[w_exist],/READ),COMPLEMENT=w_noread,$
                 NCOMPLEMENT=n_noread)
  IF n_noread NE 0 THEN $
     errmsg +=" Some of the input files exist but are unreadable: "+$
              STRJOIN(filelist[w_exist[w_noread]],',')
  IF STRLEN(errmsg) NE 0 THEN BEGIN
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,0b
  ENDIF
  RETURN,1b

END
