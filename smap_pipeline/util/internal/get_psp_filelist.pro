;+
;NAME
; get_psp_filelist
;PURPOSE
; To navigate a HCSS data directory tree and give you
; a list of Pointed Phot Timeline files and the OBSID.
;USAGE
; filelist = get_psp_filelist( dir, ALLOWED_OBSIDS=allowed_obsids,$
;                              OBSID=obsid, SUCCESS=success, ERRMSG=errmsg )
;INPUTS
; dir        Name of directory.  Should be the path to either a
;             HCSS export product or a HCSS storage pool.  Can be
;             a list of directories.
;KEYWORDS
; requireobsid Require that the obsid be determined or fail.  This
;               will be set if ALLOWED_OBSDIS is provided
;RETURNS
; A list of PointedPhotTimeline files that can be processed into a map.
;OPTIONAL INPUTS
; allowed_obsids List of allowed obsids
;OPTIONAL OUTPUTS
; obsid      A list of obsids, one for each file.
; success    1b if it found files, 0b if it didn't
; errmsg     Some explanation of what went wrong if success is 0b
;MODIFICATION HISTORY
; Author: Alex Conley, Oct 2009
;-

FUNCTION get_psp_filelist_inner, dir, OBSID=obsid, $
                                 SUCCESS=success, ERRMSG=errmsg, $
                                 DOOBSID=doobsid
  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN
  success = 0b
  errmsg  = ''

  ppt_dir = addslash(dir)+$
            'herschel.spire.ia.dataset.PointedPhotTimeline/'
  IF FILE_TEST( ppt_dir,/READ,/DIRECTORY) THEN BEGIN
     files = FILE_SEARCH( [ppt_dir+'*.fits', ppt_dir+'*.fits.gz'], $
                          /FULLY, COUNT=nfiles )
     IF nfiles EQ 0 THEN BEGIN
        errmsg = "Found PointedPhotTimeline dir, but no fits files in it"
        RETURN,''
     ENDIF

     ;;Try to grab the obsid from the headers
     IF KEYWORD_SET(doobsid) THEN BEGIN
        obsid = ULONARR( nfiles )
        obsid[*] = -1
        FOR j=0, nfiles-1 DO BEGIN
           head = HEADFITS( files[j], /SILENT, ERRMSG=head_errmsg )
           IF head_errmsg EQ '' THEN BEGIN
              val = SXPAR(head,'OBS_ID',COUNT=count,/SILENT)
              IF count NE 0 THEN obsid[j] = ULONG(val[0])
           ENDIF
        ENDFOR
     ENDIF
        
     success = 1b
     RETURN, files
  ENDIF

  ;;Since that didn't work, look for number dir(s), indicating we are 
  ;; in an HCSS export dir
  obsdirs = FILE_SEARCH( addslash(dir)+STRJOIN(REPLICATE('[0-9]',10)),$
                         /FULLY, COUNT=nobsdir )
  FOR i=0,nobsdir-1 DO BEGIN
     ;;See if it has level1, etc. in it
     ppt_dir = addslash(obsdirs[i]) + $
               'level1/herschel.spire.ia.dataset.PointedPhotTimeline/'
     IF FILE_TEST( ppt_dir, /READ, /DIRECTORY ) THEN BEGIN
        curr_files = get_psp_filelist_inner( ppt_dir, OBSID=curr_obsid,$
                                             SUCCESS=csuccess, $
                                             ERRMSG=errmsg, $
                                             DOOBSID=doobsid )
        IF csuccess EQ 0 THEN RETURN,''
        IF N_ELEMENTS(files) EQ 0 THEN files=TEMPORARY(curr_files) ELSE $
           files = [files,TEMPORARY(curr_files)]
        IF KEYWORD_SET(doobsid) THEN BEGIN
           ;;See if there are any bad obsids since we can get them
           ;; from the dir name instead
           wbadobsid = WHERE(curr_obsid LT 0, nbadobsid)
           IF nbadobsid NE 0 THEN BEGIN
              pos = RSTRPOS(obsdirs[i],PATH_SEP())
              IF pos NE -1 THEN $
                 curr_obsid[wbadobsid] = ULONG(STRMID( obsdirs[0], pos+1 ))
           ENDIF
           IF N_ELEMENTS(obsid) EQ 0 THEN obsid=TEMPORARY(curr_obsid) ELSE $
              obsid = [obsid,TEMPORARY(curr_obsid)]
        ENDIF
     ENDIF
  ENDFOR
  IF N_ELEMENTS(files) NE 0 THEN BEGIN
     success = 1b
     RETURN,files
  ENDIF

  ;;So it doesn't look like an HCSS export dir or an HCSS
  ;;storage pool
  ;;Maybe they just gave us a directory containing the timelines
  ;;Look for either psp or ppt files and hope this catches
  ;; all the cases we care about
  flsrch = addslash(dir) + ['*psp*.fits','*psp*.fits.gz',$
                            '*ppt*.fits','*ppt*.fits.gz']
  files = FILE_SEARCH( flsrch, COUNT=nfiles )
  IF nfiles NE 0 THEN BEGIN
     IF ARG_PRESENT(obsid) THEN BEGIN
        obsid = ULONARR( nfiles )
        obsid[*] = -1
        FOR j=0, nfiles-1 DO BEGIN
           head = HEADFITS( files[j], /SILENT, ERRMSG=head_errmsg )
           IF head_errmsg EQ '' THEN BEGIN
              val = SXPAR(head,'OBS_ID',COUNT=count,/SILENT)
              IF count NE 0 THEN obsid[j] = ULONG(val[0])
           ENDIF
        ENDFOR
     ENDIF

     success = 1b
     RETURN,files
  ENDIF

  errmsg = "Couldn't determine export directory type of "+dir
  RETURN,''
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION get_psp_filelist, dir, ALLOWED_OBSIDS=allowed_obsids, $
                           OBSID=obsid, SUCCESS=success,$
                           REQUIREOBSID=requireobsid, ERRMSG=errmsg
  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg  = ''

  ndir = N_ELEMENTS(dir)
  IF ndir EQ 0 THEN BEGIN
     errmsg = "No input directory passed"
     RETURN,''
  ENDIF

  IF N_ELEMENTS( allowed_obsids ) NE 0 THEN have_obsidlist = 1b ELSE $
     have_obsidlist = 0b
  doobsid = have_obsidlist OR ARG_PRESENT(obsid)

  ;;Basic tests on dir
  exists = FILE_TEST(dir)
  wnoexist = WHERE( ~ TEMPORARY(exists), nnoexist )
  IF nnoexist NE 0 THEN BEGIN
     errmsg = "Input dir(s): "+STRJOIN(dir[wnoexist],', ')+" not found"
     RETURN,''
  ENDIF
  isdir = FILE_TEST( dir, /DIRECTORY )
  wnotdir = WHERE( ~ TEMPORARY(isdir), nnotdir )
  IF nnotdir NE 0 THEN BEGIN
     errmsg = "Input dir(s): "+STRJOIN(dir[wnotdir],', ')+" not directories"
     RETURN,''
  ENDIF
  isread = FILE_TEST( dir, /READ )
  wnotread = WHERE( ~ TEMPORARY(isread), nnotread )
  IF nnotdir NE 0 THEN BEGIN
     errmsg = "Input dir(s): "+STRJOIN(dir[wnotread],', ')+" not readable"
     RETURN,''
  ENDIF

  IF ARG_PRESENT(obsid) THEN DELVARX,obsid
  FOR i=0,ndir-1 DO BEGIN
     curr_files = get_psp_filelist_inner( dir[i], OBSID=curr_obsid, $
                                          SUCCESS=csuccess, ERRMSG=errmsg,$
                                          DOOBSID=doobsid )
     IF csuccess EQ 0 THEN RETURN,''
     IF N_ELEMENTS(files) EQ 0 THEN files=TEMPORARY(curr_files) ELSE $
        files = [files,TEMPORARY(curr_files)]
     IF doobsid THEN BEGIN
        IF have_obsidlist || KEYWORD_SET(requireobsid) THEN BEGIN
           wbad = WHERE( curr_obsid LT 0, nbad )
           IF nbad NE 0 THEN BEGIN
              errmsg = "Failed to find obsid for "+$
                       STRJOIN(curr_files[wbad],', ')
              RETURN,''
           ENDIF
        ENDIF
        IF N_ELEMENTS(obsid) EQ 0 THEN obsid=TEMPORARY(curr_obsid) ELSE $
           obsid = [obsid, TEMPORARY(curr_obsid)]
     ENDIF
  ENDFOR

  IF have_obsidlist THEN BEGIN
     wpresent = WHERE_ARRAY( allowed_obsids, obsid, npresent )
     IF npresent EQ 0 THEN BEGIN
        errmsg = "No files survived allowed_obsids specification from user"
        RETURN,''
     ENDIF
     files = files[wpresent]
     obsid = obsid[wpresent]
  ENDIF

  success = 1b
  RETURN,files
END
  
