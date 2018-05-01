PRO GET_LEVEL1_OBSTIME, input, obsstart, obsend, meanmjd

;+
;
; GET_LEVEL1_OBSTIME, files or dir, obsstart, obsend, meanmjd
;
; Given a list of Level 1 data files, extract observation time
; information from the headers. Returns beginning and end of 
; observations (in string format) as well as the weighted mean
; MJD of the observation set.
;
; Used to extract obstime information for "official release"
; maps submitted to HeDaM.
;
; gmarsden@phas.ubc.ca (2010-03-04)
;
;-

  COMPILE_OPT IDL2, STRICTARRSUBS

  ;;Sort out files vs directories
  wbad = WHERE( ~ FILE_TEST( input ), nbad )
  IF nbad NE 0 THEN BEGIN
     MESSAGE,"Couldn't find some inputs: "+STRJOIN(input[wbad],',')
  ENDIF

  wdir = WHERE( FILE_TEST( input, /DIRECTORY ), ndir, COMPLEMENT=wfile,$
                NCOMPLEMENT=nfile )
  IF ndir NE 0 THEN BEGIN
     dfilelist = get_psp_filelist( input[wdir], SUCCESS=psp_success, $
                                   ERRMSG=errmsg )

     IF psp_success EQ 0 THEN BEGIN
        MESSAGE,"Error processing directory names to get files: "+errmsg
     ENDIF
     IF nfile NE 0 THEN BEGIN
        filelist = [input[wfile],TEMPORARY(dfilelist)]
     ENDIF ELSE BEGIN
        filelist = TEMPORARY(dfilelist)
     ENDELSE
  ENDIF ELSE filelist = input
  
  nfiles = N_ELEMENTS(filelist)

  begtime = STRARR(nfiles)
  endtime = STRARR(nfiles)
  
  cenmjd = DBLARR(nfiles)
  lenmjd = DBLARR(nfiles)
  
  FOR i=0,nfiles-1 DO BEGIN
     h = HEADFITS(filelist[i])
     begtime[i] = SXPAR(h, "DATE-OBS")
     endtime[i] = SXPAR(h, "DATE-END")
     
     begmjd = DATE_CONV(begtime[i], 'M')
     endmjd = DATE_CONV(endtime[i], 'M')
     cenmjd[i] = (begmjd + endmjd) / 2.0
     lenmjd[i] = endmjd - begmjd
  ENDFOR
  
  obsstart = MIN(begtime)
  obsend   = MAX(endtime)
  meanmjd  = TOTAL(cenmjd * lenmjd) / TOTAL(lenmjd)

END
