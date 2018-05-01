;+
;NAME
; parse_uplink_info
;PURPOSE
; To parse HCSS uplink info into a structure
;CATEGORY
; SPIRE SMAP pipeline
;USAGE
; uplink_struct = parse_uplink_info(file)
;INPUTS
; file      Name of file to parse.  Better be uplink info...
;           Alternatively, it can be the name of the base directory
;            of an HCSS output product.
;RETURNS
; A structure populated with the uplink info.
;MODIFICATION HISTORY
; Author: Alex Conley, May 2009
;-

FUNCTION parse_uplink_info, file, SUCCESS=success, ERRMSG=errmsg

  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg = ''
  IF ~ FILE_TEST(file) THEN BEGIN
     errmsg = "Input file/directory "+file+" does not exist"
     RETURN,!VALUES.F_NAN
  END
  IF ~ FILE_TEST(file,/READ) THEN BEGIN
     errmsg = "Input file/directory "+file+" exists but is unreadable"
     RETURN,!VALUES.F_NAN        
  END
  IF FILE_TEST(file,/DIRECTORY) THEN BEGIN
     ;;Assume this is an HCSS output product
     dir = addslash(file)+'herschel.ia.obs.auxiliary.uplink.UplinkProduct/'
     IF ~ FILE_TEST(dir) THEN BEGIN
        errmsg = "Can't find Uplink product info -- is this an HCSS product?"
        RETURN,!VALUES.F_NAN        
     ENDIF
     IF ~ FILE_TEST(dir,/READ,/DIRECTORY) THEN BEGIN
        errmsg = "Can't read info from Uplink product subdir"
        RETURN,!VALUES.F_NAN        
     ENDIF
     files = FILE_SEARCH(dir+"*.fits",/FULLY_QUALIFY)
     IF N_ELEMENTS(files) EQ 1 && files EQ '' THEN BEGIN
        errmsg = "Can't find uplink info, although directory found at: "+$
                 dir
        RETURN,!VALUES.F_NAN
     ENDIF
     readfile = files[0]
  ENDIF ELSE readfile=file
  

  FITS_OPEN, readfile, fcb, /NO_ABORT, MESSAGE=errmsg
  FITS_CLOSE, fcb
  IF errmsg NE '' THEN RETURN,!VALUES.F_NAN

  ;;We are looking for extensions that end in Parameter
  wparams = WHERE( STREGEX( fcb.extname, 'Parameter',/BOOLEAN ),$
                   nparamext )
  IF nparamext EQ 0 THEN BEGIN
     errmsg = "Couldn't find any parameter extensions -- are you sure" + $
              " this is an uplink file?"
     RETURN,!VALUES.F_NAN
  ENDIF

  ;;We have to build a list of parameters and types to use
  ;; when we call create_struct.  Then we'll loop again
  ;; to set the values.  This is inefficient because the file
  ;; is read twice, but these should be small files
  paramname = ['']
  paramtype = ['']
  FOR i=0,nparamext-1 DO BEGIN
     read_status=0
     dat = MRDFITS(readfile,wparams[i],/SILENT,STATUS=read_status)
     IF read_status LT 0 THEN BEGIN
        errmsg = STRING(wparams[i],file,$
                        FORMAT='("Error reading extension ",I0," from ",A0)')
        RETURN,!VALUES.F_NAN
     ENDIF
     IF SIZE(dat,/TNAME) NE 'STRUCT' THEN CONTINUE ;;There weren't any
     tags = TAG_NAMES(dat)
     IF N_ELEMENTS(tags) LE 1 THEN BEGIN
        errmsg = STRING(wparams[i],file,$
                        FORMAT='("Expecting more tags in ext ",I0," from ",A0)') 
        RETURN,!VALUES.F_NAN
     ENDIF
     CASE STRUPCASE(tags[1]) OF
        'BOOLPARAMVAL' : ptype = 'B'
        'DOUBLEPARAMVAL' : ptype = 'D'
        'INTEGERPARAMVAL' : ptype = 'I'
        'LONGPARAMVAL' : ptype = 'J'
        'STRINGPARAMVAL' : ptype = 'A'
        ELSE : BEGIN
           errmsg = "Unknown param type from tag: "+tags[1]
           RETURN,!VALUES.F_NAN
        ENDELSE
     ENDCASE
     
     paramname = [paramname,dat.(0)]
     paramtype = [paramtype, REPLICATE(ptype,N_ELEMENTS(dat))]
  ENDFOR

  IF N_ELEMENTS(paramname) EQ 1 THEN BEGIN
     errmsg = "Unable to find any params in any extension"
     RETURN,!VALUES.F_NAN
  ENDIF

  paramname = STRLOWCASE(STRTRIM(paramname[1:*],2))
  paramtype = paramtype[1:*]
  
  nuniqparams = N_ELEMENTS( UNIQ(paramname,SORT(paramname) ) )
  IF nuniqparams NE N_ELEMENTS(paramname) THEN BEGIN
     errmsg = "Non-unique parameter names found"
     RETURN,!VALUES.F_NAN
  ENDIF

  CREATE_STRUCT,outstruct,'',paramname,paramtype

  ;;Now loop again and fill them in
  counter = 0
  FOR i=0,nparamext-1 DO BEGIN
     read_status=0
     dat = MRDFITS(readfile,wparams[i],/SILENT,STATUS=read_status)
     IF read_status LT 0 THEN BEGIN
        errmsg = STRING(wparams[i],file,$
                        FORMAT='("Error reading extension ",I0," from ",A0)')
        RETURN,!VALUES.F_NAN
     ENDIF
     n = N_STRUCT(dat,ntags)
     FOR j=0,n-1 DO BEGIN
        var = dat[j].(1)
        CASE paramtype[j+counter] OF
           'B' : IF STRUPCASE(STRTRIM(var,2)) EQ 'T' THEN val=1b ELSE val=0b
           'A' : val = STRTRIM(var,2)
           ELSE : val = var
        ENDCASE
        outstruct.(j+counter) = val
     ENDFOR
     counter += n
  ENDFOR
  
  ;;Now look for BBID info
  wbbid = WHERE(STREGEX(fcb.extname,'blockExecution',/FOLD_CASE,/BOOLEAN),$
                nbbid)
  IF nbbid GT 1 THEN BEGIN
     errmsg = "Found more than one blockExecution extension"
     RETURN,!VALUES.F_NAN
  ENDIF ELSE IF nbbid EQ 1 THEN BEGIN
     read_status=0
     bbid = MRDFITS( readfile, wbbid[0], /SILENT, STATUS=read_status)
     IF read_status LT 0 THEN BEGIN
        errmsg = STRING(wbbid[0],file,$
                        FORMAT='("Error reading extension ",I0," from ",A0)')
        RETURN,!VALUES.F_NAN
     ENDIF
     IF N_ELEMENTS(bbid) GT 0 THEN $
        outstruct = CREATE_STRUCT( 'bbid', bbid, 'nbbid', N_ELEMENTS(bbid),$
                                   outstruct )
  ENDIF ;;If none found, do nothing

  ;;And finally add some info from the proposal header
  wobs = WHERE(STREGEX(fcb.extname,'observationRequest',/FOLD_CASE,/BOOLEAN),$
               nobs)
  IF nobs EQ 1 THEN BEGIN
     obs_head = HEADFITS(readfile,EXT=wobs[0],ERRMSG=errmsg)
     IF errmsg NE '' THEN RETURN,!VALUES.F_NAN

     ;;Non-Hierarchical keys
     keys = ['INSTRUME','TITLE','OBS_ID','TELAPSE','POINTMOD']
     rename_keys=['instrument','obstitle','obs_id','elapsed_time',$
                  'pointing_mode']
     FOR i=0,N_ELEMENTS(keys)-1 DO BEGIN
        val = FXPAR(obs_head,keys[i],COUNT=nfound)
        IF SIZE(val,/TNAME) EQ 'STRING' THEN val=STRTRIM(val,2)
        IF nfound EQ 1 THEN $
           outstruct = CREATE_STRUCT(rename_keys[i],val,outstruct)
     ENDFOR

     ;;Meta Keys
     hierarch = get_hierarch(obs_head, COUNT=nhierarch )
     wmeta = WHERE(STREGEX(hierarch[*,0],'meta',/FOLD_CASE,/BOOLEAN),nmeta)
     IF nmeta NE 0 THEN FOR i=0,nmeta-1 DO BEGIN
        val = FXPAR(obs_head,hierarch[wmeta[i],0],COUNT=nfound)
        IF SIZE(val,/TNAME) EQ 'STRING' THEN val=STRTRIM(val,2)
        IF nfound EQ 1 THEN $
           outstruct = CREATE_STRUCT(hierarch[wmeta[i],1],val,outstruct)
     ENDFOR
  ENDIF
  

  success = 1b
  RETURN,outstruct
   
END
