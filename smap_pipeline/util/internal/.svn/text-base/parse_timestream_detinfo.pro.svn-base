;+
;NAME
; parse_timestream_detinfo
;PURPOSE
; Returns a structure of meta-information about PointedPhotTimeline
; files for the SPIRE photometer.  This allows you to iterate through
; the detectors in each extention efficiently.
;USAGE
;  info = parse_timestream_detinfo( files )
;INPUTS
;  files             List of files to parse or the root level
;                     directory of an HCSS export product.
;KEYWORDS
;  signalonly        Only returns detectors expected to have
;                     astronomical signal, so excluding bare
;                     resistors, etc. (i.e., starting with P[SML]W[A-J])
;  notemperature     Don't require temperature extension.  It
;                     will still be processed if found, but will be
;                     ignored if not present.  If it is not found, then
;                     .TEMPERATURE_EXTN, .TEMPERATURE_INDEX, .NTHERMS will
;                     not be present in the output structure.
;  noquality         Don't require the quality extension.  Like 
;                     notemperature, but for .QUALITY_EXTN and
;                     .QUALITY_INDEX.  This is ON by default, since
;                     it doesn't seem to exist in our SDP data.
;  verbose           Output status info as it runs.
;OPTIONAL OUTPUTS
;  success           1 if this succeeded, 0 if something went wrong
;  errmsg            On failure, explanation of problem.  '' on success.
;RETURNS
;  An array of structures containing info about the structure of the
;   photometry products.  The structure contains the following tags:
;     .FILE              The fully qualified file name
;     .SHORTFILE         The file name
;     .BBID              The bbid of this scan
;     .OBSID             The obsid this file belongs to
;     .OBJECT            The name of the object being observed
;     .NCHANNELS         The number of channels found
;     .NSAMPS            The number of samples in this file
;     .SIGNAL_EXTN       The FITS extension containing the signal
;     .MASK_EXTN         The FITS extension containing the mask
;     .RA_EXTN           The FITS extension containing the RA
;     .DEC_EXTN          The FITS extension containing the DEC
;     .BOLOMETER_NAMES   The names of the channels returned
;     .SIGNAL_INDEX      Index into signal table giving the bolometers 
;                         specified by .bolometer_names
;     .MASK_INDEX        Same as .signal_index but for mask table
;     .RA_INDEX          Same as .signal_index but for ra table
;     .DEC_INDEX         Same as .signal_index but for dec table
;     .NTHERMS           Number of thermometers
;     .TEMPERATURE_EXTN  The FITS extension containing the thermometers
;     .QUALITY_EXTN      The FITS extension containing the quality mask
;     .LIGHT_BOLOS       Logical showing position of light devices
;     .TEMPERATURE_NAMES Array containing thermometer names.
;     .TEMPERATURE_INDEX Same as .signal_index but for temperature
;     .QUALITY_INDEX     Same as .signal_index but for quality
;EXAMPLES:
;  Say you need to do some operation on the signal, mask, ra, and dec
;   for each bolometer in a single file:
;  
;  info = parse_timestream_detinfo( file )
;  signal = MRDFITS( info.file, info.signal_extn )
;  mask   = MRDFITS( info.file, info.mask_extn )
;  ra     = MRDFITS( info.file, info.ra_extn )
;  dec    = MRDFITS( info.file, info.dec_extn )
;  FOR i=0, info.nchannels-1 DO $
;     do_some_process( signal.(info.signal_index[i]), $
;                      mask.(info.mask_index[i]),$
;                      ra.(info.ra_index[i]),$
;                      dec.(info.dec_index[i]) )
;
;  This is useful because the ra/dec tags are -not- in the same order
;   as they are for signal.
;NOTES
;  If multiple files are input, all subsequent files are put in the
;   same detector order as the first.  The code also checks to be sure
;   the same list of detectors are present.
;MODIFICATION HISTORY
; Author: Alex Conley, August 2009
; Modified: Mike Zemcov, August 2009 - added new structure fields:
;             NTHERMS, TEMPERATURE_EXTN, QUALITY_EXTN, LIGHT_BOLOS, 
;             TEMPERATURE_NAMES, TEMPERATURE_INDEX, QUALITY_INDEX,
;             OBSID, BBID, OBJECT 
;             and associated milarkey to make it work.
;-
;;Gets list of signal tags (which start with PSW, PMW, PLW).
;;This is an index into the structure, not the actual names
FUNCTION parse_timestream_detinfo_get_detlist,signal_tags,SUCCESS=success,$
   ERRMSG=errmsg
  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN

  success=0b
  errmsg = ''
 
  IF N_ELEMENTS(signal_tags) EQ 0 THEN BEGIN
     errmsg = "No tags present"
     RETURN,!VALUES.F_NAN
  ENDIF
  signalf5 = STRUPCASE(STRMID(signal_tags,0,5))
  wsignal_dets = WHERE( STREGEX( signalf5, 'P[SML]W[A-J][0-9]',/BOOLEAN,$
                                 /FOLD_CASE ), nsignal_dets )
  IF nsignal_dets EQ 0 THEN BEGIN
     errmsg = "No signal tags found"
     RETURN,!VALUES.F_NAN
  ENDIF
  
  success=1b
  RETURN,wsignal_dets
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Matches order of detectors in specified extension for quality, which
;;is written differently
FUNCTION parse_timestream_detinfo_qualitymatch, matchto, file, exten,$
   SUCCESS=success, ERRMSG=errmsg
  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN
  success = 0b
  errmsg  = ''

  qual = MRDFITS( file, exten, STATUS=errmsg, /SILENT )
  IF errmsg NE 0 THEN BEGIN
     errmsg = 'While reading header: '+errmsg+' exten: '+STRING(exten)
     RETURN,!VALUES.F_NAN
  ENDIF
  
  IF ~ TAG_EXIST( qual, 'channelname', /TOP_LEVEL ) THEN BEGIN
     errmsg = "Didn't find channelnames in quality extension"
     RETURN,!VALUES.F_NAN
  ENDIF
  tags = STRTRIM(qual.channelname)
  DELVARX,qual

  ;;Clip down to only those found in target dets
  wpresent = WHERE_ARRAY( matchto, tags, npresent )

  IF npresent NE N_ELEMENTS(matchto) THEN BEGIN
     wmissing = MISSING( tags, matchto )
     errmsg = "Missing some detectors found in signal: "+$
              STRJOIN( matchto[wmissing],', ' )
     RETURN,!VALUES.F_NAN
  ENDIF
  detidx = wpresent[ MATCH_ORDER( matchto, tags[wpresent] ) ]
  success = 1b
  RETURN,detidx
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Matches order of detectors in specified extension
FUNCTION parse_timestream_detinfo_ordermatch, matchto, file, exten,$
   SUCCESS=success, ERRMSG=errmsg
  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN
  success = 0b
  errmsg  = ''

  head = HEADFITS( file, ERRMSG=errmsg, EXTEN=exten, /SILENT )
  IF errmsg NE '' THEN BEGIN
     errmsg = 'While reading header: '+errmsg+' exten: '+STRING(exten)
     RETURN,!VALUES.F_NAN
  ENDIF
  tags = SXPAR( head, 'TTYPE*', COUNT=count, /SILENT )
  DELVARX,head
  IF count EQ 0 THEN BEGIN
     errmsg = "No TTYPE header keywords for "+shortfile+$
              "; are you sure this is a binary table?"
     RETURN,!VALUES.F_NAN
  ENDIF
  tags = STRTRIM( tags )
  ;;Clip down to only those found in target dets
  wpresent = WHERE_ARRAY( matchto, tags, npresent )
  IF npresent NE N_ELEMENTS(matchto) THEN BEGIN
     wmissing = MISSING( tags, matchto )
     errmsg = "Missing some detectors in mask found in signal: "+$
              STRJOIN( matchto[wmissing],', ' )
     RETURN,!VALUES.F_NAN
  ENDIF
  detidx = wpresent[ MATCH_ORDER( matchto, tags[wpresent] ) ]
  success = 1b
  RETURN,detidx
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION parse_timestream_detinfo, firstarg, SIGNALONLY=signalonly,$
                                   HCSSINTERNAL=hcssinternal, $
                                   NOTEMPERATURE=notemperature,$
                                   NOQUALITY=noquality,VERBOSE=verbose, $
                                   SUCCESS=success, ERRMSG=errmsg
  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg  = ''

  IF N_ELEMENTS(noquality) EQ 0 THEN noquality=1b ;;On by default

  ;;Figure out what firstarg is and test it
  IF N_ELEMENTS(firstarg) EQ 0 THEN BEGIN
     errmsg = "No input files/directory provided"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF
  IF SIZE(firstarg,/TNAME) NE 'STRING' THEN BEGIN
     errmsg = "Input file list/directory is not of string type"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF

  wbad = WHERE( ~ FILE_TEST( firstarg ), nbad )
  IF nbad NE 0 THEN BEGIN
     errmsg = "Couldn't find some inputs: "+STRJOIN(input[firstarg],',')
     RETURN,''
  ENDIF

  wdir = WHERE( FILE_TEST( firstarg, /DIRECTORY ), ndir, COMPLEMENT=wfile,$
                NCOMPLEMENT=nfile )
  IF nfile NE 0 THEN files=firstarg[wfile]
  IF ndir NE 0 THEN BEGIN
     dfiles = get_psp_filelist( firstarg[wdir], SUCCESS=psp_success, $
                                ERRMSG=errmsg )
     IF psp_success EQ 0 THEN BEGIN
        errmsg = "Error getting file listing in parse_timestream_detinfo: "+$
                 errmsg
        RETURN,''
     ENDIF
     IF nfile NE 0 THEN files=[files,TEMPORARY(dfiles)] ELSE $
        files=TEMPORARY(dfiles)
  ENDIF
  nfiles = N_ELEMENTS(files)
  obsid = ULONARR(nfiles)
  bbid = ULONARR(nfiles)
  object = STRARR(nfiles)

  ;;Main loop
  FOR i=0,nfiles-1 DO BEGIN
     shortfile = STRMID(files[i],RSTRPOS(files[i],PATH_SEP())+1)
     IF KEYWORD_SET(verbose) THEN MESSAGE,"Reading: "+shortfile,/INF

     FITS_OPEN,files[i],fcb,/NO_ABORT,MESSAGE=mssg
     FITS_CLOSE,fcb
     IF mssg NE '' THEN BEGIN
        errmsg = "Failed to read FITS file: "+shortfile
        RETURN,!VALUES.F_NAN
     ENDIF

     ;;Find signal, ra, dec, mask
     exts = STRLOWCASE(fcb.extname)
     wsignal = WHERE( exts EQ 'signal', nsignal )
     wra = WHERE( exts EQ 'ra', nra )
     wdec = WHERE( exts EQ 'dec', ndec )
     wmask = WHERE( exts EQ 'mask', nmask )
     wtemperature = WHERE( exts EQ 'temperature', ntemperature )
     wquality = WHERE( exts EQ 'quality', nquality )
     IF nsignal NE 1 THEN BEGIN 
        errmsg = "Couldn't find signal extension in "+shortfile
     ENDIF
     IF nra NE 1 THEN BEGIN 
        errmsg = "Couldn't find ra extension in "+shortfile
     ENDIF
     IF ndec NE 1 THEN BEGIN 
        errmsg = "Couldn't find dec extension in "+shortfile
     ENDIF
     IF nmask NE 1 THEN BEGIN 
        errmsg = "Couldn't find mask extension in "+shortfile
     ENDIF
     IF (~ KEYWORD_SET(notemperature)) AND ntemperature NE 1 THEN BEGIN
        errmsg = "Couldn't find temperature extension in "+shortfile
     ENDIF
     IF (~ KEYWORD_SET(noquality)) AND nquality NE 1 THEN BEGIN 
        errmsg = "Couldn't find quality extension in "+shortfile
     ENDIF
     IF errmsg NE '' THEN BEGIN
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDIF

     ;; strip out some header info of use later
     bbid[i] = SXPAR(fcb.hmain,'BBID',COUNT=count,/SILENT)
     IF count EQ 0 THEN BEGIN
        errmsg = "No BBID header keyword for "+shortfile
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDIF
     obsid[i] = SXPAR(fcb.hmain,'OBS_ID',COUNT=count,/SILENT)
     IF count EQ 0 THEN BEGIN
        errmsg = "No OBS_ID header keyword for "+shortfile
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDIF

     object[i] = SXPAR(fcb.hmain,'OBJECT',COUNT=count,/SILENT)
     IF count EQ 0 THEN BEGIN
        errmsg = "No OBJECT header keyword for "+shortfile
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDIF
     object[i] = STRTRIM(object[i])

     ;;Now get header info from each extension
     ;;Signal
     sig_head = HEADFITS( files[i], ERRMSG=errmsg, EXTEN=wsignal[0],$
                          /SILENT )
     IF errmsg NE '' THEN BEGIN
        errmsg = 'While reading signal header: '+errmsg
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDIF

     ;;Number of timesamples
     ntimesamps = SXPAR( sig_head, 'NAXIS2', COUNT=count, /SILENT )
     IF count EQ 0 THEN BEGIN
        errmsg = "Couldn't find number of timesamples for structure"
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDIF

     signal_tags = SXPAR( sig_head, 'TTYPE*', COUNT=count, /SILENT )
     DELVARX,sig_head
     IF count EQ 0 THEN BEGIN
        errmsg = "No TTYPE header keywords for "+shortfile+$
                 " signal extension; are you sure this is a binary table?"
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDIF
     signal_tags = STRTRIM( signal_tags )
     
     ; find all the light bolos
     detidx_l = parse_timestream_detinfo_get_detlist( signal_tags,$
                                                      SUCCESS=success_d,$
                                                      ERRMSG=errmsg)
     IF success_d EQ 0 THEN BEGIN
        errmsg += " while processing "+shortfile
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.D_NAN
     ENDIF
     ndets = N_ELEMENTS( detidx_l )
     nexpected_dets = 139 + 88 + 43
     IF KEYWORD_SET( verbose ) AND ndets NE nexpected_dets THEN $
        MESSAGE,STRING(nexpected_dets,ndets,shortfile,$
                       FORMAT='("WARNING: expected ",I0'+$
                       '," dets, got ",I0," in ",A0)'),/INF

     IF KEYWORD_SET( signalonly ) THEN BEGIN
        detidx = detidx_l
     ENDIF ELSE BEGIN
        DELVARX,ndets
        ;;Grab only dets that start with P
        detidx = WHERE( STRMID(signal_tags,0,1) EQ 'P', ndets )
     ENDELSE
     IF ndets EQ 0 THEN BEGIN
        errmsg = "Unable to find valid channel names in "+shortfile
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.D_NAN
     ENDIF

     ; now make lightbolos be 1 if they're light - the -1's are required
     ; because of sampletime in the beginning
     lightbolos = BYTARR(N_ELEMENTS(detidx))
     IF KEYWORD_SET(signalonly) THEN BEGIN
        lightbolos[*]=1b
     ENDIF ELSE BEGIN
        lightbolos[detidx[detidx_l - 1] - 1] = 1b
     ENDELSE

     ;;Now we will do the same for mask, ra, and dec.  Moreover, we
     ;; want to arrange things so that our index is in the same
     ;; order by detector as the signal.  Note that they may be in
     ;; different order in different extensions
     signal_dets = signal_tags[detidx]
     
     ;;Mask
     mask_detidx = parse_timestream_detinfo_ordermatch( signal_dets,$
                                                        files[i], wmask[0],$
                                                        SUCCESS=success_o,$
                                                        ERRMSG=errmsg )
     IF success_o EQ 0 THEN BEGIN
        errmsg = "For mask extension: "+errmsg
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDIF

     ;;RA
     ra_detidx = parse_timestream_detinfo_ordermatch( signal_dets,$
                                                      files[i], wra[0],$
                                                      SUCCESS=success_o,$
                                                      ERRMSG=errmsg )
     IF success_o EQ 0 THEN BEGIN
        errmsg = "For RA extension: "+errmsg
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDIF

     ;;dec
     dec_detidx = parse_timestream_detinfo_ordermatch( signal_dets,$
                                                       files[i], wdec[0],$
                                                       SUCCESS=success_o,$
                                                       ERRMSG=errmsg )
     IF success_o EQ 0 THEN BEGIN
        errmsg = "For DEC extension: "+errmsg
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDIF
     
     ;; and quality
     IF nquality NE 0 THEN BEGIN
        quality_detidx = $
           parse_timestream_detinfo_qualitymatch( signal_dets,files[i],$
                                                  wquality[0],$
                                                  SUCCESS=success_o,$
                                                  ERRMSG=errmsg )
        IF success_o EQ 0 THEN BEGIN
           errmsg = "For quality extension: "+errmsg
           IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
           RETURN,!VALUES.F_NAN
        ENDIF
        has_quality = 1b
     ENDIF ELSE has_quality = 0b

     ;; now we have to do it to temperature, which is a slightly 
     ;; different kettle of fish
     IF ntemperature NE 0 THEN BEGIN
        therm_head = HEADFITS( files[i], ERRMSG=errmsg, $
                               EXTEN=wtemperature[0], /SILENT )
        IF errmsg NE '' THEN BEGIN
           errmsg = 'While reading temperature header: '+errmsg
           IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
           RETURN,!VALUES.F_NAN
        ENDIF
        therm_tags = SXPAR( therm_head, 'TTYPE*', COUNT=count, /SILENT )
        DELVARX,therm_head
        IF count EQ 0 THEN BEGIN
           errmsg = "No TTYPE header keywords for "+shortfile+$
                    " temperature extension; are you sure this is a "+$
                    "binary table?"
           IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
           RETURN,!VALUES.F_NAN
        ENDIF
        therm_tags = STRTRIM( therm_tags )

        temp_idx = WHERE( STRMID(therm_tags,0,1) EQ 'P', ntherms )

        IF ntherms EQ 0 THEN BEGIN
           errmsg = "Unable to find valid channel names in "+shortfile
           IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
           RETURN,!VALUES.D_NAN
        ENDIF

        therm_dets = therm_tags[temp_idx]
        has_temperature = 1b
     ENDIF ELSE has_temperature = 0b

     IF N_ELEMENTS(outstruct) EQ 0 THEN BEGIN
        ;;Prepare output struct now that we know how many dets we have
        base_struct = { file: '', shortfile: '', object: '', $
                        bbid: 0uL, obsid: 0uL, nchannels: 0,$
                        nsamps: ntimesamps, signal_extn: -1, mask_extn: -1, $
                        ra_extn: -1, dec_extn: -1, $
                        bolometer_names: STRARR(ndets),$
                        light_bolos: BYTARR(ndets),$
                        signal_index: INTARR(ndets),$
                        mask_index: INTARR(ndets),$
                        ra_index: INTARR(ndets),$
                        dec_index: INTARR(ndets)$
                      }
        IF has_quality THEN $
           base_struct = CREATE_STRUCT( base_struct, $
                                        'quality_extn', -1,$
                                        'quality_index',$
                                        INTARR(ndets) )
        IF has_temperature THEN $
           base_struct = CREATE_STRUCT( base_struct, $
                                        'ntherms', 0, $
                                        'temperature_extn', -1, $
                                        'temperature_names',$
                                        STRARR(ntherms),$
                                        'temperature_index',$
                                        INTARR(ntherms) )
           

        outstruct = REPLICATE( base_struct, nfiles )

        ;;Get them in sorted order for convenience
        sort0 = SORT( signal_dets )
        signal_dets = signal_dets[sort0]
        detidx = detidx[sort0]
        mask_detidx = mask_detidx[sort0]
        ra_detidx = ra_detidx[sort0]
        dec_detidx = dec_detidx[sort0]
        lightbolos     = lightbolos[sort0]
        IF has_quality THEN quality_detidx = quality_detidx[sort0]
        IF has_temperature THEN BEGIN
           sortT = SORT( therm_dets )
           therm_dets = therm_dets[sortT]
           temp_idx   = temp_idx[sortT]
        ENDIF
     ENDIF ELSE BEGIN
        ;;Check compatability with previous files
        ;;We may need to reorder all of them if the detectors are
        ;;somehow in a different order in this file
        IF ndets NE outstruct[0].nchannels THEN BEGIN
           errmsg = "Different number of channels found for "+$
                    outstruct[0].shortfile+" and "+shortfile
           IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
           RETURN,!VALUES.F_NAN
        ENDIF   
        
        ;;Make sure the same dets are present
        ;; Note bolometer_names is already sorted
        wbad = WHERE( outstruct[0].bolometer_names NE $
                      signal_dets[ SORT(signal_dets) ], nbad )
        IF nbad NE 0 THEN BEGIN
           errmsg = "Found different list of detectors in "+$
                    outstruct[0].shortfile+" and "+shortfile+": "
           FOR j=0,nbad-1 DO errmsg += " "+ $
              (outstruct[0].bolometer_names[sort0])[wbad[j]]+" vs "+$
              (signal_dets[ SORT(signal_dets) ])[wbad[j]]
           IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
           RETURN,!VALUES.F_NAN
        ENDIF   
        
        ;;Now see if they are in the same order and do something about
        ;; it if they aren't.  We could just sort as we did for
        ;; the first element, but it's more robust against
        ;; future changes if we explicitly match
        wmisord = WHERE( outstruct[0].bolometer_names NE signal_dets, $
                         nmisord )
        IF nmisord NE 0 THEN BEGIN
           wreord = MATCH_ORDER( outstruct[0].bolometer_names,$
                                 signal_dets )
           signal_dets    = signal_dets[wreord]
           lightbolos     = lightbolos[wreord]
           detidx         = detidx[wreord]
           mask_detidx    = mask_detidx[wreord]
           ra_detidx      = ra_detidx[wreord]
           dec_detidx     = dec_detidx[wreord]
           IF has_quality THEN quality_detidx = quality_detidx[wreord]
        ENDIF

        IF has_temperature THEN BEGIN
           ;; check for the same thing with the thermometers
           wmisord = WHERE( outstruct[0].temperature_names NE therm_dets, $
                            nmisord )
           IF nmisord NE 0 OR $
              outstruct[0].ntherms NE N_ELEMENTS(therm_dets) THEN BEGIN
              ;;Now, I would like to assume all the same ones are
              ;; present, but we've already run into some files
              ;; have additional bolometers in the temperature
              ;; extension that not all files have.  So -- what to do?
              ;;Well, not much choice but to throw away the extras
              ;; and if there are some missing, then fail
              wpresent = WHERE_ARRAY(outstruct[0].temperature_names,$
                                     therm_dets, npresent )
              IF npresent LT outstruct[0].ntherms THEN BEGIN
                 errmsg = "Current file "+shortfile+" is missing some"
                 errmsg += " therm detectors found in previous files: "
                 wmissing = MISSING(therm_dets,$
                                    outstruct[0].temperature_names)
                 errmsg += STRJOIN(outstruct[0].temperature_names[wmissing],',')
                 errmsg += " -- Can't continue"
                 IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
                 RETURN,!VALUES.F_NAN
              ENDIF 
              IF N_ELEMENTS(therm_dets) GT outstruct[0].ntherms THEN BEGIN
                 ;;We have some extra detectors!
                 ;;This is survivable, but perhaps the user should
                 ;; be warned
                 IF KEYWORD_SET(verbose) THEN BEGIN
                    errmsg = "Found extra bolometers in therm extension "+$
                             " not found in previous files: "
                    wmissing = MISSING( outstruct[0].temperature_names,$
                                        therm_dets )
                    errmsg += STRJOIN( therm_dets[wmissing],',' )
                    errmsg += " -- ignoring these detectors"
                 ENDIF
                 ntherms    = npresent
                 therm_dets = therm_dets[wpresent]
                 temp_idx   = temp_idx[wpresent]
              ENDIF
              
              ;;Now... order match
              wreord = MATCH_ORDER( outstruct[0].temperature_names,$
                                    therm_dets )
              therm_dets = therm_dets[wreord]
              temp_idx   = temp_idx[wreord]
           ENDIF
        ENDIF
     ENDELSE
     outstruct[i].file              = files[i]
     outstruct[i].shortfile         = shortfile
     outstruct[i].bbid              = bbid[i]
     outstruct[i].obsid             = obsid[i]
     outstruct[i].object            = object[i]
     outstruct[i].nchannels         = ndets
     outstruct[i].signal_extn       = wsignal[0]
     outstruct[i].mask_extn         = wmask[0]
     outstruct[i].ra_extn           = wra[0]
     outstruct[i].dec_extn          = wdec[0]
     outstruct[i].bolometer_names   = TEMPORARY(signal_dets)
     outstruct[i].light_bolos       = TEMPORARY(lightbolos)
     outstruct[i].signal_index      = TEMPORARY(detidx)
     outstruct[i].mask_index        = TEMPORARY(mask_detidx)
     outstruct[i].ra_index          = TEMPORARY(ra_detidx)
     outstruct[i].dec_index         = TEMPORARY(dec_detidx)
     IF has_temperature THEN BEGIN
        outstruct[i].ntherms           = ntherms
        outstruct[i].temperature_extn  = wtemperature[0]
        outstruct[i].temperature_names = TEMPORARY(therm_dets)
        outstruct[i].temperature_index = TEMPORARY(temp_idx)
     ENDIF
     IF has_quality THEN BEGIN
        outstruct[i].quality_extn      = wquality[0]
        outstruct[i].quality_index     = TEMPORARY(quality_detidx)
     ENDIF
  ENDFOR

  success = 1b
  IF N_ELEMENTS(files) EQ 1 THEN RETURN,outstruct[0] ELSE $
     RETURN,outstruct
END
  
