;+
;NAME
; smap_read_and_filter
;PURPOSE
; Reads in and applies timestream filtering operations
; to a data set.
;USAGE
;  tods = smap_read_and_filter( input, mapparam, [, /PTRS, $
;                               SUCCESS=success, ERRMSG=errmsg, $
;                               /PCA, /MEDFILT,
;                               ASTROMOFFSETS=astromoffsets, ... ] )
;INPUTS
;  input              Either a directory, a list of files, or a
;                      pointer array containing the already read TODs.
;                      If not passed in, then !SMAP_HIPE is used as
;                      an input directory
;OUTPUTS
;  mapparam           A structure with some summary info about tods
;RETURNS
;  The TODs after processing in SMAP format.  Either an array of
;   pointers (if /PTRS is set) or a list of filenames where the TODS
;   have been written after processing as .fits files.
;KEYWORDS
;   ptrs             Return PTRARR of all TODs instead of writing to disk.
;   smaptods         Input files are SMAP TODs already.  In this case,
;                     the input must be a list of TOD filenames rather
;                     than a directory (or pointers).
;   pca              Carry out PCA on the TODs
;   medfilt          Carry out median filtering on TODs.
;   wfilt            Apply Wiener filtering to TODs
;   sps              Handling sps data
;   jackknife        Jackknife the maps
;   sourcesub        Do source subtraction
;   readdsources     Add the sources back in after processing; only
;                     makes sense if /SOURCESUB also set.
;   nodeadcheck      Don't check for dead channels
;   verbose          Print status messages
;OPTINAL INPUTS
;   polyfilt         Apply polynomial filter to TODs.  This should be
;                     a string 'p?' where ? is the polynomial order.
;                     So, for example, 'p1' means a 1st order filter
;   jackmethod       Method for setting up jackknife.  Options are
;                     'random', 'half', and 'quarter'.  random is the
;                     default.  You need to have an even number of
;                     files for this to work.
;   mapsub           Maps to subtract.  Required if /SOURCESUB set.
;                     See make_timestream_from_maps for details;
;                     usually a pointer array of maps.
;   oneband          set to 1/2/3 to read only 250/350/500 micron detector 
;                     channels
;   speedcut         set to a float arcsec/s 1 < speedcut < 29 for which 
;                     can spped at which to mask points
;   badobsid         contains badobsid structure for manual masking.
;                     Really, the full specification involves the
;                     bolometers, obsid, and bbids (and optional samples).
;   todmask          structure array containing information for
;                     masking tod based on sky coords
;   linearcorr       A structure containing linear (with time) RA/DEC
;                     correction information.  Useful for a (slowly)
;                     moving source.  The structure should be of the
;                     form: { ra_slope:, dec_slope:, basetime: }
;                     and the correction will be
;                      ra +=  ra_slope*(tod.sampletime - basetime)
;                      dec += dec_slope*(tod.sampletime - basetime)
;   allowed_obsids   List of OBSIDs to allow in data set.
;   astromoffsets    structure containing per AOR astrometry
;                    offsets. simply passed through to smap_getlevel1
;   
;OPTIONAL OUTPUTS
;   success          0b on failure, 1b on success
;   errmsg           If failure, some description of the failure,
;                     otherwise ''
;MODIFICATION HISTORY
; Author: Alex Conley, Nov 2009
;
;CHANGELOG
; 2011-02-15 (GM): add astromoffsets keyword
; 2011-04-01 (GM): add oneband keyword
; 2011-04-15 (MZ): add speedcut keyword
; 2001-04-18 (MZ): add badobsid keyword
;-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Constructs a sufficiently short obsid string 
;;  for use in filenames
FUNCTION smap_read_and_filter_shortobsid, uobsid
  COMPILE_OPT IDL2, HIDDEN

  IF N_ELEMENTS(uobsid) EQ 0 THEN BEGIN
     MESSAGE,"No obisds specified, using no_name",/INF
     RETURN,"no_name"
  ENDIF

  maxlen = 200 ;;Gives a little room for other things
  cumlen = TOTAL(STRLEN('_0x'+TO_HEX(uobsid)),/CUM,/PRES)-1
  wst    = WHERE( cumlen LT maxlen, nst )
  IF nst EQ 0 THEN BEGIN
     MESSAGE,"Unable to construct sufficiently shortened "+$
             "obsid string"+$
             " for use in file names",/INF
     MESSAGE,"Using long_name as replacement",/INF
     obsid_short="long_name"
  ENDIF ELSE obsid_short = '0x'+STRJOIN(TO_HEX(uobsid[0:wst[nst-1]]),'_0x')
  RETURN,obsid_short
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Updates mapparam info for info in tod
PRO smap_read_and_filter_updatemapparam, tod, mapparam, excludemask,$
                                         VERBOSE=verbose
  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN
  
  do_mask = 0b
  IF N_ELEMENTS(excludemask) GT 0 AND $
     TAG_EXIST( tod, 'mask_bits', /TOP_LEVEL ) THEN BEGIN
     ;;We will tolerate this not working
     maskbits = construct_mask_bitmask( excludemask, tod.mask_bits,$
                                        SUCCESS=csuccess )
     IF csuccess EQ 0 THEN BEGIN
        IF KEYWORD_SET(verbose) THEN $
           MESSAGE,"Couldn't find mask bits for tod from "+tod.shortfile,/INF
     ENDIF ELSE do_mask=1b
  ENDIF 

  IF do_mask THEN BEGIN
     wgoodsamps = WHERE( (tod.mask AND maskbits) EQ 0, ngoodsamps,$
                         NCOMPLEMENT=nbad )
     IF ngoodsamps EQ 0 THEN BEGIN
        MESSAGE,"Couldn't find RA/DEC limits for "+tod.shortfile+$
                " as everything is masked",/INF
        RETURN
     ENDIF
     IF nbad EQ 0 THEN BEGIN
        ra = tod.ra
        dec = tod.dec
     ENDIF ELSE BEGIN
        ra = tod.ra[wgoodsamps]
        dec = tod.dec[wgoodsamps]
     ENDELSE
  ENDIF ELSE BEGIN
     ra = tod.ra
     dec = tod.dec
  ENDELSE

IF 0 THEN BEGIN ; comment this out for time being
  ;; remove the 0 -> 360 degeneracy...
  ;; if the derivative of ra ever goes completely crazy, we
  ;; probably have an event - the limit is 1 degree/sample
;  raderiv = ABS(DERIV(ra))   
;  IF MAX(raderiv) GT 1. THEN BEGIN
;     whwind = WHERE(raderiv EQ MAX(raderiv))
;     thismedlo = MEDIAN(ra[0:whwind-1]) 
;     thismedhi = MEDIAN(ra[whwind:N_ELEMENTS(ra) -1])
;     IF thismedlo GT thismedhi THEN BEGIN
;        ra[whwind:N_ELEMENTS(ra)-1] = $
;           ra[whwind:N_ELEMENTS(ra)-1] + 360.0
;     ENDIF ELSE BEGIN
;        ra[0L:whwind] = ra[0:whwind] + 360.
;     ENDELSE
;  ENDIF
;  IF MAX(ra,/NAN) LT 0 THEN ra+=360.0
ENDIF

  ;; alternate algorithm for min/max
  ;; assumes extent of field is < 180 degrees
  ;; use any sample as reference point
  
  ;; test for all nans here
  finiteind = WHERE(FINITE(ra) AND FINITE(dec), nfinite)
  IF nfinite EQ 0 THEN RETURN

  ;; first, ensure ra is in range 0->360
  ;; (I don't know if this is needed, but just to be sure -GM)
  ra = ((ra MOD 360) + 360) MOD 360

  ;; use minra as ref point
  ;; use first finite sample if minra non finite
  IF FINITE(mapparam.minra) THEN ra0 = mapparam.minra $
  ELSE ra0 = ra[finiteind[0]]

  ;; subtract ref point
  ra -= ra0

  ;; convert delta ra to be between -180 and 180 deg
  ra = ((ra + 180) MOD 360) - 180
  
  ;; add offset back on
  ;; all samples are now within +/-180 deg of ref point
  ra += ra0

  ramin = min(ra,/NAN,MAX=ramax)
  decmin = min(dec,/NAN,MAX=decmax)

;  stop
  ;; if this coordinate is further from the center of the map
  ;; than the previous one, overwrite it
  ;;We still test for finiteness because if -all- ra/dec are
  ;; non-finite, min/max will still return them
  IF ramin LT mapparam.minra AND FINITE(ramin) THEN mapparam.minra = ramin
  IF ramax GT mapparam.maxra AND FINITE(ramax) THEN mapparam.maxra = ramax
  IF decmin LT mapparam.mindec AND FINITE(decmin) THEN $
     mapparam.mindec = decmin
  IF decmax GT mapparam.maxdec AND FINITE(decmax) THEN $
     mapparam.maxdec = decmax

  mapparam.midra = 0.5*(mapparam.minra + mapparam.maxra)
  mapparam.middec = 0.5*(mapparam.mindec + mapparam.maxdec)

  RETURN

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Decide which half to jackknife
FUNCTION smap_read_and_filter_jackknife, ntods, METHOD=method,$
                                         SUCCESS=success, ERRMSG=errmsg
  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN
  success = 0b
  errmsg  = ''

  IF N_ELEMENTS( method ) EQ 0 THEN method_used = 'random' ELSE $
     method_used = STRLOWCASE( method )

  IF ntods MOD 2 EQ 1 THEN BEGIN
     errmsg = "Can't jack-knife odd number of inputs"
     RETURN,0
  ENDIF

  
  CASE method_used OF
     'random' : BEGIN
        signflip = BYTARR(ntods)
        signflip[(SORT(RANDOMU(seed,ntods)))[0:ntods/2-1]]=1b
     END
     'half' : BEGIN
        signflip = BYTARR(ntods)
        signflip[0:ntods/2-1] = 1b
     END
     'quarter' : BEGIN
        ;;We already know ntods is divisible by 2, but it may
        ;; not be divisible by 4, so we have to watch that
        signflip = BYTARR(ntods)
        qlen = ntods / 4
        signflip[0:qlen-1]=1b
        signflip[2*qlen:3*qlen-1] = 1b
        IF ntods MOD 4 EQ 2 THEN signflip[3*qlen]=1b
     END
     ELSE : BEGIN
        errmsg = "Unrecognized method for smap_read_and_filter_jackknife: "+$
                 method_used
        RETURN,0
     END
  ENDCASE
  success = 1b
  RETURN,signflip
END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get list of input files and their obsids
FUNCTION smap_read_and_filter_get_filelist, input, SUCCESS=success,$
   ERRMSG=errmsg, OBSID=obsid, ALLOWED_OBSIDS=allowed_obsids
  COMPILE_OPT IDL2, HIDDEN
  success = 0b
  errmsg = ''
  
  ninput = N_ELEMENTS(input)
  IF ninput EQ 0 THEN BEGIN
     errmsg = "No input to smap_read_and_filter_get_filelist"
     RETURN,''
  ENDIF

  wbad = WHERE( ~ FILE_TEST( input ), nbad )
  IF nbad NE 0 THEN BEGIN
     errmsg = "Couldn't find some inputs: "+STRJOIN(input[wbad],',')
     RETURN,''
  ENDIF

  wdir = WHERE( FILE_TEST( input, /DIRECTORY ), ndir, COMPLEMENT=wfile,$
                NCOMPLEMENT=nfile )

  ;;Get the OBSIDs of the files
  IF nfile NE 0 THEN BEGIN
     obsid = ULONARR(nfile)
     FOR j=0, nfile-1 DO BEGIN
        head = HEADFITS( input[wfile[j]], /SILENT, ERRMSG=head_errmsg )
        IF head_errmsg EQ '' THEN BEGIN
           val = SXPAR(head,'OBS_ID',COUNT=count,/SILENT)
           IF count NE 0 THEN obsid[j] = ULONG(val[0]) ELSE BEGIN
              errmsg = "Unable to determine obsid for "+input[wfile[j]]
              RETURN,''
           ENDELSE
        ENDIF
     ENDFOR
     filelist = input[wfile]
  ENDIF

  ;;Now do the dirs
  IF ndir NE 0 THEN BEGIN
     dfilelist = get_psp_filelist( input[wdir], OBSID=dobsid, $
                                   SUCCESS=psp_success, ERRMSG=errmsg,$
                                   ALLOWED_OBSIDS=allowed_obsids,$
                                   /REQUIREOBSID )
     IF psp_success EQ 0 THEN BEGIN
        errmsg = "Error processing directory names to get files: "+errmsg
        RETURN,''
     ENDIF
     IF nfile NE 0 THEN BEGIN
        filelist = [filelist,TEMPORARY(dfilelist)]
        obsid    = [obsid,TEMPORARY(dobsid)]
     ENDIF ELSE BEGIN
        filelist = TEMPORARY(dfilelist)
        obsid    = TEMPORARY(dobsid)
     ENDELSE
  ENDIF
  
  success = 1b
  RETURN,filelist
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Applies linear (with time) RA/DEC correction to tod using
;; information in linearcorr
PRO smap_read_and_filter_linearcorr, tod, linearcorr
  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN

  nchans = tod.nchans
  prodarr = (tod.samptime - linearcorr.basetime) ## REPLICATE(1.0d0,nchans)
  tod.ra  += linearcorr.ra_slope*prodarr
  tod.dec += linearcorr.dec_slope*prodarr
END
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Applies timestream filtering
;; Right now, these are in a fixed order.  We may want
;; to allow the user to re-order these somehow
;; If you add a new filter type, make sure to look at all the places
;;  this is called.
FUNCTION smap_read_and_filter_filter, tod, mapparam, MEDFILT=medfilt,$
                                      WFILT=wfilt, POLYFILT=polyfilt, $
                                      PCA=pca, LOWPCA=lowpca, $
                                      HIGHPCA=highpca, SOURCESUB=sourcesub, $
                                      MAPSUB=mapsub, MAPMASKBITS=mapmaskbits, $
                                      CUBIC=cubic, READDSOURCES=readdsources, $
                                      SUCCESS=success, ERRMSG=errmsg
  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN
  success = 0b
  errmsg  = ''

  do_sourcesub = 0b
  IF KEYWORD_SET( sourcesub ) AND N_ELEMENTS(mapsub) NE 0 THEN BEGIN
     IF SIZE( mapsub, /TNAME ) NE 'POINTER' THEN BEGIN
        errmsg = "Mapsub not expected type (pointers)"
        RETURN,!VALUES.F_NAN
     ENDIF
     do_sourcesub = 1b
  ENDIF

  ;;Make copy to work on, but don't erase the input!
  working_tod = tod

  ;;Build thing to subtract
  IF do_sourcesub THEN BEGIN
     subtrahend = make_timestream_from_maps( working_tod, mapsub,$
                                             MAPMASKBITS=mapmaskbits,$
                                             CUBIC=cubic, SUCCESS=tsuccess,$
                                             ERRMSG=errmsg )
     IF tsuccess EQ 0 THEN BEGIN
        errmsg = "Error building subtractive timestream for "+$
                 tod.shortfile+": "+errmsg
        RETURN,!VALUES.F_NAN
     ENDIF

     ;;Should add new mask bit support
     subtract_timestream, working_tod, subtrahend, SUCCESS=ssuccess,$
                          ERRMSG=errmsg
     IF ssuccess EQ 0 THEN BEGIN
        errmsg = "Error subtracting timestream from "+tod.shortfile+": "+errmsg
        RETURN,!VALUES.F_NAN
     ENDIF
  ENDIF

  ;;Median filter
  IF KEYWORD_SET( medfilt ) THEN BEGIN
     SMAP_FILTERTOD,working_tod,'m',SUCCESS=sft_success,ERRMSG=errmsg,$
                    VERBOSE=verbose
     IF sft_success EQ 0 THEN BEGIN
        errmsg = "Error applying median filter to "+tod.shortfile+": "+errmsg
        RETURN,!VALUES.F_NAN
     ENDIF
  ENDIF

  ;;Polynomial filter
  IF N_ELEMENTS( polyfilt ) NE 0 THEN BEGIN
     SMAP_FILTERTOD,working_tod,polyfilt,SUCCESS=sft_success,ERRMSG=errmsg,$
                    VERBOSE=verbose
     IF sft_success EQ 0 THEN BEGIN
        errmsg = "Error applying polynomial filter "+polyfilt+" to "+$
                 tod.shortfile+": "+errmsg
        RETURN,!VALUES.F_NAN
     ENDIF
  ENDIF

  ;;Weiner filter
  IF KEYWORD_SET( wfilt ) THEN BEGIN
     SMAP_FILTERTOD,working_tod,'w',SUCCESS=sft_success,ERRMSG=errmsg,$
                    VERBOSE=verbose
     IF sft_success EQ 0 THEN BEGIN
        errmsg = "Error applying Weiner filter to "+tod.shortfile+": "+errmsg
        RETURN,!VALUES.F_NAN
     ENDIF
  ENDIF

  ;;PCA
  IF KEYWORD_SET( pca ) THEN BEGIN
     working_tod = smap_pca( working_tod, mapparam, NLOW=lowpca,$
                             NHIGH=highpca, VERBOSE=verbose,$
                             SUCCESS=pca_success, ERRMSG=errmsg )
     IF pca_success EQ 0 THEN BEGIN
        errmsg = "Error applying PCA to "+tod.shortfile+": "+errmsg
        RETURN,!VALUES.F_NAN
     ENDIF
  ENDIF
  
  ;;add sources back in
  IF do_sourcesub AND KEYWORD_SET( readdsources ) THEN BEGIN
     subtract_timestream, working_tod, subtrahend, /ADD, SUCCESS=ssuccess,$
                          ERRMSG=errmsg
     IF ssuccess EQ 0 THEN BEGIN
        errmsg = "Error re-adding timestream from "+tod.shortfile+": "+errmsg
        RETURN,!VALUES.F_NAN
     ENDIF
  ENDIF

  success = 1b
  RETURN,working_tod
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION smap_read_and_filter_writetod, tod, outputdir, filename,$
                                        SUCCESS=success,$
                                        ERRMSG=errmsg
  COMPILE_OPT IDL2, HIDDEN
  success = 0b
  errmsg  = ''

  IF FILE_TEST(outputdir) AND (~FILE_TEST(outputdir,/DIRECTORY)) THEN BEGIN
     errmsg = "Output dir "+outputdir+" already exists but isn't directory!"
     RETURN,''
  ENDIF

  IF ~ FILE_TEST(outputdir) THEN FILE_MKDIR,outputdir

  full_filename = ADDSLASH(outputdir)+filename

  ;;And write
  smap_writetod, tod, full_filename, /NO_ABORT, ERRMSG=errmsg,$
                 SUCCESS=success
  RETURN,full_filename
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Gets the list of files, etc.
PRO smap_read_and_filter_prepare_files, input, mapparam, filelist, fileinfo,$
                                        ALLOWED_OBSIDS=allowed_obsids,$
                                        SPS=sps, SUCCESS=success,$
                                        ERRMSG=errmsg
  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN
  success = 0b
  errmsg  = ''
  
  ;;Get file list
  filelist = smap_read_and_filter_get_filelist( input, SUCCESS=fsuccess,$
                                                ERRMSG=errmsg, OBSID=obsid,$
                                                ALLOWED_OBSIDS=allowed_obsids)
  IF fsuccess EQ 0 THEN BEGIN
     errmsg = "Failure to get filelist: "+errmsg
     RETURN
  ENDIF
  
  ;;Get file information for these files
  fileinfo = PARSE_TIMESTREAM_DETINFO(filelist, SUCCESS=ptd_success,$
                                      ERRMSG=errmsg, NOTEMPERATURE=sps, $
                                      SIGNAL=sps)
  IF ptd_success EQ 0 THEN BEGIN
     errmsg = "ERROR from parse_timestream_detinfo: " + errmsg
     RETURN
  ENDIF
  
  nfiles = N_ELEMENTS(filelist)
  
  ;;Setup map params

  ;;First, the unique obs ids
  uobsid = fileinfo[ UNIQ( fileinfo.obsid, SORT(fileinfo.obsid) ) ].obsid
  obsid_str = '0x'+STRJOIN(TO_HEX(uobsid),'_0x')
  IF STRLEN(obsid_str) GT 500 THEN $
     obsid_short=smap_read_and_filter_shortobsid(uobsid) ELSE $
        obsid_short = obsid_str

  mapparam = { obsids_short: obsid_short, $
               obsids: obsid_str, file_obsids: fileinfo.obsid,$
               file_bbids: fileinfo.bbid, bands: ['PSW','PMW','PLW'], $
               nscans: N_ELEMENTS(fileinfo), $
               filenames: filelist, scanlength: fileinfo.nsamps,$
               midra: !VALUES.F_NAN, middec: !VALUES.F_NAN,$
               minra: !VALUES.F_INFINITY, mindec: !VALUES.F_INFINITY,$
               maxra: -!VALUES.F_INFINITY, maxdec: -!VALUES.F_INFINITY }
  
  success = 1b
  RETURN
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Figures out names to write TODs to.  This must
;; be done globally because OBSIDS/BBIDS aren't unique
FUNCTION smap_read_and_filter_out_filenames, mapparam
  COMPILE_OPT IDL2, HIDDEN

  IF SIZE(mapparam,/TNAME) NE 'STRUCT' THEN $
     MESSAGE,"Input mapparam not as expected"
  
  nfiles = mapparam.nscans
  obsid_bbid = TO_HEX( mapparam.file_obsids ) + '_' + $
               TO_HEX( mapparam.file_bbids )
  uniqvals = UNIQ( obsid_bbid, SORT(obsid_bbid) )
  IF N_ELEMENTS(uniqvals) EQ nfiles THEN $
     RETURN,obsid_bbid+'_tod.fits'
  
  ;;Otherwise, we have some non-unique values
  retarr = obsid_bbid
  chars = ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o',$
           'p','q','r','s','t','u','v','w','x','y','z']
  nchars = N_ELEMENTS(chars)
  FOR i=0, N_ELEMENTS(uniqvals)-1 DO BEGIN
     ;;Do this brute-force, which is inefficient but doesn't matter
     ;; here because the number of files can never be more than a few
     ;; hundred
     wthis = WHERE( obsid_bbid EQ obsid_bbid[uniqvals[i]], nthis )
     IF nthis GT 1 THEN BEGIN
        IF nthis GE nchars*(nchars+1) THEN $
           MESSAGE,"Too many files to assign names to"
        IF nthis GT nchars THEN BEGIN
           ;;Complicated case.  Do then nchars at a time
           FOR j = 0, nthis, nchars DO BEGIN
              minidx = j
              maxidx = (j + nchars-1) < (nthis-1)
              IF j EQ 0 THEN char_start = '' ELSE char_start = chars[j/nchars-1]
              retarr[wthis[minidx:maxidx]] += char_start + $
                                              chars[0:maxidx-minidx]
           ENDFOR
        ENDIF ELSE BEGIN
           retarr[wthis] += chars[0:nthis-1]
        ENDELSE
     ENDIF
     retarr[wthis] += '_tod.fits'
  ENDFOR
  RETURN,retarr
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Main routine

FUNCTION smap_read_and_filter, input, mapparam, EXNAME=exname, SPS=sps,$
                               PTRS=ptrs, JACKKNIFE=jackknife,$
                               SUCCESS=success, ERRMSG=errmsg, WFILT=wfilt,$
                               PCA=pca, LOWPCA=lowpca, HIGHPCA=highpca,$
                               MEDFILT=medfilt, POLYFILT=polyfilt,$
                               SOURCESUB=sourcesub, SMAPTODS=smaptods,$
                               MAPSUB=mapsub, MAPMASKBITS=mapmaskbits,$
                               CUBIC=cubic, READDSOURCES=readdsources,$
                               ASTROMOFFSETS=astromoffsets, ONEBAND=oneband, $
                               JACKMETHOD=jackmethod, SPEEDCUT=speedcut, $
                               LINEARCORR=linearcorr, BADOBSID=badobsid,$
                               TODMASK=todmask, $
                               ALLOWED_OBSIDS=allowed_obsids,VERBOSE=verbose,$
                               NODEADCHECK=nodeadcheck
  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg  = ''

  IF N_ELEMENTS(excludemask) EQ 0 THEN $
     excludemask=['maskMaster','maskDead'] ELSE BEGIN
     IF SIZE( excludemask, /TNAME ) NE 'STRING' THEN BEGIN
        errmsg = "Didn't get expected string type for excludemask, but "+$
                 SIZE( excludemask, /TNAME )
        GOTO, err_handler
     ENDIF
  ENDELSE

  IF KEYWORD_SET(verbose) THEN BEGIN
     IF KEYWORD_SET( sourcesub ) AND N_ELEMENTS(mapsub) EQ 0 THEN $
        MESSAGE,"sourcesub set but no maps to subtract provided -- skipping",$
                /INF
     IF KEYWORD_SET(readdsources) AND (~KEYWORD_SET(sourcesub)) THEN $
        MESSAGE,"readdsources set but sourcesub not -- ignoring",/INF
     IF KEYWORD_SET( wfilt ) AND KEYWORD_SET( medfilt ) THEN $
        MESSAGE,"WARNING: Applying both weiner and median filter",/INF
  ENDIF

  IF N_ELEMENTS( polyfilt ) NE 0 && $
     SIZE( polyfilt, /TNAME ) NE 'STRING' THEN BEGIN
     errmsg = "Polynomial filter set, but not the expected string"
     GOTO, err_handler
  ENDIF

  ;;If input is empty, then assume !SMAP_HIPE
  IF N_ELEMENTS(input) EQ 0 THEN BEGIN
     ;;User didn't specify, use !SMAP_HIPE
     DEFSYSV,'!SMAP_HIPE',EXISTS=sh_exist
     IF sh_exist EQ 0 THEN BEGIN
        errmsg = "Don't know where to look for files; no dir or !SMAP_HIPE"+$
                 " set"
        GOTO, err_handler
     ENDIF
     i_input = !SMAP_HIPE
  ENDIF ELSE i_input = input 

  IF N_ELEMENTS( linearcorr ) NE 0 THEN BEGIN
     IF SIZE(linearcorr,/TNAME) NE 'STRUCT' THEN BEGIN
        errmsg = "linearcorr is not a structure"
        GOTO, err_handler
     ENDIF
     reqtags = ['basetime','ra_slope','dec_slope']
     FOR ctr = 0, N_ELEMENTS(reqtags)-1 DO BEGIN
        IF ~ TAG_EXIST( linearcorr, reqtags[i], /TOP ) THEN BEGIN
           errmsg = "linearcorr doesn't have ."+reqtags[i]+" information"
           GOTO, err_handler
        ENDIF
     ENDFOR
     IF ~ (FINITE( linearcorr.basetime ) AND FINITE( linearcorr.ra_slope ) AND $
           FINITE( linearcorr.dec_slope ) ) THEN BEGIN
        errmsg = "Non finite components of linearcorr"
        GOTO, err_handler
     ENDIF
  ENDIF

  ;;Main processing
  input_type = SIZE( i_input, /TNAME )
  IF input_type EQ 'STRING' THEN BEGIN

     ;;Already written to disk as TODs
     IF KEYWORD_SET( smaptods ) THEN BEGIN
        ;;The task is to setup the mapparam and possibly apply
        ;; filtering/shifting operations
        ntods = N_ELEMENTS(i_input)

        IF KEYWORD_SET( jackknife ) THEN BEGIN
           IF KEYWORD_SET(verbose) THEN MESSAGE,"Setting up jackknife",/INF
           signflip = smap_read_and_filter_jackknife(ntods,SUCCESS=jsuccess,$
                                                     METHOD=jackmethod,$
                                                     ERRMSG=errmsg)
           IF jsuccess EQ 0 THEN BEGIN
              errmsg = "Error setting up jackknife: "+errmsg
              GOTO, err_handler
           ENDIF
        ENDIF

        mapparam = { obsids_short: '', obsids: '', $
                     file_obsids: ULONARR(ntods),$
                     file_bbids: ULONARR(ntods), bands: ['PSW','PMW','PLW'], $
                     nscans: ntods,$
                     filenames: STRARR(ntods), scanlength: INTARR(ntods),$
                     midra: !VALUES.F_NAN, middec: !VALUES.F_NAN,$
                     minra: !VALUES.F_INFINITY, mindec: !VALUES.F_INFINITY,$
                     maxra: -!VALUES.F_INFINITY, maxdec: -!VALUES.F_INFINITY }
        ;;Loop over TODS
        ;;If we are writing the filtered tods out to disk, we
        ;; have to do this twice: first to get the obsid/bbids
        ;; second to do the filtering.  For PTRs we can do it once.
        IF ~ KEYWORD_SET(ptrs) THEN BEGIN
           FOR i=0,ntods-1 DO BEGIN
              curr_tod = smap_readtod( i_input[i], SUCCESS=tod_success, $
                                       ERRMSG=errmsg )
              IF tod_success EQ 0 THEN BEGIN
                 errmsg = "ERROR reading tod: "+i_input[i]+" : "+errmsg
                 GOTO, err_handler
              ENDIF
              
              mapparam.file_obsids[i]     = curr_tod.obsid
              mapparam.file_bbids[i]      = curr_tod.bbid
              mapparam.filenames[i]  = curr_tod.progenitorfile
              mapparam.scanlength[i] = curr_tod.nsamps
           ENDFOR
           uobsid = obsids[ UNIQ( obsids, SORT(obsids) ) ]
           obsid_str = '0x'+STRJOIN(TO_HEX(uobsid),'_0x')
           IF STRLEN(obsid_str) GT 500 THEN $
              obsid_short=smap_read_and_filter_shortobsid(uobsid) ELSE $
                 obsid_short = obsid_str
           mapparam.obsids  = obsid_str
           mapparam.obsids_short = TEMPORARY(obsid_short)
           outfilenames = smap_read_and_filter_out_filenames( mapparam )
           retarr = STRARR( ntods )
        ENDIF ELSE BEGIN
           retarr = PTRARR( ntods )
        ENDELSE

        FOR i=0,ntods-1 DO BEGIN
           IF KEYWORD_SET( verbose ) AND ntods GT 1 THEN $
              MESSAGE,STRING(i+1,100.0*i/(ntods-1),ntods,$
                             FORMAT='(" On scan ",I4," of ",I0," [",F5.1,"%]")'),/INF

           curr_tod = smap_readtod( i_input[i], SUCCESS=tod_success, $
                                    ERRMSG=errmsg )
           IF tod_success EQ 0 THEN BEGIN
              errmsg = "ERROR reading tod: "+i_input[i]+" : "+errmsg
              GOTO, err_handler
           ENDIF

           IF KEYWORD_SET( ptrs ) THEN BEGIN
              mapparam.file_obsids[i]     = curr_tod.obsid
              mapparam.file_bbids[i]      = curr_tod.bbid
              mapparam.filenames[i]  = curr_tod.shortfile
              mapparam.scanlength[i] = curr_tod.nsamps
           ENDIF

           ;;Update RA/DEC limit
           smap_read_and_filter_updatemapparam, curr_tod, mapparam,$
                                                excludemask, VERBOSE=verbose
        
           ;;Apply filter
           IF KEYWORD_SET(sourcesub) OR KEYWORD_SET(medfilt) OR $
              KEYWORD_SET(polyfilt) OR KEYWORD_SET(wfilt) OR $
              KEYWORD_SET(pca) THEN BEGIN
              curr_tod = $
                 smap_read_and_filter_filter(curr_tod, mapparam, PCA=pca, $
                                             LOWPCA=lowpca, HIGHPCA=highpca,$
                                             MEDFILT=medfilt, MAPSUB=mapsub, $
                                             SOURCESUB=sourcesub,$
                                             CUBIC=cubic, WFILT=wfilt,$
                                             MAPMASKBITS=mapmaskbits, $
                                             READDSOURCES=readdsources,$
                                             SUCCESS=fsuccess, ERRMSG=errmsg )
              IF fsuccess EQ 0 THEN BEGIN
                 errmsg = "Error filtering for TOD: "+errmsg
                 GOTO, err_handler
              ENDIF
           ENDIF

           ;;Apply shift
           IF N_ELEMENTS(linearcorr) NE 0 THEN $
              smap_read_and_filter_linearcorr, curr_tod, linearcorr

           
           IF KEYWORD_SET(jackknife) && signflip[i] THEN $
              filtered_tod.signal *= -1.0d0

           ;;Write or append
           IF KEYWORD_SET(ptrs) THEN BEGIN
              retarr[i] = PTR_NEW( TEMPORARY( curr_tod ) )
           ENDIF ELSE BEGIN
              retarr[i] = $
                 smap_read_and_filter_writetod(TEMPORARY(curr_tod),$
                                               outputdir, outfilenames[i],$
                                               SUCCESS=wsuccess,$
                                               ERRMSG=errmsg)
              IF wsuccess EQ 0 THEN BEGIN
                 errmsg = "Error writing out filtered tod: "+ errmsg
                 GOTO, err_handler
              ENDIF
           ENDELSE
        ENDFOR

        ;;Update mapparam info if using pointers
        IF KEYWORD_SET(ptrs) THEN BEGIN
           uobsid = mapparam.file_obsids[ UNIQ( mapparam.file_obsids, $
                                                SORT(mapparam.file_obsids) ) ]
           obsid_str = '0x'+STRJOIN(TO_HEX(uobsid),'_0x')
           mapparam.obsids  = obsid_str
           IF STRLEN(obsid_str) GT 500 THEN $
              obsid_short=smap_read_and_filter_shortobsid(uobsid) ELSE $
                 obsid_short = obsid_str
           mapparam.obsids_short = TEMPORARY(obsid_short)
        ENDIF
     ENDIF ELSE BEGIN
        ;;Normal level-1 files
        IF KEYWORD_SET(verbose) THEN $
           MESSAGE,"Preparing input files",/INF
        smap_read_and_filter_prepare_files, i_input, mapparam, filelist,$
                                            fileinfo, SUCCESS=fsuccess,$
                                            ERRMSG=errmsg, SPS=sps,$
                                            ALLOWED_OBSIDS=allowed_obsids
        IF fsuccess EQ 0 THEN BEGIN
           errmsg = "While preparing input files: "+errmsg
           GOTO, err_handler
        ENDIF
        
        ;;Construct the output directory name if needed
        IF ~ KEYWORD_SET(ptrs) THEN BEGIN
           outputdir = addslash(!SMAP_DATA)+mapparam.obsids
           IF N_ELEMENTS(exname) NE 0 && STRLEN(exname) NE '' THEN $
              outputdir += '_'+exname
           IF KEYWORD_SET(verbose) THEN $
              MESSAGE,"Will write TODs to: "+outputdir,/INF
        ENDIF
        
        ;;And output filenames if needed
        IF ~ KEYWORD_SET( ptrs ) THEN $
           outfilenames = smap_read_and_filter_out_filenames( mapparam )
        
        nfiles = N_ELEMENTS(filelist)
        
        IF KEYWORD_SET( jackknife ) THEN BEGIN
           IF KEYWORD_SET(verbose) THEN $
              MESSAGE,"Setting up jackknife",/INF
           signflip = smap_read_and_filter_jackknife(nfiles,SUCCESS=jsuccess,$
                                                     METHOD=jackmethod,$
                                                     ERRMSG=errmsg)
           IF jsuccess EQ 0 THEN BEGIN
              errmsg = "Error setting up jackknifing: "+errmsg
              GOTO, err_handler
           ENDIF
        ENDIF
        
        IF KEYWORD_SET( ptrs ) THEN retarr = PTRARR(nfiles) ELSE $
           retarr = STRARR(nfiles)
        ;;Process each TOD
        IF KEYWORD_SET( verbose ) THEN $
           MESSAGE,"Entering main loop",/INF
        FOR i=0, nfiles-1 DO BEGIN
           IF KEYWORD_SET( verbose ) THEN $
              MESSAGE,STRING(i,100.0*i/(nfiles-1),$
                             FORMAT='(" On scan ",I4," [",F5.1,"%]")'),/INF

           ;;Do reads
           current_tod = smap_getlevel1( filelist[i], FILEINFO=fileinfo[i],$
                                         ASTROMOFFSETS=astromoffsets, $
                                         ONEBAND=oneband, SPEEDCUT=speedcut, $
                                         BADOBSID=badobsid, TODMASK=todmask, $
                                         SPS=sps, SUCCESS=sgl_success,$
                                         ERRMSG=errmsg, VERBOSE=verbose)

           IF ~sgl_success THEN BEGIN
              errmsg = "Error from SMAP_GETLEVEL1: " + errmsg
              GOTO, err_handler
           ENDIF
           
           ;;Check for dead channels
           IF ~ KEYWORD_SET(nodeadcheck) THEN BEGIN
              mask_noresponse_channels, current_tod, SUCCESS=dsuccess, $
                                      ERRMSG=derrmsg
              IF ~ dsuccess THEN BEGIN
                 errmsg = "Error masking no response channels: "+derrmsg
                 GOTO, err_handler
              ENDIF
           ENDIF

           ;;Update RA/DEC limit
           smap_read_and_filter_updatemapparam, current_tod, mapparam,$
                                                excludemask, VERBOSE=verbose
           
           ;;Do filtering
           IF KEYWORD_SET(sourcesub) OR KEYWORD_SET(medfilt) OR $
              KEYWORD_SET(polyfilt) OR KEYWORD_SET(wfilt) OR $
              KEYWORD_SET(pca) THEN BEGIN
              filtered_tod = $
                 smap_read_and_filter_filter( TEMPORARY(current_tod), mapparam,$
                                              MEDFILT=medfilt, PCA=pca, $
                                              WFILT=wfilt, CUBIC=cubic,$
                                              LOWPCA=lowpca, HIGHPCA=highpca,$
                                              SOURCESUB=sourcesub, $
                                              MAPSUB=mapsub, $
                                              MAPMASKBITS=mapmaskbits, $
                                              READDSOURCES=readdsources,$
                                              SUCCESS=fsuccess, ERRMSG=errmsg )
              IF fsuccess EQ 0 THEN BEGIN
                 errmsg = "Error filtering for TOD from "+filelist[i]+": "+$
                          errmsg
                 GOTO, err_handler
              ENDIF
           ENDIF ELSE filtered_tod = TEMPORARY(current_tod)
           
           IF N_ELEMENTS(linearcorr) NE 0 THEN $
              smap_read_and_filter_linearcorr, filtered_tod, linearcorr

           IF KEYWORD_SET(jackknife) && signflip[i] THEN $
              filtered_tod.signal *= -1.0d0

           ;;Write or append
           IF KEYWORD_SET(ptrs) THEN BEGIN
              retarr[i] = PTR_NEW( TEMPORARY( filtered_tod ) )
           ENDIF ELSE BEGIN
              retarr[i] = $
                 smap_read_and_filter_writetod(TEMPORARY(filtered_tod),$
                                               outputdir, outfilenames[i], $
                                               SUCCESS=wsuccess,$
                                               ERRMSG=errmsg)
              IF wsuccess EQ 0 THEN BEGIN
                 errmsg = "Error writing out filtered tod from "+filelist[i]+$
                          ": "+ errmsg
                 GOTO, err_handler
              ENDIF
           ENDELSE
        ENDFOR
     ENDELSE
  ENDIF ELSE IF input_type EQ 'POINTER' THEN BEGIN
     ntods = N_ELEMENTS(input)
     IF KEYWORD_SET( jackknife ) THEN BEGIN
        IF KEYWORD_SET(verbose) THEN MESSAGE,"Setting up jackknife",/INF
        signflip = smap_read_and_filter_jackknife(ntods,SUCCESS=jsuccess,$
                                                  METHOD=jackmethod,$
                                                  ERRMSG=errmsg)
        IF jsuccess EQ 0 THEN BEGIN
           errmsg = "Error setting up jackknife: "+errmsg
           GOTO, err_handler
        ENDIF
     ENDIF

     ;;Construct map param
     obsids = ULONARR(ntods)
     FOR i=0,ntods-1 DO obsids[i] = (*i_input[i]).obsid
     bbids = ULONARR(ntods)
     FOR i=0,ntods-1 DO bbids[i] = (*i_input[i]).bbid
     uobsid = obsids[ UNIQ( obsids, SORT(obsids) ) ]
     obsid_str = '0x'+STRJOIN(TO_HEX(uobsid),'_0x')
     IF STRLEN(obsid_str) GT 500 THEN $
        obsid_short=smap_read_and_filter_shortobsid(uobsid) ELSE $
           obsid_short = obsid_str
     mapparam = { obsids_short: obsid_short,$
                  obsids: obsid_str, file_obsids: TEMPORARY(obsids),$
                  file_bbids: TEMPORARY(bbids), bands: ['PSW','PMW','PLW'], $
                  nscans: ntods,$
                  filenames: STRARR(ntods), scanlength: INTARR(ntods),$
                  midra: !VALUES.F_NAN, middec: !VALUES.F_NAN,$
                  minra: !VALUES.F_INFINITY, mindec: !VALUES.F_INFINITY,$
                  maxra: -!VALUES.F_INFINITY, maxdec: -!VALUES.F_INFINITY }
     FOR i=0,ntods-1 DO mapparam.filenames[i]=(*i_input[i]).progenitorfile
     FOR i=0,ntods-1 DO mapparam.scanlength[i]=(*i_input[i]).nsamps

     ;;Construct the output directory name
     IF ~ KEYWORD_SET(ptrs) THEN BEGIN
        outputdir = addslash(!SMAP_DATA)+obsid_str
        IF N_ELEMENTS(exname) NE 0 && STRLEN(exname) NE '' THEN $
           outputdir += '_'+exname
        IF KEYWORD_SET(verbose) THEN $
           MESSAGE,"Will write TODs to: "+outputdir,/INF
     ENDIF

     IF KEYWORD_SET( ptrs ) THEN retarr = PTRARR( ntods ) ELSE $
        retarr = STRARR( ntods )
     IF KEYWORD_SET( verbose ) THEN MESSAGE,"Entering main loop",/INF

     ;;And output filenames if needed
     IF ~ KEYWORD_SET( ptrs ) THEN $
        outfilenames = smap_read_and_filter_out_filenames( mapparam )

     FOR i=0, ntods-1 DO BEGIN
        IF KEYWORD_SET( verbose ) THEN $
           MESSAGE,STRING(i,100.0*i/(ntods-1),$
                          FORMAT='(" On scan ",I4," [",F5.1,"%]")'),/INF
        ;;Update RA/DEC limit
        smap_read_and_filter_updatemapparam, *i_input[i], mapparam,$
                                             excludemask, VERBOSE=verbose

        ;;Do filtering
        filtered_tod = $
           smap_read_and_filter_filter( *i_input[i], mapparam, PCA=pca, $
                                        LOWPCA=lowpca, HIGHPCA=highpca,$
                                        MEDFILT=medfilt, $
                                        SOURCESUB=sourcesub,$
                                        MAPSUB=mapsub, $
                                        MAPMASKBITS=mapmaskbits,$
                                        CUBIC=cubic, $
                                        READDSOURCES=readdsources,$
                                        WFILT=wfilt, SUCCESS=fsuccess, $
                                        ERRMSG=errmsg )
        IF fsuccess EQ 0 THEN BEGIN
           errmsg = "Error filtering for TOD: "+errmsg
           GOTO, err_handler
        ENDIF

        IF KEYWORD_SET(jackknife) && signflip[i] THEN $
           filtered_tod.signal *= -1.0d0

        ;;Write or append
        IF KEYWORD_SET(ptrs) THEN BEGIN
           retarr[i] = PTR_NEW( TEMPORARY( filtered_tod ) )
        ENDIF ELSE BEGIN
           retarr[i] = $
              smap_read_and_filter_writetod(TEMPORARY(filtered_tod),$
                                            outputdir, outfilenames[i],$
                                            SUCCESS=wsuccess,$
                                            ERRMSG=errmsg)
           IF wsuccess EQ 0 THEN BEGIN
              errmsg = "Error writing out filtered tod: "+ errmsg
              GOTO, err_handler
           ENDIF
        ENDELSE

     ENDFOR
  ENDIF ELSE BEGIN
     errmsg = "Didn't get expected type for input: "+input_type
     errmsg += " (expected either STRING or POINTER)"
     GOTO, err_handler
  ENDELSE

  ;;Write mapparam to output dir if we are doing file writes
  IF ~ KEYWORD_SET( ptrs ) THEN BEGIN
     outfile = ADDSLASH(outputdir)+mapparam.obsids+'_mapparam.fits'
     MWRFITS,mapparam,outfile,/SILENT,/CREATE,STATUS=status
     IF status NE 0 THEN BEGIN
        errmsg = "Error writing mapparam file"
        GOTO, err_handler
     ENDIF
  ENDIF

  ;;Successful exit
  success = 1b
  RETURN,retarr

  ;;Bad exit
  err_handler:
  IF KEYWORD_SET( verbose ) THEN MESSAGE,errmsg,/INF
  IF N_ELEMENTS(retarr) NE 0 && SIZE(retarr,/TNAME) EQ 'POINTER' THEN $
     PTR_FREE,retarr
  RETURN,!VALUES.F_NAN

END
