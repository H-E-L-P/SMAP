;+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  function smap_setupmap.pro
;;  Aug 14, 2009
;;  Mike Zemcov
;;  This function takes an directory path and does the
;;  following:
;;   1) reads in the hipe-formatted data 
;;   2) figures out how big the map needs to be to hold the data 
;;   3) saves the raw scans in smap-standard format 
;;   4) creates a properly initialized map structure
;;   5) (optionally) gives back the map size parameters.
;;   6) (optionally) returns the info from parse_timestream_detinfo
;;      The latter two are saved to files
;;   You can either specify a list of input files (filelist), an
;;   input directory (dir), or the input dir via !SMAP_HIPE
;;  Returns:
;;    A structure of map parameters (mapparam)
;;  Optional Inputs:
;;           exname = extended file name string
;;           scandown = lowest scan number to consider
;;           scanup = highest scan number to consider                       
;;           sps = data is from SPS, and hence is missing temperature
;;                  and quality extensions
;;           pixsize = [PSW,PMW,PLW] pixel size in degrees
;;           dir = Directory to look for files in.  See
;;                  get_psp_filelist.  Can be an array.
;;           infiles = list of input files.
;;           verbose = verbose flag, 0 = silent
;;  Optional input/outputs:
;;           fileinfo = Info from parse_timestream_detinfo for input
;;                       files.  Can be an input or an output.  If
;;                       input and scandown/scanup used, may be
;;                       modified on output
;;  Optional outputs:
;;           map250 = 250 um map structure initialized for these obs
;;           map350 = 350 um map structure initialized for these obs
;;           map500 = 500 um map structure initialized for these obs
;;           filelist =  list of TOD filenames resaved in SMAP format
;;           success = success flag, 1 = successful, 0 = not
;;           errmsg = error message string, only not empty if error
;;  File outputs:
;;     Writes a number of files to !SMAP_DATA/0x<obsid>_<exname>:
;;       1) 0x<obsid>_mapparam_<exname>_idltod.fits
;;       2) 0x<obsid>_fileinfo_<exname>_idltod.fits
;;       3) One file for each scan:
;;          0x<obsid>_<scan number>_<exname>_idltod.fits
;;  Changelog: v1.0, MZ, Aug 14 2009, original
;;             v1.1, MZ, Aug 30, 2009, changed this function to play
;;             nicely with passing whole initialize map structures back.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-
FUNCTION WRITE_ANCILLARY,datastruct,obsid,iscan,$
                         EXNAME=exname,SILENT=silent,$
                         DIR=dir, NO_ABORT=no_abort, $
                         ERRMSG=errmsg,SUCCESS=success
  COMPILE_OPT IDL2, HIDDEN

  success = 0b
  errmsg = ''

  silent = KEYWORD_SET(silent)
  abort  = ~ KEYWORD_SET(no_abort)

  ; check to see what the state of passing parameters is - if they're
  ; not defined, bail
  IF (N_ELEMENTS(datastruct) EQ 0) THEN BEGIN
     errmsg = 'No map structure passed to me, aborting!'
     IF abort THEN MESSAGE,errmsg ELSE BEGIN
        IF ~ silent THEN MESSAGE,errmsg,/INF
        RETURN,''
     ENDELSE
  ENDIF
  IF SIZE(datastruct,/TNAME) NE 'STRUCT' THEN BEGIN
     errmsg = 'Map structure is not a structure, aborting!'
     IF abort THEN MESSAGE,errmsg ELSE BEGIN
        IF ~ silent THEN MESSAGE,errmsg,/INF
        RETURN,''
     ENDELSE
  ENDIF
  IF (N_ELEMENTS(obsid) EQ 0) THEN BEGIN
     errmsg = 'No obsid passed to me, aborting!'
     IF abort THEN MESSAGE,errmsg ELSE BEGIN
        IF ~ silent THEN MESSAGE,errmsg,/INF
        RETURN,''
     ENDELSE
  ENDIF
  IF (N_ELEMENTS(iscan) EQ 0) THEN BEGIN
     errmsg = 'No scan number passed to me, aborting!'
     IF abort THEN MESSAGE,errmsg ELSE BEGIN
        IF ~ silent THEN MESSAGE,errmsg,/INF
        RETURN,''
     ENDELSE
  ENDIF

  IF SIZE(obsid,/TNAME) EQ 'STRING' THEN obsid_str = obsid ELSE $
     obsid_str = '0x'+TO_HEX(obsid)

  ;;Get output directory name
  IF N_ELEMENTS( dir ) EQ 0 THEN BEGIN
     dir = addslash(!SMAP_DATA)+obsid_str
     IF STRLEN(exname) NE 0 THEN dir += '_'+exname
     IF ~ FILE_TEST( dir ) THEN FILE_MKDIR,dir
  ENDIF
  IF ~ FILE_TEST(dir,/DIRECTORY,/WRITE) THEN BEGIN
     errmsg = "Unable to write to output directory: "+dir
     IF abort THEN MESSAGE,errmsg ELSE BEGIN
        IF ~ silent THEN MESSAGE,errmsg,/INF
        RETURN,''
     ENDELSE
  ENDIF

  ; tell me what you're up to
  IF ~ silent THEN MESSAGE,'Writing FITS files.',/INFORMATIONAL,LEVEL=-1

  ;; make the file name from known info;
  ;; file name looks like: "obsid"_"iscan"_"exname"_idltod.fits
  ;; where exname is completely omitted if it's ''
  filename = addslash(dir) + obsid_str + '_'+ $
             STRCOMPRESS(STRING(iscan),/REMOVE_ALL) 
  IF STRLEN(exname) NE 0 THEN filename += '_'+exname
  filename += '_idltod.fits'

  IF FILE_TEST(filename) AND ~ silent THEN $
     MESSAGE,"Will overwrite "+filename,/INF,LEVEL=-1

  ; make a dummy primary fits HDU
  MWRFITS, datastruct, filename, /CREATE, /SILENT
  
  success = 1b
  RETURN,filename

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION SMAP_SETUPMAP,EXNAME=exname,SCANDOWN=scandown,SCANUP=scanup,$   
                       PIXSIZE=pixsize,MAP250=map250,MAP350=map350,$
                       MAP500=map500,DIR=dir,FILELIST=filelist,$
                       SPS=sps,FILEINFO=fileinfo, VERBOSE=verbose,$
                       SUCCESS=success, ERRMSG=errmsg, INFILES=infiles

  COMPILE_OPT IDL2

  ; set up the error handling things
  success = 0b
  errmsg  = ''

  ; these are 'optional' options that require a default value to be set 
  ; if they're not specified
  IF ~(KEYWORD_SET(verbose)) THEN verbose = 0b

  ; this is the master place to set the pixel size if it's
  ; not been defined!  To put it another way: it's hardwired to
  ; make pixels of this size here, and if you don't like it you 
  ; should call with pixsize set or change the master here.
  defparams = SMAP_GETDEFPARAMS(PIXSIZE=pixsize)
  IF ~KEYWORD_SET(pixsize) THEN pixsize = defparams.pixsize

  ;; if we didn't get an exname, set it to null
  IF ~(KEYWORD_SET(exname)) THEN BEGIN
     msg = 'EXNAME not set, assuming null.'
     IF KEYWORD_SET(verbose) THEN MESSAGE,msg,/INF
     exname = ''
  ENDIF ELSE BEGIN
     ; and if we did, tell me if I want to know
     msg = 'EXNAME set to ' + exname
     IF KEYWORD_SET(verbose) THEN MESSAGE,msg,/INF
  ENDELSE

  ;; Now try to figure out where to look for files
  IF N_ELEMENTS(infiles) NE 0 AND N_ELEMENTS(dir) EQ 0 THEN BEGIN
     ;;User specified list.  We need the obsids
     wbad = WHERE( ~ FILE_TEST(infiles,/READ), nbad )
     IF nbad NE 0 THEN BEGIN
        errmsg = "Couldn't read input files: "+STRJOIN(infiles[wbad],', ')
        RETURN,success
     ENDIF
     obsid = ULONARR( N_ELEMENTS(infiles) )
     obsid[*] = -1
     FOR j=0, N_ELEMENTS( infiles )-1 DO BEGIN
        head = HEADFITS( infiles[j], /SILENT, ERRMSG=head_errmsg )
        IF head_errmsg EQ '' THEN BEGIN
           val = SXPAR(head,'OBS_ID',COUNT=count,/SILENT)
           IF count NE 0 THEN obsid[j] = ULONG(val[0])
        ENDIF
     ENDFOR
     wbad = WHERE( obsid LT 0, nbad )
     IF nbad NE 0 THEN BEGIN
        errmsg = "Couldn't get OBSID from "+STRJOIN(infiles[wbad],', ')
        RETURN,success
     ENDIF
  ENDIF ELSE BEGIN
     IF N_ELEMENTS(dir) EQ 0 THEN BEGIN
        ;;User didn't specify, use !SMAP_HIPE
        DEFSYSV,'!SMAP_HIPE',EXISTS=sh_exist
        IF sh_exist EQ 0 THEN BEGIN
           errmsg = "Don't know where to look for files; no dir or !SMAP_HIPE"+$
                    " set"
           IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
           RETURN,success
        ENDIF
        dir = addslash(!SMAP_HIPE)
     ENDIF ELSE dir = addslash(dir)
     ;;Find the files and obsid
     infiles = get_psp_filelist( dir, SUCCESS=psp_success, ERRMSG=errmsg,$
                                 OBSID=obsid, /REQUIREOBSID )
     IF psp_success EQ 0 THEN BEGIN
        errmsg = "Error getting infiles: "+errmsg
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN,success
     ENDIF
  ENDELSE

  filecount = N_ELEMENTS(infiles)
  IF filecount EQ 1 && infiles[0] EQ '' THEN BEGIN
     errmsg = "No input files found!"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,0b
  ENDIF

  ;;Do scan selection
  IF (N_ELEMENTS(scandown) NE 0) OR (N_ELEMENTS(scanup) NE 0) THEN BEGIN
     IF N_ELEMENTS(scandown) EQ 0 THEN BEGIN
        i_scandown = 0
        IF KEYWORD_SET(verbose) THEN $
           MESSAGE,'No scandown set -- assuming first file',/INF
     ENDIF ELSE i_scandown = scandown
     IF N_ELEMENTS(scanup) EQ 0 THEN BEGIN
        i_scanupn = filecount-1
        IF KEYWORD_SET(verbose) THEN $
           MESSAGE,'No scanup set -- assuming last file',/INF
     ENDIF ELSE i_scanup = scanup

     IF N_ELEMENTS(fileinfo) NE 0 AND N_ELEMENTS(fileinfo) EQ filecount THEN $
        fileinfo = fileinfo[scandown:scanup]
     infiles = infiles[scandown:scanup]
     obsid    = obsid[scandown:scanup]
     filecount = N_ELEMENTS(infiles)
     scanlisting = INDGEN(filecount)+scandown

  ENDIF ELSE BEGIN
     scanlisting = INDGEN(filecount)
     i_scandown = 0
     i_scanup = filecount-1
  ENDELSE

  ;;Make obsid string for fileinfo/mapparam.  If we have multiples, we need to
  ;; concatenate them
  uobsid = obsid[ UNIQ( obsid, SORT(obsid) ) ]
  nuobsid = N_ELEMENTS(uobsid)
  obsid_str = '0x'+TO_HEX(uobsid[0])
  FOR i=1,nuobsid-1 DO obsid_str += '_0x'+TO_HEX(uobsid[i])

  ;; these are dummy values that will get overwritten, though
  ;; this will be checked at the end of the loop
  absramin = 1e5
  absramax = -1e5
  absdecmin = 1e5
  absdecmax = -1e5

  ; this is going to hold the (time series) length of the scans
  scanlength = LONARR(filecount)

  ;;Get information about the file structure unless user passed
  ;; in one that looks valid already
  read_fileinfo = 0b
  IF N_ELEMENTS(fileinfo) EQ 0 THEN read_fileinfo = 1b ELSE BEGIN
     IF N_ELEMENTS(fileinfo) NE N_ELEMENTS(infiles) THEN $
        read_fileinfo = 1b ELSE BEGIN
        wdiff = WHERE( fileinfo.file NE infiles, ndiff )
        IF ndiff NE 0 THEN read_fileinfo = 1b
     ENDELSE
  ENDELSE
  IF read_fileinfo THEN BEGIN
     fileinfo = PARSE_TIMESTREAM_DETINFO(infiles,SUCCESS=ptd_success,$
                                         ERRMSG=errmsg,$
                                         NOTEMPERATURE=sps,SIGNAL=sps)
     IF ptd_success EQ 0 THEN BEGIN
        errmsg = "ERROR from parse_timestream_detinfo: " + errmsg
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN,0b
     ENDIF
  ENDIF

  ; main loop through list of files
  filelist = STRARR(filecount)
  FOR iscan=0,filecount-1 DO BEGIN
 
     ; tell me what's going on if I want to know
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,"Reading file " + infiles[iscan],/INF

     ; fetch the data
     data = SMAP_GETLEVEL1(infiles[iscan],FILEINFO=fileinfo[iscan],$
                           SPS=sps,SUCCESS=sgl_success,ERRMSG=sgl_errmsg)
     ; error check
     IF ~sgl_success THEN BEGIN
        errmsg = "Error from SMAP_GETLEVEL1: " + sgl_errmsg
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN,0b
     ENDIF

     ; put in the scan length
     scanlength[iscan] = N_ELEMENTS(data.samptime)

     ;; remove the 0 -> 360 degeneracy...
     ;; if the derivative of ra ever goes completely crazy, we
     ;; probably have and event - the limit is 1 degree/sample
     raderiv = ABS(DERIV(data.ra))   
     IF MAX(raderiv) GT 1. THEN BEGIN
        whwind = WHERE(raderiv EQ MAX(raderiv))
        thismedlo = MEDIAN(data.ra[0L:whwind-1L]) 
        thismedhi = MEDIAN(data.ra[whwind:N_ELEMENTS(data.ra) -1L])
        IF thismedlo GT thismedhi THEN BEGIN
           data.ra[whwind:N_ELEMENTS(data.ra)-1L] = $
              data.ra[whwind:N_ELEMENTS(data.ra)-1L] + 360.
        ENDIF ELSE BEGIN
           data.ra[0L:whwind] = data.ra[0L:whwind] + 360.
        ENDELSE
     ENDIF
     
     IF MAX(data.ra) LT 0 THEN data.ra = data.ra + 360.

     ; find the min and max of the coordinates for this scan.
     ramin = min(data.ra,/NAN)
     ramax = max(data.ra,/NAN)
     decmin = min(data.dec,/NAN)
     decmax = max(data.dec,/NAN)

     ; if this coordinate is further from the center of the map
     ; than the previous one, overwrite it
     IF ramin LT absramin AND FINITE(ramin) THEN absramin = ramin
     IF ramax GT absramax AND FINITE(ramax) THEN absramax = ramax
     IF decmin LT absdecmin AND FINITE(decmin) THEN absdecmin = decmin
     IF decmax GT absdecmax AND FINITE(decmax) THEN absdecmax = decmax

     ;;Write the tod
     outdir = addslash(!SMAP_DATA)+obsid_str
     filename = addslash(outdir) + obsid_str + '_'+ $
                STRCOMPRESS(STRING(iscan,FORMAT='(I06)'),/REMOVE_ALL) 
     IF STRLEN(exname) NE 0 THEN filename += '_'+exname
     IF ~ FILE_TEST( outdir ) THEN FILE_MKDIR,outdir
     filename += '_idltod.fits'
     filelist[iscan] = filename
     smap_writetod, data, filename, /NO_ABORT, ERRMSG=wa_errmsg,$
                    SUCCESS=wa_success
     IF ~ wa_success THEN BEGIN
        errmsg = 'WRITE_ANCILLARY kicked up error: ' + wa_errmsg
        IF KEYWORD_SET(verbose) THEN $
           MESSAGE,errmsg,/INF
        RETURN,success
     ENDIF
  ENDFOR

  errmsg1 = 'Found map specification coordinate '  
  errmsg2 = ' that does not make sense!'
  ; error check that we found a max/min coordinate value that's sane.
  IF ramin LT 0. THEN BEGIN
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,errmsg1 + 'in ramin' + errmsg2,/INF
     RETURN,success
  ENDIF
  IF ramax GT 370. THEN BEGIN
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,errmsg1 + 'in ramax' + errmsg2,/INF
     RETURN,success
  ENDIF
  IF decmin LT -90 THEN BEGIN
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,errmsg1 + 'in decmin' + errmsg2,/INF
     RETURN,success
  ENDIF
  IF decmax GT 90 THEN BEGIN
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,errmsg1 + 'in decmax' + errmsg2,/INF
     RETURN,success
  ENDIF

  ; a further check that this isn't clearly crazy
  IF ramin GT ramax THEN BEGIN
     errmsg = 'ramin > ramax!'
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,errmsg,/INF
     RETURN,success
  ENDIF
  IF decmin GT decmax THEN BEGIN
     errmsg = 'decmin > decmax!'
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,errmsg,/INF
     RETURN,success
  ENDIF

  ; ok, now make the mapparam array
  mapparam = {obsids:'',object:'',bands:STRARR(3),$
              nscans:0,scandown:0,scanup:0,$
              filenames: STRARR(filecount),$
              todnames: STRARR(filecount),$
              scanlength:LONARR(filecount),midra:0.0,middec:0.0,$
              minra:0.0,maxra:0.0,mindec:0.0,maxdec:0.0,$
              pixsize:REPLICATE(0.0,3),$
              xrange:REPLICATE(0,3),yrange:REPLICATE(0,3)}

  ;; append some simple data
  mapparam.obsids = obsid_str
  mapparam.nscans = filecount
  mapparam.scandown = i_scandown
  mapparam.scanup = i_scanup
  mapparam.object = data.object
  mapparam.bands = defparams.bands
  mapparam.scanlength = scanlength
  mapparam.filenames = fileinfo.file
  mapparam.todnames = filelist

  ;; append the min/max/range data
  mapparam.midra = (absramax + absramin) / 2
  mapparam.middec = (absdecmax + absdecmin) / 2
  mapparam.minra = absramin
  mapparam.mindec = absdecmin
  mapparam.maxra = absramax
  mapparam.maxdec = absdecmax

  ;; loop through colors and figure out how many pixels each needs, then
  ;; initialize the mapstructure for later
  doband = [0b,0b,0b]
  IF ARG_PRESENT(map250) THEN doband[0]=1b
  IF ARG_PRESENT(map350) THEN doband[1]=1b
  IF ARG_PRESENT(map500) THEN doband[2]=1b
  FOR icol=0,2 DO BEGIN
     IF ~ doband[icol] THEN CONTINUE
     mapparam.pixsize[icol] = pixsize[icol]
     mapparam.xrange[icol] = CEIL((absramax - absramin) * $
                                  COS(!PI / 180. * mapparam.middec) / $
                                  mapparam.pixsize[icol])
     mapparam.yrange[icol] = CEIL((absdecmax - absdecmin) / $
                                  mapparam.pixsize[icol]) 

     ;; make the astrometry 
     cdmat = [[-mapparam.pixsize[icol],0.0],[0.0,mapparam.pixsize[icol]]]
     crval = [mapparam.midra + pixsize[icol] / 2.,mapparam.middec]
     crpix = [mapparam.xrange[icol] / 2,mapparam.yrange[icol] / 2]
     
     MAKE_ASTR,thisast,CD=cdmat,CRPIX=crpix,CRVAL=crval
     ;; can't really error check this guy, but we can at least ask 
     ;; whether it returned anything
     IF N_ELEMENTS(thisast) EQ 0 THEN BEGIN
        errmsg = 'MAKE ASTR did not return anything!'
        IF KETWORD_SET(verbose) THEN $
           MESSAGE,errmsg,/INF
        RETURN,success
     ENDIF

     ;; ok, now get the default map struct.  We know the things we 
     ;; need to pass it from above, so that's what we'll do 
     map = GET_SMAP_MAPSTRUCT(NPIXX=mapparam.xrange[icol],$
                              NPIXY=mapparam.yrange[icol],$
                              BAND=defparams.bands[icol],ASTROMETRY=thisast,$
                              SUCCESS=gsm_success,ERRMSG=gsm_errmsg,$
                              SILENT=silent,/EXP_DBL)
     ; error check
     IF ~gsm_success THEN BEGIN
        errmsg = 'GET_SMAP_MAPSTRUCT encountered an error: ' + gsm_errmsg
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN,success
     ENDIF

     CASE icol OF
        0: map250=TEMPORARY(map)
        1: map350=TEMPORARY(map)
        2: map500=TEMPORARY(map)
     ENDCASE
  ENDFOR

  ;;Write the map params
  somestr = WRITE_ANCILLARY(mapparam,obsid_str,'mapparam',$
                            EXNAME=exname,SILENT=silent,$
                            SUCCESS=wa_success,ERRMSG=wa_errmsg)
  IF ~ wa_success THEN BEGIN
     errmsg = 'WRITE_ANCILLARY kicked up error: ' + wa_errmsg
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,errmsg,/INF
     RETURN,success
  ENDIF

  ;;And file info
  somestr = WRITE_ANCILLARY(fileinfo,obsid_str,'fileinfo',$
                            EXNAME=exname,SILENT=silent,$
                            SUCCESS=wa_success,ERRMSG=wa_errmsg)
  IF ~ wa_success THEN BEGIN
     errmsg = 'WRITE_ANCILLARY kicked up error: ' + wa_errmsg
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,errmsg,/INF
     RETURN,success
  ENDIF

  ;; ok, that's all we need, return that we're ok
  success = 1b
  RETURN,mapparam

END
