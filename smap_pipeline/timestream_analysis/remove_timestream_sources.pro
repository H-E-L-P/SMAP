;+
;NAME
; remove_timestream_sources
;PURPOSE
; Removes sources from a set of L1 timestreams given an input catalog
;  or maps, then writes the timestreams back out.
;CATEGORY
;  Herschel SPIRE timestream
;USAGE
; remove_timestream_sources,files,outfiles, [CATALOGFILES= ,MAPFILES=]
;INPUTS
;  files         List of files to process or the name of the base
;                 directory of an HCSS export dir
;  outfiles      Files to write them back out as after doing time-stream
;                 removal or the name of a directory to write them to.
;                 Both files and outfiles must be of the same type
;                 (directory or file list).  If a directory, the files 
;                 are given the same names.  The code attempts to
;                 decide if this is also a base HCSS export dir, and
;                 if it thinks it is, sticks the output in the 
;                 level1/herschel.spire.ia.PointedPhotProduct subdir
; and at least one of the following
;  catalogfiles  Name of catalogs to remove.  These should be the
;                 names of fits files.  These are relative to CATALOGDIR,
;                 which defaults to !SMAP_CATS
;  mapfiles      Names of maps to subract.  These should be the fileroot
;                 names a la read_smap_fitsmap.  Also see MAPDIR
;KEYWORDS
; General
;  verbose       Runs in verbose mode
;  hcssinternal  Treat as internal HCSS format instead of export
;                 product.  Not guaranteed to stay stable.
;  sps           Reading in SPS data, which is missing quality and temperature
; Related to catalog subtraction
;  cubic         Use a cubic approximation to SINC interpolation instead
;                 of bilinear interpolation -- see the interpolate
;                 documentation for details.  This slows the code
;                 considerably but is on by default (so set CUBIC=0
;                 to turn it off).
;  onlypositive  Only subtract positive flux detections
;  masksources   Rather than subtracting sources, try to mask a
;                 circular area around each source
;OPTIONAL INPUTS
; Related to catalog subtraction
;  catalogdir    Directory to look for catalogs in, if not !SMAP_CATS
;  pixscale      The pixel scale used to build maps from catalogs, in arcsec
;                 (def: [3,5,6] arcsec/pix)
;  maskthresh    Threshold to add mask bit saying that a source has
;                 been subtracted from the specified position.  This
;                 is in mJy, and is off by default.
;  prfoversample Oversampling to use when constructing PRF.  Set to 1
;                 to use a PSF and not a PRF (def: 9).
;  catalogmaproot This will cause the constructed maps
;                 to be written as SMAP maps using this file root.
;  catsnmin       Minimum signal-to-noise before including object.  The
;                  cut is done on ABS( flux / flux_error ), so strong negative
;                  flux detections are included (nless you set
;                  /ONLYPOISITIVE)
;  catfluxmin     Minimum flux (in mJy) before removing
;  masknsigma     Number of sigma to make the radius of the mask if
;                     /MASKSOURCES is set (Def: 2)
;  addsources	  Adds sources instead of subtracting them
; Related to map subtraction
;  mapdir        Directory to look for maps in, if not !SMAP_MAPS
;  mapmaskbits   Bit mask for input maps.  Pixels which have a
;                 non-zero or with this are not used in the map
;                 subtraction. The default is 'FFFFFFFF'xUL -- i.e., all ones.
;OPTIONAL OUTPUTS
; success        1 if it worked, 0 if it didn't
; errmsg         An error message if a problem was encountered
;NOTES
; This program makes use of IDL pointers.  Therefore, if the code
;  crashes it is highly advisable that you execute a HEAP_GC.  This
;  should not be necessary if it simply fails, only if it crashes.
;MODIFICATION HISTORY
;  Author: Alex Conley, May-September 2009
;  Author: Marco Viero, June 2011 "add sources with ADDSOURCES flag"
;-

;;;;;;;;;;;;;;;;;;;;;;;;
;; TO-DO
;; Handle map mask bits, currently ignored on output
;; Modify primary header of output file to say we've done things.
;; Can get that from the old remove_timestream_sources

PRO remove_timestream_sources, firstarg, secondarg,$
                               CATALOGFILES=catalogfiles,$
                               CATALOGDIR=catalogdir,MASKTHRESH=maskthresh,$
                               MAPFILES=mapfiles, MAPDIR=mapdir,$
                               MAPMASKBITS=mapmaskbits, VERBOSE=verbose,$
                               CUBIC=cubic,PIXSCALE=pixscale,$
                               NOCHECK=nocheck, JITTERTOL=jittertol, $
                               ONLYPOSITIVE=onlypositive,$
                               CATALOGMAPROOT=catalogmaproot,SPS=sps,$
                               CATSNMIN=catsnmin, CATFLUXMIN=catfluxmin,$
                               HCSSINTERNAL=hcssinternal, SUCCESS=success, $
                               ERRMSG=errmsg, MASKSOURCES=masksources, $
                               MASKNSIGMA=masknsigma, ADDSOURCES=addsources

  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg = ''

  IF N_ELEMENTS(pixscale) EQ 0 THEN pixscale = [3.0,5.0,6.0]
  IF N_ELEMENTS(prfoversample) EQ 0 THEN prfoversample=5
  IF N_ELEMENTS(maskthresh) EQ 0 THEN maskthresh=0.1
  IF N_ELEMENTS(catalogdir) EQ 0 THEN catalogdir=addslash(!SMAP_CATS)
  IF N_ELEMENTS(mapdir) EQ 0 THEN mapdir=addslash(!SMAP_MAPS)
  IF N_ELEMENTS(mapmaskbits) EQ 0 THEN int_mapmaskbits = 'FFFFFFFF'xUL ELSE $
     int_mapmaskbits = mapmaskbits
  IF N_ELEMENTS(cubic) EQ 0 THEN i_cubic=1b ELSE i_cubic=KEYWORD_SET(cubic)
  IF KEYWORD_SET( masksources ) THEN i_cubic = 0b ;;pointless if masking only

  ;;Lots of input validity checks
  IF N_ELEMENTS(firstarg) EQ 0 THEN BEGIN
     errmsg = "No input files/directory provided"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN
  ENDIF
  IF SIZE(firstarg,/TNAME) NE 'STRING' THEN BEGIN
     errmsg = "Input file list/directory is not of string type"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN
  ENDIF

  IF N_ELEMENTS( firstarg ) EQ 1 THEN BEGIN
     ;;See if it's a directory
     IF ~ FILE_TEST(firstarg) THEN BEGIN
        errmsg = "Can't find: " + firstarg[0] + " for reading"
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN
     ENDIF
     IF ~ FILE_TEST( firstarg, /READ ) THEN BEGIN
        errmsg = "Can't read: " + firstarg[0]
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN
     ENDIF
     IF FILE_TEST( firstarg, /DIRECTORY ) THEN BEGIN
        ;;Assume it's the base dir of an HCSS product
        is_dir = 1b
        IF KEYWORD_SET(verbose) THEN $
           MESSAGE,"Assuming "+firstarg+" is HCSS product directory",/INF
     ENDIF ELSE is_dir = 0b
  ENDIF ELSE is_dir = 0b

  IF is_dir THEN BEGIN
     IF KEYWORD_SET( hcssinternal ) THEN BEGIN
        photdir = addslash(firstarg)+$
                  'herschel.spire.ia.dataset.PointedPhotTimeline/'
     ENDIF ELSE BEGIN
        photdir = addslash(firstarg)+'level1/'+$
                  'herschel.spire.ia.dataset.PointedPhotTimeline/'
     ENDELSE
     IF ~ FILE_TEST( photdir, /DIRECTORY, /READ ) THEN BEGIN
        errmsg = "Can't read phot dir in HCSS prod dir: "+photdir
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN
     ENDIF
     is_dir = 1b
     files = FILE_SEARCH(photdir+'*.fits',/FOLD_CASE,/FULLY_QUALIFY_PATH)
     nfiles = N_ELEMENTS(files)
     IF nfiles EQ 0 THEN BEGIN
        errmsg = "Couldn't find any input files in "+photdir
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN
     ENDIF
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,"Will read files from: "+photdir,/INF
  ENDIF ELSE BEGIN
     files = firstarg
     nfiles = N_ELEMENTS(files)
     IF nfiles EQ 0 THEN BEGIN
        errmsg = "No input files to work with"
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN
     ENDIF
  ENDELSE
  
  FOR i=0,nfiles-1 DO BEGIN
     IF ~ FILE_TEST( files[i] ) THEN BEGIN
        errmsg = "Couldn't find input file: "+files[i]
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN
     ENDIF 
     IF ~ FILE_TEST( files[i], /READ ) THEN BEGIN
        errmsg = "Found but couldn't read input file: "+files[i]
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN
     ENDIF 
  ENDFOR

  IF SIZE(secondarg,/TNAME) NE 'STRING' THEN BEGIN
     errmsg = "Output file list/dir is not of string type"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN
  ENDIF
  IF is_dir THEN BEGIN
     outdir = secondarg
     IF N_ELEMENTS(outdir) GT 1 THEN BEGIN
        errmsg = "First argument was directory, so don't know how to"+$
                 " handle multiple second arguments"
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN
     ENDIF
     IF ~ FILE_TEST( outdir ) THEN BEGIN
        errmsg = "Output directory "+outdir+" not found"
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN
     ENDIF
     IF ~ FILE_TEST( outdir, /DIRECTORY ) THEN BEGIN
        errmsg = "Second argument needs to be directory if first one is"
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN
     ENDIF

     ;;Try to figure out if this is an HCSS dir
     IF KEYWORD_SET( hcssinternal ) THEN BEGIN
        IF FILE_TEST( addslash(outdir)+$
                      'herschel.spire.ia.dataset.PointedPhotTimeline/',$
                      /DIRECTORY ) THEN BEGIN
           ;;Yup, it is
           outdir= addslash(outdir)+$
                   'herschel.spire.ia.dataset.PointedPhotTimeline/'
        ENDIF
     ENDIF ELSE BEGIN
        IF FILE_TEST( addslash(outdir)+'level1/'+$
                      'herschel.spire.ia.dataset.PointedPhotTimeline/',$
                      /DIRECTORY ) THEN BEGIN
           outdir= addslash(outdir)+'level1/'+$
                   'herschel.spire.ia.dataset.PointedPhotTimeline/'
        ENDIF
     ENDELSE
     IF ~ FILE_TEST( outdir, /WRITE, /DIRECTORY ) THEN BEGIN
        errmsg = "Unable to write to output dir: "+outdir
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN
     ENDIF
     ;;Construct output file names -- same as inputs but (potentially)
     ;; in a different directory.  The reform is a little trickery
     ;; having to do with how STRMID works
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,"Will write output files to directory: "+outdir,/INF
     outfiles = addslash(outdir)+$
                STRMID(files, REFORM(RSTRPOS(files,'/')+1,1,nfiles))
  ENDIF ELSE BEGIN
     outfiles = secondarg
     IF N_ELEMENTS(outfiles) NE nfiles THEN BEGIN
        errmsg = "Different number of files in outfiles list"
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN
     ENDIF
  ENDELSE
  ;;Check to see if we are overwriting, unless the input/output are
  ;;the same
  IF KEYWORD_SET(verbose) THEN BEGIN
     warn_overwrite = 0b
     IF ~ is_dir THEN warn_overwrite = 1b ELSE $
        IF firstarg NE secondarg THEN warn_overwrite=1b
     IF warn_overwrite THEN BEGIN 
        overwrites=['']
        FOR i=0,nfiles-1 DO BEGIN
           IF FILE_TEST(outfiles[i]) THEN BEGIN
              shortname = STRMID(outfiles[i],RSTRPOS(outfiles[i],PATH_SEP())+1)
              overwrites = [overwrites,shortname]
           ENDIF
        ENDFOR
        IF N_ELEMENTS(overwrites) GT 1 THEN $
           MESSAGE,STRING(STRJOIN(overwrites[1:*],','),$
                          FORMAT='("Will overwrite: ",A0)'),/INF
     ENDIF
  ENDIF

  ;;Check catalogs and maps
  ncatalogs = N_ELEMENTS(catalogfiles)
  nmaps = N_ELEMENTS(mapfiles)
  IF ncatalogs+nmaps EQ 0 THEN BEGIN
     errmsg = "Must provide at least one catalog or map file"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN
  ENDIF
  IF ncatalogs EQ 0 THEN BEGIN
     errmsg = "Map subtraction not yet implemented"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN
  ENDIF
  IF ncatalogs NE 0 THEN FOR i=0,ncatalogs-1 DO BEGIN
     IF ~ FILE_TEST( catalogdir+catalogfiles[i] ) THEN BEGIN
        errmsg = "Couldn't find input catalog file: "+catalogfiles[i]
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN
     ENDIF 
     IF ~ FILE_TEST( catalogdir+catalogfiles[i], /READ ) THEN BEGIN
        errmsg = "Found but couldn't read input catalog file: "+catalogfiles[i]
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN
     ENDIF 
  ENDFOR
  IF nmaps NE 0 THEN FOR i=0,nmaps-1 DO BEGIN
     IF ~ FILE_TEST( mapfiles[i] ) THEN BEGIN
        errmsg = "Couldn't find input map file: "+mapfiles[i]
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN
     ENDIF 
     IF ~ FILE_TEST( mapfiles[i], /READ ) THEN BEGIN
        errmsg = "Found but couldn't read input map file: "+mapfiles[i]
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN
     ENDIF 
  ENDFOR

  ;;Get summary info
  IF KEYWORD_SET( verbose ) THEN $
     MESSAGE,"Parsing detector order information",/INF
  file_info = parse_timestream_detinfo( files, SIGNALONLY=sps,$
                                        NOTEMPERATURE=sps,$
                                        HCSSINTERNAL=hcssinternal,$
                                        SUCCESS=success_parse,$
                                        ERRMSG=errmsg )
  IF success_parse EQ 0 THEN BEGIN
     errmsg = "Error parsing file info: "+errmsg
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN
  ENDIF

  ;;Structure to hold all the maps.  Pointers!!!
  bands = ['PSW','PMW','PLW']
  nbands = N_ELEMENTS(bands)
  map_ptrarr = PTRARR( nbands*(nmaps + ncatalogs) )

  ;;Prepare catalogs

  IF ncatalogs GT 0 THEN BEGIN
     cat_astrom = create_astrometry_from_level1(file_info, PIXSCALE=pixscale,$
                                                SPS=sps,SUCCESS=asuccess,$
                                                ERRMSG=errmsg)
     IF asuccess EQ 0 THEN BEGIN
        errmsg = "Error getting astrometry: "+errmsg
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        PTR_FREE, map_ptrarr
        RETURN
     ENDIF 

     ;;Build catalogs
     FOR i=0, ncatalogs-1 DO BEGIN
        FOR j=0, N_ELEMENTS(bands)-1 DO BEGIN
           map = create_map_from_catalog( catalogfiles[i], cat_astrom[j],$
                                          bands[j], CATALOGDIR=catalogdir,$
                                          PRFOVERSAMPLE=prfoversample, $
                                          ONLYPOSITIVE=onlypositive,$
                                          CATSNMIN=catsnmin,$
                                          CATFLUXMIN=catfluxmin,$
                                          SUCCESS=cat_success,$
                                          MASKSOURCES=masksources,$
                                          MASKNSIGMA=masknsigma )
           IF cat_success EQ 0 THEN BEGIN
              errmsg = "Error building catalog in "+bands[j]+" for "+$
                       catalogfiles[i]
              IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
              PTR_FREE, map_ptrarr
              RETURN
           ENDIF 
	   ;;If adding sources (rather than subtracting),multiply by -1 
	   IF KEYWORD_SET(addsources) THEN $
	      map*=-1.0
           map_ptrarr[i*nbands+j] = PTR_NEW( TEMPORARY(map) )
        ENDFOR
     ENDFOR
  ENDIF

  ;;And read maps
  IF nmaps EQ 0 THEN BEGIN
     FOR i=0,nmaps-1 DO BEGIN
        FOR j=0,nbands-1 DO BEGIN
           map =  read_smap_fitsmap(mapfiles[i], bands[j], /SILENT,$
                                    DIR=mapdir, ERRMSG=errmsg,$
                                    SUCCESS=success,$
                                    /NOREADEXP, /NOREADERR, /STRICT,$
                                    /NO_ABORT )
           IF cat_success EQ 0 THEN BEGIN
              errmsg = "Error reading map in "+bands[j]+" for "+$
                       mapfiles[i]
              IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
              PTR_FREE, map_ptrarr
              RETURN
           ENDIF 
           map_ptrarr[i*nbands+j + ncatalogs] = PTR_NEW(TEMPORARY(map))
        ENDFOR
     ENDFOR
  ENDIF

  ;;Now process each file
  FOR i=0, N_ELEMENTS(files)-1 DO BEGIN
     level1 = smap_getlevel1( files[i], NOQUALITY=sps, NOTEMPERATURE=sps,$
                              SUCCESS=success, ERRMSG=errmsg )
     IF success EQ 0 THEN BEGIN
        errmsg = "While reading level 1 phot from: "+files[i]+": "+$
                 errmsg
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        PTR_FREE, map_ptrarr
        RETURN
     ENDIF 
     get_timestream_from_map, level1, *map_ptrarr[0], subtrahend, /ZERO,$
                              CUBIC=i_cubic, MAPMASKBITS=int_mapmaskbits,$
                              SUCCESS=success, ERRMSG=errmsg
     IF success EQ 0 THEN BEGIN
        errmsg = "While building first fake timestream: "+errmsg
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        PTR_FREE, map_ptrarr
        RETURN
     ENDIF 
     FOR j=1, N_ELEMENTS(map_ptrarr)-1 DO BEGIN
        get_timestream_from_map, level1, *map_ptrarr[j], subtrahend,$
                                 CUBIC=i_cubic, MAPMASKBITS=int_mapmaskbits,$
                                 SUCCESS=success, ERRMSG=errmsg
        IF success EQ 0 THEN BEGIN
           errmsg = "While building fake timestream: "+errmsg
           IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
           PTR_FREE, map_ptrarr
           RETURN
        ENDIF 
     ENDFOR

     ;;And subtract
     subtract_timestream, level1, subtrahend, SUCCESS=success, $
                          ERRMSG=errmsg, MASKSOURCES=masksources
     IF success EQ 0 THEN BEGIN
        errmsg = "While subtracting timestream from: "+$
                 level1.shortfile+": "+errmsg
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        PTR_FREE, map_ptrarr
        RETURN
     ENDIF 
     DELVARX, subtrahend

     ;;Now write
     write_level1_from_smap, level1, outfiles[i], { signal: 1b },$
                             SUCCESS=success, ERRMSG=errmsg
     IF success EQ 0 THEN BEGIN
        errmsg = "While writing: "+outfiles[i]+" from "+level1.shortfile
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        PTR_FREE,map_ptrarr
        RETURN
     ENDIF 
  ENDFOR
  PTR_FREE,map_ptrarr  

  ;; remove floating point errors
  error = check_math(MASK=32) 

  success=1b
END
