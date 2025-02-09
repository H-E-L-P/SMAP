;+
;NAME
; smap_make_maps
;PURPOSE
; Builds actual maps from timestreams
;USAGE
;  smap_make_maps, tods, mapparam, map250, map350, map500, [, /ITERMAP,$
;                  ITER_PARAMS=ITER_PARAMS, PIXSCALE=pixscale,$
;                  SUCCESS=success, ERRMSG=errmsg, ...]
;INPUTS
;  tods              Either a list of filenames of TODs or a pointer
;                     array actually containing the tods.  See
;                     smap_read_and_filter.
;  mapparam          Basic info about maps, also from smap_read_and_filter
;RETURNS
;   map250/350/500   The maps made using the specified algorithm.
;                     If no options are specified, the n00b/naive
;                     map maker is used.
;KEYWORDS
;   itermap          Use the iterative, not-crush map maker.
;   iter_diagnostics Save itermap diagnostics if performing /ITERMAP
;                     Setting /ITER_DIAGNOSTICS will save a data cube
;                     of maps, setting ITER_DIAGNOSTICS=2 will save
;                     the fit parameters.  Set to 3 to get both.
;   iter_properror   Create propagated error maps if performing /ITERMAP
;   verbose          Print status messages
;   no250/no350/no500 Don't make a map in this band; note you
;                      still need to pass in the dummy argument (i.e.,
;                      map250)
;   fixeddeglitch    Use previous map-based deglitching info to flag 
;                     samples, passed in in DEGLITCH250/350/500.  Only
;                     pertinent to iterative map maker
;   filter           pass /filter keyword to get_smap_mapstruct
;   nolatlon         Only use standard WCS LATPOLE/LONPOLE.  Normally,
;                     for far northern or southern fields, a
;                     non-standard value is used to reduce astrometric
;                     distortion
; 
;OPTIONAL INPUTS
;   tod_index        Array of indicies into tods determining which
;                     ones to actually use.
;   excludemask      String array of Herschel masks to consider
;                     unusable.  The default is ['maskMaster','maskDead']
;   excludeweightmask String array of additional Herschel masks to
;                     exclude when building map parameters (but are
;                     included when constructing the actual map).  Currently
;                     only affects iterative maps; see smap_itermap
;                     for details.
;   pixscale         The pixel scale for the output maps in degrees.
;   iter_params      Control parameters for iterative map making; see 
;                     smap_itermap for details.
;   iter_fixed       For iterative map making, base name of file to
;                     load and reuse previous scan by scan parameters
;   ifixed250/350/500 Actual structure of sparams250/350/500.
;                     If provided, iter_fixed is ignored
;   crvalx           optionally specify CRVAL1 (single value or 3 el. array)
;   crvaly           optionally specify CRVAL2 (single value or 3 el. array)
;   crpixx           optionally specify CRPIX1 (single value or 3 el. array)
;   crpixy           optionally specify CRPIX2 (single value or 3 el. array)
;   nxpix            optionally specify NAXIS1 (single value or 3 el. array)
;   nypix            optionally specify NAXIS2 (single value or 3 el. array)
;   projtype         set projection type (default: TAN)
;   badbolos         string array of bolometers to ignore
;   usebolos         string array of bolometers to use (single-bolometer maps)
;                      ONLY WORKS FOR NAIVE/N00B MAPPER && FOR A SINGLE
;                      BOLOMETER IN EACH ARRAY
;
;OPTIONAL OUTPUTS
;   success          0b on failure, 1b on success
;   errmsg           If failure, some description of the failure,
;                     otherwise ''
;   sparams250/350/500 Parameters of final map making iteration (if
;                     using iterative mapping).  Can be repassed in
;                     using ifixed250/350/500
;   deglitch250/350/500 2nd level deglitching info, currently itermap
;                     only.  Used as input if /FIXEDDEGLITCH is set
;MODIFICATION HISTORY
; Author: Alex Conley, Nov 2009
;
;CHANGE LOG
; 2010-01-20 (gm): add crvalx,crvaly,crpixx,crpixy,nxpix,nypix for
;                  specifying astrometry && badbolos keyword
; 2010-01-31 (tpeb): add usebolos keyword
; 2010-04-13 (gm): update map boundary calculations
; 2011-09-06 (gm): add filter keyword (passed through to get_smap_mapstruct)
; 2012-07-20 (gm): add PROJTYPE keyword for alternate projections
;                  also force crpix to be integer
;-

PRO smap_make_maps_setupmaps, mapparam, map250, map350, map500,$
                              CRVALX=crvalx, CRVALY=crvaly, $
                              CRPIXX=crpixx, CRPIXY=crpixy, $
                              NXPIX=nxpix, NYPIX=nypix,$
                              PIXSCALE=pixscale, SUCCESS=success,$
                              ERRMSG=errmsg, NO250=no250, NO350=no350, $
                              NO500=no500, FILTER=filter, NOLATLON=nolatlon, $
                              PROJTYPE=projtype
  COMPILE_OPT IDL2, HIDDEN
  success = 0b
  errmsg  = ''

  ;;Make sure we have valid limits
  IF ~ FINITE(mapparam.minra) THEN BEGIN
     errmsg = "Non-finite minimum RA" & RETURN
  ENDIF
  IF ~ FINITE(mapparam.maxra) THEN BEGIN
     errmsg = "Non-finite maximum RA" & RETURN
  ENDIF
  IF ~ FINITE(mapparam.mindec) THEN BEGIN
     errmsg = "Non-finite minimum DEC" & RETURN
  ENDIF
  IF ~ FINITE(mapparam.maxdec) THEN BEGIN
     errmsg = "Non-finite maximum DEC" & RETURN
  ENDIF
  IF ~ FINITE(mapparam.middec) THEN BEGIN
     errmsg = "Non-finite mid DEC" & RETURN
  ENDIF
  IF ~ FINITE(mapparam.midra) THEN BEGIN
     errmsg = "Non-finite mid RA" & RETURN
  ENDIF
;  IF mapparam.minra LT 0 THEN BEGIN
;     errmsg = "minra is less than zero: "+STRING(mapparam.minra)
;     RETURN
;  ENDIF
;  IF mapparam.maxra GT 370.0 THEN BEGIN
;     errmsg = "maxra is too large: "+STRING(mapparam.maxra)
;     RETURN
;  ENDIF
  IF mapparam.mindec LT -90 THEN BEGIN
     errmsg = "mindec is too small: "+STRING(mapparam.mindec)
     RETURN
  ENDIF
  IF mapparam.maxdec GT 90.0 THEN BEGIN
     errmsg = "maxdec is too large: "+STRING(mapparam.maxdec)
     RETURN
  ENDIF
  IF mapparam.minra GE mapparam.maxra THEN BEGIN
     errmsg = "minra >= maxra" & RETURN
  ENDIF
  IF mapparam.mindec GE mapparam.maxdec THEN BEGIN
     errmsg = "mindec >= maxdec" & RETURN
  ENDIF

  ;;Set pixel size, set base params
  defparams = SMAP_GETDEFPARAMS(PIXSIZE=pixscale)

  ; validate crvalx, if passed
  IF KEYWORD_SET(crvalx) THEN BEGIN
      IF N_ELEMENTS(crvalx) EQ 1 THEN crvalx = REPLICATE(crvalx, 3)
      IF N_ELEMENTS(crvalx) NE 3 THEN BEGIN
          errmsg="Input 'crvalx' wrong dimensions! Must be array size 1 or 3."
          RETURN
      ENDIF
  ENDIF

  ; validate crvaly, if passed
  IF KEYWORD_SET(crvaly) THEN BEGIN
      IF N_ELEMENTS(crvaly) EQ 1 THEN crvaly = REPLICATE(crvaly, 3)
      IF N_ELEMENTS(crvaly) NE 3 THEN BEGIN
          errmsg="Input 'crvaly' wrong dimensions! Must be array size 1 or 3."
          RETURN
      ENDIF
  ENDIF

  ; validate crpixx, if passed
  IF KEYWORD_SET(crpixx) THEN BEGIN
      IF N_ELEMENTS(crpixx) EQ 1 THEN crpixx = REPLICATE(crpixx, 3)
      IF N_ELEMENTS(crpixx) NE 3 THEN BEGIN
          errmsg="Input 'crpixx' wrong dimensions! Must be array size 1 or 3."
          RETURN
      ENDIF
  ENDIF

  ; validate crpixy, if passed
  IF KEYWORD_SET(crpixy) THEN BEGIN
      IF N_ELEMENTS(crpixy) EQ 1 THEN crpixy = REPLICATE(crpixy, 3)
      IF N_ELEMENTS(crpixy) NE 3 THEN BEGIN
          errmsg="Input 'crpixy' wrong dimensions! Must be array size 1 or 3."
          RETURN
      ENDIF
  ENDIF

  ; validate nxpix, if passed
  IF KEYWORD_SET(nxpix) THEN BEGIN
      IF N_ELEMENTS(nxpix) EQ 1 THEN nxpix = REPLICATE(nxpix, 3)
      IF N_ELEMENTS(nxpix) NE 3 THEN BEGIN
          errmsg="Input 'nxpix' wrong dimensions! Must be array size 1 or 3."
          RETURN
      ENDIF
  ENDIF

  ; validate nypix, if passed
  IF KEYWORD_SET(nypix) THEN BEGIN
      IF N_ELEMENTS(nypix) EQ 1 THEN nypix = REPLICATE(nypix, 3)
      IF N_ELEMENTS(nypix) NE 3 THEN BEGIN
          errmsg="Input 'nypix' wrong dimensions! Must be array size 1 or 3."
          RETURN
      ENDIF
  ENDIF

  ; alternate projection
  IF NOT KEYWORD_SET(projtype) THEN projtype = "TAN" ELSE BEGIN
     IF SIZE(projtype, /TNAME) NE "STRING" || STRLEN(projtype) NE 3 THEN BEGIN
        errmsg="Input 'projtype' must be 2-letter string"
        RETURN
     ENDIF
     projtype = STRUPCASE(projtype)
  ENDELSE
  
  ctype = ["RA---","DEC--"] + projtype

  ; NOTE: for CEA, PV2_1 is required. I don't really know what it does, and
  ;       whether you'd want a value other than 1.0. This may need to be
  ;       updated
  IF projtype EQ "CEA" THEN this_pv2_1=1.0

  ;;Making maps
  FOR i=0, N_ELEMENTS(defparams.bands)-1 DO BEGIN
     ;;continue not allowed in case...
     IF defparams.bands[i] EQ 'PSW' && KEYWORD_SET( no250 ) THEN CONTINUE
     IF defparams.bands[i] EQ 'PMW' && KEYWORD_SET( no350 ) THEN CONTINUE
     IF defparams.bands[i] EQ 'PLW' && KEYWORD_SET( no500 ) THEN CONTINUE

     c_pixsize = defparams.pixsize[i]
     IF ~ FINITE(c_pixsize) THEN BEGIN
        errmsg = "Non-finite pixel size"
        RETURN
     ENDIF
     

;;; CHANGE Jul20 '10 (GM): get rid of half pixel offset... hope it
;;; doesn't screw things up 
     ;crval = [mapparam.midra + 0.5*c_pixsize, mapparam.middec]
     crval = [mapparam.midra, mapparam.middec]
     IF KEYWORD_SET(crvalx) THEN crval[0] = crvalx[i]
     IF KEYWORD_SET(crvaly) THEN crval[1] = crvaly[i]

     ;;Decide on lat/long to use; we don't want the poles to be
     ;; close to the position.  Longpole is only interesting if
     ;; latpole is 0.0; in most cases it should be latpole=90
     ;; longpole=180.  Irritatingly enough, the defaults are
     ;; latpole=0, longpole=180 which puts the pole a bit close
     ;; to the COSMOS field.  Note this affects what CD matrix you
     ;; have to use to get N is up, E is left
     IF KEYWORD_SET( nolatlon ) OR ABS(mapparam.middec) LE 60.0 THEN BEGIN
        ;;default, standard values
        latpole = 90.0
        longpole = 180.0
        cdbase = [ [-1.0d0,0.0],[0.0,1.0] ]
     ENDIF ELSE BEGIN
        latpole = 0.0
        IF ( (mapparam.midra GT 150 AND mapparam.midra LT 210) OR $
             mapparam.midra LT 30 OR mapparam.midra GT 330) THEN BEGIN
           longpole = 90.0 
           cdbase = [ [0.0d0,1.0],[1.0,0.0] ]
        ENDIF ELSE BEGIN
           longpole = 180.0        
           cdbase = [ [-1.0d0,0.0],[0.0,1.0] ]
        ENDELSE
     ENDELSE

     ;; make the astrometry 
     cdmat = c_pixsize * cdbase

     ;; astrom struct, with dummy crpix for nos
     MAKE_ASTR, thisast, CD=cdmat, CRPIX=[1,1], CRVAL=crval, CTYPE=ctype, $
               DELTA=[1.0, 1.0], LATPOLE=latpole, LONGPOLE=longpole, $
               PV2=this_pv2_1, EQUINOX=2000

     ; project rectangular boundary into map space since spherical
     ; curvature is an issue for larger fields
     SMAP_GETMAPBOUNDARY, thisast, mapparam.minra, mapparam.maxra, $
                          mapparam.mindec, mapparam.maxdec, $
                          xlo, xhi, ylo, yhi
  
     crpix = [-ROUND(xlo), -ROUND(ylo)] + 1.0 ; CRPIX is 1-indexed
     IF N_ELEMENTS(crpixx) NE 0 THEN crpix[0] = crpixx[i]
     IF N_ELEMENTS(crpixy) NE 0 THEN crpix[1] = crpixy[i]

     xrange = ROUND(xhi + crpix[0])
     IF KEYWORD_SET(nxpix) THEN BEGIN
         xrange = nxpix[i]
         IF N_ELEMENTS(crpixx) EQ 0 THEN crpix[0] = FLOOR(xrange / 2)
     ENDIF

     yrange = ROUND(yhi + crpix[1])
     IF KEYWORD_SET(nypix) THEN BEGIN
         yrange = nypix[i]
         IF N_ELEMENTS(crpixy) EQ 0 THEN crpix[1] = FLOOR(yrange / 2)
     ENDIF

     ;IF xrange GT 10000 OR yrange GT 10000 THEN BEGIN
     ;IF xrange GT 20000 OR yrange GT 20000 THEN BEGIN
     IF xrange GT 40000 OR yrange GT 40000 THEN BEGIN
        errmsg = STRING(xrange,yrange,FORMAT='("Map too large! ",I0," x ",I0)')
        RETURN
     ENDIF     

     ; set crpix
     thisast.crpix = crpix

     ;; can't really error check this guy, but we can at least ask 
     ;; whether it returned anything
     IF N_ELEMENTS(thisast) EQ 0 THEN BEGIN
        errmsg = 'MAKE ASTR did not return anything!'
        RETURN
     ENDIF

     ;; ok, now get the default map struct.  We know the things we 
     ;; need to pass it from above, so that's what we'll do 
     map = GET_SMAP_MAPSTRUCT(NPIXX=xrange, NPIXY=yrange,$
                              BAND=defparams.bands[i],FILTER=filter,$
                              ASTROMETRY=thisast, SUCCESS=gsm_success,$
                              ERRMSG=gsm_errmsg, /SILENT, /EXP_DBL)
     ;; error check
     IF ~gsm_success THEN BEGIN
        errmsg = 'GET_SMAP_MAPSTRUCT encountered an error: ' + gsm_errmsg
        RETURN
     ENDIF

     CASE defparams.bands[i] OF 
        'PSW' : map250 = TEMPORARY(map)
        'PMW' : map350 = TEMPORARY(map)
        'PLW' : map500 = TEMPORARY(map)
        ELSE : BEGIN
           errmsg = "Unknown band: "+defparams.bands[i]
           RETURN
        END
     ENDCASE
  ENDFOR

  success = 1b
  RETURN
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Naive/n00b map maker
PRO smap_make_maps_naivemap, tods, mapparam, map250, map350, map500, $
                             EXCLUDEMASK=excludemask, SUCCESS=success,$
                             BADBOLOS=badbolos,USEBOLOS=usebolos,$
                             ERRMSG=errmsg, VERBOSE=verbose,$
                             NO250=no250, NO350=no350, $
                             NO500=no500
  COMPILE_OPT IDL2, HIDDEN
  success = 0b
  errmsg  = ''

  ntods = N_ELEMENTS(tods)
  IF ntods EQ 0 THEN BEGIN
     errmsg = "No tods provided to smap_make_maps_naivemap"
     RETURN
  ENDIF

  IF N_ELEMENTS(map250) NE 0 THEN wt250 = DBLARR(map250.xsize,map250.ysize)
  IF N_ELEMENTS(map350) NE 0 THEN wt350 = DBLARR(map350.xsize,map350.ysize)
  IF N_ELEMENTS(map500) NE 0 THEN wt500 = DBLARR(map500.xsize,map500.ysize)

  tod_type = SIZE( tods, /TNAME )
  FOR i=0,ntods-1 DO BEGIN
     IF KEYWORD_SET( verbose ) THEN $
        MESSAGE,STRING(i,100.0*i/(ntods-1),$
                       FORMAT='(" On scan ",I4," [",F5.1,"%]")'),/INF,$
                LEVEL=-1
     IF tod_type EQ 'POINTER' THEN curr_tod = *tods[i] ELSE $
        IF tod_type EQ 'STRING' THEN BEGIN
        curr_tod = smap_readtod( tods[i], SUCCESS=rsuccess, ERRMSG=errmsg )
        IF rsuccess EQ 0 THEN BEGIN
           errmsg = "Error reading tod from "+tods[i]+": "+errmsg
           RETURN
        ENDIF
     ENDIF ELSE BEGIN
        errmsg = "Unexpected TOD type in smap_make_maps_naivemap: "+tod_type
        RETURN
     ENDELSE
     IF N_ELEMENTS(excludemask) NE 0 && $
        TAG_EXIST( curr_tod, 'mask_bits', /TOP_LEVEL ) THEN BEGIN
        mapmaskbits = construct_mask_bitmask( excludemask, $
                                              curr_tod.mask_bits,$
                                              SUCCESS=csuccess,$
                                              ERRMSG=errmsg )
        IF csuccess EQ 0 THEN BEGIN
           errmsg = "While making mask bits for "+$
                    curr_tod.shortfile+": "+errmsg
           RETURN
        ENDIF
        IF i EQ 0 THEN BEGIN
           ;;First tod
           IF N_ELEMENTS(map250) NE 0 THEN map250.tod_excludemask=mapmaskbits
           IF N_ELEMENTS(map350) NE 0 THEN map350.tod_excludemask=mapmaskbits
           IF N_ELEMENTS(map500) NE 0 THEN map500.tod_excludemask=mapmaskbits
        ENDIF
     ENDIF ELSE DELVARX,mapmaskbits ;;make sure none from previous iter

     ;;Do actual accumulation
     SMAP_ACCUMULATEMAP, curr_tod, map250, wt250, map350 ,wt350, $
                         map500, wt500, mapparam, VERBOSE=verbose, $
                         SUCCESS=sam_success,ERRMSG=sam_errmsg,$
                         MAPMASKBITS=mapmaskbits, BADBOLOS=badbolos,$
                         USEBOLOS=usebolos, NO250=no250, NO350=no350,$
                         NO500=no500
     IF sam_success EQ 0 THEN BEGIN
        errmsg = "Error accumulating map from "+curr_tod.shortfile+": "+errmsg
        RETURN
     ENDIF
  ENDFOR


  SMAP_FINALIZEMAP, map250, wt250, map350, wt350, map500, wt500,$
                    VERBOSE=verbose, SUCCESS=sfm_success, ERRMSG=sfm_errmsg,$
                    NO250=no250, NO350=no350, NO500=no500
  IF sfm_success EQ 0 THEN BEGIN
     errmsg = "Error finalizing maps in smap_make_maps_naivemap: "+sfm_errmsg
     RETURN
  ENDIF

  success = 1b
  RETURN
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO smap_make_maps_itermap, tods, mapparam, map250, map350, map500, $
                            ITER_PARAMS=iter_params, IFIXED250=ifixed250, $
                            IFIXED350=ifixed350, IFIXED500=ifixed500, $
                            SPARAMS250=sparams250, SPARAMS350=sparams350, $
                            SPARAMS500=sparams500, VERBOSE=verbose, $
                            ITER_DIAGNOSTICS=iter_diagnostics,$
                            BADBOLOS=badbolos, EXCLUDEMASK=excludemask,$
                            EXCLUDEWEIGHTMASK=excludeweightmask,$
                            ITER_FIXED=iter_fixed, ITER_FIX_DIR=iterfixdir, $
                            SUCCESS=success, ERRMSG=errmsg, EXNAME=exname, $
                            PARAMS_TODINDEX=params_todindex,$
                            NO250=no250, NO350=no350, NO500=no500,$
                            FIXEDDEGLITCH=fixeddeglitch,$
                            DEGLITCH250=deglitch250, DEGLITCH350=deglitch350,$
                            DEGLITCH500=deglitch500, SAVEMAPDIR=savemapdir, $
                            STORE_PIXINFO=store_pixinfo
  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg  = ''

  ; decompose ITER_DIAGNOSTICS (bit flag)
  dosavemaps = 0
  dosaveparams = 0

  IF KEYWORD_SET(iter_diagnostics) THEN BEGIN
      IF ((iter_diagnostics AND 1B) NE 0) THEN dosavemaps = 1b
      IF ((iter_diagnostics AND 2B) NE 0) THEN dosaveparams = 1b
  ENDIF

  SMAP_ITERMAP, tods, mapparam, map250, map350, map500, $
                ITERMAP_PARAMS=iter_params, IFIXED250=ifixed250, $
                IFIXED350=ifixed350, IFIXED500=ifixed500, $
                SPARAMS250=sparams250, SPARAMS350=sparams350, $
                SPARAMS500=sparams500, EXCLUDEMASK=excludemask, $
                EXCLUDEWEIGHTMASK=excludeweightmask, BADBOLOS=badbolos, $
                FIXEDPARAMS=iter_fixed, FIXEDPARDIR=iterfixdir, $
                SAVEMAPS=dosavemaps, SAVEPARAMS=dosaveparams, $
                EXNAME=exname, PARAMS_TODINDEX=params_todindex, $
                SUCCESS=success, ERRMSG=errmsg, VERBOSE=verbose,$
                NO250=no250, NO350=no350, NO500=no500,$
                FIXEDDEGLITCH=fixeddeglitch, DEGLITCH250=deglitch250,$
                DEGLITCH350=deglitch350, DEGLITCH500=deglitch500,$
                SAVEMAPDIR=savemapdir, STORE_PIXINFO=store_pixinfo
  RETURN
END
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO smap_make_maps, tods, mapparam, map250, map350, map500, ITERMAP=itermap, $
                    ITER_PARAMS=iter_params, SPARAMS250=sparams250,$
                    SPARAMS350=sparams350, SPARAMS500=sparams500,$
                    EXCLUDEMASK=excludemask, PIXSCALE=pixscale,$
                    EXCLUDEWEIGHTMASK=excludeweightmask,$
                    ITER_DIAGNOSTICS=iter_diagnostics, $
                    ITER_FIXED=iter_fixed, ITER_FIX_DIR=iterfixdir, $
                    IFIXED250=ifixed250, IFIXED350=ifixed350,$
                    IFIXED500=ifixed500, TOD_INDEX=tod_index, $
                    SUCCESS=success, ERRMSG=errmsg, $
                    CRVALX=crvalx, CRVALY=crvaly, $
                    CRPIXX=crpixx, CRPIXY=crpixy, $
                    NXPIX=nxpix, NYPIX=nypix, BADBOLOS=badbolos,$
                    USEBOLOS=usebolos, EXNAME=exname, VERBOSE=verbose,$
                    NO250=no250, NO350=no350, NO500=no500,$
                    FIXEDDEGLITCH=fixeddeglitch, NOLATLON=nolatlon,$
                    DEGLITCH250=deglitch250, DEGLITCH350=deglitch350,$
                    DEGLITCH500=deglitch500, FILTER=filter,$
                    SAVEMAPDIR=savemapdir, STORE_PIXINFO=store_pixinfo, $
                    PROJTYPE=projtype
  
  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg  = ''
  
  IF KEYWORD_SET(itermap) AND KEYWORD_SET(usebolos) THEN $
     message,'USEBOLOS keyword does not work with the iterative mapper.',/inf
  IF ARG_PRESENT(deglitch250) AND ~ KEYWORD_SET(itermap) THEN $
     MESSAGE,'DEGLITCH250 is only supported for the interative mapper',/INF
  IF ARG_PRESENT(deglitch350) AND ~ KEYWORD_SET(itermap) THEN $
     MESSAGE,'DEGLITCH350 is only supported for the interative mapper',/INF
  IF ARG_PRESENT(deglitch500) AND ~ KEYWORD_SET(itermap) THEN $
     MESSAGE,'DEGLITCH500 is only supported for the interative mapper',/INF
  
  IF N_ELEMENTS( excludemask ) EQ 0 THEN $
     excludemask = ['maskMaster','maskDead']
  defparams = SMAP_GETDEFPARAMS(PIXSIZE=pixsize)
  IF ~KEYWORD_SET(pixsize) THEN pixsize = defparams.pixsize

  IF mapparam.nscans NE N_ELEMENTS(tods) THEN BEGIN
     errmsg = "Number of scans in mapparam not same as number of TODs"
     GOTO, err_handler
  ENDIF

  ;;See if we are indexing down, make culled down mapparam if we are
  ;; and copy of tods.  Note this doesn't waste memory if they are
  ;; pointers, and if they are filenames the memory waste is small
  ;; enough to be ignored
  nftods = N_ELEMENTS( tod_index )
  IF nftods NE 0 THEN BEGIN
     maxidx = MAX( tod_index, MIN=minidx )
     IF minidx LT 0 THEN BEGIN
        errmsg = "ERROR in smap_make_maps: invalid (<0) index in tod_index"
        GOTO, err_handler
     ENDIF
     IF minidx GE N_ELEMENTS(tods) THEN BEGIN
        errmsg = "ERROR in smap_make_maps: invalid (<ntods) index in tod_index"
        GOTO, err_handler
     ENDIF
     IF KEYWORD_SET( verbose ) THEN $
        MESSAGE,STRING(nftods,N_ELEMENTS(tods),$
                       FORMAT='("Using ",I0," index into ",I0," tods")'),/INF
     ;;Alter tags isn't working correctly, so brute force this
     ;; Unfortunately, this creates another thing that has to be kept
     ;; up to date...
     mapparam_used = { obsids_short: mapparam.obsids_short,$
                       obsids: mapparam.obsids,$
                       file_obsids: mapparam.file_obsids[tod_index],$
                       file_bbids: mapparam.file_bbids[tod_index],$
                       bands: mapparam.bands,$
                       nscans: nftods,$
                       filenames: mapparam.filenames[tod_index],$
                       scanlength: mapparam.scanlength[tod_index],$
                       midra: mapparam.midra, middec: mapparam.middec,$
                       minra: mapparam.minra, mindec: mapparam.mindec,$
                       maxra: mapparam.maxra, maxdec: mapparam.maxdec }
     tods_used = tods[tod_index]
  ENDIF ELSE BEGIN
     mapparam_used = mapparam
     tods_used     = tods
  ENDELSE

  ;;Step 1:
  ;;  Set up the map structures
  IF KEYWORD_SET(verbose) THEN MESSAGE,"Setting up map structures",/INF
  smap_make_maps_setupmaps, mapparam_used, map250, map350, map500,$
                            CRVALX=crvalx, CRVALY=crvaly, $
                            CRPIXX=crpixx, CRPIXY=crpixy, $
                            NXPIX=nxpix, NYPIX=nypix,$
                            PIXSCALE=pixscale, SUCCESS=msuccess,$
                            ERRMSG=errmsg, NOLATLON=nolatlon,$
                            NO250=no250, NO350=no350, NO500=no500, $
                            FILTER=filter, PROJTYPE=projtype
  IF msuccess EQ 0 THEN BEGIN
     errmsg = "Error initializing map structures: "+errmsg
     GOTO, err_handler
  ENDIF

  ;;Step 2:
  ;;  Build the maps
  tod_type = SIZE(tods,/TNAME)
  IF KEYWORD_SET( itermap ) THEN BEGIN
     IF KEYWORD_SET( verbose ) THEN MESSAGE,"Beginning iterative map making",$
                                            /INF
     smap_make_maps_itermap, tods_used, mapparam_used, map250, map350, map500,$
                             BADBOLOS=badbolos, EXCLUDEMASK=excludemask, $
                             EXCLUDEWEIGHTMASK=excludeweightmask,$
                             ITER_FIXED=iter_fixed, ITER_FIX_DIR=iterfixdir,$
                             IFIXED250=ifixed250, IFIXED350=ifixed350,$
                             IFIXED500=ifixed500, SPARAMS250=sparams250,$
                             SPARAMS350=sparams350, SPARAMS500=sparams500, $
                             ITER_PARAMS=iter_params, VERBOSE=verbose,$
                             ITER_DIAGNOSTICS=iter_diagnostics,$
                             PARAMS_TODINDEX=tod_index, $
                             SUCCESS=success, ERRMSG=errmsg, EXNAME=exname,$
                             NO250=no250, NO350=no350, NO500=no500,$
                             FIXEDDEGLITCH=fixeddeglitch,$
                             DEGLITCH250=deglitch250, DEGLITCH350=deglitch350,$
                             DEGLITCH500=deglitch500, SAVEMAPDIR=savemapdir, $
                             STORE_PIXINFO=store_pixinfo
     IF success EQ 0 THEN BEGIN
        errmsg = "While making iterative maps: "+errmsg
        GOTO, err_handler
     ENDIF
  ENDIF ELSE BEGIN
     IF KEYWORD_SET( verbose ) THEN MESSAGE,"Beginning naive map making",$
                                            /INF

     smap_make_maps_naivemap, tods_used, mapparam_used,$
                              map250, map350, map500,$
                              BADBOLOS=badbolos, USEBOLOS=usebolos,$
                              EXCLUDEMASK=excludemask, SUCCESS=success, $
                              ERRMSG=errmsg,$
                              NO250=no250, NO350=no350, NO500=no500
     IF success EQ 0 THEN BEGIN
        errmsg = "While making naive maps: "+errmsg
        GOTO, err_handler
     ENDIF
  ENDELSE

  ;;Good return
  success = 1b
  RETURN

  ;;Bad return
err_handler:
  IF KEYWORD_SET( verbose ) THEN MESSAGE,errmsg,/INF
  RETURN

END
