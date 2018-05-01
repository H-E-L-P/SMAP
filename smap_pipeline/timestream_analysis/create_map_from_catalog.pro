;+
;NAME
; create_map_from_catalog
;PURPOSE
; Takes an SMAP style catalog and converts it into a map
; given the input astrometry.  This is a higher level interface
; than build_map_from_catalog.
;CATEGORY
; Herschel SPIRE SMAP
;USAGE
; map = create_map_from_catalog( catalog, astrometry, band )
;INPUTS
; catalog            The catalog to build the map from.  Either an IDL
;                     structure or the name of a file to read in.  If a
;                     filename, relative to catalogdir.
; astrometry         IDL astrolib astrometry structure giving the pixel scale, 
;                     astrometric solutinon for output map
; band               Band to use (sets PSF)
;RETURNS
; SMAP style map structure containing map.
;OPTIONAL INPUTS
; catalogdir         Directory to look for catalogs in. (def: !SMAP_CATS)
; prfoversample      Oversampling to use when constructing PRF.  Set to 1
;                     to use a PSF and not a PRF (def: 9).  Has no
;                     effect if /MASKSOURCES is set.
; catsnmin           Minimum signal-to-noise before including object.  The
;                     cut is done on ABS( flux / flux_error ), so
;                     strong negative flux detections are included 
;                     (unless you set /ONLYPOSITIVE)
; catfluxmin         Minimum flux (in Jy) before including object
; npix               Number of pixels along one dimension of PSF.  Has
;                     defaults of 3*[18,24,35]/pixscale for P[S,M,L]W.
;                     Should be odd.
; masknsigma         Number of sigma to make the radius of the mask if
;                     /MASKSOURCES is set (Def: 2)
;KEYWORDS
; masksources        Rather than building a beam psf at each position,
;                     just create a mask of !VALUES.F_NAN at every
;                     object position.
; onlypositive       Only include positive flux detections
; verbose            Print informational messages
;OPTIONAL OUTPUTS
; beam               The beam used to build the map
; success            1 if it worked, 0 if it didn't
; errmsg             An error message if a problem was encountered
;ROUTINES CALLED
; get_spire_beam, build_prf, build_map_from_catalog
;MODIFICATION HISTORY
; Author: Alex Conley, Aug 2009
;-

FUNCTION create_map_from_catalog, arg1, astrometry, band, BEAM=beam,$
                                  CATALOGDIR=catalogdir, CATSNMIN=catsnmin,$
                                  PRFOVERSAMPLE=prfoversample,$
                                  ONLYPOSITIVE=onlypositive, SUCCESS=success,$
                                  ERRMSG=errmsg, VERBOSE=verbose,$
                                  CATFLUXMIN=catfluxmin, NPIX=npix,$
                                  MASKSOURCES=masksources, MASKNSIGMA=masknsigma

  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg  = ''

  IF N_ELEMENTS(prfoversample) EQ 0 THEN prfoversample=9
  IF N_ELEMENTS(catalogdir) EQ 0 THEN catalogdir = addslash(!SMAP_CATS)
  IF KEYWORD_SET( masksources ) THEN BEGIN
     IF N_ELEMENTS( masknsigma ) EQ 0 THEN masknsigma = 2.0
     IF masknsigma LE 0.0 THEN BEGIN
        errmsg = "Invalid masknsigma: "+STRING(masknsigma)
        GOTO, err_handler
     ENDIF
     IF N_ELEMENTS(maskrad) EQ 0 THEN BEGIN
        fwhm_to_sigma = masknsigma/SQRT(8.0*ALOG(2.0))
        maskrad = get_spire_beam_fwhm(band)*fwhm_to_sigma
     ENDIF
  ENDIF

  IF N_ELEMENTS(arg1) EQ 0 THEN BEGIN
     errmsg = "Need input catalog or filename"
     GOTO, err_handler
  ENDIF
  IF SIZE( arg1, /TNAME ) EQ 'STRING' THEN BEGIN
     ;;FITS file
     IF ~ FILE_TEST(catalogdir,/DIRECTORY) THEN BEGIN
        errmsg = "Can't find catalog input directory "+catalogdir
        GOTO, err_handler
     ENDIF
     catfile = catalogdir + arg1
     IF ~ FILE_TEST( catfile ) THEN BEGIN
        errmsg = "Can't find input catalog file: "+catfile
        GOTO, err_handler
     ENDIF
     IF ~ FILE_TEST( catfile, /READ ) THEN BEGIN
        errmsg = "Can't read catalog file: "+catfile
        GOTO, err_handler
     ENDIF

     ;;Now try to read it; the map should be in extension 1
     catalog = MRDFITS( catfile, 1, /SILENT, STATUS=read_status )
     IF read_status NE 0 THEN BEGIN
        errmsg = "FITS read of catfile "+arg1+" failed with error code: "+$
                 STRING(read_status,FORMAT='(I0)')
        GOTO, err_handler
     ENDIF
  ENDIF ELSE IF SIZE( arg1, /TNAME ) EQ 'STRUCT' THEN BEGIN
     catalog = arg1
  ENDIF ELSE BEGIN
     errmsg = "Don't understand what to do with catalog argument of type: "+$
              SIZE(arg1,/TNAME)
     GOTO, err_handler
  ENDELSE

  ;;Make sure catalog has required tags
  cat_tags = ['RA','DEC','DET']
  CASE band OF 
     'PSW' : cat_tags = [cat_tags,'F_250','DF_250']
     'PMW' : cat_tags = [cat_tags,'F_350','DF_350']
     'PLW' : cat_tags = [cat_tags,'F_500','DF_500']
     ELSE : BEGIN
        errmsg = "Unknown band: "+band
        GOTO, err_handler
     END
  ENDCASE
  wpresent = WHERE_ARRAY( TAG_NAMES(catalog), cat_tags, npresent )
  IF npresent NE N_ELEMENTS(cat_tags) THEN BEGIN
     wmissing = MISSING( TAG_NAMES(catalog), cat_tags, nmissing )
     errmsg = "Missing tags in cat structure: "+$
              STRJOIN(cat_tags[wmissing],',')
     GOTO, err_handler
  ENDIF

  ;;Now make sure astrometry has required tags
  ast_tags = ['CD','CDELT','CRPIX','CRVAL','CTYPE','LONGPOLE']
  wpresent = WHERE_ARRAY( TAG_NAMES(astrometry), ast_tags, npresent )
  IF npresent NE N_ELEMENTS(ast_tags) THEN BEGIN
     wmissing = MISSING( TAG_NAMES(astrometry), ast_tags, nmissing )
     IF nmissing EQ 0 THEN BEGIN
        errmsg = "Logic error for missing ast tags"
        GOTO, err_handler
     ENDIF 
     errmsg = "Missing tags in ast structure: "+$
              STRJOIN(ast_tags[wmissing],',')
     GOTO, err_handler
  ENDIF 

  ;;Make sure pixels are square since PSF only supports that
  fractol=0.1
  GETROT, astrometry, rot, cdelt, /SILENT
  xsize = 3600.0 * ABS(cdelt[0])
  ysize = 3600.0 * ABS(cdelt[1])
  IF ABS(xsize/ysize - 1.0) GT fractol THEN BEGIN
     errmsg = "Input ast structure does not have sufficiently square pixels"
     GOTO, err_handler
  ENDIF 
  pixscale = SQRT(xsize*ysize) ;;in arcsec/pix
  IF KEYWORD_SET( verbose ) THEN $
     MESSAGE,STRING(pixscale,FORMAT='("Using pixel size: ",F0.2," arcsec")'),$
             /INF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Build PSF/PRF/Circular mask
  IF KEYWORD_SET( verbose ) THEN $
     MESSAGE,"Getting PSF/PRF for map",/INF
  prfoversample_used = ROUND(prfoversample)
  IF prfoversample_used LT 1 THEN BEGIN
     errmsg = "PRF oversample can't be less than 1.0 in create_map_from_catalog"
     GOTO, err_handler
  ENDIF
  IF prfoversample EQ 1 OR KEYWORD_SET( masksources ) THEN is_prf = 0b ELSE $
     is_prf = 1b
  IF pixscale LE 0.0 OR ~ FINITE(pixscale) THEN BEGIN
     errmsg = "Invalid pixel scale: "+STRING(pixscale)+$
              " in create_map_from_catalog"
     GOTO, err_handler
  ENDIF

  IF KEYWORD_SET( masksources ) THEN BEGIN
     IF N_ELEMENTS(npix) EQ 0 THEN $
        npix = 2*CEIL(maskrad/pixscale)+1
     IF npix MOD 2 EQ 0 THEN npix += 1
     prfsize = npix * pixscale
     DELVARX,prfsampsize 
     mask_beam = smap_create_circular_mask( maskrad, pixscale, npix )
     wmask = WHERE(mask_beam,nmask)
     IF nmask EQ 0 THEN BEGIN
        ;;Just mask centre
        beam = REPLICATE(!VALUES.D_NAN,1,1)
        npix = 1
        prfsize = pixscale
     ENDIF ELSE BEGIN
        bmsz = SIZE(mask_beam)
        beam = DBLARR( bmsz[1], bmsz[2] )
        beam[wmask] = !VALUES.D_NAN
     ENDELSE
  ENDIF ELSE BEGIN
     IF N_ELEMENTS(npix) EQ 0 THEN BEGIN
        CASE band OF
           'PSW' : npix = ROUND(3*18.0/pixscale)
           'PMW' : npix = ROUND(3*24.0/pixscale)
           'PLW' : npix = ROUND(3*35.0/pixscale)
        ENDCASE
     ENDIF
     IF npix LE 1 THEN BEGIN
        errmsg = "Too small PSF in create_map_from_catalog using pixscale "+$
                 STRING(pixscale)
        GOTO, err_handler
     ENDIF
     IF is_prf THEN npix *= prfoversample_used
     IF npix MOD 2 EQ 0 THEN npix += 1
     IF is_prf THEN BEGIN
        prfsampsize = pixscale / DOUBLE(prfoversample_used)
        psf = get_spire_beam(band, prfsampsize, $
                             npix, npix, /SILENT)
        prf_success = 0b
        beam = build_prf( TEMPORARY(psf), prfoversample_used, $
                          SUCCESS=prfsuccess, ERRMSG=errmsg)
        IF prfsuccess EQ 0 THEN BEGIN
           errmsg = "Error building PRF in create_map_from_catalog: "+errmsg
           GOTO, err_handler
        ENDIF
        prfsize = npix * prfsampsize
     ENDIF ELSE BEGIN
        beam = get_spire_beam(band, pixscale, npix, npix, /SILENT)
        prfsize = npix * pixscale
        ;;Having this undefined is how build_map_from_catalog knows
        ;; we don't have a PRF
        DELVARX,prfsampsize 
     ENDELSE
  ENDELSE
  
  ncattot = 0 ;;Number of objects in cat
  
  IF KEYWORD_SET( verbose ) THEN $
     MESSAGE,"  Building catalog map for "+band,/INF
  catalog_success = 0b
  build_map_from_catalog, astrometry, catalog, beam, band, $
                          map, PRFSCALE=prfsampsize, VERBOSE=verbose,$
                          SNMIN=catsnmin, FLUXMIN=catfluxmin,$
                          SUCCESS=catalog_success, ERRMSG=errmsg,$
                          NCAT=ncat, ONLYPOSITIVE=onlypositive
  IF catalog_success EQ 0 THEN BEGIN
     errmsg = "While calling build_map_from_catalog: "+errmsg
     GOTO, err_handler
  ENDIF
  
  IF ncat EQ 0 AND KEYWORD_SET(verbose) THEN $
     MESSAGE,"Found no catalog objects in image for band "+$
             band,/INF

  ;; remove floating point errors
  error = check_math(MASK=32) 

  success = 1b
  RETURN,map

  err_handler:
  IF KEYWORD_SET( verbose ) THEN MESSAGE,errmsg,/INF
  RETURN,!VALUES.F_NAN

END
