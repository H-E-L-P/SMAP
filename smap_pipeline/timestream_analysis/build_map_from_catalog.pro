;+
;NAME
; build_map_from_catalog
;PURPOSE
; Build an image from a catalog of sources, a PSF/PRF, and an astrometric
; solution.
;USAGE
; build_map_from_catalog, ast, catalogfiles, psf, band, map.
; This is a slightly lower level routine than create_map_from_catalog,
;  which calls this routine.
;REQUIRED INPUTS
; catalogfiles   Either a set of catalogs as IDL structs in SMAP
;                 catalog format or a list of FITS files containing the catalog
;                 information.  Filenames are relative to CATALOGDIR.
; ast            Astrometric solution to work from (see
;                 create_astrometry_from_level1).  This will set the
;                 pixel scale of the output map.  If NAXIS is present,
;                 this will set the size of the output map unless /FREESIZE
;                 is set.
; psf/prf        The PSF or PRF (see build_prf).  This is assumed
;                 to be at the same scale as the desired map (as
;                 defined by ast) unless prfscale is used.  In the
;                 former case this is the psf, in the latter a prf.
;                 If a PRF, the pixels are assumed to be the same size
;                 as the desired map, but the positional sampling may
;                 be finer by setting prfscale.
;                 It must have an odd size, with the central
;                 pixel representing the peak flux, and is assumed
;                 to be beam-normalized (i.e., unity in the center).
; band           Which SPIRE band (PSW,PMW,PLW)
;OUTPUTS
; map            Beam normalized output map corresponding to the 
;                 catalog using the smap_mapstruct format in Jy 
;                 (see: get_smap_mapstruct)
;KEYWORDS
;  randomize     Generates a random realization from the catalog,
;                 altering the positions using their errors.
;  onlypositive  Only include positive flux sources in map.
;  freesize      Ignore NAXIS entry from ast if present, let the map size
;                 be set by the objects and ra/declims.
;  verbose       Run in verbose mode
;OPTIONAL INPUTS
;  catalogext    The extension of the catalog if catalogfile is used
;                 (def: 1)
;  catalogdir    Directory to look for catalog in. (def: !SMAP_CATS)
;  ralims        RA limits (in decimal degrees) of objects to use in catalog.
;                 A 2 element array of [ramin,ramax] (range 0--360).  
;                 Set either to NaN to ignore.  (defaults: [!VALUES.F_NAN,
;                 !VALUES.F_NAN].  If ramin is larger than ramax, then 
;                 it is assumed wrapping is occuring at ra=360 deg.
;  declims       DEC limits, just like ra limits, without the wrapping
;                 issues, and with limits of -90--90
;  prfscale      The scale of the PRF in arcsec/pix.  This is how one
;                 uses a prf instead of a psf.  This is the step
;                 between elements of the PRF, although it should represent
;                 larger pixels.
;  snmin         Minimum signal-to-noise before including object.  The
;                 cut is done on ABS( flux / flux_error ), so strong negative
;                 flux detections are included (nless you set /ONLYPOSITIVE)
;  fluxmin       Minimum flux (in Jy) before including object
;  maxpix        Maximum pixel extent per size of output (def: 20000)
;  seed          Random seed -- useful if you want to ensure the
;                 same offsets are applied in multiple runs if
;                 /RANDOMIZE is on
;OPTIONAL OUTPUTS
;  success       1 on success, 0 on failure
;  errmsg        An error message in case of failure
;  ncat          Number of sources put on map
;NOTES
;  The output image will be large enough to cover everything in the
;   catalog(s).  So if you pass in a huge catalog, you are going to get
;   an enormous output image.  You can use ralim/declim to avoid this.
;MODIFICATION HISTORY
; Author: Alex Conley, May 2009
;-

PRO build_map_from_catalog, ast, arg1, psf, band, map,$
                            CATALOGEXT=catalogext, CATALOGDIR=catalogdir,$
                            PRFSCALE=prfscale, MAXPIX=maxpix, $
                            RALIMS=ralims, DECLIMS=declims, NCAT=ncat,$
                            ONLYPOSITIVE=onlypositive, RANDOMIZE=randomize, $
                            SEED=seed, SNMIN=snmin, FLUXMIN=fluxmin, $
                            FREESIZE=freesize,SUCCESS=success,ERRMSG=errmsg, $
                            VERBOSE=verbose

  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg = ''
  IF N_ELEMENTS(maxpix) EQ 0 THEN maxpix = 20000L
  IF maxpix LE 0 THEN BEGIN
     errmsg = STRING(maxpix,FORMAT='("Maxpix is non-positive: ",I0)')
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN
  ENDIF
  
  IF SIZE(band,/TNAME) NE 'STRING' THEN BEGIN
     errmsg = "Band must be a string, your is a "+SIZE(band,/TNAME)
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN
  ENDIF
  IF N_ELEMENTS(band) NE 1 THEN BEGIN
     errmsg = "Only scalar band is supported"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN
  ENDIF
  allowed_bands = ['PSW','PMW','PLW']
  wband = WHERE( allowed_bands EQ band, nband )
  IF nband EQ 0 THEN BEGIN
     errmsg = "Input band "+band+" not recognized"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN
  ENDIF
  bandidx = wband[0]

  ;;Limits
  IF N_ELEMENTS( ralims ) EQ 0 THEN ralims = [!VALUES.F_NAN, !VALUES.F_NAN ]
  IF N_ELEMENTS( declims ) EQ 0 THEN declims = [!VALUES.F_NAN, !VALUES.F_NAN ]
  IF N_ELEMENTS(ralims) NE 2 THEN BEGIN
     errmsg = "Input RA limits doesn't have 2 elements"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN
  ENDIF
  IF N_ELEMENTS(declims) NE 2 THEN BEGIN
     errmsg = "Input DEC limits doesn't have 2 elements"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN
  ENDIF
  IF FINITE(ralims[0]) THEN ra_lowerlimit=1b ELSE ra_lowerlimit=0b
  IF FINITE(ralims[1]) THEN ra_upperlimit=1b ELSE ra_upperlimit=0b
  IF FINITE(declims[0]) THEN dec_lowerlimit=1b ELSE dec_lowerlimit=0b
  IF FINITE(declims[1]) THEN dec_upperlimit=1b ELSE dec_upperlimit=0b
  IF ra_lowerlimit AND (ralims[0] LT 0.0 OR ralims[0] GT 360.0) THEN BEGIN
     errmsg = "RA lower limit invalid (0--360): "+STRING(ralims[0])
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN
  ENDIF
  IF ra_upperlimit AND (ralims[1] LT 0.0 OR ralims[1] GT 360.0) THEN BEGIN
     errmsg = "RA upper limit invalid (0--360): "+STRING(ralims[1])
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN
  ENDIF
  IF dec_lowerlimit AND (declims[0] LT -90.0 OR declims[0] GT 90.0) THEN BEGIN
     errmsg = "DEC lower limit invalid (-90--90): "+STRING(declims[0])
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN
  ENDIF
  IF dec_upperlimit AND (declims[1] LT -90.0 OR declims[1] GT 90.0) THEN BEGIN
     errmsg = "DEC upper limit invalid (-90--90): "+STRING(declims[1])
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN
  ENDIF
  IF (dec_lowerlimit AND dec_upperlimit) && $
     (declims[1] LT declims[0]) THEN BEGIN
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,"Swapping user input DEC limits",/INF
     temp = declims[0]
     delcims[0]=declims[1]
     declims[1]=TEMPORARY(temp)
  ENDIF

  ;;Make sure AST has the info we will need
  IF SIZE(ast,/TNAME) NE 'STRUCT' THEN BEGIN
     errmsg = "Input astrometric information is not structure"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN
  ENDIF
  ast_tags = ['CD','CDELT','CRPIX','CRVAL','CTYPE','LONGPOLE']
  wpresent = WHERE_ARRAY( TAG_NAMES(ast), ast_tags, npresent )
  IF npresent NE N_ELEMENTS(ast_tags) THEN BEGIN
     wmissing = MISSING( TAG_NAMES(ast), ast_tags, nmissing )
     IF nmissing EQ 0 THEN BEGIN
        errmsg = "Logic error for missing ast tags"
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN
     ENDIF 
     errmsg = "Missing tags in ast structure: "+$
              STRJOIN(ast_tags[wmissing],',')
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN
  ENDIF 

  ;;Make sure pixels are square
  fractol=0.1
  GETROT, ast, rot, cdelt, /SILENT
  xsize = ABS(cdelt[0]) & ysize = ABS(cdelt[1])
  IF ABS(xsize/ysize - 1.0) GT fractol THEN BEGIN
     errmsg = "Input ast structure does not have sufficiently square pixels"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN
  ENDIF 
  pixscale = SQRT(xsize*ysize) * 3600.0 ;;To arcsec/pix

  ;;See if user has specified size in advance
  IF TAG_EXIST( ast, 'NAXIS', /TOP_LEVEL ) THEN BEGIN
     IF KEYWORD_SET( freesize ) THEN use_naxis = 0b ELSE $
        use_naxis = 1b
  ENDIF ELSE use_naxis = 0b
  IF use_naxis AND N_ELEMENTS( ast.naxis ) NE 2 THEN BEGIN
     errmsg = "Naxis set in astrometry struct, but wrong extent"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN
  ENDIF 

  ;;Check PSF
  szpsf = SIZE(psf)
  IF szpsf[0] NE 2 THEN BEGIN
     errmsg = "Input PSF is not 2 dimensional"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN
  ENDIF 
  IF szpsf[1] mod 2 NE 1 AND szpsf[2] mod 2 NE 1 THEN BEGIN
     errmsg = "Input PSF must be odd size, or else I don't know where" +$
              " the center is"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN
  ENDIF 
  psfcentx = szpsf[1]/2
  psfcenty = szpsf[2]/2

  IF N_ELEMENTS(prfscale) NE 0 THEN BEGIN
     IF prfscale LE 0.0 THEN BEGIN
        errmsg = "Input prfscale is non-positive"
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN
     ENDIF
     IF prfscale GT pixscale THEN BEGIN
        errmsg = "Input PRF is more coarsely measured than desired map"
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN
     ENDIF
     prfrelscale = DOUBLE(prfscale) / pixscale ;;<1
     iprfrelscale = 1.0 / prfrelscale ;; >1
     is_prf = 1b
  ENDIF ELSE is_prf = 0b

  ;;Get catalog info
  ncatalogs = N_ELEMENTS(arg1)
  IF ncatalogs EQ 0 THEN BEGIN
     errmsg = "No input catalogs provided"
     IF KEYWORD_SET( verbose ) THEN MESSAGE,errmsg,/INF
     RETURN
  ENDIF
  cattype = SIZE( arg1, /TNAME )
  IF cattype EQ 'STRING' THEN BEGIN
     IF N_ELEMENTS(catalogdir) EQ 0 THEN readdir=addslash(!SMAP_CATS) ELSE $
     readdir=addslash(catalogdir)
     IF ~ FILE_TEST( readdir, /READ, /DIRECTORY ) THEN BEGIN
        errmsg = "Can't read input directory: "+readdir
        IF KEYWORD_SET( verbose ) THEN MESSAGE,errmsg,/INF
        RETURN
     ENDIF
     catalogfiles = arg1
     catalog_fullnames = readdir+catalogfiles
     read_fits = 1b

     FOR i=0, ncatalogs-1 DO BEGIN
        file = catalog_fullnames[i]
        IF STRLEN(file) EQ 0 THEN BEGIN
           errmsg = "Input file variable is empty string"
           IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
           RETURN
        ENDIF
        IF ~ FILE_TEST( file ) THEN BEGIN
           errmsg = 'File '+file+' not found'
           IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
           RETURN
        ENDIF
        IF ~ FILE_TEST( file, /READ ) THEN BEGIN
           errmsg = 'File '+file+' found but not readible'
           IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
           RETURN
        ENDIF
     ENDFOR
  ENDIF ELSE IF cattype EQ 'STRUCT' THEN BEGIN
     catalogs = arg1
     ncatalogs = 1
     read_fits = 0b
  ENDIF ELSE BEGIN
     errmsg = "Input catalogfiles input type must be STRING, not: "+$
              SIZE(catalogfiles,/TNAME)
     IF KEYWORD_SET( verbose ) THEN MESSAGE,errmsg,/INF
     RETURN
  ENDELSE
  IF N_ELEMENTS(catalogext) EQ 0 THEN catalogext = 1


  ;;Read in the info from each catalog
  ;; We only want to do this once, so we pull info out of
  ;; the structures and save it to arrays.
  startidx = LONARR(ncatalogs)  ;;Index into ra/dec/flux arrays for each cat
  catrestr = { ra: !VALUES.D_NAN, dec: !VALUES.D_NAN, flux: !VALUES.D_NAN }
  FOR i=0,ncatalogs-1 DO BEGIN
     IF read_fits THEN BEGIN
        IF KEYWORD_SET(verbose) THEN $
           MESSAGE,"Processing catalog file: "+catalogfiles[i],/INF
        catalog_status = 0
        catalog = MRDFITS(catalog_fullnames[i],catalogext,/SILENT,$
                          STATUS=catalog_status)
        IF catalog_status LT 0 THEN BEGIN
           errmsg = "Error reading catalog HDU from "+catalogfiles[i]
           IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
           RETURN
        ENDIF
     ENDIF ELSE catalog = catalogs ;;User provided single structure
     
     ;;Make sure catalog has the tags we need
     catalog_tags = ['ID','RA','DEC','DRA','DDEC','F_250','DF_250',$
                     'F_350','DF_350','F_500','DF_500','DET']
     wpresent = WHERE_ARRAY( TAG_NAMES(catalog), catalog_tags, npresent )
     IF npresent NE N_ELEMENTS(catalog_tags) THEN BEGIN
        wmissing = MISSING( TAG_NAMES(catalog), catalog_tags, nmissing )
        IF nmissing EQ 0 THEN BEGIN
           errmsg = "Logic error for missing catalog tags"
           IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
           RETURN
        ENDIF 
        errmsg = "Missing tags in catalog structure: "+$
                 STRJOIN(catalog_tags[wmissing],',')+" for file: "+$
                 catalogfiles[i]
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN
     ENDIF 

     ncat = N_ELEMENTS(catalog.ra)
     IF ncat EQ 0 THEN BEGIN
        errmsg = "Input catalog is empty for file: "+catalogfiles[i]
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN
     ENDIF 
     
     IF KEYWORD_SET(randomize) THEN BEGIN
        ;;Positions
        wfinra = WHERE( FINITE( catalog.dra ), nfinra )
        wfindec = WHERE( FINITE( catalog.ddec ), nfindec )
        IF nfinra EQ 0 OR nfindec EQ 0 THEN BEGIN
           IF nfinra EQ 0 THEN $
              errmsg = "RANDOMIZE set but no finite RA errors"
           IF nfindec EQ 0 THEN $
              errmsg = "RANDOMIZE set but no finite DEC errors"
           IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
           RETURN
        ENDIF
        IF KEYWORD_SET(verbose) THEN $
           MESSAGE,STRING(nfinra,FORMAT='("Randomizing ",I0," RA values")'),$
                   /INF
        catalog.ra[wfinra] += catalog.dra[wfinra] * RANDOMN(seed,nfinra)
        IF KEYWORD_SET(verbose) THEN $
           MESSAGE,STRING(nfindec,FORMAT='("Randomizing ",I0," DEC values")'),$
                   /INF
        catalog.dec[wfindec] += catalog.ddec[wfindec] * RANDOMN(seed,nfindec)
        
        ;;Fluxes
        ;;Don't just do the passband of interest so that (by
        ;; passing SEED) one can get the same behavior in multiple runs
        ;; with different output passbands
        wfin250 = WHERE(FINITE( catalog.df_250 ), nfin250 )
        wfin350 = WHERE(FINITE( catalog.df_350 ), nfin350 )
        wfin500 = WHERE(FINITE( catalog.df_500 ), nfin500 )
        IF nfin250 EQ 0 OR nfin350 EQ 0 OR nfin500 EQ 0 THEN BEGIN
           IF nfin250 EQ 0 THEN $
              errmsg = "RANDOMIZE set but no finite 250 micron errors"
           IF nfin350 EQ 0 THEN $
              errmsg = "RANDOMIZE set but no finite 350 micron errors"
           IF nfin500 EQ 0 THEN $
              errmsg = "RANDOMIZE set but no finite 500 micron errors"
           IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
           RETURN
        ENDIF
        IF KEYWORD_SET(verbose) THEN $
           MESSAGE,STRING(nfin250,$
                          FORMAT='("Randomizing ",I0," 250 micron flux '+$
                          'values")'),$
                   /INF
        catalog.f_250[wfin250] += catalog.df_250[wfin250] * $
                                  RANDOMN(seed,nfin250)
        IF KEYWORD_SET(verbose) THEN $
           MESSAGE,STRING(nfin350,$
                          FORMAT='("Randomizing ",I0," 350 micron '+$
                          'flux values")'),$
                   /INF
        catalog.f_350[wfin350] += catalog.df_350[wfin350] * $
                                  RANDOMN(seed,nfin350)
        IF KEYWORD_SET(verbose) THEN $
           MESSAGE,STRING(nfin500,$
                          FORMAT='("Randomizing ",I0," 500 '+$
                          'micron flux values")'),$
                   /INF
        catalog.f_500[wfin500] += catalog.df_500[wfin500] * $
                                  RANDOMN(seed,nfin500)
     ENDIF
     
     ;;Clip down to objects detected in this band
     wgood = WHERE(catalog.det[bandidx],ngood,NCOMPLEMENT=nbad)
     IF ngood EQ 0 THEN BEGIN
        IF KEYWORD_SET(verbose) THEN BEGIN
           IF read_fits THEN BEGIN
              MESSAGE,"No objects detected in band: "+band+" for "+$
                      catalogfiles[i],/INF
           ENDIF ELSE BEGIN
              MESSAGE,"No objects detected in band: "+band,/INF
           ENDELSE
        ENDIF
        CONTINUE
     ENDIF
     IF nbad NE 0 THEN catalog = catalog[wgood]

     IF ra_lowerlimit THEN BEGIN
        wgood = WHERE(catalog.ra GE ralims[0],ngood,NCOMPLEMENT=nbad)
        IF ngood EQ 0 THEN BEGIN
           IF KEYWORD_SET(verbose) THEN $
              MESSAGE,"No objects in "+catalogfiles[i]+$
                      " survived ra lower cut",/INF
           CONTINUE
        ENDIF
        IF nbad NE 0 THEN catalog = catalog[wgood]
     ENDIF
     IF ra_upperlimit THEN BEGIN
        IF ra_lowerlimit && (ralims[1] LT ralims[0]) THEN BEGIN
           ;;Assume we have wrapping
           wgood = WHERE(catalog.ra GE ralims[0] OR $
                         catalog.ra LE ralims[1], ngood, NCOMPLEMENT=nbad )
        ENDIF ELSE wgood = WHERE(catalog.ra LT ralims[1], ngood,$
                                 NCOMPLEMENT=nbad )
        IF ngood EQ 0 THEN BEGIN
           IF KEYWORD_SET(verbose) THEN BEGIN
              IF read_fits THEN BEGIN
                 MESSAGE,"No objects in "+catalogfiles[i]+$
                         " survived ra upper cut",/INF
              ENDIF ELSE BEGIN
                 MESSAGE,"No objects in catalog survived upper ra cut",/INF
              ENDELSE
           ENDIF
           CONTINUE
        ENDIF
        IF nbad NE 0 THEN catalog = catalog[wgood]
     ENDIF
     IF dec_lowerlimit THEN BEGIN
        wgood = WHERE(catalog.dec GT declims[0],ngood,NCOMPLEMENT=nbad) 
        IF ngood EQ 0 THEN BEGIN
           IF KEYWORD_SET(verbose) THEN BEGIN
              IF read_fits THEN BEGIN
                 MESSAGE,"No objects in "+catalogfiles[i]+$
                         " survived dec lower cut",/INF
              ENDIF ELSE BEGIN
                 MESSAGE,"No objects in catalog survived lower dec cut",/INF
              ENDELSE
           ENDIF
           CONTINUE
        ENDIF
        IF nbad NE 0 THEN catalog = catalog[wgood]
     ENDIF
     IF dec_upperlimit THEN BEGIN
        wgood = WHERE(catalog.dec LE declims[1],ngood,NCOMPLEMENT=nbad) 
        IF ngood EQ 0 THEN BEGIN
           IF KEYWORD_SET(verbose) THEN BEGIN
              IF read_fits THEN BEGIN
                 MESSAGE,"No objects in "+catalogfiles[i]+$
                         " survived dec upper cut",/INF
              ENDIF ELSE BEGIN
                 MESSAGE,"No objects in catalog survived upper dec cut",/INF
              ENDELSE
           ENDIF
           CONTINUE
        ENDIF
        IF nbad NE 0 THEN catalog = catalog[wgood]
     ENDIF

     ncat = N_ELEMENTS(catalog.ra)

     curr_catinfo = REPLICATE(catrestr,ncat)
     curr_catinfo.ra = catalog.ra
     curr_catinfo.dec = catalog.dec
     CASE bandidx OF 
        0 : curr_catinfo.flux = catalog.f_250
        1 : curr_catinfo.flux = catalog.f_350
        2 : curr_catinfo.flux = catalog.f_500
     ENDCASE
     
     ;;Do cuts on minimum signal to noise
     IF N_ELEMENTS( snmin ) NE 0 THEN BEGIN
        ;;First toss all objects with 0 errors
        CASE bandidx OF
           0 : fluxerr = catalog.df_250
           1 : fluxerr = catalog.df_350
           2 : fluxerr = catalog.df_500
        ENDCASE
        wgood = WHERE( fluxerr NE 0, ngood, NCOMPLEMENT=nbad )
        IF ngood EQ 0 THEN BEGIN
           IF KEYWORD_SET( verbose ) THEN $
              MESSAGE,"No objects in "+catalogfiles[i]+$
                      " survived minimum S/N requirement (and all previous"+$
                      " cuts) of value: "+STRING(snmin),/INF
           CONTINUE
        ENDIF
        IF nbad NE 0 THEN BEGIN
           curr_catinfo = curr_catinfo[wgood]
           fluxerr      = fluxerr[wgood]
        ENDIF

        ;;Now do actual S/N cut
        wgood = WHERE( ABS(curr_catinfo.flux / fluxerr) GT snmin, ngood, $
                       NCOMPLEMENT=nbad )
        IF ngood EQ 0 THEN BEGIN
           IF KEYWORD_SET( verbose ) THEN BEGIN
              IF read_fits THEN BEGIN
                 MESSAGE,"No objects in "+catalogfiles[i]+":"+band+$
                         " survived minimum S/N requirement (and all previous"+$
                         " cuts) of value: "+STRING(snmin),/INF
              ENDIF ELSE BEGIN
                 MESSAGE,"No objects in catalog: "+band+$
                         " survived minimum S/N requirement (and all previous"+$
                         " cuts) of value: "+STRING(snmin),/INF
              ENDELSE
           ENDIF
           CONTINUE
        ENDIF
        IF nbad NE 0 THEN curr_catinfo = curr_catinfo[wgood]
     ENDIF

     ;;Do cuts on minimum flux
     IF N_ELEMENTS( fluxmin ) NE 0 THEN BEGIN
        wgood = WHERE( curr_catinfo.flux GT fluxmin, ngood, NCOMPLEMENT=nbad )
        IF ngood EQ 0 THEN BEGIN
           IF KEYWORD_SET( verbose ) THEN BEGIN
              IF read_fits THEN BEGIN
                 MESSAGE,"No objects in "+catalogfiles[i]+":"+band+$
                         " survived minimum flux requirement "+$
                         "(and all previous"+$
                         " cuts) of value: "+STRING(fluxmin)+" mJy",/INF
              ENDIF ELSE BEGIN
                 MESSAGE,"No objects in catlog:"+band+$
                         " survived minimum flux requirement "+$
                         "(and all previous"+$
                         " cuts) of value: "+STRING(fluxmin)+" mJy",/INF
              ENDELSE
           ENDIF
           CONTINUE
        ENDIF
        IF nbad NE 0 THEN curr_catinfo = curr_catinfo[wgood]
     ENDIF


     ;;And possibly for positive fluxes only
     IF KEYWORD_SET( onlypositive ) THEN BEGIN
        wgood = WHERE( curr_catinfo.flux GT 0.0, ngood, NCOMPLEMENT=nbad )
        IF ngood EQ 0 THEN BEGIN
           IF KEYWORD_SET( verbose ) THEN BEGIN
              IF read_fits THEN BEGIN
                 MESSAGE,"No objects in "+catalogfiles[i]+":"+band+$
                         " survived positive flux requirement "+$
                         "(and all previous cuts)",/INF
              ENDIF ELSE BEGIN
                 MESSAGE,"No objects in catalog:"+band+$
                         " survived positive flux requirement "+$
                         "(and all previous cuts)",/INF
              ENDELSE
           ENDIF
           CONTINUE
        ENDIF
        IF nbad NE 0 THEN curr_catinfo = curr_catinfo[wgood]
     ENDIF

     startidx[i] = N_ELEMENTS(catinfo)
     IF N_ELEMENTS(catinfo) EQ 0 THEN catinfo=curr_catinfo ELSE $
        catinfo = [catinfo,curr_catinfo]
     DELVARX, catalog
  ENDFOR


  ncat = N_ELEMENTS( catinfo )
  IF ncat EQ 0 THEN BEGIN
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,"No catalog objects in limits -- returning blank map",/INF
     success = 1b
     map_make_success = 0b
     IF use_naxis THEN BEGIN
        npixx = ast.naxis[0]
        npixy = ast.naxis[1]
     ENDIF ELSE BEGIN
        npixx = 1
        npixy = 1
     ENDELSE
     map = get_smap_mapstruct( NPIXX=npixx, NPIXY=npixy, BAND=band,$
                               ASTROMETRY=ast, /NO_ABORT, $
                               SUCCESS=map_make_success, ERRMSG=errmsg, $
                               /SILENT )
     IF ~ map_make_success THEN BEGIN
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN
     ENDIF
     RETURN
  ENDIF
     
  ;;Get the initial x/y positions of all the catalog objects
  AD2XY, catinfo.ra, catinfo.dec, ast, xpos, ypos

  ;;Buffer around edge of catalog to make sure PSFs that just
  ;; miss the official range can be partially included
  bufpix = 5
  xpsf = szpsf[1]/2
  ypsf = szpsf[2]/2
  IF is_prf THEN BEGIN
     xpsf *= prfrelscale
     ypsf *= prfrelscale
  ENDIF
  xbuf = bufpix + xpsf
  ybuf = bufpix + ypsf

  ;;Figure out the extent of the image, possibly shift the astrometry
  IF use_naxis THEN BEGIN
     npixx = ast.naxis[0]
     npixy = ast.naxis[1]

     ;;Throw away all objects more than a few PSF widths outside
     ;; the image
     AD2XY, catinfo.ra, catinfo.dec, ast, xpos, ypos
     
     wgood = WHERE( xpos GT -xbuf AND xpos LT npixx + xbuf AND $
                    ypos GT -ybuf AND ypos LT npixy + ybuf, ncat,$
                    NCOMPLEMENT=nbad )
     IF ncat EQ 0 THEN BEGIN
        ;;Empty
        map = get_smap_mapstruct( NPIXX=npixx, NPIXY=npixy, BAND=band,$
                                  ASTROMETRY=ast, /NO_ABORT, $
                                  SUCCESS=map_make_success, ERRMSG=errmsg, $
                                  /SILENT )
        IF ~ map_make_success THEN BEGIN
           IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
           RETURN
        ENDIF
        RETURN
     ENDIF
     IF nbad NE 0 THEN catinfo = catinfo[wgood]
     outast = ast
  ENDIF ELSE BEGIN
     ;;Now it's up to the code
     ;;Make an adjusted astrometric structure so that all of the
     ;; x/y positions are positive and the astrometry is centered
     ;; in the output image.  Do this by building a box of positions
     minx = MIN(xpos,wminx) - bufpix - xpsf
     maxx = MAX(xpos,wmaxx) + bufpix + xpsf
     miny = MIN(ypos,wminy) - bufpix - ypsf
     maxy = MAX(ypos,wmaxy) + bufpix + ypsf
     npixx = CEIL( maxx - minx) + 1 & npixy = CEIL( maxy - miny ) + 1
     
     xbox = [ minx, maxx, minx ] ;;STARAST only takes 2 or 3 positions
     ybox = [ miny, miny, maxy ] ;; so omit one corner
     XY2AD, xbox, ybox, ast, rabox, decbox
     STARAST, rabox, decbox, xbox, ybox, cd
     crpix = [npixx/2,npixy/2]+1 ;;+1 because FITS indexes start at 1
     XY2AD, 0.5*(maxx+minx), 0.5*(maxy+miny), ast, racen, deccen
     crval = [racen,deccen]
     MAKE_ASTR, outast, CD=cd, CRPIX=crpix, CRVAL=crval,$
                CTYPE=['RA---TAN','DEC--TAN']
     GETROT, outast, rot, cdelt, /SILENT
     xsize = ABS(cdelt[0]) & ysize = ABS(cdelt[1])
     IF ABS(xsize/ysize - 1.0) GT fractol THEN BEGIN
        errmsg = "Output ast structure does not have sufficiently square pixels"
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN
     ENDIF 

     ;;Now get new x/y positions
     AD2XY, catinfo.ra, catinfo.dec, outast, xpos, ypos
     minx = MIN(xpos,wminx) & maxx = MAX(xpos,wmaxx)
     miny = MIN(ypos,wminy) & maxy = MAX(ypos,wmaxy)
     IF minx LT 0 OR maxx GT npixx THEN BEGIN
        errmsg = "Objects outside x frame of recentered image -- this is a bug"
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN
     ENDIF 
     IF miny LT 0 OR maxy GT npixy THEN BEGIN
        errmsg = "Objects outside y frame of recentered image -- this is a bug"
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN
     ENDIF 
  ENDELSE 

  ;;Get the array of positions in the image plane of the PSF
  ;;These are PIXEL positions, for real ones multiply by pixscale
  psfxpos = FINDGEN(szpsf[1])-psfcentx
  psfypos = FINDGEN(szpsf[2])-psfcenty
  IF is_prf THEN BEGIN
     ;;The PRF is more finely scaled than it's size would suggest,
     ;; which is the whole point
     psfxpos *= prfrelscale
     psfypos *= prfrelscale
     IF MAX(psfxpos)-MIN(psfxpos) LT 1 THEN BEGIN
        errmsg="PRF is too small in the x dimension -- fits "+$
               "entirely in single output pix"
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN
     ENDIF 
     IF MAX(psfypos)-MIN(psfypos) LT 1 THEN BEGIN
        errmsg="PRF is too small in the y dimension -- fits "+$
               "entirely in single output pix"
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN
     ENDIF 
  ENDIF
  
  ;;Build image
  IF KEYWORD_SET( verbose ) THEN BEGIN
     mssg = STRING(npixx,npixy,FORMAT='("Output image will be ",I0," by ",I0)')
     MESSAGE,mssg,/INF
     mssg = STRING(ncat,FORMAT='("and contain ",I0," objects")')
     MESSAGE,mssg,/INF
  ENDIF
  IF npixx GT maxpix OR npixy GT maxpix THEN BEGIN
     errmsg = STRING(npixx,npixy,maxpix,$
                     FORMAT='("Output map will be too large: ",I0," by ",I0,'+$
                     '" with maxsize: ",I0)')
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN
  ENDIF
  image = DBLARR( npixx, npixy )
  FOR i=0,ncat-1 DO BEGIN
     ;;Note: these don't have to be integral!
     ;; So we use interpolation even with no rebinning
     xposcent = xpos[i]
     yposcent = ypos[i]

     ;;Get the boundaries of the pixel box we need to change
     ;; in the image.  Recall psfxpos/psfypos are in image space pixels
     xchangeimmin = FLOOR(xposcent + psfxpos[0]) > 0
     xchangeimmax = CEIL(xposcent + psfxpos[szpsf[1]-1]) < npixx-1
     ychangeimmin = FLOOR(yposcent + psfypos[0]) > 0
     ychangeimmax = CEIL(yposcent + psfypos[szpsf[2]-1]) < npixy-1

     ;;Now interpolate on the PSF to find the values at all those
     ;; positions.  This requires the positions of each of the above
     ;; image pixels in the PSF/PRF frame
     ;;The point is that psfcentx should line up with xposcent, but the
     ;; possibility of truncation makes this tricky
     xvals = DINDGEN( xchangeimmax-xchangeimmin+1) + xchangeimmin - xposcent
     yvals = DINDGEN( ychangeimmax-ychangeimmin+1) + ychangeimmin - yposcent
     IF is_prf THEN BEGIN
        xvals *= iprfrelscale
        yvals *= iprfrelscale
     ENDIF
     xvals += psfcentx
     yvals += psfcenty

     xvals_full = TRANSPOSE(xvals ## REPLICATE(1.0,N_ELEMENTS(yvals)))
     yvals_full = yvals ## REPLICATE(1.0,N_ELEMENTS(xvals))
     DELVARX,xvals,yvals
     psf_interp = BILINEAR( psf, xvals_full, yvals_full, MISSING = 0.0 )
     psf_interp *= catinfo[i].flux
     image[xchangeimmin:xchangeimmax,ychangeimmin:ychangeimmax] += psf_interp

  ENDFOR
  
  ;;Wrap this puppy up as a smap_mapstruct
  map_make_success = 0b
  map = get_smap_mapstruct( NPIXX=npixx, NPIXY=npixy, BAND=band,$
                            ASTROMETRY=outast, /NOERR, /NOEXP, /NOMASK,$
                            /NO_ABORT, SUCCESS=map_make_success, $
                            ERRMSG=errmsg, /SILENT )
  IF ~ map_make_success THEN BEGIN
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN
  ENDIF
  map.image = TEMPORARY(image)

  success = 1b
  RETURN
END
