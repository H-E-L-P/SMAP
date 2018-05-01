;+
;NAME
; prepare_scanmaps
;PURPOSE
; To build false maps in all bands from a list of catalogs and
; input maps that can be used to make simulated timestreams
;USAGE
; map_ptrs = prepare_scanmaps( [CATALOGS=catalogs, MAPS=maps,$
;                               BANDS=bands, MAPPARAM=mapparam] )
;INPUTS

;RETURNS
; A pointer array of maps.
; Yes, pointers, which are evil in IDL, but the only reasonable way
; to do this.
;OPTIONAL INPUTS
; catalogs         A list of SMAP style catalogs, either the
;                   filenames, a single catalog structure, or a pointer
;                   array of catalog structures.  Mapparam is required.
; maps             Similarly, a list of SMAP style maps as filenames,
;                   a single map structure, or a pointer array of them
; bands            The bands to make maps for.  Def: ['PSW','PMW','PLW']
;                   Affects catalogs, and maps if you pass in a
;                   list of map filenames.
;OPTIONS RELATED TO CATALOG FILES
; mapparam         Map parameters to build maps based on
; pixscale         Size of pixels used in fake maps in degrees
; onlypositive     Only take positive flux detections
; prfoversample    Oversampling to use in fake map creation
; catsnmin         Minimum S/N ratio to use catalog detection
; pixscale         Pixel scale of fake maps
; masknsigma       Number of sigma to make the radius of the mask if
;                     /MASKSOURCES is set (Def: 2)
;KEYWORDS RELATED TO CATALOG SOURCES
; masksources        Rather than building a beam psf at each position,
;                     just create a mask of !VALUES.F_NAN at every
;                     position.  Can not be combined with maps.
;OPTIONAL OUTPUTS
; success          1b if it worked, 0b if not
; errmsg           An error message explaining why it failed if it did
;MODIFICATION HISTORY
; Author: Alex Conley, Nov 2009
;-

FUNCTION prepare_scanmaps_get_astrom, mapparam, band, pixscale,$
                                      SUCCESS=success, ERRMSG=errmsg
  COMPILE_OPT IDL2, HIDDEN
  success = 0b
  errmsg  = ''

  ;;Make sure we have valid limits
  IF ~ FINITE(mapparam.minra) THEN BEGIN
     errmsg = "Non-finite minimum RA" & RETURN,!VALUES.F_NAN
  ENDIF
  IF ~ FINITE(mapparam.maxra) THEN BEGIN
     errmsg = "Non-finite maximum RA" & RETURN,!VALUES.F_NAN
  ENDIF
  IF ~ FINITE(mapparam.mindec) THEN BEGIN
     errmsg = "Non-finite minimum DEC" & RETURN,!VALUES.F_NAN
  ENDIF
  IF ~ FINITE(mapparam.maxdec) THEN BEGIN
     errmsg = "Non-finite maximum DEC" & RETURN,!VALUES.F_NAN
  ENDIF
  IF ~ FINITE(mapparam.middec) THEN BEGIN
     errmsg = "Non-finite mid DEC" & RETURN,!VALUES.F_NAN
  ENDIF
  IF ~ FINITE(mapparam.midra) THEN BEGIN
     errmsg = "Non-finite mid RA" & RETURN,!VALUES.F_NAN
  ENDIF
  IF mapparam.minra LT 0 THEN BEGIN
     errmsg = "minra is less than zero: "+STRING(mapparam.minra)
     RETURN,!VALUES.F_NAN
  ENDIF
  IF mapparam.maxra GT 370.0 THEN BEGIN
     errmsg = "maxra is too large: "+STRING(mapparam.maxra)
     RETURN,!VALUES.F_NAN
  ENDIF
  IF mapparam.mindec LT -90 THEN BEGIN
     errmsg = "mindec is too small: "+STRING(mapparam.mindec)
     RETURN,!VALUES.F_NAN
  ENDIF
  IF mapparam.maxdec GT 90.0 THEN BEGIN
     errmsg = "maxdec is too large: "+STRING(mapparam.maxdec)
     RETURN,!VALUES.F_NAN
  ENDIF
  IF mapparam.minra GE mapparam.maxra THEN BEGIN
     errmsg = "minra >= maxra" & RETURN,!VALUES.F_NAN
  ENDIF
  IF mapparam.mindec GE mapparam.maxdec THEN BEGIN
     errmsg = "mindec >= maxdec" & RETURN,!VALUES.F_NAN
  ENDIF

  ;; Make the map
  IF ~ FINITE(pixscale) THEN BEGIN
     errmsg = "Non-finite pixel size"
     RETURN, !VALUES.F_NAN
  ENDIF
  
  xrange = CEIL( (mapparam.maxra - mapparam.minra) * $
                 COS(!PI / 180. * mapparam.middec) / pixscale )
  yrange = CEIL( (mapparam.maxdec - mapparam.mindec) / $
                 pixscale )
  
  ;; make the astrometry 
  cdmat = [[-pixscale,0.0],[0.0,pixscale]]
  crval = [mapparam.midra + 0.5*pixscale, mapparam.middec]
  crpix = [0.5*xrange,0.5*yrange]
     
  MAKE_ASTR,thisast,CD=cdmat,CRPIX=crpix,CRVAL=crval
  thisast = STRUCT_ADDTAGS( thisast, 'naxis', 'LONARR(2)' )
  thisast.naxis[0] = xrange
  thisast.naxis[1] = yrange

  ;; can't really error check this guy, but we can at least ask 
  ;; whether it returned anything
  IF N_ELEMENTS(thisast) EQ 0 THEN BEGIN
     errmsg = 'MAKE ASTR did not return anything!'
     RETURN,!VALUES.F_NAN
  ENDIF
  
  success = 1b
  RETURN,thisast
END


FUNCTION prepare_scanmaps, CATALOGS=catalogs, MAPS=maps,$
                           BANDS=bands, PIXSCALE=pixscale, MAPPARAM=mapparam,$
                           PRFOVERSAMPLE=prfoversample,$
                           ONLYPOSITIVE=onlypositive,CATSNMIN=catsnmin,$
                           MASKNSIGMA=masknsigma, MASKSOURCES=masksources,$
                           SUCCESS=success, ERRMSG=errmsg, VERBOSE=verbose
  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg  = ''

  IF N_ELEMENTS(bands) EQ 0 THEN bands = ['PSW','PMW','PLW']
  IF SIZE(bands,/TNAME) NE 'STRING' THEN BEGIN
     errmsg = "Bands not a string array as expected"
     GOTO, err_handler
  ENDIF
  nbands = N_ELEMENTS(bands)

  ;;Setup pixel scale
  npixscl = N_ELEMENTS(pixscale)
  IF npixscl EQ 0 THEN BEGIN
     i_pixscale = FLTARR(nbands)
     FOR i=0,nbands-1 DO CASE bands[i] OF
        'PSW' : i_pixscale[i] = 4
        'PMW' : i_pixscale[i] = 6
        'PLW' : i_pixscale[i] = 8
        ELSE : BEGIN
           errmsg = "Unknown band: "+bands[i]
           GOTO, err_handler
        END
     ENDCASE
     i_pixscale /= 3600.0 ;;To deg
  ENDIF ELSE IF npixscl EQ 1 THEN BEGIN
     i_pixscale = REPLICATE(pixscale[0],nbands)
  ENDIF ELSE IF npixscl LT nbands THEN BEGIN
     errmsg = "Unsure how to extend provided pixel scale to all bands"
     GOTO, err_handler
  ENDIF ELSE i_pixscale = pixscale

  IF MAX(i_pixscale) GT 1/60.0 THEN $
     MESSAGE,"Very large pixscale -- did you remember to convert to deg?",/INF

  ;;Decide how many catalogs we have
  IF N_ELEMENTS(catalogs) NE 0 THEN BEGIN
     IF SIZE( catalogs, /TNAME ) EQ 'STRUCT' THEN BEGIN
        ncatalogs = 1
     ENDIF ELSE IF SIZE( catalogs, /TNAME ) EQ 'STRING' THEN BEGIN
        ncatalogs = N_ELEMENTS(catalogs)
     ENDIF ELSE IF SIZE( catalogs, /TNAME ) EQ 'POINTER' THEN BEGIN
        ncatalogs = N_ELEMENTS(catalogs)
     ENDIF ELSE BEGIN
        errmsg = "Don't understand input catalog type: "+SIZE(catalogs,/TNAME)
        GOTO, err_handler
     ENDELSE
  ENDIF ELSE ncatalogs=0
   IF ncatalogs NE 0 AND N_ELEMENTS(mapparam) EQ 0 THEN BEGIN
      errmsg = "Reqiure mapparam if catalogs provided"
      GOTO, err_handler
   ENDIF

   ;;And how many maps
   IF N_ELEMENTS(maps) NE 0 THEN BEGIN
      IF SIZE( maps, /TNAME ) EQ 'STRUCT' THEN BEGIN
         nmaps = N_ELEMENTS(maps)
      ENDIF ELSE IF SIZE( maps, /TNAME ) EQ 'STRING' THEN BEGIN
         nmaps = N_ELEMENTS(maps)*nbands
      ENDIF ELSE IF SIZE( maps, /TNAME ) EQ 'POINTER' THEN BEGIN
         nmaps = N_ELEMENTS(maps)
      ENDIF ELSE BEGIN
         errmsg = "Don't understand input map type: "+SIZE(maps,/TNAME)
         GOTO, err_handler
      ENDELSE
   ENDIF ELSE nmaps=0

   IF ncatalogs + nmaps EQ 0 THEN BEGIN
      errmsg = "No input catalogs or maps!"
      GOTO,err_handler
   ENDIF
   IF KEYWORD_SET( masksources ) AND nmaps NE 0 THEN BEGIN
      errmsg = "Can't combine input masks and /MASKSOURCES"
      GOTO,err_handler
   ENDIF

   retarr = PTRARR( ncatalogs*nbands + nmaps)

   ;;Load in catalogs
   FOR i=0,ncatalogs-1 DO BEGIN
      IF SIZE( catalogs, /TNAME ) EQ 'STRUCT' THEN BEGIN
         ccat = catalogs[i]
      ENDIF ELSE IF SIZE(catalogs,/TNAME) EQ 'POINTER' THEN BEGIN
         ccat = *catalogs[i]
      ENDIF ELSE IF SIZE(catalogs,/TNAME) EQ 'STRING' THEN BEGIN
        IF ~ FILE_TEST( addslash(!SMAP_CATS)+catalogs[i], /READ ) THEN BEGIN
           errmsg = "Couldn't read: "+catalogs[i]
           GOTO, err_handler
        ENDIF
        ccat = MRDFITS( addslash(!SMAP_CATS)+catalogs[i], 1, $
                        STATUS=status, /SILENT )
        IF status LT 0 THEN BEGIN
           errmsg = "Error reading catalog: "+catalogs[i]
           GOTO, err_handler
        ENDIF
     ENDIF
     FOR j=0,nbands-1 DO BEGIN
        
        ;;Get the astrometry
        astrom = prepare_scanmaps_get_astrom( mapparam, bands[j], $
                                              i_pixscale[j],$
                                              SUCCESS=asuccess,$
                                              ERRMSG=errmsg )
        IF asuccess EQ 0 THEN BEGIN
           errmsg = "Error preparing astrometry: "+errmsg
           GOTO, err_handler
        ENDIF

        cmap = create_map_from_catalog( ccat, astrom, bands[j], $
                                        PRFOVERSAMPLE=prfoversample,$
                                        ONLYPOSITIVE=onlypositive,$
                                        CATSNMIN=catsnmin,$
                                        CATFLUXMIN=catfluxmin,$
                                        SUCCESS=cat_success,$
                                        ERRMSG=errmsg,$
                                        MASKSOURCES=masksources,$
                                        MASKNSIGMA=masknsigma )
        IF cat_success EQ 0 THEN BEGIN
           errmsg = "While making map from catalog: "+errmsg
           GOTO, err_handler
        ENDIF
        retarr[ i*nbands + j ] = PTR_NEW( TEMPORARY(cmap) )
     ENDFOR
     DELVARX,ccat
  ENDFOR

  IF nmaps NE 0 THEN BEGIN
     IF SIZE( maps, /TNAME ) EQ 'STRUCT' THEN BEGIN
        FOR i=0, nmaps-1 DO retarr[ ncatalogs*nbands + i ] = PTR_NEW(maps[i])
     ENDIF ELSE IF SIZE( maps, /TNAME ) EQ 'STRING' THEN BEGIN
        FOR i=0,nmaps-1 DO BEGIN
           FOR j=0,nbands-1 DO BEGIN
              curr_map = read_smap_fitsmap( maps[i], bands[j], /SILENT,$
                                            ERRMSG=errmsg, SUCCESS=msuccess,$
                                            /NOREADEXP, /NOREADERR, /STRICT,$
                                            /NO_ABORT )
              IF msuccess EQ 0 THEN BEGIN
                 errmsg = "Error reading map in "+bands[j]+" for "+$
                          maps[i]
                 GOTO, err_handler
              ENDIF
              retarr[ncatalogs*nbands + i*nbands + j] = $
                 PTR_NEW(TEMPORARY(curr_map))
           ENDFOR
        ENDFOR
     ENDIF ELSE IF SIZE( maps, /TNAME ) EQ 'POINTER' THEN BEGIN
        retarr[ncatalogs*nbands:*] = maps
     ENDIF
  ENDIF

  success=1b
  RETURN,retarr

err_handler:
  IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
  IF N_ELEMENTS(retarr) NE 0 THEN PTR_FREE,retarr
  RETURN,!VALUES.F_NAN
END
