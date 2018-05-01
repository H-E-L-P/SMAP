;+
;NAME
;  get_scan_fluxes
;PURPOSE
;  Given an input image and a list of RA/DEC, generates a
;  L1 timestream flux density list.
;CATEGORY
;  Herschel SPIRE timestream
;USAGE
;  fluxes = get_scan_fluxes( map, ra, dec )
;INPUTS
;  map         Map to scan.  Structure in smap_mapstruct form --
;               see, e.g., build_map_from_catalog or get_smap_mapstruct
;  ra          List of RA values in degrees
;  dec         List of DEC values in degrees
;RETURNS
;  A list of fluxes for every value of ra, dec
;KEYWORDS
;  cubic       Use a cubic approximation to SINC interpolation instead
;               of bilinear interpolation -- see the interpolate
;               documentation for details.  This slows the code
;               considerably, but seems to work a lot better and therefore
;               is on by default (set CUBIC=0 to turn off).
;  nocheck     Don't perform a check to make sure the scan line is mostly
;               along the x axis
;  verbose     Runs in verbose mode
;OPTIONAL INPUTS
;  maskbits    If maskmap is passed, list of mask bits to compare with
;               maskmap.  If the AND of these with the bits at this
;               position is non-zero, then ignore the value in the 
;               map. The default is 'FFFFFFFF'xUL -- i.e., all ones.
;  jittertol   If /NOCHECK is not set, the maximum tolerance (in
;               arcsec) for deviations from the x axis (def: 5 arcsec)
;OPTIONAL OUTPUTS
;  success     1 if it worked, 0 if it didn't
;  errmsg      An error message if a problem was encountered
;NOTES
;  Ideally, the scan direction should lie almost entirely along
;   the x-axis (with some jitter), and the ra/dec list should represent
;   a single scan with no changes in direction.  If this is not the
;   case, the effects of the sampling rate as implemented in
;   build_map_from_catalog will be incorrect.
;  Scan positions that lie outside the map are assumed to contribute
;   zero flux.
;MODIFICATION HISTORY
;  Author: Alex Conley, May 2009
;-

FUNCTION get_scan_fluxes, map, ra, dec, $
                          MASKBITS=maskbits, CUBIC=cubic, $
                          JITTERTOL=jittertol, NOCHECK=nocheck, $
                          VERBOSE=verbose, SUCCESS=success, ERRMSG=errmsg

  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg = ''
  
  IF N_ELEMENTS(cubic) EQ 0 THEN cubic=1b
  IF N_ELEMENTS( jittertol ) EQ 0 THEN jittertol = 5.0 ;; in arcsec
  IF jittertol LE 0.0 AND ~ KEYWORD_SET( nocheck ) THEN BEGIN
     errmsg = "Jitter tolerance is non-positive"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF

  IF size(map,/TNAME) NE 'STRUCT' THEN BEGIN
     errmsg = "Input map is not a structure"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF

  map_tags = ['IMAGE','HAS_MASK','ASTROMETRY','XSIZE','YSIZE']
  wpresent = WHERE_ARRAY( TAG_NAMES(map), map_tags, npresent )
  IF npresent NE N_ELEMENTS(map_tags) THEN BEGIN
     wmissing = MISSING( TAG_NAMES(map), map_tags, nmissing )
     IF nmissing EQ 0 THEN BEGIN
        errmsg = "Logic error for missing map tags"
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDIF 
     errmsg = "Missing tags in map structure: "+$
              STRJOIN(map_tags[wmissing],',')
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF 

  npos = N_ELEMENTS( ra )
  IF N_ELEMENTS( dec ) NE npos THEN BEGIN
     errmsg = "Number of input DEC values not same as number of RA values"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF
  IF ( SIZE(ra) )[0] EQ 0 THEN is_scalar=1b ELSE is_scalar=0b

  IF map.has_mask THEN BEGIN
     has_mask=1b
     IF N_ELEMENTS(maskbits) EQ 0 THEN int_maskbits = 'FFFFFFFF'xUL ELSE $
        int_maskbits = maskbits
     IF SIZE(maskbits,/TNAME) NE 'ULONG' THEN $
        MESSAGE,"WARNING: input maskbits type is not ULONG as expected, but "+$
                SIZE(maskbits,/TNAME)
  ENDIF ELSE has_mask=0b

  IF KEYWORD_SET(verbose) THEN $
     MESSAGE,STRING(npos,FORMAT='("Processing ",I0," positions")'),/INF

  AD2XY, ra, dec, map.astrometry, xpos, ypos

  ;;Check to make sure we are mostly moving along x
  IF ~ KEYWORD_SET( nocheck ) THEN BEGIN
     maxdist = ABS(MAX( ypos - MEAN(ypos) ))
     pixsizey = SQRT( map.astrometry.cd[0,1]^2 + astr.cd[1,1]^2 ) * 3600.0
     pixtol = jittertol / pixsizey
     IF KEYWORD_SET(verbose) THEN BEGIN
        mssg = STRING(maxdist/pixsizey,$
                      FORMAT='("Maximum deviance from straightness: ",'+$
                      'E10.3," arcsec")')
        MESSAGE,mssg,/INF
        mssg = STRING(STDEV(ypos)/pixsizey,$
                      FORMAT='("Standard deviation of jitter: ",'+$
                      'E10.3," arcsec")')
        MESSAGE,mssg,/INF
     ENDIF
     IF maxdist GT pixtol THEN BEGIN
        errmsg = "Scan line is not along x axis to specified tolerance"
        IF KEYWORD_SET( verbose ) THEN MESSAGE, errmsg, /INF
        RETURN,!VALUES.F_NAN
     ENDIF
  ENDIF

  ;;Check boundaries
  wcontained = WHERE( xpos GE 0 AND xpos LT map.xsize AND $
                      ypos GE 0 AND ypos LT map.ysize, ncontained,$
                      NCOMPLEMENT=nnotcontained )
  IF nnotcontained NE 0 AND KEYWORD_SET(verbose) THEN BEGIN
     fracnot = DOULBE(nnotcontained)/DOUBLE(npos)*100.0
     mssg = STRING(nnotcontained,FORMAT='(I0," scan positions not contained "'+$
                   '"[",F5.1,"%]")')
     MESSAGE,mssg,/INF
  ENDIF

  ;;Quick return
  IF ncontained EQ 0 THEN BEGIN
     success = 1b
     IF is_scalar THEN RETURN,0.0 ELSE RETURN,DBLARR(npos)
  ENDIF

  IF KEYWORD_SET( cubic ) THEN $
     flux = INTERPOLATE( map.image, xpos, ypos, CUBIC=-0.5, MISSING=0.0 ) ELSE $
        flux = INTERPOLATE( map.image, xpos, ypos, MISSING=0.0 )

  IF has_mask THEN BEGIN
     ;;We obviously can't interpolate on the mask, so we have to
     ;; just take the 4 nearest points.
     fx = FLOOR(xpos) & fy = FLOOR(ypos)
     wcontained = WHERE( fx GE 0 AND fx LT map.xsize AND fy GE 0 AND $
                         fy LT map.ysize, ncontained )
     IF ncontained NE 0 THEN BEGIN
        fx = fx[wcontained] & fy = fy[wcontained]
        map_maskbits = map.mask[ fx, fy ] OR $
                       map.mask[ (fx + 1) < (map.xsize-1), fy ] OR $
                       map.mask[ fx, (fy + 1) < (map.ysize-1) ] OR $
                       map.mask[ (fx + 1) < (map.xsize-1), $
                                 (fy + 1) < (map.ysize-1) ]
        wbad = WHERE( int_maskbits AND map_maskbits, nbad )
        IF nbad NE 0 AND KEYWORD_SET(verbose) THEN BEGIN
           mssg = STRING(nbad,FORMAT='("Mask eliminated ",I0," flux points")')
           MESSAGE,mssg,/INF
        ENDIF
        IF nbad NE 0 THEN flux[wcontained[wbad]] = 0.0
     ENDIF
  ENDIF

  success = 1b
  RETURN,flux

END
