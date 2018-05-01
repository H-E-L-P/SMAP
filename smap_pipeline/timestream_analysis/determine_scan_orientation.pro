;+
;NAME
; determine_scan_orientation
;PURPOSE
; To determine the orientation of a L1 scan, setting up a WCS
; coordinate system with the scan as the x direction.
;USAGE
; ast = determine_scan_orientation(file)
;OR 
; ast = determine_scan_orientation(ravals,decvals)
;REQUIRED INPUTS
; file           The name of a HIPE output L1 scan produce FITS file
;OR
; ravals,decvals The ra/dec values as 1d arrays in degrees.
;RETURNS
; ast            A FITS astrometry structure as used by IDL Astrolib
;KEYWORDS
; nocheck       Don't check to make sure scan line is straight
; verbose       Run in verbose mode
;OPTIONAL INPUTS
; bolometer     Name of bolometer to use for RA/DEC calculations.
;                Only useful when file is provided.  (def: PSWE8)
; pixscale      Pixel scale of output coordinate system (def: 3")
; jittertol     If /NOCHECK is not set, the maximum tolerance (in
;                 arcsec) for deviations from the x axis (def: 10 arcsec)
;OPTIONAL OUTPUTS
; ralims        Min/Max RA values in scan
; declims       Min/Max DEC values in scan
; success       1 on success, 0 on failure
; errmsg        An error message in case of failure
;MODIFICATION HISTORY
; Author: Alex Conley, May 2009
;-

FUNCTION determine_scan_orientation, arg1, arg2, BOLOMETER=bolometer,$
                                     PIXSCALE=pixscale, NOCHECK=nocheck,$
                                     RALIMS=ralims, DECLIMS=declims,$
                                     JITTERTOL=jittertol, SUCCESS=success,$
                                     ERRMSG=errmsg, VERBOSE=verbose

  COMPILE_OPT IDL2, STRICTARRSUBS

  success = 0b
  errmsg = ''

  IF N_ELEMENTS(bolometer) EQ 0 THEN bolometer = 'PSWE8'
  IF N_ELEMENTS(pixscale) EQ 0 THEN pixscale = 3.0
  IF N_ELEMENTS(jittertol) EQ 0 THEN jittertol = 10.0 ;;arcsec

  IF pixscale LE 0.0 THEN BEGIN
     errmsg = 'Invalid pixel scale '+STRING(pixscale)
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF
  IF ~ KEYWORD_SET( nocheck ) AND jittertol LE 0.0 THEN BEGIN
     errmsg = 'Invalid jitter tolerance '+STRING(jittertol)
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF

  IF N_PARAMS() EQ 1 THEN BEGIN
     file = arg1
     IF SIZE(file,/TNAME) NE 'STRING' THEN BEGIN
        errmsg = "Input file variable not of right type: "+STRING(file)
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDIF
     IF STRLEN(file) EQ 0 THEN BEGIN
        errmsg = "Input file variable is empty string"
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDIF
     IF ~ FILE_TEST( file ) THEN BEGIN
        errmsg = 'File '+file+' not found'
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDIF
     IF ~ FILE_TEST( file, /READ ) THEN BEGIN
        errmsg = 'File '+file+' found but not readible'
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDIF

     ;;Figure out which extensions have RA/DEC info
     FITS_OPEN,file,fcb,/NO_ABORT,MESSAGE=mssg
     FITS_CLOSE,fcb
     IF mssg NE '' THEN BEGIN
        errmsg = "Failed to read FITS file: "+file 
        RETURN,!VALUES.F_NAN
     ENDIF
     wra = WHERE( exts EQ 'ra', nra )
     wdec = WHERE( exts EQ 'dec', ndec )

     IF nra NE 1 THEN BEGIN 
        errmsg = "Couldn't find ra extension in "+file
        RETURN,!VALUES.F_NAN
     ENDIF
     IF ndec NE 1 THEN BEGIN 
        errmsg = "Couldn't find dec extension in "+file
        RETURN,!VALUES.F_NAN
     ENDIF

     ra_hdu = wra[0]
     dec_hdu = wdec[0]

     ra_status = 0
     dat_ra = MRDFITS(file,ra_hdu,ra_head,/SILENT,STATUS=ra_status)
     IF ra_status LT 0 THEN BEGIN
        errmsg = "Error reading RA HDU from "+file
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDIF
     IF STRUPCASE(STRTRIM(sxpar(ra_head,'EXTNAME'))) NE 'RA' THEN BEGIN
        errmsg = "RA HDU does not contain ra values"
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDIF
     dec_status = 0
     dat_dec = MRDFITS(file,dec_hdu,dec_head,/SILENT,STATUS=dec_status)
     IF dec_Status LT 0 THEN BEGIN
        errmsg = "Error reading DEC HDU from "+file
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDIF
     IF STRUPCASE(STRTRIM(sxpar(dec_head,'EXTNAME'))) NE 'DEC' THEN BEGIN
        errmsg = "DEC HDU does not contain dec values"
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDIF
     
     ;;Make sure both have the tag name we want
     IF SIZE(bolometer,/TNAME) NE 'STRING' THEN BEGIN
        errmsg = "Input bolometer is not of string type, which is required"
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDIF
     IF STRLEN(bolometer) EQ 0 THEN BEGIN
        errmsg = "Input bolometer is empty string"
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDIF
     wbol_ra = WHERE( STRUPCASE(bolometer) EQ TAG_NAMES(dat_ra), nbol )
     IF nbol NE 1 THEN BEGIN
        errmsg = "Specified bolometer "+bolometer+$
                 " not found in RA structure"
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDIF
     wbol_dec = WHERE( STRUPCASE(bolometer) EQ TAG_NAMES(dat_dec), nbol )
     IF nbol NE 1 THEN BEGIN
        errmsg = "Specified bolometer "+bolometer+$
                 " not found in DEC structure"
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDIF

     ;;These are in degrees
     ;;I should check this using TUNIT!
     npos = N_ELEMENTS(dat_ra)
     
     ravals = dat_ra.(wbol_ra[0])
     decvals = dat_dec.(wbol_dec[0])
     DELVARX,dat_ra,dat_dec
  ENDIF ELSE BEGIN
     npos = N_ELEMENTS( arg1 )
     IF N_ELEMENTS( arg2 ) NE npos THEN BEGIN
        errmsg = "decvals not same length as ravals"
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDIF
     ravals = arg1
     decvals = arg2
  ENDELSE

  IF ARG_PRESENT(ralims) THEN BEGIN
     ralims = DBLARR(2)
     ralims[0] = MIN( ravals, MAX=maxra )
     ralims[1] = maxra
  ENDIF
  IF ARG_PRESENT(declims) THEN BEGIN
     declims = DBLARR(2)
     declims[0] = MIN( decvals, MAX=maxdec )
     declims[1] = maxdec
  ENDIF
  
  IF npos LT 2 THEN BEGIN
     errmsg = "Require 2 or more scan points to determine orientation"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF

  ;;Choose the two points at the end
  ;;Don't use three points here or we will mess up the y scale!
  ra_vec = [ravals[0],ravals[npos-1]]
  dec_vec = [decvals[0],decvals[npos-1]]

  ;;Set up x values to get right pixel scale
  ;;dist will be in arcsec
  GCIRC,2,ravals[0],decvals[0],ravals[npos-1],decvals[npos-1],dist
  distpix = dist / pixscale

  ;;Get the CD matrix
  STARAST, ra_vec, dec_vec, [0.0,distpix], [0.0,0.0], cd

  ;;Turn this into a real AST structure
  ;;The scan line should be parallel to the x axis at y=NAXIS/2
  ;;The reference pixel is in the middle of the image
  ;;Should check we are close to pole, use a different one if we are!
  npix = ROUND(distpix)
  IF KEYWORD_SET(verbose) THEN BEGIN
     mssg = STRING(npix,FORMAT='("Length of scan line in pix: ",F7.2)')
     MESSAGE,mssg,/INF
  ENDIF
  crpix = [npix/2,npix/2] + 1 ;;The +1 is because FITS indexing starts at 1
  crval = [ 0.5*(ravals[0]+ravals[npos-1]),0.5*(decvals[0]+decvals[npos-1]) ]
  
  MAKE_ASTR, astr, CD=cd, CRPIX=crpix, CRVAL=crval,$
             CTYPE=['RA---TAN','DEC--TAN']
  astr = STRUCT_ADDTAGS( astr, 'naxis', 'LONARR(2)' )

  ;;Now we do some checks to make sure that the scan line actually
  ;; defines a line
  IF ~ KEYWORD_SET( nocheck ) THEN BEGIN
     AD2XY, ravals, decvals, astr, xproj, yproj
     maxdist = ABS(MAX( yproj - MEAN(yproj) ))
     IF KEYWORD_SET(verbose) THEN BEGIN
        mssg = STRING(maxdist * pixscale,$
                      FORMAT='("Maximum deviance from straightness: ",'+$
                      'E10.3," arcsec")')
        MESSAGE,mssg,/INF
        mssg = STRING(STDEV(yproj)*pixscale,$
                      FORMAT='("Standard deviation of jitter: ",'+$
                      'E10.3," arcsec")')
        MESSAGE,mssg,/INF
     ENDIF
     IF maxdist*pixscale GT jittertol THEN BEGIN
        errmsg = "Scan line is not straight to specified tolerance"
        IF KEYWORD_SET( verbose ) THEN MESSAGE, errmsg, /INF
        RETURN,!VALUES.F_NAN
     ENDIF
  ENDIF

  success = 1b
  RETURN,astr

END
