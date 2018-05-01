;+
;NAME
; create_astrometry_from_level1
;PURPOSE
; Sets up image params for an image that contains all observations
; from a set of L1 data
;CATEGORY
; Herschel SPIRE SMAP
;USAGE
;  mapparams = create_astrometry_from_level1( files )
; or
;  mapparams = create_astrometry_from_level1( hcss_export_dir )
; or
;  mapparams = create_astrometry_from_level1( timestream_info )
;INPUTS
; One of:
;   files           List of files to process (PointedPhotTimelines)
;   hcss_export_dir Directory to look in for data (HCSS export product)
;   timestream_info Output of parse_timestream_detinfo
;RETURNS
; A structure giving the astrometric solution in each band and the
;  image size
;OPTIONAL INPUTS
; bands             List of bands (def: ['PSW','PMW','PLW'])
; pixscale          Pixel scale for returned map, in arcsec (def: [6,9,12])
; excludemask       String list of timesamples to exclude.  Def: ['maskMaster']
;KEYWORDS
; verbose           Runs in verbose mode
; sps               Data is from SPS, which has no quality or
;                    temperature extensions
;OPTIONAL OUTPUTS
; success           1b on success, 0b on failure
; errmsg            Error message for failure
;NOTES
; The algorithm implicitly assumes the scan legs are relatively straight
;MODIFICATION HISTORY
; Author: Alex Conley, Aug 27, 2009
;-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get astrometric solution for given band (or bolometer) by finding at least
;; one file with a detector in that range
FUNCTION create_astrometry_from_level1_basicastr, file_info, band, pixscale,$
   JITTERTOL=jittertol, NOCHECK=nocheck, SUCCESS=success, ERRMSG=errmsg

  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg  = ''
  
  nfiles = N_ELEMENTS(file_info)
  IF nfiles EQ 0 THEN BEGIN
     errmsg = "No files to check"
     RETURN,!VALUES.F_NAN
  ENDIF
  
  IF pixscale LE 0.0 THEN BEGIN
     errmsg = "Invalid (non-positive) pixel scale "+STRING(pixscale)
     RETURN,!VALUES.F_NAN
  ENDIF
  
  IF STRLEN( band ) LE 3 THEN BEGIN
     ;;It's an actual band
     astr_fileidx = -1
     bol_idx = -1
     FOR j=0, nfiles-1 DO BEGIN
        wband=where(STRUPCASE(STRMID(file_info[j].bolometer_names,0,3)) EQ $
                    band, nband)
        IF nband NE 0 THEN BEGIN
           astr_fileidx = j
           bol_idx = wband[0]
           BREAK
        ENDIF
     ENDFOR
  ENDIF ELSE BEGIN
     ;;It's a specified bolometer
     astr_fileidx = -1
     bol_idx = -1
     FOR j=0, nfiles-1 DO BEGIN
        wbol=where(STRUPCASE(file_info[j].bolometer_names) EQ $
                    band, nbol)
        IF nbol NE 0 THEN BEGIN
           astr_fileidx = j
           bol_idx = wband[0]
           BREAK
        ENDIF
     ENDFOR
  ENDELSE
  
  IF astr_fileidx EQ -1 THEN BEGIN
     errmsg = "Couldn't find any detectors in: "+band
     RETURN, !VALUES.F_NAN
  ENDIF

  ;;Get astrometric solution for map from selected
  FXBOPEN, unit_ra, file_info[astr_fileidx].file, $
           file_info[astr_fileidx].ra_extn, $
           ACCESS='R', ERRMSG=errmsg
  IF errmsg NE '' THEN BEGIN
     FXBCLOSE,unit_ra
     errmsg = "While opening ra extn for "+$
              file_info[astr_fileidx].shortfile+$
              ": "+errmsg
     RETURN, !VALUES.F_NAN
  ENDIF
  FXBOPEN, unit_dec, file_info[astr_fileidx].file, $
           file_info[astr_fileidx].dec_extn, $
           ACCESS='R', ERRMSG=errmsg
  IF errmsg NE '' THEN BEGIN
     FXBCLOSE,unit_ra
     FXBCLOSE,unit_dec
     errmsg = "While opening dec extn for "+$
              file_info[astr_fileidx].shortfile+$
              ": "+errmsg
     RETURN,!VALUES.F_NAN
  ENDIF
  FXBREADM,unit_ra,file_info[astr_fileidx].ra_index[bol_idx]+1,ra,$
           STATUS=status1,ERRMSG=errmsg1
  FXBREADM,unit_dec,file_info[astr_fileidx].dec_index[bol_idx]+1,$
           dec,STATUS=status2,ERRMSG=errmsg2
  IF status1[0] EQ 0 OR status2[0] EQ 0 THEN BEGIN
     errmsg = "While reading ra/dec for "+$
              file_info[astr_fileidx].bolometer_names[bol_idx]+" "+$
              file_info[astr_fileidx].shortfile + ": "
     IF errmsg1 NE '' THEN errmsg += errmsg1
     IF errmsg2 NE '' THEN errmsg += errmsg2
     FXBCLOSE,unit_ra
     FXBCLOSE,unit_dec
     RETURN,!VALUES.F_NAN
  ENDIF
  FXBCLOSE,unit_ra
  FXBCLOSE,unit_dec
        
  ;;Get astrometric solution
  band_astr = determine_scan_orientation( ra, dec, $
                                          PIXSCALE=pixscale,$
                                          JITTERTOL=jittertol,$
                                          NOCHECK=nocheck,$
                                          SUCCESS=astr_success,$
                                          ERRMSG=errmsg )
  IF astr_success EQ 0 THEN BEGIN
     errmsg = "While getting astrometry from "+$
              file_info[astr_fileidx].shortfile+": "+errmsg
     RETURN, !VALUES.F_NAN
  ENDIF

  success = 1b
  RETURN,band_astr
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION create_astrometry_from_level1, arg1, BANDS=bands, PIXSCALE=pixscale, $
                                        HCSSINTERNAL=hcssinternal, SPS=sps, $
                                        VERBOSE=verbose, SUCCESS=success, $
                                        ERRMSG=errmsg, EXCLUDEMASK=excludemask
  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg  = ''

  ;;Input checks and defaults
  IF N_ELEMENTS(bands) EQ 0 THEN bands = ['PSW','PMW','PLW']
  IF N_ELEMENTS(pixscale) EQ 0 THEN int_pixscale = [6.0,9.0,12.0] ELSE $
     int_pixscale = pixscale
  IF N_ELEMENTS(int_pixscale) LT N_ELEMENTS(bands) THEN BEGIN
     IF N_ELEMENTS(int_pixscale) EQ 1 THEN BEGIN
        int_pixscale = REPLICATE( int_pixscale[0], N_ELEMENTS(bands) )
     ENDIF ELSE BEGIN
        errmsg = "Don't know how to assign pixel scales to all bands"
        GOTO, err_handler
     ENDELSE
  ENDIF
  IF N_ELEMENTS(excludemask) EQ 0 THEN $
     excludemask=['maskMaster'] ELSE BEGIN
     IF SIZE( excludemask, /TNAME ) NE 'STRING' THEN BEGIN
        errmsg = "Didn't get expected string type for excludemask, but "+$
                 SIZE( excludemask, /TNAME )
        GOTO, err_handler
     ENDIF
  ENDELSE

  ;;Check arg and decide what it is.  Use this to get
  ;;parse_timestream_detinfo.  We only care about signal from the sky
  IF SIZE( arg1, /TNAME ) EQ 'STRUCT' THEN BEGIN
     file_info = arg1
  ENDIF ELSE IF SIZE( arg1, /TNAME ) EQ 'STRING' THEN BEGIN
     file_info = parse_timestream_detinfo( arg1, SUCCESS=parse_success,$
                                           ERRMSG=errmsg, $
                                           HCSSINTERNAL=hcssinternal,$
                                           NOTEMP=sps, SIGNALONLY=sps )
     IF parse_success EQ 0 THEN BEGIN
        errmsg = "Error encountered in parse_timestream_detinfo: "+errmsg
        GOTO, err_handler
     ENDIF
  ENDIF ELSE BEGIN
     errmsg = "Don't understand first argument"
     GOTO, err_handler
  ENDELSE
  
  ;;Get basic astronometry for each band
  nbands = N_ELEMENTS(bands)
  FOR i=0, nbands-1 DO BEGIN
     ;;Get basic astrometric solution for this band
     astr = $
        create_astrometry_from_level1_basicastr( file_info, bands[i], $
                                                 int_pixscale[i],$
                                                 SUCCESS=basic_success,$
                                                 ERRMSG=errmsg,$
                                                 /NOCHECK )
     IF basic_success EQ 0b THEN BEGIN
        errmsg = "While setting up basic astrometry for "+bands[i]+": "+errmsg
        GOTO, err_handler
     ENDIF

     IF N_ELEMENTS(astr_arr) EQ 0 THEN astr_arr = TEMPORARY(astr) ELSE $
        astr_arr = [astr_arr, TEMPORARY(astr)]
  ENDFOR

  ;;Now loop over files, building info for map
  limits = REPLICATE( { xmin: 1000000L, xmax: -1000000L, $
                        ymin: 1000000L, ymax: -1000000L }, nbands )
  FOR i=0, N_ELEMENTS( file_info )-1 DO BEGIN
     ;;Read in RA/DEC extension for this file
     ra = MRDFITS( file_info[i].file, file_info[i].ra_extn,$
                   /SILENT, STATUS=status_read )
     IF status_read LT 0 THEN BEGIN
        errmsg = "Error reading RA extension for "+fits_info.shortfile
        GOTO, err_handler
     ENDIF
     dec = MRDFITS( file_info[i].file, file_info[i].dec_extn,$
                    /SILENT, STATUS=status_read )
     IF status_read LT 0 THEN BEGIN
        errmsg = "Error reading DEC extension for "+fits_info.shortfile
        GOTO, err_handler
     ENDIF

     ;;Mask
     do_masking = 0b
     head = HEADFITS( file_info[i].file, ERRMSG=head_errmsg )
     IF head_errmsg NE '' THEN BEGIN
        mask_bits = get_mask_bits( head, SUCCESS=mask_success )
        IF mask_success NE 0 THEN BEGIN
           mapmaskbits = construct_mask_bitmask( excludemask, mask_bits )
           IF mapmaskbits NE 0 THEN do_masking = 1b
        ENDIF
     ENDIF
     IF do_masking THEN BEGIN
        mask = MRDFITS( file_info[i].file, file_info[i].mask_extn,$
                        /SILENT, STATUS=status_read )
        IF status_read LT 0 THEN BEGIN
           errmsg = "Error reading MASK extension for "+fits_info.shortfile
           GOTO, err_handler
        ENDIF
     ENDIF

     ;;Loop over all dets in this band
     FOR j=0, nbands-1 DO BEGIN
        signalf5 = STRUPCASE(STRMID(file_info[i].bolometer_names,0,5))
        wsignal_dets = WHERE( STREGEX( signalf5, bands[j]+'[A-J][0-9]',$
                                       /BOOLEAN, /FOLD_CASE ), nsignal_dets )
        IF nsignal_dets EQ 0 THEN BEGIN
           errmsg = "Couldn't find dets to use in "+bands[j]+" for "+$
                    file_info[i].shortfile
           GOTO, err_handler
        ENDIF

        ;;And all bolometers in this band, updating the limits
        curr_astr = astr_arr[j]
        FOR k = 0, nsignal_dets-1 DO BEGIN
           ;;RA/DEC
           currra = ra.(file_info[i].ra_index[ wsignal_dets[k] ])
           currdec = dec.(file_info[i].dec_index[ wsignal_dets[k] ])
          
           IF do_masking THEN BEGIN
              mask = mask.(file_info[i].mask_index[wsignal_dets[k]])
              wkeep = WHERE( FINITE(currra) AND FINITE(currdec) AND $
                             (mapmaskbits AND mask EQ 0), nkeep,$
                             NCOMPLEMENT=nremove )
              IF nkeep EQ 0 THEN CONTINUE
              IF nremove NE 0 THEN BEGIN
                 currra = currra[wkeep]
                 currdec = currdec[wkeep]
              ENDIF
           ENDIF
 
           ;;Get 4 corners of this scan
           minra = MIN( currra, wminra, MAX=maxra, $
                        SUBSCRIPT_MAX= wmaxra, /NAN )
           mindec = MIN( currdec, wmindec, MAX=maxdec, $
                         SUBSCRIPT_MAX= wmaxdec, /NAN )
           
           ;;Get x/y of corners, use these to update
           AD2XY, currra[wminra], currdec[wminra], curr_astr,$
                  x_ramin,y_ramin
           AD2XY, currra[wmaxra], currdec[wmaxra], curr_astr,$
                  x_ramax,y_ramax
           AD2XY, currra[wmindec], currdec[wmindec], curr_astr,$
                  x_decmin,y_decmin
           AD2XY, currra[wmaxdec], currdec[wmaxdec], curr_astr,$
                  x_decmax,y_decmax
           curr_minx = MIN( [x_ramin, x_ramax, x_decmin, x_decmax], $
                            MAX=curr_maxx )
           curr_miny = MIN( [y_ramin, y_ramax, y_decmin, y_decmax], $
                            MAX=curr_maxy )

           
           limits[j].xmin <= FLOOR(curr_minx)
           limits[j].xmax >= CEIL(curr_maxx)
           limits[j].ymin <= FLOOR(curr_miny)
           limits[j].ymax >= CEIL(curr_maxy)
        ENDFOR
     ENDFOR
  ENDFOR
  ;;Make sure limits are sane
  FOR j=0,nbands-1 DO BEGIN
     IF limits[j].xmin GE 1000000L THEN BEGIN
        errmsg = "Couldn't set xmin in "+bands[j]
        GOTO, err_handler
     ENDIF
     IF limits[j].xmax LE -1000000L THEN BEGIN
        errmsg = "Couldn't set xmax in "+bands[j]
        GOTO, err_handler
     ENDIF
     IF limits[j].ymin GE 1000000L THEN BEGIN
        errmsg = "Couldn't set ymin in "+bands[j]
        GOTO, err_handler
     ENDIF
     IF limits[j].ymax LE -1000000L THEN BEGIN
        errmsg = "Couldn't set ymax in "+bands[j]
        GOTO, err_handler
     ENDIF
  ENDFOR

  ;;Shift the astrometry so that the pixel scales start at 0
  ;;Note that the astrometry structure pixels start at 1
  FOR i=0, nbands-1 DO BEGIN
     astr_arr[i].naxis[0] = limits[i].xmax - limits[i].xmin + 1
     astr_arr[i].naxis[1] = limits[i].ymax - limits[i].ymin + 1
     astr_arr[i].crpix[0] -= limits[i].xmin
     astr_arr[i].crpix[1] -= limits[i].ymin
     IF KEYWORD_SET(verbose) THEN BEGIN
        ;;Get min/max ra/dec
        XY2AD, 0, 0, astr_arr[i], ra1, dec1
        XY2AD, 0, astr_arr[i].naxis[1]-1, astr_arr[i], ra2, dec2
        XY2AD, astr_arr[i].naxis[0]-1, astr_arr[i].naxis[1]-1, $
               astr_arr[i], ra3, dec3
        XY2AD, astr_arr[i].naxis[0]-1, 0, astr_arr[i], ra4, dec4
        min_ra = MIN( [ra1,ra2,ra3,ra4], MAX=max_ra )
        min_dec = MIN( [dec1,dec2,dec3,dec4], MAX=max_dec )
        MESSAGE,STRING(bands[i],astr_arr[i].naxis[0],astr_arr[i].naxis[1],$
                       FORMAT='("For band ",A0," extent: ",I0," ",I0)'),/INF
        MESSAGE,STRING(min_ra,max_ra,FORMAT='("RA limits: ",F0.6," ",F0.6)'),$
                /INF
        MESSAGE,STRING(min_dec,max_dec,$
                       FORMAT='("DEC limits: ",F0.6," ",F0.6)'),/INF
     ENDIF
  ENDFOR

  ;;Add band name to astr structs
  new_astr = STRUCT_ADDTAGS( astr_arr, 'band', "''")
  new_astr.band = bands
        
  success=1b
  RETURN, new_astr

  err_handler:
  IF KEYWORD_SET( verbose ) THEN message,errmsg,/INF
  RETURN,!VALUES.F_NAN

END
