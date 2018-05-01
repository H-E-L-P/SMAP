;+
;NAME
;  convert_scat_catalog
;PURPOSE
;  To convert files from the SCAT output format into the
;  SMAP map format.  
;USAGE
;  convert_scat_catalog, cat250file, cat350file, cat500file, outfile
;INPUTS
;  cat250file         Fits file containing 250 micron catalog
;  cat350file         Fits file containing 350 micron catalog
;  cat500file         Fits file containing 500 micron catalog
;  outfile            Output file to write catalog to
;OPTIONAL INPUTS
;  nfwhm              Sets matching radius in terms of FWHM of
;                      the images being matched in terms of the
;                      quadrature sum of the two bands involved (def: 2)
;KEYWORDS
;  verbose            Run in verbose mode
;OPTIONAL OUTPUTS
;  success            1 if the combination succeeded, 0 if it didn't
;  errmsg             Information about failure if not successful, ''
;                      otherwise.   
;MODIFICATION HISTORY
;  Author: Alex Conley, Aug 11, 2009
;-

PRO convert_scat_catalog, cat250file, cat350file, cat500file, outfile,$
                          NFWHM=nfwhm, SUCCESS=success, ERRMSG=errmsg, $
                          VERBOSE=verbose

  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg  = ''

  IF N_ELEMENTS( nfwhm ) EQ 0 THEN nfwhm = 2.0
  IF nfwhm LE 0.0 THEN BEGIN
     errmsg = "nfwhm is invalid (non-positive)"
     GOTO, err_handling
  ENDIF

  IF N_ELEMENTS(cat250file) EQ 0 THEN BEGIN
     errmsg = 'cat250 file not present'
     GOTO, err_handling
  ENDIF
  IF SIZE( cat250file, /TNAME ) NE 'STRING' THEN BEGIN
     errmsg = 'cat250 file wrong type: '+SIZE(cat250file,/TNAME)+$
              ' (expected string)'
     GOTO, err_handling
  ENDIF
  IF N_ELEMENTS(cat350file) EQ 0 THEN BEGIN
     errmsg = 'cat350 file not present'
     GOTO, err_handling
  ENDIF
  IF SIZE( cat350file, /TNAME ) NE 'STRING' THEN BEGIN
     errmsg = 'cat350 file wrong type: '+SIZE(cat350file,/TNAME)+$
              ' (expected string)'
     GOTO, err_handling
  ENDIF
  IF N_ELEMENTS(cat500file) EQ 0 THEN BEGIN
     errmsg = 'cat500 file not present'
     GOTO, err_handling
  ENDIF
  IF SIZE( cat500file, /TNAME ) NE 'STRING' THEN BEGIN
     errmsg = 'cat500 file wrong type: '+SIZE(cat500file,/TNAME)+$
              ' (expected string)'
     GOTO, err_handling
  ENDIF

  IF KEYWORD_SET( verbose ) AND FILE_TEST( outfile ) THEN $
     MESSAGE,"WARNING: will overwrite: "+outfile,/INF

  ;;Make sure all the input files exist and are readable
  filelist = [cat250file, cat350file, cat500file]
  FOR i=0, N_ELEMENTS(filelist)-1 DO BEGIN
     IF ~ FILE_TEST( filelist[i] ) THEN BEGIN
        errmsg = "Input file "+filelist[i]+" doesn't exist"
        GOTO, err_handling
     ENDIF
     IF ~ FILE_TEST( filelist[i], /READ ) THEN BEGIN
        errmsg = "Input file "+filelist[i]+" is not readable"
        GOTO, err_handling
     ENDIF
  ENDFOR

  ;;Now read em in
  ;;Assume catalog is always in the first extension because the
  ;; examples we have don't do a good job with extension names
  cat250 = MRDFITS( cat250file, 1, /SILENT, STATUS=status )
  IF status LT 0 THEN BEGIN
     errmsg = "Error reading cat250 FITS file: "+cat250file
     GOTO, err_handling
  ENDIF
  cat350 = MRDFITS( cat350file, 1, /SILENT, STATUS=status )
  IF status LT 0 THEN BEGIN
     errmsg = "Error reading cat350 FITS file: "+cat350file
     GOTO, err_handling
  ENDIF
  cat500 = MRDFITS( cat500file, 1, /SILENT, STATUS=status )
  IF status LT 0 THEN BEGIN
     errmsg = "Error reading cat500 FITS file: "+cat500file
     GOTO, err_handling
  ENDIF

  ;;Now we need to source match between the catalogs
  ;; to figure out how many unique objects we have, etc.
  ;;This will be slightly messy

  ;;Use SRCOR
  ;;Status arrays will be 0: this band only 1: this band and 250
  ;; 2: this band and 350 3: this band and 500 4: all bands
  
  statusobj = { idx250: -1, idx350: -1, idx500: -1 }
  status_250 = REPLICATE( statusobj, N_ELEMENTS(cat250) )
  status_350 = REPLICATE( statusobj, N_ELEMENTS(cat350) )
  status_500 = REPLICATE( statusobj, N_ELEMENTS(cat500) )
  status_250.idx250 = INDGEN( N_ELEMENTS(cat250) )
  status_350.idx350 = INDGEN( N_ELEMENTS(cat350) )
  status_500.idx500 = INDGEN( N_ELEMENTS(cat500) )
  
  ;;First, compare 250 with 350
  dist_250_350 = SQRT( 18.0^2 + 24.0^2 ) * nfwhm
  SRCOR, cat250.ra, cat250.dec, cat350.ra, cat350.dec, dist_250_350,$
         ind_250_350, ind_350_250, MAGNITUDE=cat250.flux,$
         SPHERICAL=2, OPTION=1, /SILENT
  n_250_350 = N_ELEMENTS( ind_250_350 )

  IF n_250_350 NE 0 THEN BEGIN
     status_250[ ind_250_350 ].idx350 = ind_350_250
     status_350[ ind_350_250 ].idx250 = ind_250_350
  ENDIF

  ;;And 250 with 500
  dist_250_500 = SQRT( 18.0^2 + 35.0^2 ) * nfwhm
  SRCOR, cat250.ra, cat250.dec, cat500.ra, cat500.dec, dist_250_500,$
         ind_250_500, ind_500_250, MAGNITUDE=cat250.flux,$
         SPHERICAL=2, OPTION=1, /SILENT
  n_250_500 = N_ELEMENTS( ind_250_500 )
  IF n_250_500 NE 0 THEN BEGIN
     status_250[ ind_250_500 ].idx500 = ind_500_250
     status_500[ ind_500_250 ].idx250 = ind_250_500
  ENDIF

  ;;And 350 with 500
  dist_350_500 = SQRT( 24.0^2 + 35.0^2 ) * nfwhm
  SRCOR, cat350.ra, cat350.dec, cat500.ra, cat500.dec, dist_350_500, $
         ind_350_500, ind_500_350, MAGNITUDE=cat350.flux,$
         SPHERICAL=2, OPTION=1, /SILENT
  n_350_500 = N_ELEMENTS( ind_350_500 )
  IF n_350_500 NE 0 THEN BEGIN
     status_350[ ind_350_500 ].idx500 = ind_500_350
     status_500[ ind_500_350 ].idx350 = ind_350_500
  ENDIF  
  DELVARX, ind_250_350, ind_250_250, ind_250_500, ind_500_250
  DELVARX, ind_350_500, ind_500_350

  ;;Ones found in all, index into 250
  w_all = WHERE( status_250.idx350 GE 0 AND status_250.idx500 GE 0,$
                     n_all )

  ;;Now look for things found in 250+350 but not 500
  ;; index into 250
  w_250_350_only = WHERE( status_250.idx350 GE 0 AND status_250.idx500 EQ -1,$
                          n_250_350_only )
     
  ;;250+500, not 350
  ;; index into 250
  w_250_500_only = WHERE( status_250.idx350 EQ -1 AND status_250.idx500 GE 0,$
                          n_250_500_only )

  ;;350+500, not 250
  ;; index into 350
  w_350_500_only = WHERE( status_350.idx250 EQ -1 AND status_350.idx500 GE 0,$
                          n_350_500_only )

  ;;250 only, index into 250, plus 350/500
  w_250_only = WHERE( status_250.idx350 EQ -1 AND status_250.idx500 EQ -1,$
                      n_250_only)
  w_350_only = WHERE( status_350.idx250 EQ -1 AND status_350.idx500 EQ -1,$
                      n_350_only)
  w_500_only = WHERE( status_500.idx250 EQ -1 AND status_500.idx350 EQ -1,$
                      n_500_only)


  ;;Now actually construct output struct
  ntot = n_all + n_250_350_only + n_250_350_only + n_350_500_only + $
         n_250_only + n_350_only + n_500_only
  catsizes=[ N_ELEMENTS(cat250), N_ELEMENTS(cat350), N_ELEMENTS(cat500) ]
  IF ntot LT MAX(catsizes) THEN BEGIN
     errmsg = "Error cross-matching -- total found too small to be possible"
     GOTO, err_handling
  ENDIF
  ;;Define catalog struct
  outarr = REPLICATE( { id: 0L, $
                        x_cen_250: !VALUES.F_NAN, y_cen_250: !VALUES.F_NAN,$
                        dx_cen_250: !VALUES.F_NAN, dy_cen_250: !VALUES.F_NAN,$
                        x_cen_350: !VALUES.F_NAN, y_cen_350: !VALUES.F_NAN,$
                        dx_cen_350: !VALUES.F_NAN, dy_cen_350: !VALUES.F_NAN,$
                        x_cen_500: !VALUES.F_NAN, y_cen_500: !VALUES.F_NAN,$
                        dx_cen_500: !VALUES.F_NAN, dy_cen_500: !VALUES.F_NAN,$
                        ra: !VALUES.D_NAN, dec: !VALUES.D_NAN,$
                        dra: !VALUES.D_NAN, ddec: !VALUES.D_NAN,$
                        glon: !VALUES.D_NAN, dglon: !VALUES.D_NAN,$
                        glat: !VALUES.D_NAN, dglat: !VALUES.D_NAN,$
                        f_250: !VALUES.F_NAN, df_250: !VALUES.F_NAN,$
                        f_350: !VALUES.F_NAN, df_350: !VALUES.F_NAN,$
                        f_500: !VALUES.F_NAN, df_500: !VALUES.F_NAN,$
                        det: BYTARR(3), conf: BYTARR(3), blend: BYTARR(3) },$
                      ntot )

  ;;Start filling i.  There are 7 cases to deal with
  outarr.id = INDGEN(ntot)+1
  ;;First, all 
  idxmin = 0 & idxmax=n_all-1
  outarr[idxmin:idxmax].det[0] = 1b
  outarr[idxmin:idxmax].det[1] = 1b
  outarr[idxmin:idxmax].det[2] = 1b
  cstatus = status_250[w_all]
  ;;Weighted mean to get position.  Assumes uncorrelated, which may
  ;; be a poor assumption, and no band-filling
  FOR i=0,n_all-1 DO BEGIN
     ravar_250 = 0.25*( ABS(cat250[cstatus[i].idx250].rapluserr) + $
                        ABS(cat250[cstatus[i].idx250].raminuserr) )^2
     ravar_350 = 0.25*( ABS(cat350[cstatus[i].idx350].rapluserr) + $
                        ABS(cat350[cstatus[i].idx350].raminuserr) )^2
     ravar_500 = 0.25*( ABS(cat500[cstatus[i].idx500].rapluserr) + $
                        ABS(cat500[cstatus[i].idx500].raminuserr) )^2
     IF ravar_250 EQ 0.0 OR ravar_350 EQ 0.0 OR ravar_500 EQ 0.0 THEN BEGIN
        ;;Straight average
        ra_val = MEAN( [cat250[cstatus[i].idx250].ra,$
                        cat350[cstatus[i].idx350].ra,$
                        cat500[cstatus[i].idx500].ra], /NAN )
        ra_err = 0.0
     ENDIF ELSE BEGIN
        wts = 1.0 / [ ravar_250, ravar_350, ravar_500 ]
        ra_err = SQRT( 1.0 / TOTAL(wts) )
        wts /= TOTAL(wts)
        ra_val = wts[0]*cat250[cstatus[i].idx250].ra + $
                 wts[1]*cat350[cstatus[i].idx350].ra + $
                 wts[2]*cat500[cstatus[i].idx500].ra 
     ENDELSE
     outarr[idxmin+i].ra = ra_val
     outarr[idxmin+i].dra = ra_err

     decvar_250 = 0.25*( ABS(cat250[cstatus[i].idx250].decpluserr) + $
                         ABS(cat250[cstatus[i].idx250].decminuserr) )^2
     decvar_350 = 0.25*( ABS(cat350[cstatus[i].idx350].decpluserr) + $
                         ABS(cat350[cstatus[i].idx350].decminuserr) )^2
     decvar_500 = 0.25*( ABS(cat500[cstatus[i].idx500].decpluserr) + $
                         ABS(cat500[cstatus[i].idx500].decminuserr) )^2
     IF decvar_250 EQ 0.0 OR decvar_350 EQ 0.0 OR decvar_500 EQ 0.0 THEN BEGIN
        ;;Straight average
        dec_val = MEAN( [cat250[cstatus[i].idx250].dec,$
                         cat350[cstatus[i].idx350].dec,$
                         cat500[cstatus[i].idx500].dec], /NAN )
        dec_err = 0.0
     ENDIF ELSE BEGIN
        wts = 1.0 / [ decvar_250, decvar_350, decvar_500 ]
        dec_err = SQRT( 1.0 / TOTAL(wts) )
        wts /= TOTAL(wts)
        dec_val = wts[0]*cat250[cstatus[i].idx250].dec + $
                  wts[1]*cat350[cstatus[i].idx350].dec + $
                  wts[2]*cat500[cstatus[i].idx500].dec 
     ENDELSE
     outarr[idxmin+i].dec = dec_val
     outarr[idxmin+i].ddec = dec_err
  ENDFOR
  outarr[idxmin:idxmax].f_250 = cat250[cstatus.idx250].flux
  outarr[idxmin:idxmax].df_250 = $
     0.5* (cat250[cstatus.idx250].fluxpluserr+ $
           cat250[cstatus.idx250].fluxminuserr )
  outarr[idxmin:idxmax].f_350 = cat350[cstatus.idx350].flux
  outarr[idxmin:idxmax].df_350 = $
     0.5* (cat350[cstatus.idx350].fluxpluserr+ $
           cat350[cstatus.idx350].fluxminuserr )
  outarr[idxmin:idxmax].f_500 = cat500[cstatus.idx500].flux
  outarr[idxmin:idxmax].df_500 = $
     0.5* (cat500[cstatus.idx500].fluxpluserr+ $
           cat500[cstatus.idx500].fluxminuserr )
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Now those found in 250 and 350 but not 500
  idxmin = idxmax+1 & idxmax = idxmin+n_250_350_only-1
  outarr[idxmin:idxmax].det[0] = 1b
  outarr[idxmin:idxmax].det[1] = 1b
  outarr[idxmin:idxmax].det[2] = 0b
  cstatus = status_250[w_250_350_only]
  ;;Weighted mean to get position.  Assumes uncorrelated, which may
  ;; be a poor assumption, and no band-filling
  FOR i=0,n_250_350_only-1 DO BEGIN
     ravar_250 = 0.25*( ABS(cat250[cstatus[i].idx250].rapluserr) + $
                        ABS(cat250[cstatus[i].idx250].raminuserr) )^2
     ravar_350 = 0.25*( ABS(cat350[cstatus[i].idx350].rapluserr) + $
                        ABS(cat350[cstatus[i].idx350].raminuserr) )^2
     IF ravar_250 EQ 0.0 OR ravar_350 EQ 0.0 THEN BEGIN
        ;;Straight average
        ra_val = MEAN( [cat250[cstatus[i].idx250].ra,$
                        cat350[cstatus[i].idx350].ra], /NAN )
        ra_err = 0.0
     ENDIF ELSE BEGIN
        wts = 1.0 / [ ravar_250, ravar_350 ]
        ra_err = SQRT( 1.0 / TOTAL(wts) )
        wts /= TOTAL(wts)
        ra_val = wts[0]*cat250[cstatus[i].idx250].ra + $
                 wts[1]*cat350[cstatus[i].idx350].ra
     ENDELSE

     outarr[idxmin+i].ra = ra_val
     outarr[idxmin+i].dra = ra_err

     decvar_250 = 0.25*( ABS(cat250[cstatus[i].idx250].decpluserr) + $
                         ABS(cat250[cstatus[i].idx250].decminuserr) )^2
     decvar_350 = 0.25*( ABS(cat350[cstatus[i].idx350].decpluserr) + $
                         ABS(cat350[cstatus[i].idx350].decminuserr) )^2
     IF decvar_250 EQ 0.0 OR decvar_350 EQ 0.0 THEN BEGIN
        ;;Straight average
        dec_val = MEAN( [cat250[cstatus[i].idx250].dec,$
                         cat350[cstatus[i].idx350].dec], /NAN )
        dec_err = 0.0
     ENDIF ELSE BEGIN
        wts = 1.0 / [ decvar_250, decvar_350 ]
        dec_err = SQRT( 1.0 / TOTAL(wts) )
        wts /= TOTAL(wts)
        dec_val = wts[0]*cat250[cstatus[i].idx250].dec + $
                  wts[1]*cat350[cstatus[i].idx350].dec
     ENDELSE
     outarr[idxmin+i].dec = dec_val
     outarr[idxmin+i].ddec = dec_err
  ENDFOR
  outarr[idxmin:idxmax].f_250 = cat250[cstatus.idx250].flux
  outarr[idxmin:idxmax].df_250 = $
     0.5* (cat250[cstatus.idx250].fluxpluserr+ $
           cat250[cstatus.idx250].fluxminuserr )
  outarr[idxmin:idxmax].f_350 = cat350[cstatus.idx350].flux
  outarr[idxmin:idxmax].df_350 = $
     0.5* (cat350[cstatus.idx350].fluxpluserr+ $
           cat350[cstatus.idx350].fluxminuserr )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Next 250+500 only
  idxmin = idxmax+1 & idxmax = idxmin+n_250_500_only-1
  outarr[idxmin:idxmax].det[0] = 1b
  outarr[idxmin:idxmax].det[1] = 0b
  outarr[idxmin:idxmax].det[2] = 1b
  cstatus = status_250[w_250_500_only]
  FOR i=0,n_250_500_only-1 DO BEGIN
     ravar_250 = 0.25*( ABS(cat250[cstatus[i].idx250].rapluserr) + $
                        ABS(cat250[cstatus[i].idx250].raminuserr) )^2
     ravar_500 = 0.25*( ABS(cat500[cstatus[i].idx500].rapluserr) + $
                        ABS(cat500[cstatus[i].idx500].raminuserr) )^2
     IF ravar_250 EQ 0.0 OR ravar_500 EQ 0.0 THEN BEGIN
        ;;Straight average
        ra_val = MEAN( [cat250[cstatus[i].idx250].ra,$
                        cat500[cstatus[i].idx500].ra], /NAN )
        ra_err = 0.0
     ENDIF ELSE BEGIN
        wts = 1.0 / [ ravar_250, ravar_500 ]
        ra_err = SQRT( 1.0 / TOTAL(wts) )
        wts /= TOTAL(wts)
        ra_val = wts[0]*cat250[cstatus[i].idx250].ra + $
                 wts[1]*cat500[cstatus[i].idx500].ra
     ENDELSE
     outarr[idxmin+i].ra = ra_val
     outarr[idxmin+i].dra = ra_err

     decvar_250 = 0.25*( ABS(cat250[cstatus[i].idx250].decpluserr) + $
                         ABS(cat250[cstatus[i].idx250].decminuserr) )^2
     decvar_500 = 0.25*( ABS(cat500[cstatus[i].idx500].decpluserr) + $
                         ABS(cat500[cstatus[i].idx500].decminuserr) )^2
     IF decvar_250 EQ 0.0 OR decvar_500 EQ 0.0 THEN BEGIN
        ;;Straight average
        dec_val = MEAN( [cat250[cstatus[i].idx250].dec,$
                         cat500[cstatus[i].idx500].dec], /NAN )
        dec_err = 0.0
     ENDIF ELSE BEGIN
        wts = 1.0 / [ decvar_250, decvar_500 ]
        dec_err = SQRT( 1.0 / TOTAL(wts) )
        wts /= TOTAL(wts)
        dec_val = wts[0]*cat250[cstatus[i].idx250].dec + $
                  wts[1]*cat500[cstatus[i].idx500].dec
     ENDELSE
     outarr[idxmin+i].dec = dec_val
     outarr[idxmin+i].ddec = dec_err
  ENDFOR
  outarr[idxmin:idxmax].f_250 = cat250[cstatus.idx250].flux
  outarr[idxmin:idxmax].df_250 = $
     0.5* (cat250[cstatus.idx250].fluxpluserr+ $
           cat250[cstatus.idx250].fluxminuserr )
  outarr[idxmin:idxmax].f_500 = cat500[cstatus.idx500].flux
  outarr[idxmin:idxmax].df_500 = $
     0.5* (cat500[cstatus.idx500].fluxpluserr+ $
           cat500[cstatus.idx500].fluxminuserr )
  

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Next 350+500 only
  idxmin = idxmax+1 & idxmax = idxmin+n_350_500_only-1
  outarr[idxmin:idxmax].det[0] = 0b
  outarr[idxmin:idxmax].det[1] = 1b
  outarr[idxmin:idxmax].det[2] = 1b
  cstatus = status_350[w_350_500_only]
  FOR i=0,n_350_500_only-1 DO BEGIN
     ravar_350 = 0.25*( ABS(cat350[cstatus[i].idx350].rapluserr) + $
                        ABS(cat350[cstatus[i].idx350].raminuserr) )^2
     ravar_500 = 0.25*( ABS(cat500[cstatus[i].idx500].rapluserr) + $
                        ABS(cat500[cstatus[i].idx500].raminuserr) )^2
     IF ravar_350 EQ 0.0 OR ravar_500 EQ 0.0 THEN BEGIN
        ;;Straight average
        ra_val = MEAN( [cat350[cstatus[i].idx350].ra,$
                        cat500[cstatus[i].idx500].ra], /NAN )
        ra_err = 0.0
     ENDIF ELSE BEGIN
        wts = 1.0 / [ ravar_350, ravar_500 ]
        ra_err = SQRT( 1.0 / TOTAL(wts) )
        wts /= TOTAL(wts)
        ra_val = wts[0]*cat350[cstatus[i].idx350].ra + $
                 wts[1]*cat500[cstatus[i].idx500].ra 
     ENDELSE
     outarr[idxmin+i].ra = ra_val
     outarr[idxmin+i].dra = ra_err

     decvar_350 = 0.25*( ABS(cat350[cstatus[i].idx350].decpluserr) + $
                         ABS(cat350[cstatus[i].idx350].decminuserr) )^2
     decvar_500 = 0.25*( ABS(cat500[cstatus[i].idx500].decpluserr) + $
                         ABS(cat500[cstatus[i].idx500].decminuserr) )^2
     IF decvar_350 EQ 0.0 OR decvar_500 EQ 0.0 THEN BEGIN
        ;;Straight average
        dec_val = MEAN( [cat350[cstatus[i].idx350].dec,$
                         cat500[cstatus[i].idx500].dec], /NAN )
        dec_err = 0.0
     ENDIF ELSE BEGIN
        wts = 1.0 / [ decvar_350, decvar_500 ]
        dec_err = SQRT( 1.0 / TOTAL(wts) )
        wts /= TOTAL(wts)
        dec_val = wts[0]*cat350[cstatus[i].idx350].dec + $
                  wts[1]*cat500[cstatus[i].idx500].dec
     ENDELSE
     outarr[idxmin+i].dec = dec_val
     outarr[idxmin+i].ddec = dec_err
  ENDFOR
  outarr[idxmin:idxmax].f_350 = cat350[cstatus.idx350].flux
  outarr[idxmin:idxmax].df_350 = $
     0.5* (cat350[cstatus.idx350].fluxpluserr+ $
           cat350[cstatus.idx350].fluxminuserr )
  outarr[idxmin:idxmax].f_500 = cat500[cstatus.idx500].flux
  outarr[idxmin:idxmax].df_500 = $
     0.5* (cat500[cstatus.idx500].fluxpluserr+ $
           cat500[cstatus.idx500].fluxminuserr )
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;250 only
  idxmin = idxmax+1 & idxmax = idxmin+n_250_only-1
  outarr[idxmin:idxmax].det[0] = 1b
  outarr[idxmin:idxmax].det[1] = 0b
  outarr[idxmin:idxmax].det[2] = 0b
  cstatus = status_250[w_250_only]
  outarr[idxmin:idxmax].ra = cat250[cstatus.idx250].ra
  outarr[idxmin:idxmax].dra = $
     0.5*(cat250[cstatus.idx250].rapluserr + cat250[cstatus.idx250].raminuserr)
  outarr[idxmin:idxmax].dec = cat250[cstatus.idx250].dec
  outarr[idxmin:idxmax].ddec = $
     0.5*(cat250[cstatus.idx250].decpluserr + $
          cat250[cstatus.idx250].decminuserr)
  outarr[idxmin:idxmax].f_250 = cat250[cstatus.idx250].flux
  outarr[idxmin:idxmax].df_250 = $
     0.5* (cat250[cstatus.idx250].fluxpluserr+ $
           cat250[cstatus.idx250].fluxminuserr )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;350 only
  idxmin = idxmax+1 & idxmax = idxmin+n_350_only-1
  outarr[idxmin:idxmax].det[0] = 0b
  outarr[idxmin:idxmax].det[1] = 1b
  outarr[idxmin:idxmax].det[2] = 0b
  cstatus = status_350[w_350_only]
  outarr[idxmin:idxmax].ra = cat350[cstatus.idx350].ra
  outarr[idxmin:idxmax].dra = $
     0.5*(cat350[cstatus.idx350].rapluserr + cat350[cstatus.idx350].raminuserr)
  outarr[idxmin:idxmax].dec = cat350[cstatus.idx350].dec
  outarr[idxmin:idxmax].ddec = $
     0.5*(cat350[cstatus.idx350].decpluserr + $
          cat350[cstatus.idx350].decminuserr)
  outarr[idxmin:idxmax].f_350 = cat350[cstatus.idx350].flux
  outarr[idxmin:idxmax].df_350 = $
     0.5* (cat350[cstatus.idx350].fluxpluserr+ $
           cat350[cstatus.idx350].fluxminuserr )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;500 only
  idxmin = idxmax+1 & idxmax = idxmin+n_500_only-1
  outarr[idxmin:idxmax].det[0] = 0b
  outarr[idxmin:idxmax].det[1] = 0b
  outarr[idxmin:idxmax].det[2] = 1b
  cstatus = status_500[w_500_only]
  outarr[idxmin:idxmax].ra = cat500[cstatus.idx500].ra
  outarr[idxmin:idxmax].dra = $
     0.5*(cat500[cstatus.idx500].rapluserr + cat500[cstatus.idx500].raminuserr)
  outarr[idxmin:idxmax].dec = cat500[cstatus.idx500].dec
  outarr[idxmin:idxmax].ddec = $
     0.5*(cat500[cstatus.idx500].decpluserr + $
          cat500[cstatus.idx500].decminuserr)
  outarr[idxmin:idxmax].f_500 = cat500[cstatus.idx500].flux
  outarr[idxmin:idxmax].df_500 = $
     0.5* (cat500[cstatus.idx500].fluxpluserr+ $
           cat500[cstatus.idx500].fluxminuserr )

  ;;Make basic primary header
  MKHDR, base_hdr, '', /EXTEND
  SXADDPAR,base_hdr,'COMMENT',$
           'This is a header for an SMAP pipeline product.'
  SXADDPAR,base_hdr,'TIMESYS','UTC     ','All dates are in UTC time'
  SXADDPAR,base_hdr,'CREATOR',getenv('USER'),'User who created this file'
  SXADDPAR,base_hdr,'TELESCOP','Herschel','Name of the telescope'
  SXADDPAR,base_hdr,'INSTRUME','SPIRE','Name of the instrument'
  SXADDPAR,base_hdr,'DESC','Catalog','Description of file contents'
  SXADDPAR,base_hdr,'FILE250',cat250file,'250 micron input catalog'
  SXADDPAR,base_hdr,'FILE350',cat350file,'350 micron input catalog'
  SXADDPAR,base_hdr,'FILE500',cat500file,'500 micron input catalog'
  SXADDPAR,base_hdr,'HISTORY','Merged from SCAT catalogs by '+$
           'convert_scat_catalog'
  SXADDPAR, base_hdr, 'HISTORY', ' on '+systime()

  ;;Write it
  IF FILE_TEST( outfile ) THEN FILE_DELETE,outfile
  WRITEFITS, outfile, 0, base_hdr, /CHECKSUM

  ;;Append catalog
  FXBHMAKE,cat_hdr, N_ELEMENTS(outarr), 'catalog', 'Object Catalog'
  MWRFITS, outarr, outfile, cat_hdr, STATUS=status, /SILENT
  IF status NE 0 THEN BEGIN
     errmsg = "Error writing catalog file: "+outfile
     GOTO, err_handling
  ENDIF

  success = 1b
  RETURN

  err_handling :
  IF KEYWORD_SET(verbose) THEN $
     MESSAGE,errmsg,/INF
  RETURN

END
