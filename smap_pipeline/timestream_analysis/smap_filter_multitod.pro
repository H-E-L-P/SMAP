;+
;NAME
; smap_filter_multitod
;CATEGORY
; SMAP pipeline
;PURPOSE
; To apply a timestream filter to a set of TODS by stitching them
; together in time, applying the filter, and then ripping them
; back apart
;USAGE
; smap_filter_multitod, tods [, SUCCESS=success, ERRMSG=errmsg ]
;INPUTS/OUTPUTS
; tods       A pointer array of SMAP style TODS -- see smap_read_and_filter.
;             The data is replaced on output by the filtered data
;NOTES
; Should add flexibility for multiple filters near the end of the 
; "Main Loop"; right now it's only a butterworth filter. 
;
;MODIFICATION HISTORY
; Initial Author: Alex Conley, March 2010
; Modified:       Timothy Ellsworth Bowers, March 2010
;                 Mike Zemcov, Aug 2010
;-
PRO smap_filter_multitod, tods, VERBOSE=verbose, $
                          SUCCESS=success, ERRMSG=errmsg

  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg  = ''

  ;; Gaps less than 1 hour will be filled with white noise & a linear
  ;; baseline from the mean of one scan to the next
  ;; PROBABLY NEED TO THINK HARDER ABOUT THIS LIMIT...
  max_gap_allowed = 3600. 
    
  ntods = N_ELEMENTS(tods)
  IF ntods EQ 0 THEN BEGIN
     errmsg = "No TODs provided!"
     GOTO, err_handler
  ENDIF
  
  message,string(ntods,format="('Number of TODS provided: ',I0)"),/inf
  
  IF SIZE( tods, /TNAME ) NE 'POINTER' THEN BEGIN
     errmsg = "Was expecting an array of pointers to TODs, got: "+$
              SIZE(tods,/TNAME)
     GOTO, err_handler
  ENDIF
  
  ;; Collect some basic information to get started
  startsamp    = LONARR( ntods )
  endsamp      = LONARR( ntods )
  gapsamp      = LONARR( ntods )
  totsamps     = 0
  nbolos       = (*tods[0]).nchans
  nsamps       = (*tods[0]).nsamps
  IF nsamps EQ 0 THEN BEGIN
     errmsg = (*tods[0]).shortfile+" has no data!"
     GOTO,err_handler
  ENDIF
  sampstep = 1. / (*tods[0]).sampfreq ; in seconds
  scanrate = determine_scan_rate(tods,/VERBOSE,SUCCESS=success, ERRMSG=errmsg)
  IF KEYWORD_SET( verbose ) THEN $
     message,string(scanrate,format=$
                    "('Scan rate = ',F6.3,' arcsec / second')"),/inf
  
  
  ;; Now, figure out where each TODs data will go in the combined
  ;;  bolometer array
  startsamp[0] = 0
  endsamp[0]   = nsamps-1
  FOR i=1, ntods-1 DO BEGIN
     IF (*tods[i]).nchans NE nbolos THEN BEGIN
        errmsg = "TOD from "+(*tods[i]).shortfile+" has unexpected number "+$
                 " of bolometers: "+STRING( (*tods[i]).nchans, FORMAT='(I0)')+$
                 " instead of "+STRING(nbolos,FORMAT='(I0)')
        GOTO, err_handler
     ENDIF
     nsamps = (*tods[i]).nsamps
     IF nsamps EQ 0 THEN BEGIN
        errmsg = (*tods[i]).shortfile+" has no data!"
        GOTO,err_handler
     ENDIF
     startsamp[i] = endsamp[i-1]+1
     endsamp[i]   = startsamp[i] + nsamps - 1
  ENDFOR
  IF KEYWORD_SET(verbose) THEN $
     MESSAGE,STRING(endsamp[ntods-1]+1,$
                    FORMAT='("Total number of samples: ",I0)'),/INF
  
  ;; Create a test timestream for the purposes of determining gaps
  test_timestream = DBLARR( endsamp[ntods-1]+1 )

  ;;Check to see if the combined timestream has big gaps in it
  ;; use test_timestream to hold sample time
  FOR j=0,ntods-1 DO BEGIN
     test_timestream[startsamp[j]:endsamp[j]]=(*tods[j]).samptime
  ENDFOR
  gaps = test_timestream[1:*] - test_timestream[0:*]
  maxgap = MAX( gaps, /NAN )
  IF maxgap GT max_gap_allowed THEN BEGIN
     errmsg = "Unacceptably large gap in TOD for filtering " + $
              STRING(maxgap,FORMAT='(F0.3)')+" seconds"
     GOTO,err_handler
  ENDIF
  IF KEYWORD_SET( verbose ) THEN $
     message,string(maxgap,format="('Maximum gap is ',F0.3,' seconds')"),/inf
  
  ;; Look at the gaps and determine where they are & how to fill them
  ;; gaps == spacing larger than 2x sample step
  gapind = WHERE( gaps GE 2.d * sampstep, ngaps )
  IF KEYWORD_SET( verbose ) THEN $
     message,string(ngaps,format="('Number of gaps: ',I0)"),/inf
  
  gap_samples = ROUND( gaps / sampstep )
  
  ;; Build ubertimestream with gap data  -- already checked for errors
  test_endsamp = endsamp
  nsamps = (*tods[0]).nsamps
  startsamp[0] = 0
  endsamp[0]   = nsamps-1
  FOR i=1, ntods-1 DO BEGIN
     
     nsamps = (*tods[i]).nsamps
     
     junk = WHERE( gapind EQ test_endsamp[i-1], nyes )
     
     IF( nyes EQ 1 ) THEN BEGIN
        IF KEYWORD_SET( verbose ) THEN $
           message,string(i+1,gap_samples[test_endsamp[i-1]],format=$
                          "('In gap before TOD #',I3,', ',I6,' samples to be added.')"),/inf
        
        ;; Where gapsamp is the number of filler samples to be placed
        ;; BEFORE the associated [j] timestream
        gapsamp[i] = gap_samples[test_endsamp[i-1]]
     ENDIF
     startsamp[i] = endsamp[i-1]+1
     endsamp[i]   = startsamp[i] + nsamps - 1 + gapsamp[i]
  ENDFOR
  IF KEYWORD_SET(verbose) THEN $
     MESSAGE,STRING(endsamp[ntods-1]+1,$
                    FORMAT='("Gap-filled total number of samples: ",I0)'),/INF

    
  ;;Main loop for filtering
  FOR i=0,nbolos-1 DO BEGIN

     ubertimestream = DBLARR( endsamp[ntods-1]+1 )

     ;;Build the combined timestream for this bolometer
     bol = (*tods[0]).chan[i]
     
     IF KEYWORD_SET( verbose ) THEN $
        message,string(i+1,nbolos,(*tods[0]).chan[i],format=$
                       "('Filtering bolometer ',I3,' of ',I0,': ',A0)"),/inf
             
        ubertimestream[ startsamp[0]:endsamp[0] ] = (*tods[0]).signal[i,*]
     
        FOR j=0,ntods-1 DO BEGIN
           ;;Find the bolometer, -not- assuming they are in the same order
           wbolo = WHERE( (*tods[j]).chan EQ bol, nbolo )
           IF nbolo NE 1 THEN BEGIN
              errmsg = "Couldn't find "+bol+" bolometer in timestream for "+$
                       (*tods[j]).shortfile
              GOTO, err_handler
           ENDIF
           
           ;; Place the scan data in the ubertimestream
           ubertimestream[startsamp[j]+gapsamp[j]:endsamp[j]] = $
              (*tods[j]).signal[i,*]
           
           ;; Figure out how to add linear (w/ noise) continuous filler data in
           ;; between scans.
           IF( gapsamp[j] GT 0 ) THEN BEGIN
              
              ;; Determine the noise level to add -- first, get band
              band = STRMID( (*tods[j]).chan[i], 0, 3)
              IF (band EQ 'PSW') THEN n_level = 38.23d-3 $
              ELSE IF (band EQ 'PMW') THEN n_level = 37.86d-3 $
              ELSE IF (band EQ 'PLW') THEN n_level = 52.87d-3 $
              ELSE n_level = !VALUES.F_NAN   ;; this should never happen

              ; this is for channels which are still calibrated in V
              IF STRMID( (*tods[j]).chan[i], 3, 1) EQ 'T' OR $
                 STRMID( (*tods[j]).chan[i], 3, 2) EQ 'DP' THEN BEGIN
                 ;; WARNING: this number is a hack based on Darren's 
                 ;; best guess.  What really needs to happen is we 
                 ;; measure the noise properies of the dark and 
                 ;; thermistor channels and actually put that in 
                 ;; (homework for another day).
                 n_level = n_level / 800000. 
              ENDIF
              
              ;; Get the mean levels for both ends of the gap-filling data
              mean_lo = MEAN( (*tods[j-1]).signal[i,*], /NAN )  ;; Pervious TOD
              mean_up = MEAN( (*tods[j]).signal[i,*], /NAN )     ;; This TOD
              
              ;; Generate gap-filling data
              fsamps  = INDGEN( gapsamp[j] )
              slope   = (mean_up - mean_lo) / double(gapsamp[j])
              fill_noise = $
                 RANDOMN( seed, gapsamp[j], /DOUBLE, /NORMAL) * n_level
              filler  = double(slope * fsamps + mean_lo) + $
                        TEMPORARY( fill_noise )
              
              ;; Add filler data to the ubertimestream BEFORE the scan in
              ;; question
              ubertimestream[startsamp[j]:startsamp[j]+gapsamp[j]-1] = $
                 TEMPORARY( filler )
              
           ENDIF
        
        ENDFOR
     
        ;;Look for bad data, do something about it
        ;;Only non-finiteness currently implemented
        wbad = WHERE( ~ FINITE(ubertimestream), nbad, NCOMPLEMENT=ngood )
        IF ngood EQ 0 THEN BEGIN
           IF KEYWORD_SET( verbose ) THEN $
              MESSAGE,"No good samples for bol: "+bol+" -- skipping",/INF
           CONTINUE
        ENDIF
        IF nbad NE 0 THEN BEGIN
           ;;This could be a lot more sophisticated, I'm sure
           ;;It would be nice to use some sort of local mean
           ;; but we then have to be careful of gaps of a few
           ;; bad values in a row
           mn  = MEAN( ubertimestream, /NAN )
           sig = STDDEV( ubertimestream, /NAN )
           badvals = ubertimestream[wbad]
           ;ubertimestream[wbad] = mn + sig*RANDOMN(seed,nbad )
        ENDIF

        ;;Apply the filter to ubertimestream
        ;; CALL YOUR FAVORITE FILTER HERE
        ;; (i.e. smap_cirrusfilter,ubertimestream, scanrate, sampstep, $
        ;;                  /VERBOSE, SUCCESS=success, ERRMSG=errmsg)
        smap_lpfilter,ubertimestream,scanrate,sampstep,ANGSCALE=60.d,$
                      /VERBOSE,SUCCESS=success,ERRMSG=errmsg

        ;;Restore any bad values to ubertimestream, whatever that
        IF nbad NE 0 THEN $
           ubertimestream[TEMPORARY(wbad)] = TEMPORARY(badvals)
     
        ;;Now unpack back into original tods
        FOR j=0, ntods-1 DO BEGIN
           wbolo = WHERE( (*tods[j]).chan EQ bol ) ;;Previously checked
           (*tods[j]).signal[i,*] = $
              ubertimestream[startsamp[j]+gapsamp[j]:endsamp[j]]
        ENDFOR

        DELVARX,ubertimestream

     ENDIF

  ENDFOR

  success = 1b
  RETURN
  
  err_handler:
  IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
  RETURN
END

