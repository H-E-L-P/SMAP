;+
;Name
;   SMAP_WFILTER
;PURPOSE
;   To wiener filter the timestreams
;USAGE
;   smap_wfilter, tod
;INPUTS/OUTPUTS
;   tod         The SMAP tod structure
;RETURNS
;   The weiner filtered timestream
;KEYWORDS
;   verbose     Output informational messages
;   meanrestore Make the mean before and after filtering the same
;OPTIONAL OUTPUTS
;   success     1b on success, 0b on failure
;   errmsg      On failure, some explanation of what happened
;NOTES
;   This is currently hardwired to 30"/sec scanning speed.
;MODIFICATION HISTORY
;   Author: Naseem Rangwala, 2009
;-
PRO SMAP_WFILTER, tod, MEANRESTORE=meanrestore,$
                  VERBOSE=verbose, SUCCESS=success, ERRMSG=errmsg
  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg  = ''
  
  IF SIZE(tod,/TNAME) NE 'STRUCT' THEN BEGIN
     errmsg = "Input is not a structure, as expected"
     RETURN
  ENDIF

  scanspeed = 30.0 ;;Need to find way to handle other speeds
  sampstep = 1.0/tod.sampfreq ;;s

  ;;--------------------------------------------
  ;;defining the frequency grid we will work on
  ;;--------------------------------------------
  fgrid = tod.sampfreq*DINDGEN(tod.nsamps/2+1)/tod.nsamps
  ffull = 0.5*tod.sampfreq*DINDGEN(tod.nsamps+1)/tod.nsamps

  ;;Main loop
  bands = ['PSW','PMW','PLW']
  FOR i=0, N_ELEMENTS(bands)-1 DO BEGIN
     wbol = WHERE(STRUPCASE(STRMID(tod.chan,0,3)) EQ bands[i], nbol )
     IF nbol EQ 0 THEN BEGIN
        IF KEYWORD_SET(verbose) THEN $
           MESSAGE,"No bolometers found in band: "+bands[i]+" skipping",/INF
        CONTINUE
     ENDIF ELSE IF KEYWORD_SET(verbose) THEN $
        MESSAGE,"Doing Weiner filter for band: "+bands[i],/INF

     beamFWHM = get_spire_beam_fwhm(bands[i])

     ;;Get a fourier transform of the beam
     gaussx = DINDGEN(tod.nsamps)*sampstep
     sig2fwhm = 2.354820045d0  ;;Conversion from sigma to FWHM: SQRT*ALOG(2)
     params = [1.0,max(gaussx)/2.0,beamFWHM/(scanspeed*sig2fwhm)]
     gauss = GAUSSIAN(TEMPORARY(gaussx),params)
     fg = FFT(gauss,/DOUBLE)

     ;;--------------------------------------------------
     ;;Check for dead bolometers
     ;;--------------------------------------------------
     wgood = WHERE(FINITE(tod.signal[wbol,*]), ngood ,COMPLEMENT=wbad,$
                   NCOMPLEMENT=nbad)
     IF ngood EQ 0 THEN BEGIN
        IF KEYWORD_SET(verbose) THEN $
           MESSAGE,"No good bolomters in band "+bands[i]+" -- skipping",/INF
        CONTINUE
     ENDIF
     ;;Get column numbers of good bols in this band
     wgoodbols = wbol[wgood[0:ngood/tod.nsamps-1]] 
     IF nbad NE 0 AND KEYWORD_SET( verbose ) THEN BEGIN
        wbadbols = wbad[0:nbad/tod.nsamps-1] ;;And bad
        MESSAGE,STRING(N_ELEMENTS(badbols),$
                       FORMAT='("Number of bad bols: ",I0,)'),/INF
     ENDIF

     ;;Decide chunk size for PSDs
     npower = ROUND( ALOG( tod.nsamps/8 ) / ALOG(2) )
     nchunk = 2^npower
     sz     = nchunk/2 + 1

     ;;Main loop
     ngb = N_ELEMENTS(wgoodbols)
     FOR j=0,ngb-1 DO BEGIN
        sig = REFORM( tod.signal[wgoodbols[j],*] )

        IF KEYWORD_SET( meanrestore ) THEN $
           mnval = MEAN(sig)

        ;;Get PSD
        curr_psd = AUTO_CORRELATE( sig, nchunk, FREQ=freq,$
                                   SUCCESS=asuccess, $
                                   ERRMSG=errmsg )
        IF asuccess EQ 0 THEN BEGIN
           errmsg = "Error getting auto_spectrum in smap_Wfilter "+$
                    "for "+tod.chan[wgoodbols[i]] + " from "+$
                    tod.progenitorfile + ": "+errmsg
           RETURN
        ENDIF
        curr_psd /= tod.sampfreq
        freq *= tod.sampfreq

        ;;Interpolate it to frequency grid we want
        interp_psd = INTERPOL( TEMPORARY(curr_psd), freq, fgrid )

        ;;add back the negative frequencies
        psdfull = [ interp_psd, REVERSE( interp_psd ) ]

        ;;Make the Weiner filter
        wfl = ABS(fg)/TEMPORARY(psdfull)

        ;;Get the FFT of the signal
        fsig = FFT( TEMPORARY(sig), /DOUBLE )

        ;;Apply the weiner filter and reverse transform to get
        ;;filtered signal
        newsig = FFT( fsig * wfl, /INVERSE, /DOUBLE )

        IF KEYWORD_SET( meanrestore ) THEN $
           newsig += mnval - MEAN(newsig)

        tod.signal[wgoodbols[j],*] = TEMPORARY(newsig)
     ENDFOR ;;Loop over bols
  ENDFOR ;;Loop over bands

  success = 1b
  RETURN
END
