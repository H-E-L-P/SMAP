;+
;Name
;   SMAP_PCA
;PURPOSE
;   To remove the correlated 1/f noise from the timestreams using
;principle component analysis
;USAGE
;   smap_pca, input, mapparam
;INPUTS
;   input          Either a file (which much be .fits file
;                   written in smap TOD output format) or the TOD structure.
;   mapparam       Information about the maps.
;RETURN
;   A SMAP TOD with the PCA removal.
;KEYWORDS
;   nlow - lower limit of the eigenfunction to be removed (def: 0)
;   nhigh - upper limit of the eigenfunction to be removed (def: 0)
;   nosub - set this keyword to not subtract any eigenfunctions
;EXAMPLE
;   SMAP_PCA,'file.fits',nlow=0,nhigh=1 
;   This will subtract the first 2 eigenfunctions 
;   SMAP_PCA,'file.fits',/nosub; will not subtract any eigenfunctions
;NOTES
;   Note that the channels in the returned TOD are not necessarily in 
;   the same order as the input one.
;MODIFICATION HISTORY
;   Author: Naseem Rangwala, 2009
;-
FUNCTION SMAP_PCA, input, mapparam, NLOW = nlow, NHIGH = nhigh, NOSUB=nosub,$
                   SUCCESS=success, ERRMSG=errmsg, VERBOSE=verbose

  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg  = ''

  ;;----------------------------------------------------------------------
  ;;Reading the input timestreams
  ;;----------------------------------------------------------------------

  IF SIZE(input,/TNAME) EQ 'STRUCT' THEN BEGIN
     infile = input
  ENDIF ELSE IF SIZE(input,/TNAME) EQ 'STRING' THEN BEGIN
     infile = smap_readtod( input, SUCCESS=rd_success, ERRMSG=errmsg )
     IF rd_success NE 0 THEN BEGIN
        errmsg = "Unable to read "+input+" in SMAP_PCA"
        GOTO, err_handler
     ENDIF
  ENDIF ELSE BEGIN
     errmsg = "Unexptected type for first argument in SMAP_PCA"
     GOTO, err_handler
  ENDELSE

  tbols = infile.nchans
  band_tags = ['PSW','PMW','PLW']
  band3 = STRUPCASE( STRMID( infile.chan, 0, 3 ) )
  wPSW = WHERE( band3 EQ band_tags[0], nbPSW )
  wPMW = WHERE( band3 EQ band_tags[1], nbPMW )
  wPLW = WHERE( band3 EQ band_tags[2], nbPLW )
  nbols=[nbpsw,nbpmw,nbplw]
  nsamp = infile.nsamps

  signalPSW = DBLARR(nbols[0],nsamp)
  signalPMW = DBLARR(nbols[1],nsamp)
  signalPLW = DBLARR(nbols[2],nsamp)


  ;;-----------------------------------------------------------------------
  ;;Sorting the input timestreams using the name of the bolometers in
  ;;  CHAN. 
  ;;-----------------------------------------------------------------------
  ;;This doesn't seem to be necessary, so it's commented out for now.
  ;bname = SORT(infile.chan)
  ;infile.CHAN = infile.CHAN[bname]
  ;infile.SIGNAL = infile.SIGNAL[bname,*]
  ;infile.RA = infile.RA[bname,*]
  ;infile.DEC = infile.DEC[bname,*]
  ;infile.MASK = infile.MASK[bname,*]

  ;;---------------------------------------------------------------------
  ;;Separating the SIGNAL into 3 bands
  ;;---------------------------------------------------------------------
  FOR band = 0, n_elements(band_tags)-1 DO BEGIN
     band_tag = band_tags[band]
     array = WHERE(STRMID(infile.CHAN,0,3) EQ band_tag)
     sn = infile.SIGNAL[array,*]
     numbols = nbols[band]

     ;;--------------------------------------------------------------------
     ;; Do masking
     ;; Right now, only removing dead channels is supported
     ;; If the input TOD has mask info, use that, if it doesn't
     ;; just look for channels with NaNs; the latter relates
     ;; to dealing with SPS data.
     ;;--------------------------------------------------------------------
     IF TAG_EXIST( infile, 'mask_bits', /TOP_LEVEL ) THEN BEGIN
        dead_bitmask = $
           construct_mask_bitmask( 'maskDead', infile.mask_bits )
        IF dead_bitmask NE 0 THEN BEGIN
           ;;This will be one everywhere we are either non-finite or
           ;; have the bad mask bit set
           bad_samps = (infile.mask[array,*] AND dead_bitmask) OR $
                       (~FINITE( sn ))
           good_chans = WHERE( TOTAL(bad_samps,2) EQ 0, n_good_chans,$
                               COMPLEMENT=bad_chans,NCOMPLEMENT=n_bad_chans )
           IF n_good_chans EQ 0 THEN MESSAGE,"All channels masked!"
        ENDIF ELSE BEGIN
           good_chans = INDGEN(numbols)
           n_good_chans = numbols
           n_bad_chans = 0
        ENDELSE
     ENDIF ELSE BEGIN
        ;;Now just do finiteness test
        bad_samps = ~FINITE( sn )
        maskval = TOTAL( bad_samps, 2 ) EQ 0 
        good_chans = WHERE( TOTAL(bad_samps,2) EQ 0, n_good_chans,$
                            COMPLEMENT=bad_chans,NCOMPLEMENT=n_bad_chans )
        IF n_good_chans EQ 0 THEN MESSAGE,"All channels masked!"
     ENDELSE

     IF KEYWORD_SET( verbose ) THEN $
        MESSAGE,STRING(n_bad_chans,numbols,$
                       FORMAT='(" ",I0," of ",I0, " dead bolometers")'),/INF

     ;;-----------------------------------------------------------------
     ;; Subtracting the Means
     ;;-----------------------------------------------------------------

     snsub = sn[good_chans,*]
     szsub = SIZE(snsub)
     means = TOTAL(snsub,2,/NAN) / szsub[szsub[0]]
     snsub -= means#REPLICATE(1.0d0,szsub[szsub[0]])

     ;;----------------------------------------------------------------------
     ;;Covariance Matrix
     ;;----------------------------------------------------------------------
     st = 1.0d0/(nsamp-1.0)
     cx = st*(snsub # TRANSPOSE(snsub))

     ;;--------------------------------------------------------------
     ;; Solving the eigenvalue problem; PCs are the eigenvectors, 
     ;; dimensions = # of bolometers
     ;;--------------------------------------------------------------
     ;;Should switch to LA_EIGENQL for speed
     eigenvalues = EIGENQL(cx, EIGENVECTORS=evec, /DOUBLE)

     ;;--------------------------------------------------------------
     ;; Eigenfunction: amplitudes of the eigenvectors: projection of 
     ;; data on the PCs
     ;;--------------------------------------------------------------

     amps = DBLARR(n_good_chans,nsamp)
     amps = TEMPORARY(snsub) ## TRANSPOSE(evec)

     ;;--------------------------------------------------------------
     ;;Setting the eigenfunctions to zero
     ;;--------------------------------------------------------------
     IF KEYWORD_SET(nosub) then pcasub = 0b else pcasub = 1b

     IF N_ELEMENTS(nlow) EQ 0 THEN nlow = 0
     IF N_ELEMENTS(nhigh) EQ 0 THEN nhigh = 0
     IF pcasub THEN amps[nlow:nhigh,*]=0. 

     ;;--------------------------------------------------------------
     ;;Reconstructing the signal
     ;;--------------------------------------------------------------

     recondata = DBLARR(nbols[band],nsamp)
     
     recon = DBLARR(n_good_chans,nsamp)
    
     ntotal = total(mapparam.scanlength)
     means *= double(nsamp)/ntotal

     ;;Could we do this without a for loop?
     FOR j = 0,n_good_chans-1 DO recon[j,*] = evec[j,*]#amps + means[j]

     recondata[good_chans,*] = TEMPORARY(recon)

     IF n_bad_chans NE 0 then recondata[bad_chans,*] = !VALUES.F_NAN

     infile.signal[array,*] = TEMPORARY(recondata)
  ENDFOR

  ;;Good return
  success = 1b
  RETURN, infile
  
  ;;Problem
  err_handler:
  IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
  RETURN,!VALUES.F_NAN
end

