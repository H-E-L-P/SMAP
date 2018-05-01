;+
;NAME
; delta_coverage
;PURPOSE
; To determine the (statistical) coverage of a 1D P(D) delta function
; model by mapping out the likelihood.
;USAGE
;  retinfo = delta_coverage(n0, s0, pixsize, area, fwhm, nsims,
;                           nlike, rangefrac [, SIGMA=] )
;INPUTS
;  n0           Number of sources per sq deg in model
;  s0           Flux of sources
;  pixsize      Size of final pixels, in arcsec
;  area         Area of generated image, in sq deg
;  fwhm         FWHM of Gaussian beam, in arcsec
;  nsims        Number of simulations to perform
;  nlike        Number of likelihood values to compute
;  rangefrac    Likelihood computation covers n0 +- n0*rangefrac
;RETURNS
;  A structure holding the likelihood for each simulation
;OPTIONAL INPUTS
;  sigma        White noise to add (def: 0)
;  npd          Number of elements in P(D) (def: 65536)
;  seed         Random number generator seed
;KEYWORDS
;  verbose      Output informational messages
;MODIFICATION HISTORY
; Author: Alex Conley, Jan 2012
;-

FUNCTION delta_coverage, n0, s0, pixsize, area, fwhm, nsims, nlike,$
                         rangefrac, SIGMA=sigma, SEED=seed, $
                         VERBOSE=verbose, NPD=npd
  COMPILE_OPT IDL2, HIDDEN

  ;;defaults
  IF N_ELEMENTS( sigma ) EQ 0 THEN sigma = 0.0
  IF N_ELEMENTS( npd ) EQ 0 THEN npd = 65536

  ;;input checks
  IF n0 LE 0 THEN MESSAGE,"Invalid (non-positive) n0"
  IF s0 LE 0 THEN MESSAGE,"Invalid (non-positive) s0"
  IF pixsize LE 0 THEN MESSAGE,"Invalid (non-positive) pixsize"
  IF area LE 0 THEN MESSAGE,"Invalid (non-positive) area"
  IF fwhm LE 0 THEN MESSAGE,"Invalid (non-positive) fwhm"
  IF fwhm LT 3*pixsize THEN MESSAGE,"FWHM too small relative to pixel size"
  IF sigma LE 0 THEN MESSAGE,"Invalid sigma"
  IF npd LE 0 THEN MESSAGE,"Invalid (non-positive) npd"

  ;;return structure
  maxn0 = n0 * (1.0+rangefrac)
  minn0 = n0 * (1.0-rangefrac)
  n0vals = (maxn0-minn0)*FINDGEN(nlike)/(nlike-1.0) + minn0
  beamarea = TOTAL(get_spire_beam('PSW',pixsize,FWHM=fwhm,/SILENT))

  retstruct = REPLICATE( { n0: n0, s0: s0, fwhm: fwhm, sigma: sigma, $
                           area: area, beamarea: beamarea,$
                           nsources: 0L, n0vals: TEMPORARY(n0vals),$
                           pixsize: pixsize, loglike: DBLARR(nlike) }, nsims )
  
  ;;Main loop
  FOR i=0, nsims-1 DO BEGIN
     IF KEYWORD_SET( verbose ) THEN $
        MESSAGE,STRING(i+1,nsims,FORMAT='(" Simulation: ",I0," of ",I0)'),/INF

     ;;Generate simulation
     map = delta_sim( n0, s0, pixsize, area, fwhm, SIGMA=sigma, SEED=seed, $
                      /FIXNSOURCES, NSOURCES=nsources )
     retstruct[i].nsources = nsources

     maxmap = MAX(map.image,/NAN) * 1.2
     
     hpix = HISTOGRAM( map.image[*], NBINS=128, LOCATIONS=binedge )
     bincent = binedge + 0.5*(binedge[1]-binedge[0])

     ;;Get likelihoods
     FOR j=0,nlike-1 DO BEGIN
        ;;Get PD, normalize and shift
        n0val = retstruct[i].n0vals[j]
        pd = delta_pd( maxmap, npd, n0val, s0, fwhm, SIGMA=sigma )

        ;;Now we resample to the histogram and renorm
        pdinterp = INTERPOL( pd.prob > 0, pd.fluxes, bincent )
        pdinterp /= TOTAL(pdinterp)
        
        ;;The P(D) can be zero due to numeric precision
        ;; this causes problems for the log-like, so floor it
        pdinterp >= 1d-5*MAX(pdinterp,/NAN)

        ;;Compute actual likelihood
        loglike = TOTAL( hpix*ALOG(pdinterp) )

        ;;Comput neg log like
        retstruct[i].loglike[j] = loglike
           
     ENDFOR
  ENDFOR

  RETURN,retstruct
END
