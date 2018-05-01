;+
;NAME
; delta_sim
;PURPOSE
; Simulates a delta function number counts model with Gaussian beams
;USAGE
; map = delta_sim( n0, s0, pixsize, area, fwhm, SIGMA=] )
;INPUTS
;  n0           Number of sources per sq deg in model
;  s0           Flux of sources
;  pixsize      Size of final pixels, in arcsec
;  area         Area of generated image, in sq deg
;  fwhm         FWHM of Gaussian beam, in arcsec
;RETURNS
;  A smap map structure with the simulated image
;OPTIONAL INPUTS
;  sigma        White noise to add (def: 0)
;  seed         Random number generator seed
;KEYWORDS
;  verbose      Output informational messages
;  fixnsources  Don't randomly vary ntot
;OPTIONAL OUTPUTS
;  nsources     Actual number of sources inserted
;NOTES
; The image is mean subtracted
;MODIFICATION HISTORY
; Author: Alex Conley, Jan 2012
;-

FUNCTION delta_sim, n0, s0, pixsize, area, fwhm, SIGMA=sigma, SEED=seed, $
                    NSOURCES=nsources, VERBOSE=verbose, FIXNSOURCES=fixnsources
  COMPILE_OPT IDL2, STRICTARRSUBS

  ;;defaults
  IF N_ELEMENTS( sigma ) EQ 0 THEN sigma = 0.0

  ;;input checks
  IF n0 LE 0 THEN MESSAGE,"Invalid (non-positive) n0"
  IF s0 LE 0 THEN MESSAGE,"Invalid (non-positive) s0"
  IF pixsize LE 0 THEN MESSAGE,"Invalid (non-positive) pixsize"
  IF area LE 0 THEN MESSAGE,"Invalid (non-positive) area"
  IF fwhm LE 0 THEN MESSAGE,"Invalid (non-positive) fwhm"
  IF fwhm LT 3*pixsize THEN MESSAGE,"FWHM too small relative to pixel size"
  IF sigma LE 0 THEN MESSAGE,"Invalid sigma"

  ;;compute actual area
  npix = ROUND(SQRT(area)*3600.0/pixsize)
  truearea = (pixsize/3600.0)^2 * npix * npix

  ;;number of sources
  nsources = n0 * truearea
  IF ~ KEYWORD_SET( fixnsources ) THEN $
     nsources = RANDOMU(seed,POISSON=nsources,/DOUBLE)
  nsources = ROUND( nsources )
  IF KEYWORD_SET(verbose) THEN $
     MESSAGE,STRING(nsources,FORMAT='("Number of sources: ",I0)'),/INF
  
  ;;get mapstruct for output
  IF sigma GE 0.0 THEN noerr = 0b ELSE noerr = 1b
  map = get_smap_mapstruct(NPIXX=npix, NPIXY=npix, /NOMASK, /NOEXP,$
                           NOERR=noerr, /SILENT, ERRMSG=errmsg,$
                           SUCCESS=getmap_succ, /NO_ABORT, BAND='PSW')
  IF ~ getmap_succ THEN MESSAGE,"Error getting structure: "+errmsg

  ;;insert sources; must use for loop
  xpos = ROUND( (npix-1)*RANDOMU(seed, nsources) )
  ypos = ROUND( (npix-1)*RANDOMU(seed, nsources) )
  FOR i=0, nsources-1 DO map.image[xpos[i],ypos[i]] += s0
     
  ;;convolve on the beam
  IF KEYWORD_SET( verbose ) THEN MESSAGE,"Convolving on beam",/INF
  beam = get_spire_beam('PSW',pixsize,FWHM=fwhm,/SILENT)
  map.image = CONVOLVE( map.image, beam )
  
  ;;add noise
  IF sigma GE 0.0 THEN BEGIN
     map.error = sigma
     map.image += sigma*RANDOMN(seed,npix,npix)
  ENDIF

  ;;meansub
  map.image -= MEAN(map.image,/NAN)

  ;;setup astrometry
  map.astrometry.cdelt = [1,1]
  map.astrometry.crpix = [npix/2,npix/2]
  map.astrometry.crval = [120,15]
  map.pixscale = pixsize
  map.astrometry.cd[0,0] = -pixsize/3600.0
  map.astrometry.cd[1,1] = pixsize/3600.0

  RETURN,map
END
                    
