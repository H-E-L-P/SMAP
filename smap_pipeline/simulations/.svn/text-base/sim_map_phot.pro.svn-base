;+
;NAME
; sim_map_phot
;PURPOSE
; Test the map photometry code on simulated images with smoothing
;USAGE
; ret = sim_map_phot( band )
;INPUTS
; band             One of 'PSW', 'PMW', or 'PLW'
;RETURNS
; Structure holding true and map based fluxes
;OPTIONAL INPUTS
; area             Area of simulated map in sq deg (def: 9)
; pixsize          Pixel size, in arcsec (def: 3)
; cutoff           Flux above which to measure sources in Jy (def: 0.02)
; error            Map error (before smoothing; def: 0.008,0.011,0.015
;                   depending on band)
; npix_phot        Number of pixels to use in photometry (def: 9)
; seed             Random number generator seed
;KEYWORDS
; verbose          Print informational message
;MODIFICATION HISTORY
; Author: Alex Conley, May 2012
;-

FUNCTION sim_map_phot, band, AREA=area, PIXSIZE=pixsize, FIXNTOT=fixntot, $
                       CUTOFF=cutoff,ERROR=error,NPIX_PHOT=npix_phot,$
                       SEED=seed,VERBOSE=verbose, SAVEMAP=savemap
                       

  COMPILE_OPT IDL2, STRICTARRSUBS

  IF N_ELEMENTS(area) EQ 0 THEN area = 9.0
  IF area LE 0.0 THEN MESSAGE,"Invalid (non-positive) area"
  IF N_ELEMENTS(pixsize) EQ 0 THEN pixsize = 3.0
  IF pixsize LE 0.0 THEN MESSAGE,"Invalid (non-positive) pixel size"
  IF N_ELEMENTS(cutoff) EQ 0 THEN cutoff = 0.020 ;;Jy
  IF cutoff LE 0.0 THEN MESSAGE,"Invalid (non-positive) cutoff"
  IF N_ELEMENTS(npix_phot) EQ 0 THEN npix_phot = 9
  IF npix_phot LE 0 THEN MESSAGE,"Invalid (non-positive) npix_phot"

  bands = ['PSW','PMW','PLW']
  wave = [250.0,350.0,500.0] ;;microns
  beamsizes = [17.8, 24.0, 35.3] ;;arcsec
  bgsubsize = 48
  base_error = [0.008,0.011,0.015] ;;default

  idx = WHERE( STRUPCASE(band) EQ bands, nidx )
  IF nidx NE 1 THEN MESSAGE,"Couldn't match your band "+band

  IF N_ELEMENTS(error) EQ 0 THEN error = base_error[idx]
  IF error LT 0.0 THEN MESSAGE,"Invalid (negative) error"

  ;;Read in catalog construction inputs
  basedir = addslash(!SMAP_PIPELINE_PATH) + 'simulations/data/'
  dndlogl = MRDFITS(basedir+'bethermin_dNdLogLdzdOmega.fits',1,/SILENT,$
                    STATUS=status)
  IF status LT 0 THEN MESSAGE,"Error reading in dNdLogLdzdOmega file"
  cold = MRDFITS(basedir+'ias_cold.fits',1,/SILENT,$
                    STATUS=status)
  IF status LT 0 THEN MESSAGE,"Error reading in cold template file"
  starburst = MRDFITS(basedir+'ias_starburst.fits',1,/SILENT,$
                    STATUS=status)
  IF status LT 0 THEN MESSAGE,"Error reading in starburst template file"
  
  ;;Figure out area we will generate
  npix = ROUND(SQRT(area)*3600.0/pixsize)
  truearea = npix^2*(pixsize/3600.0)^2
  IF KEYWORD_SET(verbose) THEN $
     MESSAGE,STRING(npix,npix,truearea,FORMAT='("Generating ",I0,"x",I0," simulated image with area: ",F0.1," sq deg")'),/INF

  ;;Determine how many sources we will generate
  ntot = dndlogl.totsources * (!PI/180.0)^2 ;;to sq deg
  ntot *= truearea
  IF ntot EQ 0 THEN MESSAGE,"Error: no sources to generate"
  IF ~ KEYWORD_SET( fixntot ) THEN $
     ntot = RANDOMU(seed,POISSON=ntot,/DOUBLE)
  ntot = ROUND(ntot)
  IF ntot EQ 0 THEN MESSAGE,"Not generating any sources!"
  IF KEYWORD_SET(verbose) THEN $
     MESSAGE,STRING(ntot,FORMAT='("Generating ",I0," sources")'),/INF

  ;;Make the catalog
  cat = bethermin_gencat( ntot, dndlogl, cold, starburst, $
                          SEED=seed, WAVE=wave[idx] )
  cat.obsflux *= 1d-3 ;;to Jy from mJy

  ;;Chose everything above specified flux in catalog to photometer
  ;; do this now so as not to waste time if we get none
  wcat = WHERE(cat.obsflux GE cutoff, ncat )
  IF ncat EQ 0 THEN MESSAGE,"No sources above cutoff"

  ;;Generate output structure
  map = get_smap_mapstruct(NPIXX=npix,NPIXY=npix,$
                           /NOMASK,/NOEXP,/SILENT,$
                           ERRMSG=errmsg, SUCCESS=getmap_succ,$
                           /NO_ABORT, BAND=bands[idx], LAMBDA=wave[idx])
  IF ~ getmap_succ THEN MESSAGE,"Error getting structure: "+errmsg
  map.astrometry.cdelt = [1,1]
  map.astrometry.crpix = [npix/2,npix/2]
  map.astrometry.crval = [20.0,5.0]
  map.pixscale = pixsize
  map.astrometry.cd[0,0] = -pixsize/3600.0
  map.astrometry.cd[1,1] = pixsize/3600.0


  ;;Get positions for each source
  IF KEYWORD_SET(verbose) THEN MESSAGE,"Placing sources",/INF
  xpos = FLOOR( RANDOMU( seed, ntot ) * npix )
  ypos = FLOOR( RANDOMU( seed, ntot ) * npix )
  
  ;;plop them on
  FOR j=0,ntot-1 DO $
     map.image[ xpos[j], ypos[j] ] += cat[j].obsflux[0]

  ;st = WRITE_SMAP_FITSMAP( map, 'precon', /NO_ABORT, ERRMSG=errmsg, DIR='.',$
  ;                         /SILENT )
  ;IF st EQ 0 THEN MESSAGE,"Error writing map: "+errmsg

  IF KEYWORD_SET(verbose) THEN MESSAGE,"Convolving with beam",/INF
  ;;Generate the beam
  kernel1d = GET_SPIRE_BEAM(bands[idx],pixsize,FWHM=beamsizes[idx],$
                            /SILENT,/FACTOR)  
  ;;do convolution
  map.image = CONVOLVE_FACTOR( map.image, kernel1d )
     
  ;st = WRITE_SMAP_FITSMAP( map, 'postcon', /NO_ABORT, ERRMSG=errmsg, DIR='.',$
  ;                         /SILENT )
  ;IF st EQ 0 THEN MESSAGE,"Error writing map: "+errmsg


  ;;Add the error
  map.error = REPLICATE(error,npix,npix)
  map.image += error*RANDOMN(seed,npix,npix)
  map.image -= MEAN(map.image)

  ;;Smooth
  IF KEYWORD_SET(verbose) THEN MESSAGE,"Smoothing",/INF
  smap_redsource_smooth,map,/BRUTE,/NOPEAKCORR
 
  IF N_ELEMENTS(savemap) NE 0 THEN BEGIN
     st = WRITE_SMAP_FITSMAP( map, savemap, /NO_ABORT, ERRMSG=errmsg, DIR='.',$
                              /SILENT )
     IF st EQ 0 THEN MESSAGE,"Error writing map: "+errmsg
  ENDIF


  mask = BYTARR(npix,npix)
  bgmap = smap_bgestimator(map.image,mask,bgsubsize)
  
  
  ;;Photometer
  ;;We can't handle things that are too close to the edge, so clip again
  hlf = npix_phot/2
  wcat = WHERE( xpos GT hlf AND xpos LT npix-hlf AND $
                ypos GT hlf AND ypos LT npix-hlf AND $
                cat.obsflux GE cutoff, ncat )
  IF ncat EQ 0 THEN MESSAGE,"No sources above cutoff with good position"


  IF KEYWORD_SET(verbose) THEN MESSAGE,"Doing photometry",/INF
  flux = smap_simplephot(TEMPORARY(map), xpos[wcat], ypos[wcat], $
                         FWHM=35.3*SQRT(2),/XYPOS,$
                         ERR=flux_err, NPIX=npix_phot, $
                         BGRND=TEMPORARY(bgmap))
  
  ;;Output structure
  retstr = REPLICATE( { flux_true: !VALUES.F_NAN, flux_map: !VALUES.F_NAN,$
                        flux_map_err: !VALUES.F_NAN, xpos: !VALUES.F_NAN,$
                        ypos: !VALUES.F_NAN}, ncat )
  retstr.flux_true    = cat[wcat].obsflux[0]
  retstr.flux_map     = TEMPORARY(flux)
  retstr.flux_map_err = TEMPORARY(flux_err)
  retstr.xpos         = xpos[wcat]
  retstr.ypos         = ypos[wcat]
  RETURN,retstr

END


