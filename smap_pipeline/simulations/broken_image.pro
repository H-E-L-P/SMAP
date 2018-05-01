;+
; NAME
;  broken_image
; PURPOSE
;  Create a fake image using a multiply broken power law as input
; USAGE
;  broken_image, outfile, knotpos, knotval [, AREA=, PIXSIZE=, BEAMFWHM=,
;                                SIGMA=, BEAMFILE=, CATFILE= ]
; INPUTS
;  outfile       The file the image is written to, as SMAP FITS
; OPTIONAL INPUTS
;  area          The area of the image (in sq. deg)
;  pixsize       The linear extent of the pixel, in arcsec
;  convolvesize  The size of the pixels used when convolving.  If set
;                 to something different than pixsize, the map is made
;                 at this size, convovled, and then rebinned to pixsize.
;                 Pixsize must be an odd integral multiple of convolvesize.
;  beamfwhm      The FWHM of the (Gaussian) beam, in arcsec.  Set to
;                 zero to not include beam effects
;  beamfile      FITS file to read beam from.  It is up to you to
;                 assure that the pixel scale matches convolvesize
;  sigma         Gaussian noise added on a per-pixel basis
;  nfluxes       Number of points to generate cumulative flux
;                 distribution at (def: 65536)
;  racen/deccen  RA/DEC of center in decimal degrees (def: 120/10)
;  catfile       Write the catalog of sources to this file (fits)
; KEYWORDS
;  fixntot       Fix the total number of sources instead of treating
;                 it as poisson distributed
;  notruearea    Don't adjust ntot for realized area -- useful when
;                 generating multiple images with same seed
;  mask          Randomly masks 5% of the pixels; this adds another extension
;                 to the output file.
;  spline        Use spline based model
;  verbose       Prints status messages
; SIDE EFFECTS
;  Writes a fits file to outfile
; NOTES
;  This assumes per-beam normalization, and strictly positive beams.
;  It also assumes that per-beam normalization means that the mean of
;  the psf over the final peak pixel is one, not that the center of
;  the peak pixel is one.
; MODIFICATION HISTORY
;  Author: Alex Conley, May 13, 2009
;-

PRO broken_image, outfile, knotpos, knotval, AREA=area, $
                  PIXSIZE=pixsize, BEAMFWHM=beamfwhm, SIGMA=sigma,$
                  FIXNTOT=fixntot, SEED=seed, NOMEANSUB=nomeansub,$
                  CONVOLVESIZE=convolvesize, MASK=mask, NOTRUEAREA=notruearea,$
                  BEAMFILE=beamfile, NFLUXES=nfluxes, VERBOSE=verbose,$
                  RACEN=racen, DECCEN=deccen, CATFILE=catfile, SPLINE=spline

  COMPILE_OPT IDL2, STRICTARRSUBS

  IF N_ELEMENTS(racen) EQ 0 THEN racen = 120.0
  IF N_ELEMENTS(deccen) EQ 0 THEN deccen = 10.0
  IF N_ELEMENTS(nfluxes) EQ 0 THEN nfluxes = 65536
  IF nfluxes LE 0 THEN MESSAGE,"Invalid nfluxes "+STRING(nfluxes)
  nknots = N_ELEMENTS(knotpos)
  IF nknots EQ 0 THEN MESSAGE,"Need some knots!"
  IF nknots LT 2 THEN MESSAGE,"Need 2 or more knots!"
  IF nknots NE N_ELEMENTS(knotval) THEN MESSAGE,"Knotpos/knotval not same size"
  IF N_ELEMENTS(area) EQ 0 THEN area=1.0
  IF area LT 0 THEN MESSAGE,"Invalid area"
  IF N_ELEMENTS(pixsize) EQ 0 THEN pixsize=4.0 ;;arcsec
  IF pixsize LT 0 THEN MESSAGE,"Invalid pixsize"
  IF pixsize/3600.0 GT SQRT(area)/100.0 THEN $
     MESSAGE,"Pixsize too large -- expect at least 100x100 pixels"
  IF N_ELEMENTS( beamfwhm ) EQ 0 THEN beamfwhm=18.6
  IF beamfwhm LT 0 THEN MESSAGE,"Invalid beamfwhm"
  IF beamfwhm NE 0.0 AND pixsize GT beamfwhm/2.0 THEN $
     MESSAGE,"Pixels too large to sample beam"
  IF N_ELEMENTS(sigma) EQ 0 THEN sigma=0.0
  IF sigma LT 0.0 THEN MESSAGE,"Invalid sigma"

  IF N_ELEMENTS(convolvesize) NE 0 AND beamfwhm NE 0.0 THEN BEGIN
     IF convolvesize LE 0.0 THEN MESSAGE,"Invalid convolvesize"
     final_rebin=1b
     IF convolvesize GT pixsize AND KEYWORD_SET(verbose) THEN $
        MESSAGE,"Convolve size less than pixsize -- this makes no sense"
     IF ABS( convolvesize - pixsize ) LT 0.01 THEN BEGIN
        final_rebin = 0b
        init_pixsize = pixsize
     ENDIF ELSE BEGIN
        pixratio = pixsize / convolvesize
        IF ABS( (pixratio MOD 2.0) - 1.0 ) GT 0.01 THEN $
           MESSAGE,"pixsize must be odd integral multiple of convolvesize"
        rebinfac = ROUND(pixratio)
        init_pixsize = convolvesize
     ENDELSE
  ENDIF ELSE BEGIN
     init_pixsize = pixsize
     final_rebin=0b
  ENDELSE

  IF FILE_TEST(outfile) AND KEYWORD_SET(verbose) THEN $
     MESSAGE,"Warning -- output file "+outfile+$
             " will be overwritten",/INF

  IF final_rebin THEN BEGIN
     npixperside = ROUND(3600.0*SQRT(area) / pixsize)
     npixperside = ROUND( npixperside * rebinfac )
  ENDIF ELSE npixperside = ROUND(3600.0*SQRT(area)/init_pixsize)
  truearea = (npixperside*init_pixsize/3600.0)^2

  ;;Estimate average flux per pixel
  meanperarea = broken_meanflux( knotpos, knotval )
  meanperpixel = meanperarea * ( init_pixsize / 3600.0 )^2
  IF KEYWORD_SET(verbose) THEN BEGIN
     IF final_rebin THEN BEGIN
        PRINT,"Expected mean flux per pixel: ",$
              meanperarea * ( pixsize / 3600.0 )^2
     ENDIF ELSE PRINT,"Expected mean flux per pixel: ",meanperpixel
  ENDIF

  ;;Get the cumulative probability distribution and the
  ;; mean total number
  ;;Use exponential spacing
  ;fluxes = (MAX(knotpos) - MIN(knotpos))*DINDGEN(nfluxes)/(nfluxes-1.0)+$
  ;         MIN(knotpos) ;;linear spacing
  kval = ALOG( MAX(knotpos)/MIN(knotpos) )/(nfluxes-1.0)
  fluxes = MIN(knotpos)*EXP(kval*DINDGEN(nfluxes))
  IF ~ KEYWORD_SET( spline ) THEN BEGIN
     prob = broken_cumn( fluxes, knotpos, knotval, NTOT=ntot )
  ENDIF ELSE BEGIN
     prob = spline_cumn( fluxes, knotpos, knotval, NTOT=ntot )
  ENDELSE
  prob /= MAX(prob) ;;Shouldn't be needed, but being cautious
  IF KEYWORD_SET(notruearea) THEN BEGIN
     ntot *= area ;;Assuming the normalization is per degree
  ENDIF ELSE ntot *= truearea
  IF ~ KEYWORD_SET( fixntot ) THEN $
     ntot = RANDOMU(seed,POISSON=ntot,/DOUBLE)
  ntot = ROUND(ntot)
  IF KEYWORD_SET(verbose) THEN $
     PRINT,"Generating ",ntot," sources"

  image = DBLARR( npixperside, npixperside )
  maxflux = -1e10
  cat = REPLICATE( {xpos: 0, ypos: 0,$
                    fluxes: !VALUES.D_NAN, z: 0.0}, ntot )
  cat.fluxes = INTERPOL( fluxes, prob, RANDOMU(seed,ntot) )
  maxflux >= MAX(cat.fluxes)

  ;;Randomly assign each flux to an image pixel
  cat.xpos = FLOOR( RANDOMU( seed, ntot ) * npixperside )
  cat.ypos = FLOOR( RANDOMU( seed, ntot ) * npixperside )
  
  ;; randomly assign a redshift to each source (hack to get MZs code to work)
  cat.z = RANDOMN(SEED,ntot,POISSON=2)

  ;;Can't do this 'in parallel' because IDL can't handle
  ;; multiple sources ending up in the same pixel
  ;;That is,
  ;; image[cat.xpos,cat.ypos] += cat.fluxes
  ;;doesn't work, and worse, appears to work while
  ;; quietly doing the wrong thing.  
  FOR j=0,ntot-1 DO $
     image[ cat[j].xpos, cat[j].ypos ] += cat[j].fluxes

  IF KEYWORD_SET(verbose) THEN $
     PRINT,"Brightest source: ",maxflux

  ;;Convolve if needed
  IF beamfwhm NE 0 OR N_ELEMENTS(beamfile) NE 0 THEN BEGIN
     IF N_ELEMENTS(beamfile) EQ 0 THEN BEGIN
        MESSAGE,"Convolving with Gaussian beam",/INF
        ;;Build the Gaussian beam profile
        kernel = get_spire_beam( 'PSW', init_pixsize,$
                                 FWHM=beamfwhm, /SILENT )
     ENDIF ELSE BEGIN
        MESSAGE,"Convolving with user provided beam",/INF
        kernel = MRDFITS( beamfile, /SILENT, STATUS=status )
        IF status NE 0 THEN $
           MESSAGE,"Error reading user provided beam file: "+beamfile
     ENDELSE
     ;;Wrap around to the other side -- not perfect, but a decent
     ;; solution as long as the map is a lot bigger than 3 sigma
     ;image = CONVOL(image, kernel, /CENTER, /EDGE_WRAP)

     ;;Use CONVOLVE to make it faster
     image=CONVOLVE(image,kernel)
  ENDIF
  
  ;;Rebin
  IF final_rebin THEN BEGIN
     npixperside = ROUND(npixperside / rebinfac)
     image = REBIN(image,npixperside,npixperside)
     cat.xpos /= rebinfac
     cat.ypos /= rebinfac
  ENDIF

  IF N_ELEMENTS(catfile) NE 0 THEN BEGIN
     MWRFITS, TEMPORARY(cat), catfile, /SILENT, /CREATE
  ENDIF ELSE a=N_ELEMENTS(TEMPORARY(cat))

  ;;Add noise
  IF sigma GT 0.0 THEN image += sigma*RANDOMN(seed,npixperside,npixperside)

  mnval = MEAN(image)
  PRINT,"Realized mean per pixel: ",mnval
  IF ~KEYWORD_SET(nomeansub) THEN image -= mnval

  ;;Write
  nomask = ~ KEYWORD_SET( mask )
  mapstruct = get_smap_mapstruct(NPIXX=npixperside,NPIXY=npixperside,$
                                 NOMASK=nomask,/NOEXP,/NOERR,/SILENT,$
                                 ERRMSG=errmsg, SUCCESS=getmap_succ,$
                                 /NO_ABORT)
  IF ~ getmap_succ THEN MESSAGE,"Error getting structure: "+errmsg

  mapstruct.image = TEMPORARY(image)
  mapstruct.astrometry.cd[0,0] = -pixsize/3600.0
  mapstruct.astrometry.cd[1,1] = pixsize/3600.0
  mapstruct.astrometry.cdelt = [1,1]
  mapstruct.astrometry.crpix = [npixperside/2,npixperside/2]
  mapstruct.astrometry.crval = [racen,deccen]
  mapstruct.pixscale = pixsize

  IF KEYWORD_SET(mask) THEN BEGIN
     mask = BYTARR( npixperside, npixperside )
     npix = N_ELEMENTS(mask)
     mask[ ROUND( npix*RANDOMU(seed, ROUND(0.05*npix)) ) < npix ] = 1b
     mapstruct.mask = TEMPORARY(mask)
  ENDIF

  success = WRITE_SMAP_FITSMAP(mapstruct,outfile,DIR='.',ERRMSG=errmsg)
  IF ~ success THEN MESSAGE,"Error writing output file1: "+errmsg

END
