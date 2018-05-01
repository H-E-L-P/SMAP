;+
;NAME
; smap_gridmap
;PURPOSE
; Make a very simple set of SMAP maps with a grid of fixed
; flux sources
;USAGE
; smap_gridmap, flux, nsrcs, area, pixsize, sigma, outbase, [OUTDIR=, /NOSIG]
;INPUTS
; flux       Flux density of sources, in Jy.  Scalar or 3 element vector
; nsrcs      Number of sources to insert along each dimension (def: 10)
; area       Area of generated maps (squarish) in sq. deg
; pixsize    Pixel size, in arcsec.  Scalar -- all maps have the same
;                                              size
; sigma      Noise levels in Jy, either scalar or 3 element vector
; outbase    Base name of output files
;OPTIONAL INPUTS
; outdir     Directory to write to, default !SMAP_MAPS
;KEYWORDS
; nosig      Noise is not added to the simulated image, although the
;             error extension is set.
;SIDE EFFECTS
; Writes three simulated maps
;MODIFICATION HISTORY
; Author: Alex Conley, October 2014
;-

PRO smap_gridmap, flux, nsrcs, area, pixsize, sigma, outbase, OUTDIR=outdir,$
                  NOSIG=nosig
  COMPILE_OPT IDL2, STRICTARRSUBS

  IF area LE 0 THEN MESSAGE, "Area must be positive"
  IF nsrcs LE 0 THEN MESSAGE, "nsrcs must be positive"
  IF pixsize LE 0 THEN MESSAGE, "pixsize must be positive"
  IF N_ELEMENTS(flux) LT 3 THEN fluxes = REPLICATE(flux[0], 3) ELSE $
     fluxes = flux[0:2]
  IF MIN(fluxes) LE 0.0 THEN MESSAGE, "Flux values must be positive"
  IF N_ELEMENTS(sigma) LT 3 THEN sigmas = REPLICATE(sigma[0], 3) ELSE $
     sigmas = sigma[0:2]
  IF MIN(sigmas) LT 0.0 THEN MESSAGE, "Sigma values must be non-negative"
  
  npix_perdim = ROUND(SQRT(area) * 3600.0 / pixsize) ;; squarish
  extent_perside = npix_perdim * pixsize ;; arcsec
  spacing = ROUND(npix_perdim / (nsrcs + 1))

  ;; Set up map structure; because the pixel sizes are all the same,
  ;; we can reuse for all bands
  bands = ['PSW', 'PMW', 'PLW']
  bandstr = ['250 microns', '350 microns', '500 microns']
  wave = [250., 350, 500]
  fwhm = [17.6, 23.9, 35.2] ;; arcsec
  fwhm_pix = fwhm / pixsize
  IF spacing LT 4 * MAX(fwhm_pix) THEN $
     MESSAGE, "Warning: Sources may be too close; "+$
              "reduce nsrcs or increase area"

  mapstruct = get_smap_mapstruct(NPIXX=npix_perdim,NPIXY=npix_perdim,$
                                 /SILENT, ERRMSG=errmsg, SUCCESS=getmap_succ,$
                                 /NO_ABORT, BAND=bands[0], LAMBDA=wave[0])
  IF ~ getmap_succ THEN MESSAGE,"Error getting structure: "+errmsg
  mapstruct.exposure = 1.0 ;; arbitrary
  mapstruct.astrometry.cdelt = [1,1]
  mapstruct.astrometry.crpix = 0.5*[npix_perdim, npix_perdim]
  mapstruct.astrometry.crval = [120, 15]
  mapstruct.pixscale = pixsize
  mapstruct.astrometry.cd[0,0] = -pixsize / 3600.0
  mapstruct.astrometry.cd[1,1] = pixsize / 3600.0

  ;; Generational loop
  FOR i=0, N_ELEMENTS(bands)-1 DO BEGIN
     mapstruct.lambda = wave[i]
     mapstruct.names = bands[i]
     mapstruct.bands = bandstr[i]
     mapstruct.error = sigmas[i]

     ;; Setup grid, could be done more efficiently, but meh
     mapstruct.image = 0.0 ;; clear old
     fval = fluxes[i]
     FOR j=0, nsrcs-1 DO FOR k=0, nsrcs-1 DO $
        mapstruct.image[(j+1)*spacing, (k+1)*spacing] = fval

     ;; Apply beam
     kernel1d = GET_SPIRE_BEAM('PSW', pixsize, FWHM=fwhm[i], /SILENT, /FACTOR)
     kernel1d /= MAX(kernel1d)
     mapstruct.image = CONVOLVE_FACTOR(mapstruct.image, kernel1d, /WRAP)

     ;; Add noise
     IF ~ KEYWORD_SET(nosig) THEN $
        mapstruct.image += sigmas[i] * RANDOMN(seed, npix_perdim, npix_perdim)

     ;; Write
     st = WRITE_SMAP_FITSMAP(mapstruct, outbase, DIR=outdir,$
                             /NO_ABORT, ERRMSG=errmsg)
     IF st EQ 0 THEN $
        MESSAGE,"Error writing " + bands[i] + " map: " + errmsg
  ENDFOR
END
