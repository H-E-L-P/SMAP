;+
; NAME
;  smap_generate_double_colour
; PURPOSE
;  Create a pair of fake images using a spline with knots in band1
;   and a colour model for band2.  The colour model is Log Normal
;   by default, but Gaussian is a supported option
; USAGE
;  smap_generate_double_colour, params, beamfile1, beamfile2,
;                               outfile [, AREA=, CATFILE=, /GAUSSIAN,
;                               k, pk ]
; INPUTS
;  params        A structure holding the model parameters.  This
;                 should have fields:
;                  .nfluxknots        Number of flux knots in band 1
;                  .fluxknotpos       Positions of flux knots in band
;                                      1 (array)
;                  .fluxknotval       Log10 n(f1) -- i.e., values at
;                                                    knot positions in
;                                                    band1 (array)
;                  .nsigmaknots       Number of colour dispersion
;                                      knots
;                  .sigmaknotpos      Position of sigma knots
;                  .sigmaknotval      Values at sigma knots
;                  .noffsetknots      Number of offset knots
;                  .offsetknotpos     Positions of offset knots
;                  .offsetknotval     Values at offset knots
;  beamfile1     Name of FITS file with beam in band 1
;  beamfile2     Name of FITS file with beam in band 2.  Must have
;                 same pixel size as beamfile1
;  outfile       Root name for output files
; OPTIONAL INPUTS
;  area          The area of the image (in sq. deg).  May not be
;                 perfectly realized on output
;  rebinfac      Factor to rebin output maps by.  Must be integral
;  sigma1        Gaussian noise added on a per-pixel basis in band1
;  sigma2        Gaussian noise added on a per-pixel basis in band2
;  nfluxes       Number of points to generate cumulative flux
;                 distribution at (def: 65536)
;  racen/deccen  RA/DEC of center in decimal degrees (def: 120/10)
;  catfile       Write the catalog of sources to this file (fits)
;  seed          Seed for random number generator
;  band1         Name of band 1 (def: PSW)
;  band2         Name of band 2 (def: PMW)
;  lambda1/lambda2 Wavelengths of bands
;  k             k values for P(k), in inverse arcmin
;  pk            P(k) -- if this (and k) are provided, then the
;                        sources are distributed with clustering
;                        according to P(k)
; KEYWORDS
;  ranmask       Randomly mask about this percentage of the pixels (def: 0)
;  gaussian      Use a Gaussian colour model rather than a Log Normal one
;  fixntot       Fix the total number of sources instead of treating
;                 it as poisson distributed
;  notruearea    Don't adjust ntot for realized area -- useful when
;                 generating multiple images with same seed
;  verbose       Prints status messages
; SIDE EFFECTS
;  Writes two fits files to outfile_band1.fits and outfile_band2.fits
; NOTES
; MODIFICATION HISTORY
;  Author: Alex Conley, Mar 2, 2011
;-

FUNCTION smap_generate_double_colour_getpixsize, head, filename
  COMPILE_OPT IDL2, HIDDEN

  pixscale = SXPAR(head,'PIXSCALE',COUNT=cnt)
  IF cnt GT 0 THEN RETURN,pixscale[0]
  pixscale = SXPAR(head,'PIXSIZE',COUNT=cnt)
  IF cnt GT 0 THEN RETURN,pixscale[0]

  ;;Try ast
  EXAST, head, astr, NOPARAMS=success
  IF success LT 0 THEN $
     MESSAGE,"Couldn't find plate scale info for "+filename
  GETROT, head, rot, cdelt, /SILENT
  RETURN, 3600.0*SQRT(cdelt[0]*cdelt[1])
END

PRO smap_generate_double_colour, params, beamfile1, beamfile2, outfile,$
                                 AREA=area, REBINFAC=rebinfac, $
                                 SIGMA1=sigma1, SIGMA2=sigma2,$
                                 FIXNTOT=fixntot, SEED=seed, $
                                 NOTRUEAREA=notruearea, NFLUXES=nfluxes, $
                                 VERBOSE=verbose, RACEN=racen, DECCEN=deccen, $
                                 CATFILE=catfile, BAND1=band1, BAND2=band2,$
                                 GAUSSIAN=gaussian, K=k, PK=pk, $
                                 RANMASK=ranmask, LAMBDA1=lambda1, $
                                 LAMBDA2=lambda2

  COMPILE_OPT IDL2, STRICTARRSUBS

  IF N_ELEMENTS(racen) EQ 0 THEN racen = 120.0
  IF N_ELEMENTS(deccen) EQ 0 THEN deccen = 10.0
  IF N_ELEMENTS(nfluxes) EQ 0 THEN nfluxes = 65536
  IF nfluxes LE 0 THEN MESSAGE,"Invalid nfluxes "+STRING(nfluxes)
  IF N_ELEMENTS(area) EQ 0 THEN area=1.0
  IF area LE 0 THEN MESSAGE,"Invalid area"
  IF N_ELEMENTS(sigma1) EQ 0 THEN sigma1=0.0
  IF N_ELEMENTS(sigma2) EQ 0 THEN sigma2=0.0
  IF sigma1 LT 0.0 OR sigma2 LT 0.0 THEN MESSAGE,"Invalid sigma value"
  IF N_ELEMENTS(band1) EQ 0 THEN band1='PSW'
  IF N_ELEMENTS(band2) EQ 0 THEN band2='PMW'
  IF N_ELEMENTS(k) NE 0 THEN BEGIN
     IF N_ELEMENTS(pk) NE N_ELEMENTS(k) THEN $
        MESSAGE,"pk and k must be same length"
     has_clustering = 1b
  ENDIF ELSE has_clustering=0b
  IF N_ELEMENTS(ranmask) EQ 0 THEN ranmask = 0.0
  IF ranmask LT 0 OR ranmask GT 100 THEN MESSAGE,"Invalid random mask"

  ;;Test to make sure that input params are valid
  IF SIZE(params,/TNAME) NE 'STRUCT' THEN $
     MESSAGE,"Input parameters not structure"
  required_tags = ["nfluxknots","fluxknotpos","fluxknotval",$
                   "nsigmaknots","sigmaknotpos","sigmaknotval",$
                   "noffsetknots","offsetknotpos","offsetknotval"]
  FOR i=0,N_ELEMENTS(required_tags)-1 DO $
     IF ~ TAG_EXIST(params,required_tags[i],/TOP_LEVEL) THEN $
        MESSAGE,"Required tag: "+required_tags[i]+" not found in params"
  IF params.nfluxknots EQ 0 THEN MESSAGE,"Need some flux knots"
  IF params.nfluxknots NE N_ELEMENTS(params.fluxknotpos) THEN $
     MESSAGE,"Number of flux knot positions doesn't match specificiation"
  IF params.nfluxknots NE N_ELEMENTS(params.fluxknotval) THEN $
     MESSAGE,"Number of flux values doesn't match specificiation"
  IF params.nsigmaknots EQ 0 THEN MESSAGE,"Need some sigma knots"
  IF params.nsigmaknots NE N_ELEMENTS(params.sigmaknotpos) THEN $
     MESSAGE,"Number of sigma knot positions doesn't match specificiation"
  IF params.nsigmaknots NE N_ELEMENTS(params.sigmaknotval) THEN $
     MESSAGE,"Number of sigma values doesn't match specificiation"
  IF params.noffsetknots EQ 0 THEN MESSAGE,"Need some offset knots"
  IF params.noffsetknots NE N_ELEMENTS(params.offsetknotpos) THEN $
     MESSAGE,"Number of offset knot positions doesn't match specificiation"
  IF params.noffsetknots NE N_ELEMENTS(params.offsetknotval) THEN $
     MESSAGE,"Number of offset values doesn't match specificiation"
  IF MIN(params.fluxknotpos) LE 0 THEN $
     MESSAGE,"Flux knot positions must be positive"
  IF MIN(params.sigmaknotpos) LE 0 THEN $
     MESSAGE,"Sigma knot positions must be positive"
  IF MIN(params.sigmaknotval) LE 0 THEN $
     MESSAGE,"Sigma knot values must be positive"
  IF MIN(params.offsetknotpos) LE 0 THEN $
     MESSAGE,"Offset knot positions must be positive"

  IF band1 EQ band2 THEN MESSAGE,"Band1 and band2 must be distinct"

  IF FILE_TEST(outfile+'_'+band1+'.fits') AND KEYWORD_SET(verbose) THEN $
     MESSAGE,"Warning -- output file "+outfile+'_'+band1+'.fits'+$
             " will be overwritten",/INF
  IF FILE_TEST(outfile+'_'+band2+'.fits') AND KEYWORD_SET(verbose) THEN $
     MESSAGE,"Warning -- output file "+outfile+'_'+band2+'.fits'+$
             " will be overwritten",/INF

  ;;read beams, find pixel scale
  kernel1 = MRDFITS( beamfile1, 0, bmhead1, /SILENT, STATUS=status )
  IF status NE 0 THEN $
     MESSAGE,"Error reading user provided beam file: "+beamfile1
  kernel2 = MRDFITS( beamfile2, 0, bmhead2, /SILENT, STATUS=status )
  IF status NE 0 THEN $
     MESSAGE,"Error reading user provided beam file: "+beamfile2

  pixscale1 = smap_generate_double_colour_getpixsize(bmhead1,beamfile1)
  pixscale2 = smap_generate_double_colour_getpixsize(bmhead2,beamfile2)
  IF ABS( (pixscale1 - pixscale2)/pixscale1 ) GT 1d-5 THEN $
     MESSAGE,"Input beams do not have same pixel scale"

  ;;Now try to figure out how large to make the output maps
  ;; this depends if we are going to rebin or not
  IF N_ELEMENTS(rebinfac) NE 0 && ABS(rebinfac-1.0) GT 1d-5 THEN $
     final_rebin = 1b ELSE final_rebin = 0b
  IF final_rebin THEN BEGIN
     rebinfac = FIX(rebinfac) ;;Make integer
     npixperside = ROUND(SQRT(area)*3600.0/(rebinfac*pixscale1))
     genpixperside = npixperside*rebinfac
     truearea = (npixperside*rebinfac*pixscale1/3600.0)^2
  ENDIF ELSE BEGIN
     npixperside = ROUND(SQRT(area)*3600.0/pixscale1)
     genpixperside = npixperside
     truearea = (npixperside*pixscale1/3600.0)^2
  ENDELSE

  ;;Get the cumulative probability distribution and the
  ;; mean total number
  ;;Use exponential spacing
  kval = ALOG( MAX(params.fluxknotpos)/MIN(params.fluxknotpos) )/(nfluxes-1.0)
  fluxes = MIN(params.fluxknotpos)*EXP(kval*DINDGEN(nfluxes))
  prob = spline_cumn( fluxes, params.fluxknotpos, params.fluxknotval, $
                      NTOT=ntot )
  IF KEYWORD_SET(notruearea) THEN BEGIN
     ntot *= area ;;Assuming the normalization is per deg
  ENDIF ELSE ntot *= truearea
  IF ntot EQ 0 THEN MESSAGE,"Error: no sources to generate"
  IF ~ KEYWORD_SET( fixntot ) THEN $
     ntot = RANDOMU(seed,POISSON=ntot,/DOUBLE)
  ntot = ROUND(ntot)
  IF KEYWORD_SET(verbose) THEN $
     PRINT,"Generating ",ntot," sources"

  ;;Make image
  image1 = FLTARR( genpixperside, genpixperside )
  image2 = FLTARR( genpixperside, genpixperside )
  cat = REPLICATE( {xpos: 0, ypos: 0,$
                    flux1: !VALUES.F_NAN, flux2: !VALUES.F_NAN}, ntot )
  cat.flux1 = INTERPOL( fluxes, prob, RANDOMU(seed,ntot) )

  ;;Now generate gaussian flux2
  ;;Generate sigma and offset for each flux value
  IF params.nsigmaknots EQ 1 THEN BEGIN
     sigvals = params.sigmaknotval[0]
     sminval = params.sigmaknotval[0]
  ENDIF ELSE BEGIN
     sigvals = DBLARR(ntot)
     minsgknot = MIN(params.sigmaknotpos,swmin,MAX=maxsgknot,$
                     SUBSCRIPT_MAX=swmax)
     sminval = params.sigmaknotval[swmin]
     wlow = WHERE(cat.flux1 LE minsgknot, nlow)
     IF nlow NE 0 THEN sigvals[wlow] = params.sigmaknotval[swmin]
     wmed = WHERE(cat.flux1 GT minsgknot AND cat.flux1 LE maxsgknot, nmed)
     IF nmed NE 0 THEN BEGIN
        spl = SPL_INIT(params.sigmaknotpos, params.sigmaknotval)
        sigvals[wmed]=SPL_INTERP(params.sigmaknotpos,$
                                 params.sigmaknotval, spl,$
                                 cat[wmed].flux1)
     ENDIF
     whi = WHERE(cat.flux1 GT maxsgknot,nhi)
     IF nhi NE 0 THEN sigvals[whi] = params.sigmaknotval[swmax]
  ENDELSE
  IF KEYWORD_SET(gaussian) THEN sigvals *= cat.flux1

  IF params.noffsetknots EQ 1 THEN BEGIN
     offvals = params.offsetknotval[0] 
     ominval = params.offsetknotval[0]
  ENDIF ELSE BEGIN
     offvals = DBLARR(ntot)
     minsgknot = MIN(params.offsetknotpos,owmin,MAX=maxsgknot,$
                     SUBSCRIPT_MAX=owmax)
     ominval = params.offsetknotval[owmin]
     wlow = WHERE(cat.flux1 LE minsgknot, nlow)
     IF nlow NE 0 THEN offvals[wlow] = params.offsetknotval[owmin]
     wmed = WHERE(cat.flux1 GT minsgknot AND cat.flux1 LE maxsgknot, nmed)
     IF nmed NE 0 THEN BEGIN
        spl = SPL_INIT(params.offsetknotpos, params.offsetknotval)
        offvals[wmed]=SPL_INTERP(params.offsetknotpos,$
                                 params.offsetknotval,spl, $
                                 cat[wmed].flux1)
     ENDIF
     whi = WHERE(cat.flux1 GT maxsgknot,nhi)
     IF nhi NE 0 THEN offvals[whi] = params.offsetknotval[owmax]
  ENDELSE
  IF KEYWORD_SET(gaussian) THEN sigvals *= cat.flux1

  minflux1 = MIN(params.fluxknotpos)
  minflux2 = minflux1 + ominval*minflux1 - $
             7.0*sminval*minflux1
  minflux2 >= 1d-3*minflux1
  IF KEYWORD_SET( gaussian ) THEN BEGIN
     cat.flux2 = (cat.flux1 + TEMPORARY(offvals) + $
                  TEMPORARY(sigvals)*RANDOMN(seed,ntot,/DOUBLE)) > minflux2
  ENDIF ELSE BEGIN
     ;;Log normal -- to generate, make a gaussian number and
     ;;              exponentiate it
     gval = TEMPORARY(offvals) + TEMPORARY(sigvals)*RANDOMN(seed,ntot,/DOUBLE)
     cat.flux2 = cat.flux1 * EXP( temporary(gval) ) > minflux2
  ENDELSE

  ;;Randomly assign each flux to an image pixel
  IF has_clustering THEN BEGIN
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,"Generating clustered source positions",/INF
     prob_im = GENERATE_SOURCE_PROB( k, pk, genpixperside, $
                                     PIXSIZE=pixscale1, SEED=seed )
     SMAP_POPULATE_SOURCES, TEMPORARY(prob_im), ntot, xpos, ypos, SEED=seed
     cat.xpos = TEMPORARY(xpos)
     cat.ypos = TEMPORARY(ypos)
  ENDIF ELSE BEGIN
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,"Generating randomized source positions",/INF
     cat.xpos = FLOOR( RANDOMU( seed, ntot ) * genpixperside )
     cat.ypos = FLOOR( RANDOMU( seed, ntot ) * genpixperside )
  ENDELSE
  
  ;;Can't do this 'in parallel' because IDL can't handle
  ;; multiple sources ending up in the same pixel
  ;;That is,
  ;; image[cat.xpos,cat.ypos] += cat.flux
  ;;doesn't work, and worse, appears to work while
  ;; quietly doing the wrong thing.  
  FOR j=0,ntot-1 DO BEGIN
     image1[ cat[j].xpos, cat[j].ypos ] += cat[j].flux1
     image2[ cat[j].xpos, cat[j].ypos ] += cat[j].flux2
  ENDFOR

  IF KEYWORD_SET(verbose) THEN $
     PRINT,"Brightest source band 1: ",MAX(cat.flux1)," band2: ",$
           MAX(cat.flux2)

  ;;Convolve
  MESSAGE,"Convolving",/INF
  image1=CONVOLVE(image1,kernel1)
  image2=CONVOLVE(image2,kernel2)

  ;;Rebin
  IF final_rebin THEN BEGIN
     image1 = REBIN(image1,npixperside,npixperside)
     image2 = REBIN(image2,npixperside,npixperside)
     cat.xpos /= rebinfac
     cat.ypos /= rebinfac
  ENDIF

  IF N_ELEMENTS(catfile) NE 0 THEN BEGIN
     MWRFITS, TEMPORARY(cat), catfile, /SILENT, /CREATE
  ENDIF ELSE a=N_ELEMENTS(TEMPORARY(cat))

  ;;Add noise
  IF sigma1 GT 0.0 THEN image1 += sigma1*RANDOMN(seed,npixperside,npixperside)
  IF sigma2 GT 0.0 THEN image2 += sigma2*RANDOMN(seed,npixperside,npixperside)

  mnval1 = MEAN(image1)
  PRINT,"Realized mean per pixel, band 1: ",mnval1
  image1 -= mnval1
  mnval2 = MEAN(image2)
  PRINT,"Realized mean per pixel, band 2: ",mnval2
  image2 -= mnval2

  do_mask = ranmask GT 0
  IF do_mask THEN BEGIN
     ;; This doesn't work correctly because it ignores overlaps
     ;; in the randomly generated sample.  But that should be okay in practice
     totpix = npixperside * npixperside
     nmask = ROUND(0.01 * ranmask * totpix) < totpix
     maskidx = ROUND(totpix * RANDOMU(seed, nmask)) < (totpix-1) > 0
  ENDIF

  ;;Write to SMAP structures
  mapstruct = get_smap_mapstruct(NPIXX=npixperside,NPIXY=npixperside,$
                                 NOMASK=~do_mask,/NOEXP,/NOERR,/SILENT,$
                                 ERRMSG=errmsg, SUCCESS=getmap_succ,$
                                 /NO_ABORT, BAND=band1, LAMBDA=lambda1, /SMALL)
  IF ~ getmap_succ THEN MESSAGE,"Error getting structure: "+errmsg

  mapstruct.image = TEMPORARY(image1)
  mapstruct.astrometry.cd[0,0] = -pixscale1/3600.0
  mapstruct.astrometry.cd[1,1] = pixscale1/3600.0
  mapstruct.astrometry.cdelt = [1,1]
  mapstruct.astrometry.crpix = [npixperside/2,npixperside/2]
  mapstruct.astrometry.crval = [racen,deccen]
  mapstruct.pixscale = pixscale1
  IF do_mask THEN mapstruct.mask[maskidx] = 1
  success = WRITE_SMAP_FITSMAP(mapstruct,outfile,DIR='.',ERRMSG=errmsg)
  IF ~ success THEN MESSAGE,"Error writing output file1: "+errmsg

  mapstruct = get_smap_mapstruct(NPIXX=npixperside,NPIXY=npixperside,$
                                 NOMASK=~do_mask,/NOEXP,/NOERR,/SILENT,$
                                 ERRMSG=errmsg, SUCCESS=getmap_succ, /SMALL,$
                                 /NO_ABORT, BAND=band2, LAMBDA=lambda2)
  IF ~ getmap_succ THEN MESSAGE,"Error getting structure: "+errmsg
  mapstruct.image = TEMPORARY(image2)
  IF final_rebin THEN BEGIN
     mapstruct.astrometry.cd[0,0] = -rebinfac*pixscale1/3600.0
     mapstruct.astrometry.cd[1,1] = rebinfac*pixscale1/3600.0
  ENDIF ELSE BEGIN
     mapstruct.astrometry.cd[0,0] = -pixscale1/3600.0
     mapstruct.astrometry.cd[1,1] = pixscale1/3600.0
  ENDELSE
  mapstruct.astrometry.cdelt = [1,1]
  mapstruct.astrometry.crpix = [npixperside/2,npixperside/2]
  mapstruct.astrometry.crval = [racen,deccen]
  mapstruct.pixscale = pixscale1
  IF do_mask THEN mapstruct.mask[maskidx] = 1
  success = WRITE_SMAP_FITSMAP(mapstruct,outfile,DIR='.',ERRMSG=errmsg)
  IF ~ success THEN MESSAGE,"Error writing output file2: "+errmsg
END
