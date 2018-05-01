;+
; NAME
;  redsource_bethermin
; PURPOSE
;  Create a set of fake catalogs and images using the Bethermin 2010
;  model but supplemented with some additional red sources
; USAGE
;  redsource_bethermin, dndlogldzdomega, cold, starburst, 
;                       redparams, redtplfile, catalog, map250, map350, map500,
;                       PIXSCALE=, AREA=, K=, PK=,
;                       NFLUXES=,SEED=,LOCALZ=, REBINFAC=,
;                       RACEN=,DECCEN=,SIGMAS=
; INPUTS
;  dndlogldzdomega Output of bethermin_dndlogldzdomega
;  cold          Holds information about IAS cold templates
;  starburst     Holds information about IAS starburst templates
;  redparams     Structure holding information about red sources
;                 to simulate with fields:
;                   .n      -- number of sources
;                   .zmin   -- min z
;                   .zmax   -- max z
;                   .min500 -- minimum 500 micron flux [in mJy]
;                   .max500 -- maximum 500 micron flux [in mJy]
;                   .slope  -- Power law slope of red source fluxes (def: 0)
;  redtplfile    File containing red source template in wave fnu
;                 format, with wave in microns
; OPTIONAL INPUTS
;  area          The area of the image (in sq. deg).  May not be
;                 perfectly realized on output
;  pixscale      Output pixel scale in arcsec (def: 4 arcsec)
;  rebinfac      Factor to rebin output maps by.  Must be integral
;  sigmas        Array of Gaussian noise added on a per-pixel basis in
;                 each band [in Jy]
;  racen/deccen  RA/DEC of center in decimal degrees (def: 120/10)
;  nfluxes       Number of points to generate cumulative flux
;                 distribution at (def: 65536)
;  seed          Seed for random number generator

;  localz        Set this to mask out sources at lower z than this
;                 on the assumption they wouldn't be included anyways
;  maxzbeth      Maximum bethermin z
;  k             k values for P(k), in inverse arcmin
;  pk            P(k) -- if this (and k) are provided, then the
;                        sources are distributed with clustering
;                        according to P(k)
; OUTPUTS
;  catalog       Catalog containing source information
;  map250        Simulated 250 micron map
;  map350        Simulated 350 micron map
;  map500        Simulated 500 micron map
; KEYWORDS
;  fixntot       Fix the total number of sources instead of treating
;                 it as poisson distributed
;  notruearea    Don't adjust ntot for realized area -- useful when
;                 generating multiple images with same seed
;  verbose       Prints status messages
;  nomeansub     Don't do mean subtraction
; NOTES
;   The images are mean subtracted.
;   The simulated red sources are uniformly distributed in z and 500
;   micron flux between the values given in redparams
; MODIFICATION HISTORY
;  Author: Alex Conley, Oct 7, 2011
;-

PRO redsource_bethermin, dndlogldzdomega, cold, starburst, $
                         redparams, redtplfile, cat, map250, $
                         map350, map500, PIXSCALE=pixscale, AREA=area, $
                         K=k, PK=pk, NFLUXES=nfluxes, SEED=seed, LOCALZ=localz,$
                         REBINFAC=rebinfac, RACCEN=raccen, DECCEN=deccen,$
                         SIGMAS=sigmas, FIXNTOT=fixntot, NOTRUEAREA=notruearea,$
                         MAXZBETH=maxzbeth, NOSMOOTH=nosmooth, $
                         NOMEANSUB=nomeansub, VERBOSE=verbose
  
  COMPILE_OPT IDL2, STRICTARRSUBS

  ;;Input parse and defaults
  IF N_ELEMENTS(racen) EQ 0 THEN racen = 120.0
  IF N_ELEMENTS(deccen) EQ 0 THEN deccen = 10.0
  IF N_ELEMENTS(nfluxes) EQ 0 THEN nfluxes = 65536
  IF nfluxes LE 0 THEN MESSAGE,"Invalid (non-positive) nfluxes "+STRING(nfluxes)
  IF N_ELEMENTS(area) EQ 0 THEN area=1.0
  IF area LE 0 THEN MESSAGE,"Invalid area "+STRING(area)
  IF N_ELEMENTS(sigmas) EQ 0 THEN i_sigmas = REPLICATE(0.0,3)
  IF N_ELEMENTS(sigmas) EQ 1 THEN i_sigmas = REPLICATE(sigmas,3)
  IF N_ELEMENTS(sigmas) GE 3 THEN i_sigmas = sigmas[0:2]
  IF N_ELEMENTS(i_sigmas) NE 3 THEN $
     MESSAGE,"Number of sigmas must match number of SPIRE bands (3)"
  IF MIN(i_sigmas) LT 0.0 THEN MESSAGE,"Invalid (non-positive) sigma value"
  IF N_ELEMENTS(k) NE 0 THEN BEGIN
     IF N_ELEMENTS(pk) NE N_ELEMENTS(k) THEN $
        MESSAGE,"pk and k must be same length"
     has_clustering = 1b
  ENDIF ELSE has_clustering=0b
  IF N_ELEMENTS(pixscale) EQ 0 THEN i_pixscale = 4.0 ELSE $
     i_pixscale = pixscale
  IF i_pixscale LE 0 THEN MESSAGE,"Invalid (non-positive) pixel scale"
  IF SIZE(redparams,/TNAME) NE 'STRUCT' THEN $
     MESSAGE,"Input red source parameters not valid"
  IF ~ FILE_TEST( redtplfile, /READ ) THEN $
     MESSAGE,"Unable to read red template file"

  bandname = ['PSW','PMW','PLW']
  wave = [250,350,500] ;;SPIRE wavelengths
  beamsizes = [17.8, 24.0, 35.3]
  ;;Now try to figure out how large to make the output maps
  ;; this depends if we are going to rebin or not
  IF N_ELEMENTS(rebinfac) NE 0 && ABS(rebinfac-1.0) GT 1d-5 THEN $
     final_rebin = 1b ELSE final_rebin = 0b
  IF final_rebin THEN BEGIN
     rebinfac = FIX(rebinfac) ;;Make integer
     npixperside = ROUND(SQRT(area)*3600.0/i_pixscale)
     genpixperside = npixperside*rebinfac
     genpixscale = i_pixscale / rebinfac
  ENDIF ELSE BEGIN
     npixperside = ROUND(SQRT(area)*3600.0/i_pixscale)
     genpixperside = npixperside
     genpixscale = i_pixscale
  ENDELSE
  truearea = (genpixperside*genpixscale/3600.0)^2

  IF KEYWORD_SET(verbose) THEN $
     MESSAGE,STRING(truearea,FORMAT='("Map area will be: ",F0.1," sq deg")'),$
             /INF

  ;;Figure out how crowded the redsources are going to be, require
  ;; less than 0.1 per beam
  IF (redparams.n GT 0) THEN BEGIN
     bmarea = !PI/4.0*(beamsizes[2]/3600.0)^2 ;;approximate, but that's okay
     nbeams = truearea/bmarea
     IF redparams.n / nbeams GT 0.1 THEN $
        MESSAGE,"Too many red sources!"
  ENDIF

  ;;nbeth is the number of Bethermin sources
  nbeth = dndlogldzdomega.totsources * (!PI/180.0)^2 ;;to sq deg
  IF KEYWORD_SET(notruearea) THEN BEGIN
     nbeth *= area 
  ENDIF ELSE nbeth *= truearea
  IF nbeth EQ 0 THEN MESSAGE,"Error: no sources to generate"
  IF ~ KEYWORD_SET( fixnbeth ) THEN $
     nbeth = RANDOMU(seed,POISSON=nbeth,/DOUBLE)
  nbeth = ROUND(nbeth)
  IF nbeth EQ 0 THEN MESSAGE,"Not generating any sources!"
  IF KEYWORD_SET(verbose) THEN $
     MESSAGE,STRING(nbeth,FORMAT='("Generating ",I0," Bethermin sources")'),/INF

  ntot = nbeth + redparams.n

  ;;Make catalog
  IF KEYWORD_SET(verbose) THEN MESSAGE,"Generating Bethermin catalog",/INF
  cat = REPLICATE( {xpos: 0, ypos: 0, z: !VALUES.F_NAN, loglum: !VALUES.F_NAN,$
                    type: 0b, red: 0b, true_fluxes: FLTARR(3),$
                    obs_fluxes: FLTARR(3) }, ntot )

  ;;Bethermin sources
  bcat = bethermin_gencat( nbeth, dndlogldzdomega, cold, starburst, $
                           SEED=seed, WAVE=wave )
  cat[0:nbeth-1].z = bcat.z
  cat[0:nbeth-1].loglum = bcat.loglum
  cat[0:nbeth-1].type = bcat.type
  cat[0:nbeth-1].true_fluxes = bcat.obsflux*1d-3 ;;to Jy from mJy
  st = N_ELEMENTS(temporary(bcat))

  ;;Red sources
  IF KEYWORD_SET(verbose) THEN $
     MESSAGE,STRING(redparams.n,FORMAT='("Generating ",I0," red sources")'),$
             /INF
  IF redparams.n GT 0 THEN BEGIN
     READCOL,redtplfile, redwave, redfnu, FORMAT='(D,D)',/SILENT
     IF N_ELEMENTS(redwave) LT 10 THEN $
        MESSAGE,"Not enough coverage in red template"
     cat[nbeth:*].z = (redparams.zmax-redparams.zmin)*$
                      RANDOMU(seed,redparams.n)+$
                      redparams.zmin
     opz = 1.0 + cat[nbeth:*].z
     IF ABS(redparams.slope) LT 1e-3 THEN BEGIN
        cat[nbeth:*].true_fluxes[2] = (redparams.max500-redparams.min500)*$
                                      RANDOMU(seed,redparams.n)+redparams.min500
     ENDIF ELSE BEGIN
        RANDOMP, f500, redparams.slope, redparams.n, $
                 RANGE_X=[redparams.min500,redparams.max500],$
                 SEED=seed
        cat[nbeth:*].true_fluxes[2] = TEMPORARY(f500)
     ENDELSE
     cat[nbeth:*].true_fluxes[2] *= 1e-3 ;; to mJy
     scalfac = cat[nbeth:*].true_fluxes[2]/$
               INTERPOL(redfnu, redwave, wave[2]/opz )
     cat[nbeth:*].true_fluxes[0] = scalfac*$
                                   INTERPOL(redfnu, redwave, wave[0]/opz )
     cat[nbeth:*].true_fluxes[1] = TEMPORARY(scalfac)*$
                                   INTERPOL(TEMPORARY(redfnu), $
                                            TEMPORARY(redwave), $
                                            wave[1]/TEMPORARY(opz))
     cat[nbeth:*].red = 1b
  ENDIF

  ;;Get positions of each source
  IF has_clustering THEN BEGIN
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,"Generating clustered source positions",/INF
     prob_im = GENERATE_SOURCE_PROB( k, pk, genpixperside, $
                                     PIXSIZE=genpixscale, SEED=seed )
     SMAP_POPULATE_SOURCES, TEMPORARY(prob_im), ntot, xpos, ypos, SEED=seed
     cat.xpos = TEMPORARY(xpos)
     cat.ypos = TEMPORARY(ypos)
  ENDIF ELSE BEGIN
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,"Generating un-clustered source positions",/INF
     cat.xpos = FLOOR( RANDOMU( seed, ntot ) * genpixperside )
     cat.ypos = FLOOR( RANDOMU( seed, ntot ) * genpixperside )
  ENDELSE


  IF N_ELEMENTS(localz) NE 0 THEN BEGIN
     ;;Don't screen red sources
     wgood = WHERE(cat.z GE localz[0] OR cat.red, ntot, NCOMPLEMENT=nbad)
     IF ntot EQ 0 THEN MESSAGE,"Local z masked all sources"
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,STRING(nbad,FORMAT='("Local z cut masking: ",I0," sources")'),$
                /INF
     cat = cat[wgood]
  ENDIF

  IF N_ELEMENTS(maxzbeth) NE 0 THEN BEGIN
     wgood = WHERE(cat.z LE maxzbeth OR cat.red,ntot,NCOMPLEMENT=nbad)
     IF ntot EQ 0 THEN MESSAGE,"maxzbeth masked all sources"
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,STRING(nbad,FORMAT='("maxzbeth cut masking: ",I0," sources")'),$
                /INF
     cat = cat[wgood]
  ENDIF

  ;;Now, generate the image in each band
  FOR i=0,3-1 DO BEGIN
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,STRING(wave[i],beamsizes[i],$
                       FORMAT='("Doing image with wave: ",I0,'+$
                       '" beamsize: ",F0.1)'),/INF

     ;;Get the beam
     kernel = GET_SPIRE_BEAM(bandname[i],genpixscale,$
                             FWHM=beamsizes[i],/SILENT)
     image = DBLARR(genpixperside,genpixperside)

     karea = TOTAL(kernel)

     ;;Can't do this 'in parallel' because IDL can't handle
     ;; multiple sources ending up in the same pixel
     ;;That is,
     ;; image[cat.xpos,cat.ypos] += cat.flux
     ;;doesn't work, and worse, appears to work while
     ;; quietly doing the wrong thing.
     FOR j=0,ntot-1 DO $
        image[ cat[j].xpos, cat[j].ypos ] += cat[j].true_fluxes[i]

     ;;Convolve
     IF KEYWORD_SET(verbose) THEN MESSAGE," Convolving",/INF
     image=CONVOLVE(image,kernel)

     ;;Rebin
     IF final_rebin THEN image = REBIN(image,npixperside,npixperside)

     ;;add noise
     IF i_sigmas[i] GT 0.0 THEN $
        image += i_sigmas[i]*RANDOMN(seed,npixperside,npixperside)

     ;;Find noise estimate
     IF i_sigmas[i] GT 0.0 THEN BEGIN
        IF final_rebin THEN karea /= rebinfac^2
        noiseval = SQRT( (i_sigmas[i] / SQRT(karea))^2)
     ENDIF ELSE noiseval = 0.0
     ;;Conf noise  
     IF ~ KEYWORD_SET( noconfusion ) THEN BEGIN
        conf_noise = [5.5e-3,6.0e-3,5.2e-3]
        IF ~ KEYWORD_SET( nosmooth ) THEN $
           conf_noise *= SQRT(2)*35.3 / [17.8, 24.0, 35.3]
        noiseval = SQRT( noiseval^2 + conf_noise[i]^2 )
     ENDIF

     ;;Add noise into simulated fluxes
     cat.obs_fluxes[i] = cat.true_fluxes[i] + noiseval*RANDOMN(seed,ntot)     
     
     ;;Write to SMAP structures
     mapstruct = get_smap_mapstruct(NPIXX=npixperside,NPIXY=npixperside,$
                                    /NOMASK,/NOEXP,/SILENT,$
                                    ERRMSG=errmsg, SUCCESS=getmap_succ,$
                                    /NO_ABORT, BAND=bandname[i], $
                                    LAMBDA=wave[i])
     IF ~ getmap_succ THEN MESSAGE,"Error getting structure: "+errmsg
     mapstruct.error = REPLICATE(sigmas[i],npixperside,npixperside)

     mapstruct.image = TEMPORARY(image)
     mapstruct.astrometry.cdelt = [1,1]
     mapstruct.astrometry.crpix = [npixperside/2,npixperside/2]
     mapstruct.astrometry.crval = [racen,deccen]
     mapstruct.pixscale = i_pixscale
     IF final_rebin THEN BEGIN
        mapstruct.astrometry.cd[0,0] = -rebinfac*genpixscale/3600.0
        mapstruct.astrometry.cd[1,1] = rebinfac*genpixscale/3600.0
     ENDIF ELSE BEGIN
        mapstruct.astrometry.cd[0,0] = -i_pixscale/3600.0
        mapstruct.astrometry.cd[1,1] = i_pixscale/3600.0
     ENDELSE

     ;;Smooth
     IF ~ KEYWORD_SET(nosmooth) THEN BEGIN
        IF KEYWORD_SET(verbose) THEN MESSAGE," Smoothing",/INF
        smap_redsource_smooth, mapstruct, VERBOSE=verbose, /NOMEANSUB
     ENDIF
        
     ;;Deal with mean sub, assume it affects catalog equally
     IF ~ KEYWORD_SET( nomeansub ) THEN BEGIN
        mnval = MEAN(mapstruct.image,/NAN)
        IF KEYWORD_SET(verbose) THEN $
           MESSAGE, STRING(bandname[i],mnval,$
                           FORMAT='("Realized mean per pixel, band : ",A0,'+$
                           '" is ",F0.5," Jy")'),/INF
        mapstruct.image -= mnval
        cat.obs_fluxes[i] -= mnval
     ENDIF

     CASE bandname[i] OF 
        'PSW' : map250 = TEMPORARY(mapstruct)
        'PMW' : map350 = TEMPORARY(mapstruct)
        'PLW' : map500 = TEMPORARY(mapstruct)
        ELSE : MESSAGE,"Unknown band: "+bandname[i]
     ENDCASE

  ENDFOR

  ;;Output catalog
  IF final_rebin THEN BEGIN
     cat.xpos /= rebinfac
     cat.ypos /= rebinfac
  ENDIF

END
