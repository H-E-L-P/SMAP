;+
; NAME
;  redsource_bethermin_noim
; PURPOSE
;  Create a set of fake catalogs using the Bethermin 2010
;  model but supplemented with some additional red sources
;  and 'confusion sources' simulating map fluctuations
; USAGE
;  cat = redsource_bethermin_noim( dndlogldzdomega, cold, starburst, 
;                                  redparams, redtplfile,
;                                  AREA=, NFLUXES=,SEED=,LOCALZ=
;                                  SIGMAS=, LENSPROB=, /VERBOSE )
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
;  sigmas        Array of Gaussian noise added on a per-beam basis in
;                 each band [in Jy]
;  nfluxes       Number of points to generate cumulative flux
;                 distribution at (def: 65536)
;  seed          Seed for random number generator

;  localz        Set this to mask out sources at lower z than this
;                 on the assumption they wouldn't be included anyways
;  maxzbeth      Maximum bethermin z
;  lensprob      Lensing probability model for bethermin_gencat
; OUTPUTS
;  catalog       Catalog containing source information
; KEYWORDS
;  fixntot       Fix the total number of sources instead of treating
;                 it as poisson distributed
;  notruearea    Don't adjust ntot for realized area -- useful when
;                 generating multiple images with same seed
;  verbose       Prints status messages
; NOTES
;   The simulated red sources are uniformly distributed in z and as 
;   a power law in 500 micron flux between the values given in redparams
; MODIFICATION HISTORY
;  Author: Alex Conley, Oct 18, 2011
;-

FUNCTION redsource_bethermin_noim, dndlogldzdomega, cold, starburst, $
                                   redparams, redtplfile, AREA=area, $
                                   NFLUXES=nfluxes, SEED=seed, LOCALZ=localz,$
                                   SIGMAS=sigmas, FIXNTOT=fixntot, $
                                   NOTRUEAREA=notruearea,$
                                   MAXZBETH=maxzbeth, VERBOSE=verbose,$
                                   LENSPROB=lensprob
  
  COMPILE_OPT IDL2, STRICTARRSUBS

  ;;Input parse and defaults
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
  IF SIZE(redparams,/TNAME) NE 'STRUCT' THEN $
     MESSAGE,"Input red source parameters not valid"
  IF ~ FILE_TEST( redtplfile, /READ ) THEN $
     MESSAGE,"Unable to read red template file"

  bandname = ['PSW','PMW','PLW']
  wave = [250,350,500] ;;SPIRE wavelengths
  beamsizes = [17.8, 24.0, 35.3]
  convbeamsize = beamsizes[2]*SQRT(2)

  ;;Now try to figure out how large to make the output maps
  ;;4 arcsec pixels
  npixperside = ROUND(SQRT(area)*3600.0/4.0)
  truearea = (npixperside*4.0/3600.0)^2
  
  IF KEYWORD_SET(verbose) THEN $
     MESSAGE,STRING(truearea,FORMAT='("Map area will be: ",F0.1," sq deg")'),$
             /INF
  
  ;;Figure out how crowded the redsources are going to be, require
  ;; less than 0.1 per beam
  bmarea = TOTAL( get_spire_beam('PSW',1.0,FWHM=convbeamsize,/SILENT) ) / $
           3600.0^2
  IF (redparams.n GT 0) THEN BEGIN
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

  ;;nconf is the number of confusion sources
  nconf = ROUND( truearea / bmarea )
  
  ntot = nbeth + redparams.n + nconf

  ;;Make catalog
  cat = REPLICATE( {z: !VALUES.F_NAN, loglum: !VALUES.F_NAN,$
                    type: 0b, red: 0b, conf: 0b, true_fluxes: FLTARR(3),$
                    obs_fluxes: FLTARR(3) }, ntot )

  ;;Bethermin sources
  bcat = bethermin_gencat( nbeth, dndlogldzdomega, cold, starburst, $
                           SEED=seed, WAVE=wave, LENSPROB=lensprob )
  cat[0:nbeth-1].z = bcat.z
  cat[0:nbeth-1].loglum = bcat.loglum
  cat[0:nbeth-1].type = bcat.type
  cat[0:nbeth-1].true_fluxes = bcat.obsflux*1d-3 ;;to Jy from mJy
  st = N_ELEMENTS(temporary(bcat))

  ;;Red sources
  ntop = nbeth + redparams.n - 1
  IF KEYWORD_SET(verbose) THEN $
     MESSAGE,STRING(redparams.n,FORMAT='("Generating ",I0," red sources")'),$
             /INF
  IF redparams.n GT 0 THEN BEGIN
     READCOL,redtplfile, redwave, redfnu, FORMAT='(D,D)',/SILENT
     IF N_ELEMENTS(redwave) LT 10 THEN $
        MESSAGE,"Not enough coverage in red template"
     cat[nbeth:ntop].z = (redparams.zmax-redparams.zmin)*$
                      RANDOMU(seed,redparams.n)+$
                      redparams.zmin
     opz = 1.0 + cat[nbeth:ntop].z
     IF ABS(redparams.slope) LT 1e-3 THEN BEGIN
        cat[nbeth:ntop].true_fluxes[2] = (redparams.max500-redparams.min500)*$
                                         RANDOMU(seed,redparams.n)+$
                                         redparams.min500
     ENDIF ELSE BEGIN
        RANDOMP, f500, redparams.slope, redparams.n, $
                 RANGE_X=[redparams.min500,redparams.max500],$
                 SEED=seed
        cat[nbeth:ntop].true_fluxes[2] = TEMPORARY(f500)
     ENDELSE
     cat[nbeth:ntop].true_fluxes[2] *= 1e-3 ;; to mJy
     scalfac = cat[nbeth:ntop].true_fluxes[2]/$
               INTERPOL(redfnu, redwave, wave[2]/opz )
     cat[nbeth:ntop].true_fluxes[0] = scalfac*$
                                      INTERPOL(redfnu, redwave, wave[0]/opz )
     cat[nbeth:ntop].true_fluxes[1] = TEMPORARY(scalfac)*$
                                      INTERPOL(TEMPORARY(redfnu), $
                                               TEMPORARY(redwave), $
                                               wave[1]/TEMPORARY(opz))
     cat[nbeth:ntop].red = 1b
  ENDIF

  ;;Confusion sources
  conf_noise = [5.8,6.3,6.8]*1e-3*convbeamsize/beamsizes
  IF KEYWORD_SET( verbose ) THEN $
     MESSAGE,STRING(nconf,FORMAT='("Generating ",I0," confusion sources")'),$
             /INF
  IF nconf GT 0 THEN BEGIN
     ;;Treat as Gaussian, although this is a horrible approximation
     nbot = nbeth + redparams.n
     ntop = nbot + nconf - 1
     cat[nbot:ntop].conf = 1b
     cat[nbot:ntop].true_fluxes[0] = conf_noise[0]*RANDOMN(seed,nconf)
     cat[nbot:ntop].true_fluxes[1] = conf_noise[1]*RANDOMN(seed,nconf)
     cat[nbot:ntop].true_fluxes[2] = conf_noise[2]*RANDOMN(seed,nconf)
  ENDIF

  IF N_ELEMENTS(localz) NE 0 THEN BEGIN
     ;;Don't screen red or confusion sources
     wgood = WHERE(cat.z GE localz[0] OR cat.red OR cat.conf, ntot, $
                   NCOMPLEMENT=nbad)
     IF ntot EQ 0 THEN MESSAGE,"Local z masked all sources"
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,STRING(nbad,FORMAT='("Local z cut masking: ",I0," sources")'),$
                /INF
     cat = cat[wgood]
  ENDIF

  IF N_ELEMENTS(maxzbeth) NE 0 THEN BEGIN
     wgood = WHERE(cat.z LE maxzbeth OR cat.red OR cat.conf, ntot,$
                   NCOMPLEMENT=nbad)
     IF ntot EQ 0 THEN MESSAGE,"maxzbeth masked all sources"
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,STRING(nbad,FORMAT='("maxzbeth cut masking: ",I0," sources")'),$
                /INF
     cat = cat[wgood]
  ENDIF

  
  ;;Add noise into simulated fluxes
  wnotconf = WHERE( ~ cat.conf, nnotconf, COMPLEMENT=wconf,$
                    NCOMPLEMENT=nconf)
  IF nnotconf NE 0 THEN BEGIN
     FOR i=0,3-1 DO BEGIN
        ;;both conf and instrument
        noiseval = SQRT( conf_noise[i]^2 + i_sigmas[i]^2 )
        cat[wnotconf].obs_fluxes[i] = cat.true_fluxes[i] + $
                            noiseval*RANDOMN(seed,nnotconf)
     ENDFOR
  ENDIF
  IF nconf NE 0 THEN BEGIN
     ;;confused 'sources' get only instrument noise
     FOR i=0,3-1 DO BEGIN
        cat[wnotconf].obs_fluxes[i] = cat.true_fluxes[i] + $
                            i_sigmas[i]*RANDOMN(seed,nnotconf)
     ENDFOR
  ENDIF

  RETURN,cat
END
