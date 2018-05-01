;+
; NAME
;  redsource_optimize_bethermin
; PURPOSE
;  Find the purity for a set of input map combinations based
;  on the redsource_bethermin model.  The map combination model
;  is k1*map_250 + k2*map_350 + sqrt(1-k1^2-k2^2)*map_500
; USAGE
;  purityarr = 
;   redsource_optimize_bethermin( nk1, mink1, maxk1, nk2, mink2,
;             maxk2, completeness, dndlogldzdomega, cold, starburst, redparams,
;             redtplfile, CATFILE=, MAPBASE=, OUTDIR=, PIXSCALE=, AREA=,$
;             K=, PK=, SEED=, LOCALZ=, REBINFAC=, SIGMAS=)
; INPUTS
;  nk1             Number of k1 values
;  mink1           Minimum k1 value
;  maxk1           Maximum k1
;  nk2/mink2/maxk2 Same but for k2
;  completeness    Array of fractional completeness to test down to
;  dndlogldzdomega Output of bethermin_dndlogldzdomega
;  cold            Holds information about IAS cold templates
;  starburst       Holds information about IAS starburst templates
;  redparams       Structure holding information about red sources
;                   to simulate with fields:
;                    .n      -- number of sources
;                    .zmin   -- min z
;                    .zmax   -- max z
;                    .min500 -- minimum 500 micron flux [in mJy]
;                    .max500 -- maximum 500 micron flux [in mJy]
;                    .slope  -- Power law slope of red sources (def: 0)
;  redtplfile      File containing red source template in wave fnu
;                   format, with wave in microns
; OUTPUTS
;  A structure giving the purity as a function of completeness and the
;  k values
; OPTIONAL INPUTS
;  catfile         File name to write catalog to
;  mapbase         Base name to writ maps to
;  outdir          Output directory (default: '.')
;  area            The area of the image (in sq. deg).  May not be
;                   perfectly realized on output
;  pixscale        Output pixel scale in arcsec (def: 4 arcsec)
;  rebinfac        Factor to rebin output maps by.  Must be integral
;  sigmas          Array of Gaussian noise added on a per-pixel basis in
;                   each band [in Jy]
;  seed            Seed for random number generator
;  localz          Set this to mask out sources at lower z than this
;                   on the assumption they wouldn't be included anyways
;  k               k values for P(k), in inverse arcmin
;  pk              P(k) -- if this (and k) are provided, then the
;                         sources are distributed with clustering
;                         according to P(k)
; KEYWORDS
;  verbose         Print informational messages as it runs
;  nomeansub       Don't do mean subtraction on simulated maps so that
;                   sources stay positive
;  noconfusion     Don't add confusion noise to flux estimates
; NOTES
;  See redsource_bethermin and smap_generate_bethermin for more
;  details of the model.  While this internally produces maps, in
;  the current implementation the map differencing is done on the
;  catalog.  Maybe in the future it will be done directly on the
;  maps.
; MODIFICATION HISTORY
;  Author: Alex Conley, Oct 7, 2011
;-

FUNCTION redsource_optimize_bethermin, nk1, mink1, maxk1, nk2, mink2,$
                                       maxk2, completeness, dndlogldzdomega, $
                                       cold, starburst, redparams, redtplfile, $
                                       CATFILE=catfile, MAPBASE=mapbase, $
                                       OUTDIR=outdir, PIXSCALE=pixscale, $
                                       AREA=area, K=k, PK=pk, SEED=seed, $
                                       LOCALZ=localz, REBINFAC=rebinfac, $
                                       NOMEANSUB=nomeansub, SIGMAS=sigmas, $
                                       VERBOSE=verbose, NOCONFUSION=noconfusion
  COMPILE_OPT IDL2, STRICTARRSUBS

  IF SIZE(redparams,/TNAME) NE 'STRUCT' THEN $
     MESSAGE,"Input red source parameters not valid"
  IF redparams.n LT 10 THEN MESSAGE,"Insufficient red sources to test on!"
  
  ncomplete = N_ELEMENTS(completeness)
  IF ncomplete EQ 0 THEN MESSAGE,"No completeness levels!"
  IF MIN(completeness) LE 0.0 THEN $
     MESSAGE,"Invalid (non-positive) completeness level"
  IF MAX(completeness) GE 1.0 THEN $
     MESSAGE,"Invalid (>=1) completeness level"
  IF nk1 LE 0 THEN MESSAGE,"nk1 must be positive"
  IF mink1 LT -1.0 THEN MESSAGE,"Invalid (<-1) mink1"
  IF maxk1 GT 1.0 THEN MESSAGE,"Invalid (>1) maxk1"
  IF nk2 LE 0 THEN MESSAGE,"nk2 must be positive"
  IF mink2 LT -1.0 THEN MESSAGE,"Invalid (<-1) mink2"
  IF maxk2 GT 1.0 THEN MESSAGE,"Invalid (>1) maxk2"
  IF N_ELEMENTS(sigmas) EQ 0 THEN i_sigmas = REPLICATE(0.005,3)
  IF N_ELEMENTS(sigmas) EQ 1 THEN i_sigmas = REPLICATE(sigmas,3)
  IF N_ELEMENTS(sigmas) GE 3 THEN i_sigmas = sigmas[0:2]
  IF N_ELEMENTS(i_sigmas) NE 3 THEN $
     MESSAGE,"Number of sigmas must match number of SPIRE bands (3)"
  IF N_ELEMENTS( outdir ) EQ 0 THEN i_outdir = './' ELSE $
     i_outdir = addslash( outdir )

  ;;Get constants for map construction, start building output structure
  ;;Construct output structure
  purity = { nk1: nk1, nk2: nk2, k1: (maxk1-mink1)*FINDGEN(nk1)/(nk1-1)+mink1,$
             k2: (maxk2-mink2)*FINDGEN(nk2)/(nk2-1)+mink2,$
             valid: BYTARR(nk1, nk2), dvalue: FLTARR(ncomplete,nk1,nk2),$
             ncomplete: ncomplete,$
             nsources: redparams.n, ngen: 0,$
             completeness: completeness, $
             purity: FLTARR( ncomplete, nk1, nk2 ) }

  purity.valid = 1b
  purity.purity = !VALUES.F_NAN
  k500 = REPLICATE(!VALUES.F_NAN,nk1,nk2)
  k500sq = FLTARR(nk2)
  FOR i=0,nk1-1 DO BEGIN
     k500sq = 1.0-purity.k1[i]^2-purity.k2^2
     wbad = WHERE( k500sq LT 0.0, nbad, COMPLEMENT=wgood,$
                   NCOMPLEMENT=ngood)
     IF nbad NE 0 THEN purity.valid[i,wbad] = 0b
     IF ngood NE 0 THEN k500[i,wgood] = SQRT(k500sq[wgood])
  ENDFOR
  st = WHERE( purity.valid, nvalid )
  IF nvalid EQ 0 THEN MESSAGE,"No valid subtractions found"
  IF KEYWORD_SET(verbose) THEN $
     MESSAGE,STRING(nvalid,FORMAT='("Number valid ks to test: ",I0)'),/INF
  
  ;;Build base maps and catalog
  IF KEYWORD_SET( verbose ) THEN MESSAGE,"Building base map and catalog",/INF
  minzred = MIN( [redparams.zmin, redparams.zmax] )
  redsource_bethermin, dndlogldzdomega, cold, starburst, redparams,$
                       redtplfile, cat, map250, map350, map500,$
                       PIXSCALE=pixscale, AREA=area, K=k, PK=pk,$
                       SEED=seed, LOCALZ=localz, MAXZBETH=minzred, $
                       REBINFAC=rebinfac, SIGMAS=i_sigmas, VERBOSE=verbose,$
                       NOMEANSUB=nomeansub
  purity.ngen = N_ELEMENTS(cat) 
  wred = WHERE( cat.red, nred, COMPLEMENT=wnotred, NCOMPLEMENT=nnotred )
  IF nnotred EQ 0 THEN MESSAGE,"No non-red sources!"

  ;;Save them if asked to
  IF N_ELEMENTS( catfile ) NE 0 THEN $
     MWRFITS, cat, i_outdir + catfile, /CREATE
  IF N_ELEMENTS( mapbase ) NE 0 THEN BEGIN
     st = SMAP_WRITE3COLORS( mapbase, map250, map350, map500, $
                             DIR=i_outdir, /SILENT )
     IF st EQ 0 THEN MESSAGE,"Error writing maps out"
  ENDIF

  noise_maps = REPLICATE(0.0,3) ;;realized map noise
  IF map250.has_error THEN noise_maps[0] = MEDIAN(map250.error)
  IF map350.has_error THEN noise_maps[1] = MEDIAN(map350.error)
  IF map500.has_error THEN noise_maps[2] = MEDIAN(map500.error)

  ;;Add in confusion
  IF ~ KEYWORD_SET( noconfusion ) THEN BEGIN
     ;;confusion noise with extra beam size
     conf_noise = FLTARR(3)
     conf_noise[0] = SQRT(2)*35.3/17.8*5.5e-3
     conf_noise[1] = SQRT(2)*35.3/24.0*6.0e-3
     conf_noise[1] = SQRT(2)*35.3/35.3*5.2e-3
     noise_maps = SQRT(noise_maps^2 + conf_noise^2)
  ENDIF

  ;;Don't need maps any more -- but we do still need the cat!
  st = SIZE(temporary(map250))
  st = SIZE(temporary(map350))
  st = SIZE(temporary(map500))

  ;;Main loop
  max_sigma = MAX(noise_maps)
  quarterstep = nk1/4
  IF KEYWORD_SET( verbose ) THEN MESSAGE,"Starting main loop",/INF
  FOR i=0,nk1-1 DO BEGIN
     m250val = purity.k1[i]*cat.obs_fluxes[0]

     IF KEYWORD_SET( verbose ) && i GT 0 && nk1 GT 1 && $
        i+1 MOD quarterstep EQ 0 THEN $
           MESSAGE,STRING(100.0*(i+1)/quarterstep,$
                          FORMAT='(" Completed ",F0.1,"%")'),/INF

     FOR j=0,nk2-1 DO BEGIN
        
        IF ~ purity.valid[i,j] THEN CONTINUE

        ;;construct d value and noise value (assuming there is noise!)
        ;; For now, do this in map space only
        d = m250val + purity.k2[j] * cat.obs_fluxes[1] + $
            k500[i,j] * cat.obs_fluxes[2]

        IF max_sigma GT 0.0 THEN BEGIN
           ;; We have noise, so use S/N
           ;; This doesn't change anything at all about
           ;; the result, since the noise is the same for red
           ;; and non-red sources, but it's convenient to
           ;; quote D in sigma
           noisevar = (noise_maps[0] * purity.k1[i])^2 + $
                      (noise_maps[1] * purity.k2[j])^2 + $
                      (noise_maps[2] * k500[i,j])^2
           IF noisevar LE 0.0 THEN MESSAGE,"Invalid noise!"
           d /= SQRT( noisevar )
        ENDIF ;;just use D straight

        ;;Now find purity for each completeness level
        ;;However, there is a complication, which is that d must
        ;; be positive for us to detect something.
        ;;For completeness levels -below- where d goes positive,
        ;; set the purity to NaN
        dred = d[wred]
        dred = dred[SORT(dred)]
        st = WHERE( dred GT 0, nst )
        maxpure = 1.0*nst/nred
        IF maxpure LT MIN(completeness) THEN BEGIN
           ;;Never enough purity to satisfy specified levels
           purity.dvalue[*,i,j] = !VALUES.F_NAN
           purity.purity[*,i,j] = !VALUES.F_NAN
        ENDIF ELSE BEGIN
           FOR kidx=0,ncomplete-1 DO BEGIN
              IF maxpure LT completeness[kidx] THEN BEGIN
                 purity.dvalue[kidx,i,j] = !VALUES.F_NAN
                 purity.purity[kidx,i,j] = !VALUES.F_NAN
              ENDIF ELSE BEGIN
                 ;;Find s/n or flux for specified red source completeness
                 dred_idx = FLOOR( (1.0-purity.completeness[kidx])*nred )
                 purity.dvalue[kidx,i,j] = dred[dred_idx]
                 n_true = nred - dred_idx ;;number of true red sources found

                 w_false = WHERE( d[wnotred] GE purity.dvalue[kidx,i,j],$
                                  n_false )
                 purity.purity[kidx,i,j] = 1.0*n_true/(n_true+n_false)
              ENDELSE
           ENDFOR
        ENDELSE
     ENDFOR
  ENDFOR


  RETURN,purity
END
