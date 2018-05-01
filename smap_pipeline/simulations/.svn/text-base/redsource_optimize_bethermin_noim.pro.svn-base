;+
; NAME
;  redsource_optimize_bethermin_noim
; PURPOSE
;  Find the purity for a set of input map combinations based
;  on the redsource_bethermin model.  The map combination model
;  is k1*map_250 + k2*map_350 + sqrt(1-k1^2-k2^2)*map_500
; USAGE
;  purityarr = 
;   redsource_optimize_bethermin_noim( nk1, mink1, maxk1, nk2, mink2,
;             maxk2, completeness, fluxlim, dndlogldzdomega, cold, 
;             starburst, redparams, redtplfile, CATFILE=, AREA=,
;             SEED=, LOCALZ=, SIGMAS=, LENSPROB=)
; INPUTS
;  nk1             Number of k1 values
;  mink1           Minimum k1 value
;  maxk1           Maximum k1
;  nk2/mink2/maxk2 Same but for k2
;  completeness    Array of fractional completeness to test down to
;  fluxlim         Array of 500 micron flux limits to go down to, in mJy
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
;  lensprob        Lensing probability structure
;  catfile         File name to write catalog to
;  area            The area of the image (in sq. deg).  May not be
;                   perfectly realized on output
;  sigmas          Array of Gaussian noise added on a per-beam basis in
;                   each band [in Jy]
;  seed            Seed for random number generator
;  localz          Set this to mask out sources at lower z than this
;                   on the assumption they wouldn't be included anyways
; KEYWORDS
;  verbose         Print informational messages as it runs
; NOTES
;  See redsource_bethermin_noim and smap_generate_bethermin for more
;  details of the model.  While this internally produces maps, in
;  the current implementation the map differencing is done on the
;  catalog.  Maybe in the future it will be done directly on the
;  maps.
; MODIFICATION HISTORY
;  Author: Alex Conley, Oct 7, 2011
;-

FUNCTION redsource_optimize_bethermin_noim, nk1, mink1, maxk1, nk2, mink2,$
   maxk2, completeness, fluxlim, dndlogldzdomega, cold, starburst, redparams, $
   redtplfile, CATFILE=catfile, PIXSCALE=pixscale, AREA=area, SEED=seed, $
   LOCALZ=localz, SIGMAS=sigmas, VERBOSE=verbose, LENSPROB=lensprob
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
  nfluxlim = N_ELEMENTS(fluxlim)
  IF nfluxlim EQ 0 THEN MESSAGE,"No flux limits!"
  IF min(fluxlim) LT 0 THEN MESSAGE,"Invalid (non-positive) flux limits"
  IF max(fluxlim) LT redparams.min500 THEN $
     MESSAGE,"WARNING: max flux limit below faintest generated red source"
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

  ;;Get constants for map construction, start building output structure
  ;;Construct output structure
  purity = { nk1: nk1, nk2: nk2, k1: (maxk1-mink1)*FINDGEN(nk1)/(nk1-1)+mink1,$
             k2: (maxk2-mink2)*FINDGEN(nk2)/(nk2-1)+mink2,$
             valid: BYTARR(nk1, nk2), $
             dvalue: FLTARR(nfluxlim,ncomplete,nk1,nk2),$
             nfluxlim: nfluxlim,$
             ncomplete: ncomplete,$
             nsources: redparams.n, ngen: 0,$
             fluxlim: fluxlim,$
             completeness: completeness, $
             purity: FLTARR( nfluxlim, ncomplete, nk1, nk2 ) }

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
     MESSAGE,STRING(nvalid,FORMAT='("Number of valid combinations to test: ",I0)'),/INF
  
  ;;Build base catalog
  IF KEYWORD_SET( verbose ) THEN MESSAGE,"Building base catalog",/INF
  minzred = MIN( [redparams.zmin, redparams.zmax] )
  cat = redsource_bethermin_noim( dndlogldzdomega, cold, starburst, redparams,$
                                  redtplfile, AREA=area, SEED=seed, $
                                  LOCALZ=localz, MAXZBETH=minzred, $
                                  SIGMAS=i_sigmas, VERBOSE=verbose,$
                                  LENSPROB=lensprob )
  purity.ngen = N_ELEMENTS(cat) 
  wred = WHERE( cat.red, nred, COMPLEMENT=wnotred, NCOMPLEMENT=nnotred )
  IF nnotred EQ 0 THEN MESSAGE,"No non-red sources!"
  catred = cat[wred]

  IF N_ELEMENTS( catfile ) NE 0 THEN $
     MWRFITS, cat, catfile, /CREATE

  ;;Get noise values including confusion
  beamsizes = [17.8, 24.0, 35.3]
  convbeamsize = beamsizes[2]*SQRT(2)
  conf_noise = [5.8,6.3,6.8]*1e-3*convbeamsize/beamsizes
  total_noise = SQRT( conf_noise^2 + i_sigmas^2 )

  ;;Main loop
  max_sigma = MAX(i_sigmas)
  quarterstep = nk1/4
  IF KEYWORD_SET( verbose ) THEN MESSAGE,"Starting main loop",/INF
  FOR i=0,nk1-1 DO BEGIN
     m250val = purity.k1[i]*cat.obs_fluxes[0]

     IF KEYWORD_SET( verbose ) && i GT 0 && nk1 GT 1 && $
        ( (i+1) MOD quarterstep ) EQ 0 THEN $
           MESSAGE,STRING(100.0*(i+1)/nk1,$
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
           noisevar = (total_noise[0] * purity.k1[i])^2 + $
                      (total_noise[1] * purity.k2[j])^2 + $
                      (total_noise[2] * k500[i,j])^2
           IF noisevar LE 0.0 THEN MESSAGE,"Invalid noise!"
           d /= SQRT( noisevar )
        ENDIF ;;just use D straight

        ;;Now find purity for each completeness level and flux limit
        ;;However, there is a complication, which is that d must
        ;; be positive for us to detect something.
        ;;For completeness levels -below- where d goes positive,
        ;; set the purity to NaN
        dred = d[wred]
        dnotred = d[wnotred]
        FOR fidx=0,nfluxlim-1 DO BEGIN
           curr_fluxlim = fluxlim[fidx]*1e-3 ;;to Jy
           
           ;;Find red sources we are looking for
           wcred = WHERE( catred.true_fluxes[2] GE curr_fluxlim,$
                          ncred )
           
           dcred = dred[wcred]
           dcred = dcred[SORT(dcred)]
           st = WHERE( dcred GT 0, nst ) ;;nst is number that can be detected
           maxcomplete = 1.0*nst/ncred

           IF maxcomplete LT MIN(completeness) THEN BEGIN
              ;;Never enough purity to satisfy specified levels
              purity.dvalue[fidx,*,i,j] = !VALUES.F_NAN
              purity.purity[fidx,*,i,j] = !VALUES.F_NAN
           ENDIF ELSE BEGIN
              FOR kidx=0,ncomplete-1 DO BEGIN
                 IF maxcomplete LT completeness[kidx] THEN BEGIN
                    purity.dvalue[fidx,kidx,i,j] = !VALUES.F_NAN
                    purity.purity[fidx,kidx,i,j] = !VALUES.F_NAN
                 ENDIF ELSE BEGIN
                    ;;Find s/n or flux for specified red source
                    ;;completeness down to this flux limit
                    dred_idx = FLOOR( (1.0-purity.completeness[kidx])*ncred )
                    purity.dvalue[fidx,kidx,i,j] = dcred[dred_idx]

                    ;;number of red sources we would find down to the
                    ;; specified completeness limit
                    n_true = ncred - dred_idx 

                    ;;Compute purity to that limit
                    ;; Only count non-red sources in purity, not red
                    ;; sources that fall below the flux limit.  That
                    ;; is, we would be happy to find the fainter true
                    ;; red sources, so it makes no sense to be
                    ;; penalized for that
                    w_false = WHERE(dnotred GE purity.dvalue[fidx,kidx,i,j],$
                                    n_false )
                    purity.purity[fidx,kidx,i,j] = 1.0*n_true/(n_true+n_false)
                 ENDELSE
              ENDFOR ;;over completeness
           ENDELSE ;;maxcomplete test
        ENDFOR ;;over flux limit
     ENDFOR ;; over k2
  ENDFOR ;; over k1


  RETURN,purity
END
