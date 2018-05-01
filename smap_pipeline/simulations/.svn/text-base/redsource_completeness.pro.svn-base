;+
;NAME
; redsource_completeness
;PURPOSE
; Injects red sources into a pre-existing map to measure completeness
;USAGE
; recovercat = redsource_completeness(mapbase, fluxes, nsrcs,$
;                                     SIGMA_FLUX=sigma_flux,$
;                                     COEFFS=coeffs, ERRORS=errors
;                                     NSIMS=nsims, MAPDIR=mapdir,
;                                     /VERBOSE, /GRID, OUTMAP=outmap,$
;                                     OUTDIR=outmapdir, MASKDIR=maskdir,$
;                                     MASKFILES=maskfiles, SN=sn,$
;                                     CORR=corr, MATCHRAD=matchrad,$
;                                     FLUXRAT=fluxrat, USEPEAKPOS=usepeakpos)
;INPUTS
; mapbase        Base map name of unsmoothed maps
; fluxes         Injected source [f250, f350, f500] in Jy
; nsrcs          Number of sources injected sources per simulation
;RETURNS
; Array giving recovery fraction for each sim
;OPTIONAL INPUTS
; nsims          Number of times to simulate this map (def: 20)
; sigma_flux     Uncertainties in fluxes, in Jy
; coeffs         Linear map combination coefficients [k1,k2,k3];
;                 default are Darren's coeffs [-(SQRT(1-0.92^2),0,0.92]
; errors         Two element vector of [sigma_i,sigma_c] in Jy/beam
;                 for combined image; has a default
; mapdir         Directory to look for maps in
; maskfiles      List of masks to use; see linear_map_combine
; maskdir        Directory to look for masks in
; matchrad       Matching radius, in arcsec (def: 10.0)
; fluxrat        Require map fluxes of f350 >= fluxrat[0]*f250 and
;                 f500 >= fluxrat[1]*f350.  (def: [1.0,1.0]).  This
;                 can be multi-dimensional, in which case each is
;                 tried in turn
; sn             Required S/N for detection, can be an array (def: 4)
; corr           Minimum correlation coefficient
; seed           Seed for random number generation
; outcat         Write out cat to this file; only done for first sim
; outmap         Write out simulated maps to this base name; also only
;                 done for first sim
; outdir         Write out simulated map (and cat) to this directory
; peaksearchrad  Radius for peak pixel search if /FINDPEAKPIX is set (arcsec)
;KEYWORDS
; verbose        Prints status messages
; grid           Distribute sources on a grid rather than randomly
; posinfo        Keep track of how well positions are recovered
; findpeakpix    Also find the peak pixel within peaksearchrad
; usepeakpos     Use the positions from the peak pixel location for
;                 the map based fluxes rather than those from
;                 starfinder.
; optfilt        Use Chapin-style optimal filtering
;MODIFICATION HISTORY
; Author: Alex Conley, Sep 2011
;-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Applies matched filtering.  Copied from matched_filter_red
PRO redsource_completeness_match, map, padded_weightmap, sm_wtmap, $
                                  filter_fft, npad

  COMPILE_OPT IDL2, STRICTARRSUBS

  nx = map.xsize
  ny = map.ysize

  ;; Padding
  IF npad GT 0 THEN BEGIN
     working_map = DBLARR(nx + 2 * npad, ny + 2 * npad)
     working_map[npad:npad+nx-1, npad:npad+ny-1] = map.image
  ENDIF ELSE working_map = map.image

  sm_map = FFT(FFT(working_map * padded_weightmap, 1, /DOUBLE) * filter_fft,-1)
  sm_map /= sm_wtmap
  sm_noise = SQRT(1.0 / TEMPORARY(sm_wtmap))

  IF npad GT 0 THEN BEGIN
     map.image = sm_map[npad:npad+nx-1, npad:npad+ny-1]
     map.error = sm_noise[npad:npad+nx-1, npad:npad+ny-1]
  ENDIF ELSE BEGIN
     map.image = sm_map
     map.error = sm_noise
  ENDELSE
END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Injects fake sources, returning x/y positions
FUNCTION redsource_completeness_inject, map250, map350, map500, nsrcs, $
                                        fluxes, seed, SIGMA_FLUX=sigma_flux

  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN

  ;;Inject fake sources. However, avoid locations where the mask is
  ;; set, since these will be off the edge and not found.  This isn't
  ;; quite ideal if there are other masks set besides the edge, but is
  ;; better than not doing anything
  IF map250.xsize NE map350.xsize OR map250.ysize NE map350.ysize THEN $
        MESSAGE,"Input 250 and 350 micron maps not same size"
  IF map250.xsize NE map500.xsize OR map250.ysize NE map500.ysize THEN $
        MESSAGE,"Input 250 and 500 micron maps not same size"
    
  ;;The steps are: 1) find the source positions
  ;;               2) Add the sources
  ;;               3) Convolve in the beam
  ;;               4) Add them to the original map

  ;;Bad pixels -- don't stick a source on one
  ;;note that this is because we have applied the users masks
  IF map250.has_mask OR map350.has_mask OR map500.has_mask THEN BEGIN
     badpix = BYTARR(map250.xsize,map250.ysize)
     IF map250.has_mask THEN BEGIN
        wbad = WHERE(map250.mask NE 0, nbad)
        IF nbad NE 0 THEN badpix[TEMPORARY(wbad)] = 1b
     ENDIF
     IF map350.has_mask THEN BEGIN
        wbad = WHERE(map350.mask NE 0, nbad)
        IF nbad NE 0 THEN badpix[TEMPORARY(wbad)] = 1b
     ENDIF
     IF map500.has_mask THEN BEGIN
        wbad = WHERE(map500.mask NE 0, nbad)
        IF nbad NE 0 THEN badpix[TEMPORARY(wbad)] = 1b
     ENDIF
     wgood = WHERE( TEMPORARY(badpix) EQ 0, ngood )
     IF ngood EQ 0 THEN MESSAGE,"Masking removes all of map"
  ENDIF ELSE BEGIN
     ngood = map250.xsize * map250.ysize 
     wgood = LINDGEN(ngood)
  ENDELSE

  IF nsrcs GT 0.1*ngood THEN MESSAGE,"Injecting too many sources"

  ;;Randomly populate, allow multiple sources in the same
  ;; position to allow for source collision
  
  mappos = wgood[ FLOOR( ngood * RANDOMU(seed,nsrcs) ) ]

  IF N_ELEMENTS(sigma_fluxes) GE 3 THEN has_errors = 1b ELSE $
     has_errors = 0b

  fwhm = [17.6, 23.9, 35.2] ;;same as in smap_redsource_smooth
  ;; Has to be done in loop in case multiples go in same pixel
  ;;  beam sizes as in smap_redsource_smooth
  ;; Note we are injecting -unsmoothed- sources at the native
  ;;  resolution
  IF fluxes[0] GT 0.0 THEN BEGIN
     srcimg = DBLARR(map250.xsize, map250.ysize)
     IF has_errors THEN $
        f250 = sigma_flux[0]*RANDOMN(seed,nsrcs)+fluxes[0] ELSE $
           f250 = REPLICATE(fluxes[0],nsrcs)
     FOR i=0,nsrcs-1 DO srcimg[mappos[i]] += f250[i]
     kernel250 = get_spire_beam('PSW',map250.pixscale,FWHM=fwhm[0],/SILENT,$
                                /FACTOR)
     srcimg = CONVOLVE_FACTOR(srcimg, TEMPORARY(kernel250))
     map250.image += TEMPORARY(srcimg) ;;NaNs take care of themselves
  ENDIF

  IF fluxes[1] GT 0.0 THEN BEGIN
     srcimg = DBLARR(map250.xsize, map250.ysize)
     IF has_errors THEN $
        f350 = sigma_flux[1]*RANDOMN(seed,nsrcs)+fluxes[1] ELSE $
           f350 = REPLICATE(fluxes[1],nsrcs)
     FOR i=0,nsrcs-1 DO srcimg[mappos[i]] += f350[i]
     kernel350 = get_spire_beam('PMW',map350.pixscale,FWHM=fwhm[1],/SILENT,$
                                /FACTOR)
     srcimg = CONVOLVE_FACTOR( srcimg, TEMPORARY(kernel350) )
     map350.image += TEMPORARY(srcimg)
  ENDIF

  IF fluxes[2] GT 0.0 THEN BEGIN
     srcimg = DBLARR( map250.xsize, map250.ysize )
     IF has_errors THEN $
        f500 = sigma_flux[2]*RANDOMN(seed,nsrcs)+fluxes[2] ELSE $
           f500 = REPLICATE(fluxes[2],nsrcs)
     FOR i=0,nsrcs-1 DO srcimg[mappos[i]] += f500[i]
     kernel500 = get_spire_beam('PLW',map500.pixscale,FWHM=fwhm[2],/SILENT,$
                                /FACTOR)
     srcimg = CONVOLVE_FACTOR( srcimg, TEMPORARY(kernel500) )
     map500.image += TEMPORARY(srcimg)
  ENDIF

  retstruct = REPLICATE({x: 0L, y: 0L}, nsrcs)
  retstruct.x = mappos MOD map250.xsize
  retstruct.y = TEMPORARY(mappos) / map250.xsize
  RETURN,retstruct

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION redsource_completeness, mapbase, fluxes, nsrcs, $
                                 SIGMA_FLUX=sigma_flux, $
                                 CONF=conf, COEFFS=coeffs, $
                                 NSIMS=nsims, MAPDIR=mapdir, VERBOSE=verbose,$
                                 GRID=grid, MASKFILES=maskfiles, $
                                 MASKDIR=maskdir, SN=sn, CORR=corr, $
                                 MATCHRAD=matchrad, BGSUBSIZE=bgsubsize,$
                                 OUTMAP=outmap, OUTDIR=outdir, OUTCAT=outcat, $
                                 FLUXRAT=fluxrat, SEED=seed, $
                                 POSINFO=posinfo, FINDPEAKPIX=findpeakpix,$
                                 PEAKSEARCHRAD=peaksearchrad,$
                                 USEPEAKPOS=usepeakpos, OPTFILT=optfilt
  COMPILE_OPT IDL2, STRICTARRSUBS

  IF N_ELEMENTS(mapbase) EQ 0 THEN MESSAGE,"Need mapbase"
  IF N_ELEMENTS(fluxes) EQ 0 THEN MESSAGE,"Need fluxes"
  IF N_ELEMENTS(nsrcs) EQ 0 THEN MESSAGE,"Need nsrcs"

  IF N_ELEMENTS(nsims) EQ 0 THEN i_nsims = 20 ELSE i_nsims = nsims
  IF N_ELEMENTS(sn) EQ 0 THEN sn=4.0
  IF N_ELEMENTS(corr) EQ 0 THEN corr=0.85
  IF N_ELEMENTS(matchrad) EQ 0 THEN matchrad = 10.0
  IF N_ELEMENTS(fluxrat) EQ 0 THEN fluxrat = [1.0,1.0]
  IF N_ELEMENTS(coeffs) EQ 0 THEN coeffs = [-SQRT(1-0.92^2),0.0,0.92]
  IF N_ELEMENTS(conf) EQ 0 THEN conf = 4.23e-3 ;;sig_conf
  IF KEYWORD_SET(usepeakpos) && ~ KEYWORD_SET(findpeakpix) THEN $
     MESSAGE,"Must set /FINDPEAKPIX if using /USEPEAKPOS"

  szf = SIZE(fluxrat)
  nfluxdim = szf[0]
  IF nfluxdim LE 0 OR nfluxdim GT 2 THEN MESSAGE,"Can't interpret fluxrat"
  nfluxslice = szf[nfluxdim]
  IF nfluxslice NE 2 THEN MESSAGE,"Fluxrat must have 2 elements per slice"
  CASE nfluxdim OF
     1 : nflux = 1
     2 : nflux = szf[1]
     ELSE : MESSAGE,"Cant' interpret fluxrat"
  END

  nsn = N_ELEMENTS(sn)
  IF nsn EQ 0 THEN MESSAGE,"No S/N information"

  IF N_ELEMENTS(outdir) EQ 0 THEN i_outdir = './' ELSE $
     i_outdir = ADDSLASH(outdir)

  IF fluxes[0] LT 0.0 THEN MESSAGE,"Invalid (negative) f250"
  IF fluxes[1] LT 0.0 THEN MESSAGE,"Invalid (negative) f350"
  IF fluxes[2] LT 0.0 THEN MESSAGE,"Invalid (negative) f500"
  IF ~FINITE(fluxes[0]) THEN MESSAGE,"Invalid (non-finite) f250"
  IF ~FINITE(fluxes[1]) THEN MESSAGE,"Invalid (non-finite) f350"
  IF ~FINITE(fluxes[2]) THEN MESSAGE,"Invalid (non-finite) f500"
  IF N_ELEMENTS( sigma_flux ) NE 0 THEN BEGIN
     IF N_ELEMENTS(sigma_flux) LT 3 THEN $
        MESSAGE,"Need at least 3 sigma_flux values"
     wbad=WHERE( sigma_flux LT 0 OR ~FINITE(sigma_flux), nbad )
     IF nbad NE 0 THEN MESSAGE,"Some invalid sigma_flux values encountered"
     has_flux_errors = 1b
  ENDIF ELSE has_flux_errors = 0b

  IF nsrcs LE 0 THEN MESSAGE,"Invalid (non-positive) nsrcs"
  IF i_nsims LE 0 THEN MESSAGE,"Invalid (non-positive) nsims"
  IF MIN(sn) LE 0 THEN MESSAGE,"Invalid starfinder sig (non-positive)"
  IF corr LE 0 THEN MESSAGE,"Invalid starfinder corr (non-positive)"
  IF matchrad LE 0 THEN MESSAGE,"Invalid (non-positive) matchrad"

  IF KEYWORD_SET(grid) THEN MESSAGE,"Grid not yet implemented"

  ;;Read in base (unsmoothed) maps
  basemap250 = READ_SMAP_FITSMAP(mapbase, 'PSW', DIR=mapdir, $
                                 /NO_ABORT, SUCCESS=success, ERRMSG=errmsg,$
                                 /SILENT )
  IF success EQ 0 THEN MESSAGE, "Error opening 250 micron map: "+errmsg
  IF ~basemap250.has_exposure THEN MESSAGE,"Need exposure for 250"
  IF ~basemap250.has_mask THEN MESSAGE,"Need mask for 250"
  basemap350 = READ_SMAP_FITSMAP(mapbase, 'PMW', DIR=mapdir, $
                                 /NO_ABORT, SUCCESS=success, ERRMSG=errmsg,$
                                 /SILENT )
  IF success EQ 0 THEN MESSAGE, "Error opening 350 micron map: "+errmsg
  IF ~basemap350.has_exposure THEN MESSAGE,"Need exposure for 350"
  basemap500 = READ_SMAP_FITSMAP(mapbase, 'PLW', DIR=mapdir, $
                                 /NO_ABORT, SUCCESS=success, ERRMSG=errmsg,$
                                 /SILENT )
  IF success EQ 0 THEN MESSAGE, "Error opening 500 micron map: "+errmsg
  IF ~basemap500.has_exposure THEN MESSAGE,"Need exposure for 500"

  IF basemap250.xsize NE basemap350.xsize OR $
     basemap250.ysize NE basemap350.ysize THEN $
        MESSAGE,"Input maps not same size"
  IF ABS((basemap250.pixscale - basemap350.pixscale)/basemap250.pixscale) $
     GT 1d-3 THEN MESSAGE,"Input 250, 350 micron maps don't have same pixel scale"
  IF basemap250.xsize NE basemap500.xsize OR $
     basemap250.ysize NE basemap500.ysize THEN $
        MESSAGE,"Input maps not same size"
  IF ABS((basemap250.pixscale - basemap500.pixscale)/basemap250.pixscale) $
     GT 1d-3 THEN MESSAGE,"Input 250, 500 micron maps don't have same pixel scale"

  ;;If masks are present, we want to add them to the input maps
  ;; so we don't have to call polywind continually.  We really only
  ;; have to do this to one of the files, then linear_map_combine
  ;; will do the rest.
  IF N_ELEMENTS(maskfiles) NE 0 THEN BEGIN
     IF ~ basemap250.has_mask THEN $
        MESSAGE,"Need mask in map250 to use maskfiles"
     add_user_mapmask, basemap250, maskfiles, /ANDPLUS, MASKDIR=maskdir
  ENDIF

  ;;Return variable
  retelem = {f500: fluxes[2], f350: fluxes[1], f250: fluxes[0],$
             df500: !VALUES.F_NAN, df350: !VALUES.F_NAN,$
             df250: !VALUES.F_NAN, k1: coeffs[0], k2: coeffs[1],$
             k3: coeffs[2], optfilt: KEYWORD_SET(optfilt),$
             ninjected: nsrcs, nrecovered: 0,$
             nadditional: 0, sn: !VALUES.F_NAN,$
             usepeakpos: KEYWORD_SET(usepeakpos),$
             fluxrat0: !VALUES.F_NAN, fluxrat1: !VALUES.F_NAN,$
             delta_f250: !VALUES.F_NAN, stdev_delta_f250: !VALUES.F_NAN,$
             delta_f350: !VALUES.F_NAN, stdev_delta_f350: !VALUES.F_NAN,$
             delta_f500: !VALUES.F_NAN, stdev_delta_f500: !VALUES.F_NAN}
  IF KEYWORD_SET(posinfo) THEN BEGIN
     retelem = CREATE_STRUCT(retelem,$
                             'delta_ra',!VALUES.F_NAN,$
                             'delta_dec', !VALUES.F_NAN,$
                             'stdev_delta_ra', !VALUES.F_NAN,$
                             'stdev_delta_dec', !VALUES.F_NAN,$
                             'mnoffset', !VALUES.F_NAN, $
                             'stdevoffset', !VALUES.F_NAN)
     IF KEYWORD_SET(findpeakpix) THEN $
        retelem = CREATE_STRUCT(retelem,$
                                'delta_ra_peak',!VALUES.F_NAN,$
                                'delta_dec_peak', !VALUES.F_NAN,$
                                'stdev_delta_ra_peak', !VALUES.F_NAN,$
                                'stdev_delta_dec_peak', !VALUES.F_NAN,$
                                'mnoffset_peak', !VALUES.F_NAN, $
                                'stdevoffset_peak', !VALUES.F_NAN)
  ENDIF
  retstruct = REPLICATE(retelem, nsims, nsn, nflux)

  matchrad_pix = matchrad / basemap250.pixscale
  IF has_flux_errors THEN BEGIN
     retstruct.df500 = sigma_flux[2]
     retstruct.df350 = sigma_flux[1]
     retstruct.df250 = sigma_flux[0]
  ENDIF

  ;; Do smoothing on pre-injected maps, save filter info.
  ;; Also figure out fwhm
  IF KEYWORD_SET(optfilt) THEN BEGIN
     tmap250 = basemap250
     tmap350 = basemap350
     tmap500 = basemap500
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE, "Doing initial optimal filter to get filter information", /INF
     matched_filter_red, tmap250, tmap350, tmap500, conf, BEAM=beam,$
                         FILT250=filt250, FILT350=filt350, FILT500=filt500
     ;; Determine FWHM
     init_fwhm = 35.2 / map500.pixscale ;;in pix
     maxbm = MAX(beam, wmax)
     szbm = SIZE(beam)
     init_pos_x = wmax MOD szbm[1]
     init_pos_y = wmax / szbm[1]
     censize = 7 * CEIL(init_fwhm)
     minx = (init_pos_x - censize) > 0
     maxx = (init_pos_x + censize) < (szbm[1] - 1)
     miny = (init_pos_y - censize) > 0
     maxy = (init_pos_y + censize) < (szbm[2] - 1)
     beam = beam[minx:maxx, miny:maxy]
     maxbm = MAX(beam, wmax)
     szbm = SIZE(beam)
     init_pos_x = wmax MOD szbm[1]
     init_pos_y = wmax / szbm[1]
     init_params = [0.0, 1.0, 0.5 * init_fwhm, 0.5 * init_fwhm, $
                    init_pos_x, init_pos_y, 1.0]
     yfit = MPFIT2DPEAK(beam, params, /CIRCULAR,$
                        ESTIMATES=init_params, /GAUSSIAN)
     fwhm = 2 * params[2] * map500.pixscale
  ENDIF ELSE fwhm = SQRT(2) * 35.2

  ;;Main simulation loop
  IF KEYWORD_SET(verbose) THEN BEGIN
     fmt = '("Processing ",I0," sims with ",I0," S/N ratios and ",I0,'+$
           '" fluxrats")'
     MESSAGE,STRING(nsims,nsn,nflux,FORMAT=fmt),/INF
  ENDIF
  FOR i=0, nsims-1 DO BEGIN
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,STRING(i+1,FORMAT='("Simulation: ",I0)'),/INF
     tmap250 = basemap250
     tmap350 = basemap350
     tmap500 = basemap500

     ;;Inject fake sources into unsmoothed maps
     pos = redsource_completeness_inject(tmap250, tmap350, tmap500, nsrcs, $
                                         fluxes, seed, SIGMA_FLUX=sigma_flux)

     IF KEYWORD_SET( verbose ) THEN $
        MESSAGE," Doing smoothing",/INF
     IF KEYWORD_SET(optfilt) THEN BEGIN
        ;; Matched filter using pre-saved filter information
        ;;  This is much faster than doing all this anew on every
        ;;  iteration.
        IF coeffs[0] NE 0 THEN $
           redsource_completeness_match, tmap250, filt250.padded_weightmap, $
                                         filt250.sm_wtmap, $
                                         filt250.filter_fft, filt250.npad
        IF coeffs[1] NE 0 THEN $
           redsource_completeness_match, tmap350, filt350.padded_weightmap, $
                                         filt350.sm_wtmap, $
                                         filt350.filter_fft, filt350.npad
        IF coeffs[2] NE 0 THEN $
           redsource_completeness_match, tmap500, filt500.padded_weightmap, $
                                         filt500.sm_wtmap, $
                                         filt500.filter_fft, filt500.npad
     ENDIF ELSE BEGIN
        smap_redsource_smooth, tmap250, /BRUTE, /NOEXPSMOOTH,$
                               /NOPEAK
        smap_redsource_smooth, tmap350, /BRUTE, /NOEXPSMOOTH,$
                               /NOPEAK
        smap_redsource_smooth, tmap500, /BRUTE, /NOEXPSMOOTH,$
                               /NOPEAK
     ENDELSE

     ;;Now -- find the sources.  
     IF KEYWORD_SET( verbose ) THEN $
        MESSAGE," Doing sourcefinding",/INF
     FOR j=0, nsn-1 DO BEGIN
        curr_sn = sn[j]
        retstruct[i,j,*].sn = curr_sn
        IF KEYWORD_SET(verbose) THEN $
           MESSAGE,STRING(curr_sn,FORMAT='("  Doing S/N:",F0.1)'),/INF
        FOR k=0, nflux-1 DO BEGIN
           IF nfluxdim EQ 1 THEN fvals=[fluxrat[0],fluxrat[1]] ELSE $
              fvals = [ fluxrat[k,0], fluxrat[k,1] ]
           retstruct[i,j,k].fluxrat0 = fvals[0]
           retstruct[i,j,k].fluxrat1 = fvals[1]

           IF KEYWORD_SET(verbose) THEN $
              MESSAGE,STRING(fvals[0],fvals[1],$
                             FORMAT='("    Doing Flux0:",F0.2,'+$
                             '" Flux1:",F0.2)'),/INF

           ;;Find the sources using the usual pipeline.  Note
           ;; they are already smoothed
           cat = red_sourcefind_dofind(tmap250, tmap350, tmap500, /NOSMOOTH,$
                                       FWHM=fwhm, COEFFS=coeffs, SN=curr_sn,$
                                       BGSUBSIZE=bgsubsize, MIN_CORR=corr,$
                                       FLUXRAT=fvals, CONF=conf,$
                                       SUCCESS=getcat_success, /NOWRITE,$
                                       FINDPEAKPIX=findpeakpix,$
                                       PEAKSEARCHRAD=peaksearchrad,$
                                       USEPEAKPOS=usepeakpos)

           nfound = N_ELEMENTS(cat)
           IF ~ getcat_success THEN BEGIN
              ;;Well then... failure
              retstruct[i,j,k].nrecovered = 0
              retstruct[i,j,k].nadditional = nfound
           ENDIF ELSE BEGIN
              ;;success of some sort
              ;;Match lists
              SRCOR, cat.x_sub, cat.y_sub, pos.x, pos.y, $
                     matchrad_pix, ind1, ind2, COUNT=count, /SILENT
              
              retstruct[i,j,k].nrecovered = count
              retstruct[i,j,k].nadditional= nfound - count
        
              ;;Compute flux offsets between measured and input
              ;;Note that we ignore the sigma_flux information, since
              ;; that should average to zero.
              ;; Note that the catalog fluxes are in mJy, the
              ;; input fluxes are in Jy
              retstruct[i,j,k].delta_f250 = MEAN(cat.map_f250) - fluxes[0]*1e3
              retstruct[i,j,k].delta_f350 = MEAN(cat.map_f350) - fluxes[1]*1e3
              retstruct[i,j,k].delta_f500 = MEAN(cat.map_f500) - fluxes[2]*1e3
              IF nfound GT 2 THEN BEGIN
                 retstruct[i,j,k].stdev_delta_f250 = STDEV(cat.map_f250)
                 retstruct[i,j,k].stdev_delta_f350 = STDEV(cat.map_f350)
                 retstruct[i,j,k].stdev_delta_f500 = STDEV(cat.map_f500)
              ENDIF

              ;;Compute offset between measured and input
              IF KEYWORD_SET(posinfo) && count GT 0 THEN BEGIN
                 xy2ad, pos[ind2].x, pos[ind2].y, tmap250.astrometry, $
                        ratrue, dectrue
                 gcirc,2,cat[ind1].ra_sub,cat[ind1].dec_sub,ratrue,dectrue,dis
                 retstruct[i,j,k].mnoffset = MEAN(dis)
                 IF N_ELEMENTS(dis) GT 2 THEN $
                    retstruct[i,j,k].stdevoffset = STDEV(TEMPORARY(dis))
                 deccen = tmap250.astrometry.crval[1]
                 dra = (cat[ind1].ra_sub - ratrue)*COS(!PI*deccen/180.0)
                 retstruct[i,j,k].delta_ra = MEAN(dra)
                 IF N_ELEMENTS(dra) GT 2 THEN $
                    retstruct[i,j,k].stdev_delta_ra = STDEV(TEMPORARY(dra))
                 ddec = (cat[ind1].dec_sub - dectrue)
                 retstruct[i,j,k].delta_dec = MEAN(ddec)
                 IF N_ELEMENTS(ddec) GT 2 THEN $
                    retstruct[i,j,k].stdev_delta_dec = STDEV(TEMPORARY(ddec))

                 IF KEYWORD_SET(findpeakpix) THEN BEGIN
                    gcirc,2,cat[ind1].ra_peak,cat[ind1].dec_peak,$
                          ratrue,dectrue,dis
                    retstruct[i,j,k].mnoffset_peak = MEAN(dis)
                    IF N_ELEMENTS(dis) GT 2 THEN $
                       retstruct[i,j,k].stdevoffset_peak = STDEV(TEMPORARY(dis))
                    deccen = tmap250.astrometry.crval[1]
                    dra = (cat[ind1].ra_peak - ratrue)*COS(!PI*deccen/180.0)
                    retstruct[i,j,k].delta_ra_peak = MEAN(dra)
                    IF N_ELEMENTS(dra) GT 2 THEN $
                       retstruct[i,j,k].stdev_delta_ra_peak = $
                       STDEV(TEMPORARY(dra))
                    ddec = (cat[ind1].dec_sub - dectrue)
                    retstruct[i,j,k].delta_dec_peak = MEAN(ddec)
                    IF N_ELEMENTS(ddec) GT 2 THEN $
                       retstruct[i,j,k].stdev_delta_dec_peak = $
                       STDEV(TEMPORARY(ddec))
                 ENDIF
              ENDIF

              ;;output first catalogs
              IF N_ELEMENTS(outcat) NE 0 AND i EQ 0 AND j EQ 0 AND $
                 k EQ 0 THEN BEGIN
                 MWRFITS, pos, i_outdir+outcat, /CREATE, /SILENT
                 MWRFITS, cat, i_outdir+outcat, /SILENT
              ENDIF
           ENDELSE

        ENDFOR ;; fluxrat loop
     ENDFOR ;; sn loop
  ENDFOR
  
  RETURN,retstruct
END

