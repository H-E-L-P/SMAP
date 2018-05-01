;+
;NAME
; redsource_recover
;PURPOSE
; Injects red sources into a pre-existing map and tries to recover
;them.  Closely related to redsource_completeness.
;USAGE
; recovercat = redsource_recover(mapbase, f250, f350, f500, nsrcs,$
;                                COEFFS=coeffs,ERRORS=errors
;                                NSIMS=nsims, MAPDIR=mapdir,
;                                /VERBOSE, /GRID, OUTMAP=outmap,$
;                                OUTDIR=outmapdir, MASKDIR=maskdir,$
;                                MASKFILES=maskfiles, SN=sn,$
;                                CORR=corr, MATCHRAD=matchrad,$
;                                FLUXRAT=fluxrat, USEPEAKPOS=usepeakpos)
;INPUTS
; mapbase        Base map name
; f250, f350, f500 Injected map fluxes in Jy
; nsrcs          Number of sources injected sources per simulation
;RETURNS
; Array giving recovery fraction for each sim
;OPTIONAL INPUTS
; nsims          Number of times to simulate this map (def: 100)
; coeffs         Linear map combination coefficients [k1,k2,k3];
;                 default are Darren's coeffs [-(SQRT(1-0.92^2),0,0.92]
; errors         Two element vector of [sigma_i,sigma_c] in Jy/beam
;                 for combined image; has a default
; mapdir         Directory to look for maps in
; maskfiles      List of masks to use; see linear_map_combine
; maskdir        Directory to look for masks in
; matchrad       Matching radius, in arcsed (def: 10.0)
; fluxrat        Require map fluxes of f350 >= fluxrat[0]*f250 and
;                 f500 >= fluxrat[1]*f350.  (def: [1.0,1.0]).  This
;                 must have 2 elements
; sn             Required S/N for detection, scalar (def: 4)
; corr           Minimum correlation coefficient
; seed           Seed for random number generation
; outmap         Write out simulated maps to this base name; also only
;                 done for first sim
; outdir         Write out simulated map (and cat) to this directory
; peaksearchrad  Radius for peak pixel search if /USEPEAKPOS is set (arcsec)
;KEYWORDS
; verbose        Prints status messages
; grid           Distribute sources on a grid rather than randomly
; usepeakpos     Use the positions from the peak pixel location for
;                 the map based fluxes rather than those from starfinder.
;MODIFICATION HISTORY
; Author: Alex Conley, Mar 2013
;-


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Injects fake sources, returning x/y positions in structure
FUNCTION redsource_recover_inject, f250, f350, f500, map250, map350, $
                                   map500, nsrcs, seed, EXC_X=exc_x,$
                                   EXC_Y=exc_y, MATCHRAD=matchrad,$
                                   SUCCESS=success

  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN
  success = 0b

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
     wgood = LINDGEN( ngood )
  ENDELSE

  IF nsrcs GT 0.002*ngood THEN MESSAGE,"Injecting too many sources"

  ;;Randomly populate, allow multiple sources in the same
  ;; position to allow for source collision
  mappos = wgood[FLOOR(ngood * RANDOMU(seed,nsrcs))]

  fwhm = [17.6, 23.9, 35.2] ;;same as in smap_redsource_smooth
  ;;Has to be done in loop in case multiples go in same pixel
  ;;beam sizes as in smap_redsource_smooth
  IF f250 GT 0.0 THEN BEGIN
     srcimg = DBLARR( map250.xsize, map250.ysize )
     FOR i=0,nsrcs-1 DO srcimg[mappos[i]] += f250
     kernel250 = get_spire_beam('PSW',map250.pixscale,FWHM=fwhm[0],/SILENT,$
                                /FACTOR)
     srcimg = CONVOLVE_FACTOR( srcimg, TEMPORARY(kernel250) )
     map250.image += TEMPORARY(srcimg) ;;NaNs take care of themselves
  ENDIF

  IF f350 GT 0.0 THEN BEGIN
     srcimg = DBLARR( map250.xsize, map250.ysize )
     FOR i=0,nsrcs-1 DO srcimg[mappos[i]] += f350
     kernel350 = get_spire_beam('PMW',map350.pixscale,FWHM=fwhm[1],/SILENT,$
                                /FACTOR)
     srcimg = CONVOLVE_FACTOR( srcimg, TEMPORARY(kernel350) )
     map350.image += TEMPORARY(srcimg)
  ENDIF

  IF f500 GT 0.0 THEN BEGIN
     srcimg = DBLARR( map250.xsize, map250.ysize )
     FOR i=0,nsrcs-1 DO srcimg[mappos[i]] += f500
     kernel500 = get_spire_beam('PLW',map500.pixscale,FWHM=fwhm[2],/SILENT,$
                                /FACTOR)
     srcimg = CONVOLVE_FACTOR( srcimg, TEMPORARY(kernel500) )
     map500.image += TEMPORARY(srcimg)
  ENDIF

  retstruct = REPLICATE({x: 0, y: 0}, nsrcs)
  retstruct.x = mappos MOD map250.xsize
  retstruct.y = TEMPORARY(mappos) / map250.xsize

  ;;Now throw out any excluded pixels
  
  IF N_ELEMENTS(exc_x) NE 0 OR N_ELEMENTS(exc_y) NE 0 THEN BEGIN
     IF N_ELEMENTS(matchrad) EQ 0 THEN mrad = 4.0 ELSE mrad = FLOAT(matchrad)
     IF N_ELEMENTS(exc_x) EQ 0 THEN MESSAGE,"No exc_x, but exc_y"
     IF N_ELEMENTS(exc_y) EQ 0 THEN MESSAGE,"No exc_y, but exc_x"
     SRCOR, exc_x, exc_y, retstruct.x, retstruct.y, mrad,$
            ind1, ind2, COUNT=count, /SILENT
     IF count NE 0 THEN BEGIN
        ;;Some to remove
        toss = BYTARR(nsrcs)
        toss[ind2] = 1b
        wkeep = WHERE(~toss, nkeep)
        IF nkeep EQ 0 THEN RETURN,!VALUES.F_NAN ;;removed all
        ;;Don't accept more than 50% rejection either
        IF 1.0 * nkeep / nsrcs LT 0.5 THEN RETURN,!VALUES.F_NAN 
        retstruct = retstruct[wkeep]
     ENDIF
  ENDIF

  success = 1b
  RETURN,retstruct

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION redsource_recover, mapbase, f250, f350, f500, nsrcs, $
                            ERRORS=errors, COEFFS=coeffs, $
                            NSIMS=nsims, MAPDIR=mapdir, VERBOSE=verbose,$
                            GRID=grid, MASKFILES=maskfiles, $
                            MASKDIR=maskdir, SN=sn, CORR=corr, $
                            MATCHRAD=matchrad, BGSUBSIZE=bgsubsize,$
                            OUTMAP=outmap, OUTDIR=outdir, $
                            FLUXRAT=fluxrat, SEED=seed, $
                            PEAKSEARCHRAD=peaksearchrad,$
                            USEPEAKPOS=usepeakpos

  COMPILE_OPT IDL2, STRICTARRSUBS

  IF N_ELEMENTS(mapbase) EQ 0 THEN MESSAGE,"Need mapbase"
  IF N_ELEMENTS(f250) EQ 0 THEN MESSAGE,"Need f250"
  IF N_ELEMENTS(f350) EQ 0 THEN MESSAGE,"Need f350"
  IF N_ELEMENTS(f500) EQ 0 THEN MESSAGE,"Need f500"
  IF N_ELEMENTS(nsrcs) EQ 0 THEN MESSAGE,"Need nsrcs"

  IF N_ELEMENTS(nsims) EQ 0 THEN i_nsims = 100 ELSE i_nsims = nsims
  IF N_ELEMENTS(sn) EQ 0 THEN sn=4.0
  IF N_ELEMENTS(sn) GT 1 THEN MESSAGE,"sn must be scalar"
  IF N_ELEMENTS(corr) EQ 0 THEN corr=0.85
  IF N_ELEMENTS(matchrad) EQ 0 THEN matchrad = 10.0
  IF N_ELEMENTS(fluxrat) EQ 0 THEN fluxrat = [1.0,1.0]
  IF N_ELEMENTS(fluxrat) NE 2 THEN MESSAGE,"fluxrat must have 2 elements"
  IF N_ELEMENTS(coeffs) EQ 0 THEN coeffs = [-SQRT(1-0.92^2),0.0,0.92]
  IF N_ELEMENTS(errors) EQ 0 THEN errors = [6.74, 4.23]*1e-3 ;;sig_i, conf

  IF N_ELEMENTS(outdir) EQ 0 THEN i_outdir = './' ELSE $
     i_outdir = ADDSLASH(outdir)

  IF f250 LT 0.0 THEN MESSAGE,"Invalid (negative) f250"
  IF f350 LT 0.0 THEN MESSAGE,"Invalid (negative) f350"
  IF f500 LT 0.0 THEN MESSAGE,"Invalid (negative) f500"
  IF ~FINITE(f250) THEN MESSAGE,"Invalid (non-finite) f250"
  IF ~FINITE(f350) THEN MESSAGE,"Invalid (non-finite) f350"
  IF ~FINITE(f500) THEN MESSAGE,"Invalid (non-finite) f500"

  IF nsrcs LE 0 THEN MESSAGE,"Invalid (non-positive) nsrcs"
  IF i_nsims LE 0 THEN MESSAGE,"Invalid (non-positive) nsims"
  IF sn LE 0 THEN MESSAGE,"Invalid starfinder sig (non-positive)"
  IF corr LE 0 THEN MESSAGE,"Invalid starfinder corr (non-positive)"
  IF matchrad LE 0 THEN MESSAGE,"Invalid (non-positive) matchrad"

  IF KEYWORD_SET(grid) THEN MESSAGE,"Grid not yet implemented"

  ;;Read in base maps
  basemap250 = READ_SMAP_FITSMAP(mapbase, 'PSW', DIR=mapdir, $
                                 /NO_ABORT, SUCCESS=success, ERRMSG=errmsg,$
                                 /SILENT )
  IF success EQ 0 THEN MESSAGE,"Error opening 250 micron map: "+errmsg
  IF ~basemap250.has_exposure THEN MESSAGE,"Need exposure for 250"
  IF ~basemap250.has_mask THEN MESSAGE,"Need mask for 250"
  basemap350 = READ_SMAP_FITSMAP(mapbase, 'PMW', DIR=mapdir, $
                                 /NO_ABORT, SUCCESS=success, ERRMSG=errmsg,$
                                 /SILENT )
  IF success EQ 0 THEN MESSAGE,"Error opening 350 micron map: "+errmsg
  IF ~basemap350.has_exposure THEN MESSAGE,"Need exposure for 350"
  basemap500 = READ_SMAP_FITSMAP(mapbase, 'PLW', DIR=mapdir, $
                                 /NO_ABORT, SUCCESS=success, ERRMSG=errmsg,$
                                 /SILENT )
  IF success EQ 0 THEN MESSAGE,"Error opening 500 micron map: "+errmsg
  IF ~basemap500.has_exposure THEN MESSAGE,"Need exposure for 500"

  IF basemap250.xsize NE basemap350.xsize OR $
     basemap250.ysize NE basemap350.ysize THEN $
        MESSAGE,"Input maps not same size"
  IF ABS( (basemap250.pixscale - basemap350.pixscale)/basemap250.pixscale ) $
     GT 1d-3 THEN MESSAGE,"Input 250, 350 micron maps don't have same pixel scale"
  IF basemap250.xsize NE basemap500.xsize OR $
     basemap250.ysize NE basemap500.ysize THEN $
        MESSAGE,"Input maps not same size"
  IF ABS( (basemap250.pixscale - basemap500.pixscale)/basemap250.pixscale ) $
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
  ;; ninjected is the number of sources inserted.  This can vary from
  ;; simulation to simulation because we don't insert sources
  ;; too close to sources in the original image
  retstruct = {f500: f500, f350: f350, f250: f250,$
               k1: coeffs[0], k2: coeffs[1],$
               k3: coeffs[2], nsims: nsims, $
               pixscale: basemap250.pixscale,$
               usepeakpos: KEYWORD_SET(usepeakpos),$
               norig: 0, ninjected: INTARR(nsims),$
               nrecovered: INTARR(nsims),$
               nadditional: INTARR(nsims), sn: sn, fluxrat: fluxrat,$
               xpos_injected: FLTARR(nsims, nsrcs, /NOZERO),$ ;;where do we put sources?
               ypos_injected: FLTARR(nsims, nsrcs, /NOZERO),$
               found: BYTARR(nsims, nsrcs),$        ;;Did we find it?
               xpos_recovered: FLTARR(nsims, nsrcs, /NOZERO),$ ;;where did we find it?
               ypos_recovered: FLTARR(nsims, nsrcs, /NOZERO),$
               f250_recovered: FLTARR(nsims, nsrcs, /NOZERO),$ ;;what fluxes did it have?
               f350_recovered: FLTARR(nsims, nsrcs, /NOZERO),$
               f500_recovered: FLTARR(nsims, nsrcs, /NOZERO)}

  matchrad_pix = matchrad / basemap250.pixscale

  ;;First -- find sources in the original maps so we don't
  ;;         re-find them later.  This was not so important for the
  ;;         efficiency sims because they were such a small
  ;;         contribution.  Here, where the recovery fraction should
  ;;         be much lower, we want to mask against any input sources
  ;;         being so close.
  ;; Also -- save the smoothed exposure map for re-use
  IF KEYWORD_SET(verbose) THEN MESSAGE,"Finding sources in original map",/INF
  tmap250 = basemap250
  tmap350 = basemap350
  tmap500 = basemap500
  smap_redsource_smooth, tmap250, /BRUTE, /NOERRORSMOOTH, /NOPEAK
  smap_redsource_smooth, tmap350, /BRUTE, /NOERRORSMOOTH, /NOPEAK
  smap_redsource_smooth, tmap500, /BRUTE, /NOERRORSMOOTH, /NOPEAK
  exp250 = tmap250.exposure
  exp350 = tmap350.exposure
  exp500 = tmap500.exposure
  cat = red_sourcefind_dofind(tmap250, tmap350, tmap500, $
                              COEFFS=coeffs, SN=sn, $
                              BGSUBSIZE=bgsubsize, MIN_CORR=corr,$
                              FLUXRAT=fvals, ERRORS=errors,$
                              SUCCESS=getcat_success, /NOWRITE,$
                              FINDPEAKPIX=usepeakpos,$
                              PEAKSEARCHRAD=peaksearchrad,$
                              USEPEAKPOS=usepeakpos)
  has_origcat = 0b
  IF getcat_success THEN BEGIN
     
     orig_x = cat.x_sub
     orig_y = cat.y_sub
     retstruct.norig = N_ELEMENTS(cat)
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,STRING(retstruct.norig,$
                       FORMAT='(" Found: ",I0," sources in initial map")'),/INF
  ENDIF
  
  ;;Main simulation loop
  IF KEYWORD_SET(verbose) THEN $
     perfmt = '("Simulation: ",I0," of ",I0," [",F5.1,"%]")'
  FOR i=0, nsims-1 DO BEGIN
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,STRING(i+1, nsims, 100 * (i+1.0) / nsims, FORMAT=perfmt),/INF
     tmap250 = basemap250
     tmap350 = basemap350
     tmap500 = basemap500

     ;;Inject fake sources
     inject_success = 0b
     iter = 0
     WHILE ~ inject_success DO BEGIN
        ;;Exclude all sources too near the ones in the original map
        pos = redsource_recover_inject(f250, f350, f500, tmap250, tmap350, $
                                       tmap500, nsrcs, seed, $
                                       EXC_X=orig_x, EXC_Y=orig_y, $
                                       MATCHRAD=1.2 * matchrad_pix,$
                                       SUCCESS=inject_success)
        IF KEYWORD_SET(verbose) AND ~inject_success THEN $
           MESSAGE," Failed to inject sources -- trying again",/INF
        iter += 1
        IF iter GE 10 THEN MESSAGE,"Unable to inject sources for some reason"
     ENDWHILE
     npos = N_ELEMENTS(pos)
     retstruct.ninjected[i] = npos
     retstruct.found[i, *] = 0b
     retstruct.xpos_injected[i, 0:npos-1] = pos.x
     retstruct.ypos_injected[i, 0:npos-1] = pos.y
     IF npos LT nsrcs THEN BEGIN
        retstruct.found[i, npos:*] = 0b
        retstruct.xpos_injected[i, npos:*] = !VALUES.F_NAN
        retstruct.ypos_injected[i, npos:*] = !VALUES.F_NAN
        retstruct.xpos_recovered[i, npos:*] = !VALUES.F_NAN
        retstruct.ypos_recovered[i, npos:*] = !VALUES.F_NAN
        retstruct.f250_recovered[i, npos:*] = !VALUES.F_NAN
        retstruct.f350_recovered[i, npos:*] = !VALUES.F_NAN
        retstruct.f500_recovered[i, npos:*] = !VALUES.F_NAN
     ENDIF
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE, STRING(npos, FORMAT='(" Injected: ",I0," sources")'),/INF

     ;;Smooth the maps.  Don't smooth the errors maps because
     ;; we just toss those inside red_sourcefind_dofind using ERRORS=
     ;; Also don't exposure smooth except for the first map, since
     ;; the exposure time doesn't change and we can reuse it
     IF KEYWORD_SET( verbose ) THEN $
        MESSAGE," Doing smoothing",/INF

     smap_redsource_smooth, tmap250, /BRUTE, /NOERRORSMOOTH, /NOEXPSMOOTH,$
                            /NOPEAK
     smap_redsource_smooth, tmap350, /BRUTE, /NOERRORSMOOTH, /NOEXPSMOOTH,$
                            /NOPEAK
     smap_redsource_smooth, tmap500, /BRUTE, /NOERRORSMOOTH, /NOEXPSMOOTH,$
                            /NOPEAK
     tmap250.exposure = exp250
     tmap350.exposure = exp350
     tmap500.exposure = exp500

     ;;Now -- find the sources.  
     IF KEYWORD_SET( verbose ) THEN $
        MESSAGE," Doing sourcefinding",/INF

     ;;Find the sources using the usual pipeline     
     cat = red_sourcefind_dofind(tmap250, tmap350, tmap500, $
                                 COEFFS=coeffs, SN=sn, $
                                 BGSUBSIZE=bgsubsize, MIN_CORR=corr,$
                                 FLUXRAT=fvals, ERRORS=errors,$
                                 SUCCESS=getcat_success, /NOWRITE,$
                                 FINDPEAKPIX=usepeakpos,$
                                 PEAKSEARCHRAD=peaksearchrad,$
                                 USEPEAKPOS=usepeakpos)

     IF ~ getcat_success THEN BEGIN
        ;;Well then... failure
        MESSAGE," No sources found",/INF
        retstruct.nrecovered[i] = 0
        retstruct.nadditional[i] = 0
        retstruct.xpos_recovered[i, *] = !VALUES.F_NAN
        retstruct.ypos_recovered[i, *] = !VALUES.F_NAN
        retstruct.f250_recovered[i, *] = !VALUES.F_NAN
        retstruct.f350_recovered[i, *] = !VALUES.F_NAN
        retstruct.f500_recovered[i, *] = !VALUES.F_NAN
     ENDIF ELSE BEGIN
        ;;success of some sort -- doesn't mean they match the
        ;;                        inputs though

        ;;Match lists to input
        nfound = N_ELEMENTS(cat)
        SRCOR, cat.x_sub, cat.y_sub, pos.x, pos.y, $
               matchrad_pix, ind1, ind2, COUNT=count, /SILENT
        retstruct.nrecovered[i] = count
        retstruct.nadditional[i] = nfound - count
        
        ;;Make sure to NaN out the stuff we didn't find.
        IF count EQ 0 THEN BEGIN
           MESSAGE," No sources matching inputs found",/INF
           retstruct.xpos_recovered[i, *] = !VALUES.F_NAN
           retstruct.ypos_recovered[i, *] = !VALUES.F_NAN
           retstruct.f250_recovered[i, *] = !VALUES.F_NAN
           retstruct.f350_recovered[i, *] = !VALUES.F_NAN
           retstruct.f500_recovered[i, *] = !VALUES.F_NAN
        ENDIF ELSE BEGIN
           IF count LT nsrcs THEN BEGIN
              didfind = BYTARR(nsrcs)
              didfind[ind2] = 1b
              wnotfound = WHERE(~TEMPORARY(didfind), nnotfound)
              IF nnotfound EQ 0 THEN MESSAGE,"Logic error"
              retstruct.xpos_recovered[i, wnotfound] = !VALUES.F_NAN
              retstruct.ypos_recovered[i, wnotfound] = !VALUES.F_NAN
              retstruct.f250_recovered[i, wnotfound] = !VALUES.F_NAN
              retstruct.f350_recovered[i, wnotfound] = !VALUES.F_NAN
              retstruct.f500_recovered[i, wnotfound] = !VALUES.F_NAN
           ENDIF
           
           ;; Store information about what we got
           ;; Note that the catalog fluxes are in mJy, the
           ;; input fluxes are in Jy
           IF count GT 0 THEN BEGIN
              fmt = '(" ",I0," sources matching inputs found")'
              MESSAGE,STRING(count,FORMAT=fmt),/INF
              retstruct.found[i, ind2] = 1b
              retstruct.xpos_recovered[i, ind2] = cat[ind1].x_sub
              retstruct.ypos_recovered[i, ind2] = cat[ind1].y_sub
              retstruct.f250_recovered[i, ind2] = 1e-3 * cat[ind1].map_f250
              retstruct.f350_recovered[i, ind2] = 1e-3 * cat[ind1].map_f350
              retstruct.f500_recovered[i, ind2] = 1e-3 * cat[ind1].map_f500
           ENDIF
        ENDELSE
     ENDELSE
  ENDFOR
  
  RETURN,retstruct
END

