;+
;NAME
; redsource_purity
;PURPOSE
; To do red source purity computations using the Bethermin
; model in SPIRE bands
;USAGE
; nsources = redsource_purity( mapbase, k1, k2, cutvals,
;                              maxflux, beamfiles [, NSIMS=, SN=,
;                              REBINFAC=, MASKFILES=, NFOUNDBINS=, OUTBASE=])
;INPUTS
; mapbase    Base name of smap maps to base simulations on.
; k1         PSW coefficient for map combination
; k2         PMW coefficient for map combination
; cutvals    2 element array; sources must have f350>cutvals[0]*f250
;             and f500>cutvals[1]*f350 in the map to be accepted.
;             This can actually be 2D (ncuts by 2), in which case
;             the various values are looped over
; maxflux    Sources brighter than this (3 element array, one for each
;             flux, in Jy) will be excluded from the generated images
; beamfiles  List of beam files to use.  Must be 3 (PSW, PMW, PLW)
;             at same pixel scale as mapin (unless rebinfac is set)
;RETURNS
; The number of sources found, which would be zero in an ideal case
;KEYWORDS
; brute      Use brute force convolution in red map construction
; noclus     Don't include clustering
; nolens     Don't include lensing
; usepeakpos Use positions from peak flux location for map based fluxes
;OPTIONAL INPUTS
; outbase    Base name for output files
; mapdir     Directory to look for input maps in
; nsims      Number of simulations to do (def: 20)
; sn         Required signal-to-noise of detection; can be an array (def: 8)
; rebinfac   Amount to rebin maps when generating fake images.  By
;             default this isn't applied, and probably shouldn't be
;             for the small pixel sized red source maps this will
;             usually be used with
; maskfiles  Mask files to use in redmap construction; 
;             see construct_user_mapmask
; maskdir    Directory to look for input masks in
;SIDE EFFECTS
;  Writes some temporary maps (outbase+'_P?W.fits') to the current
;  directory while working.  
;MODIFICATION HISTORY
; Author: Alex Conley, Dec 2011
;-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION redsource_purity, mapbase, coeffs, cutvals, maxflux, beamfiles, $
                           NSIMS=nsims, SN=sn, REBINFAC=rebinfac, BRUTE=brute,$
                           MASKFILES=maskfiles, VERBOSE=verbose, NOCLUS=noclus,$
                           NOLENS=nolens, SEED=seed, MAPDIR=mapdir, $
                           MASKDIR=maskdir, OUTBASE=outbase, ERRORS=errors, $
                           CORR=corr, USEPEAKPOS=usepeakpos
  COMPILE_OPT IDL2, STRICTARRSUBS

  ;;input checks and defaults
  IF N_ELEMENTS(mapbase) EQ 0 THEN MESSAGE,"mapbase not provided"
  IF N_ELEMENTS(coeffs) NE 3 THEN MESSAGE,"Coeffs should have 3 elements"
  szc = SIZE(cutvals)
  ncutsdim = szc[0]
  IF ncutsdim LE 0 OR ncutsdim GT 2 THEN MESSAGE,"Can't interpret cutvals"
  ncutsslice = szc[ncutsdim]
  IF ncutsslice NE 2 THEN MESSAGE,"Cutvals must have 2 elements per slice"
  CASE ncutsdim OF
     1 : ncuts = 1
     2 : ncuts = szc[1]
     ELSE : MESSAGE,"Cant' interpret cutvals"
  END

  IF N_ELEMENTS(maxflux) LT 3 THEN MESSAGE,"Maxflux must have 3 elements"
  IF N_ELEMENTS(beamfiles) LT 3 THEN MESSAGE,"Need 3 beam files"
  IF N_ELEMENTS(nsims) EQ 0 THEN nsims=20
  IF nsims LE 0 THEN MESSAGE,"Invalid (non-positive) number of sims"
  IF N_ELEMENTS(sn) EQ 0 THEN sn=8
  IF MIN(sn) LE 0 THEN MESSAGE,"Invalid (non-positive) sn"
  IF N_ELEMENTS(outbase) EQ 0 THEN i_outbase='temp_redmap' ELSE $
     i_outbase=outbase
  IF N_ELEMENTS(rebinfac) NE 0 && rebinfac LE 0 THEN $
     MESSAGE,"Invalid (non-positive) rebinfac"
  ;;sig_i*sqrt(s), conf
  IF N_ELEMENTS(errors) EQ 0 THEN errors = [6.74, 4.23]*1e-3 
  IF N_ELEMENTS(corr) EQ 0 THEN corr=0.85

  ;;Get info for sims
  simdatadir = addslash(!SMAP_PIPELINE_PATH)+'simulations/data/'
  dNdLogL = MRDFITS(simdatadir+'bethermin_dNdLogLdzdOmega.fits',1,/SILENT)
  cold = MRDFITS(simdatadir+'ias_cold.fits',1,/SILENT)
  starburst = MRDFITS(simdatadir+'ias_starburst.fits',1,/SILENT)
  IF ~ KEYWORD_SET( nolens ) THEN $
     RESTORE,simdatadir+'debernardis_lensing_prob.sav'
  IF ~ KEYWORD_SET( noclus ) THEN BEGIN
     READCOL,simdatadir+'pk500mu_nfw_3Rvir_v230709.out',k,p1a,p1b,p2,pk,$
             FORMAT='(D,F,F,F,D)',/SILENT
     clus = {k:TEMPORARY(k),pk:TEMPORARY(pk)}
  ENDIF

  bands = ['PSW','PMW','PLW']
  bgsubsize = 48

  outdir = ADDSLASH( !SMAP_MAPS )

  ;;setup return variable
  nsn = N_ELEMENTS(sn)
  retstr = { sn: !VALUES.F_NAN, nfound: 0, min_df: !VALUES.F_NAN,$
             max_df: !VALUES.F_NAN, cut0: !VALUES.F_NAN,$
             cut1: !VALUES.F_NAN}
  retvar = REPLICATE(retstr,nsims,nsn,ncuts)

  fmt = '("Processing ",I0," sims with ",I0," S/N ratios and ",I0," cuts")'
  MESSAGE,STRING(nsims,nsn,ncuts,FORMAT=fmt),/INF

  ;;The main event
  ;;In killred, always remove only truly red sources (f500 > f350),
  ;; whatever cutvals is.
  killred = { max: maxflux, clr: [0.1,1.0] }
  sigmas = [1.0,1.0,1.0]
  IF KEYWORD_SET( verbose ) THEN MESSAGE,"Entering main loop",/INF
  FOR i=0,nsims-1 DO BEGIN
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,STRING(i+1,nsims,FORMAT='("Simulation ",I0," of ",I0)'),/INF
     
     ;;Generate fake images in all three bands, write to disk
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE," Generating simulated images",/INF
     SMAP_GENERATE_BETHERMIN_FROMMAP, dNdLogL, cold, starburst, mapbase,$
                                      bands, beamfiles, i_outbase, $
                                      REBINFAC=rebinfac, SIGMAS=sigmas, $
                                      SEED=seed, LOCALZ=0.1, CLUS=clus, $
                                      OUTDIR=outdir, KILLRED=killred, $
                                      MASKFILES=maskfiles, MAPDIR=mapdir,$
                                      MASKDIR=maskdir;, CATFILE='input_cat.fits'

     ;;Read back in generated images and remove from disk
     map250 = READ_SMAP_FITSMAP(i_outbase,bands[0],/SILENT,DIR=outdir)
     map350 = READ_SMAP_FITSMAP(i_outbase,bands[1],/SILENT,DIR=outdir)
     map500 = READ_SMAP_FITSMAP(i_outbase,bands[2],/SILENT,DIR=outdir)
     FOR bnd=0,N_ELEMENTS(bands)-1 DO $
        FILE_DELETE, outdir+i_outbase+'_'+bands[bnd]+'.fits',/ALLOW

     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,"Smoothing simulated maps",/INF
     ;;Don't smooth error since we toss that information inside the
     ;; sourcefinder based on ERRORS=
     smap_redsource_smooth, map250, /BRUTE, /NOERRORSMOOTH, /NOPEAK
     smap_redsource_smooth, map350, /BRUTE, /NOERRORSMOOTH, /NOPEAK
     smap_redsource_smooth, map500, /BRUTE, /NOERRORSMOOTH, /NOPEAK

     ;;Write em as test
     ;st = write_smap_fitsmap(map250,'smooth',/SILENT)
     ;st = write_smap_fitsmap(map350,'smooth',/SILENT)
     ;st = write_smap_fitsmap(map500,'smooth',/SILENT)

     ;;Find sources
     IF KEYWORD_SET( verbose ) THEN $
        MESSAGE," Finding sources",/INF
     FOR ctr=0,nsn-1 DO BEGIN
        retvar[i,ctr,*].sn = sn[ctr]
        IF KEYWORD_SET(verbose) THEN $
           MESSAGE,STRING(sn[ctr],FORMAT='("  Doing S/N:",F0.1)'),/INF
        FOR ctr2 = 0, ncuts-1 DO BEGIN
           IF ncutsdim EQ 1 THEN ctvals = [cutvals[0],cutvals[1]] ELSE $
              ctvals = [ cutvals[ctr2,0], cutvals[ctr2,1] ]
           retvar[i,ctr,ctr2].cut0 = ctvals[0]
           retvar[i,ctr,ctr2].cut1 = ctvals[1]
           IF KEYWORD_SET(verbose) THEN $
              MESSAGE,STRING(ctvals[0],ctvals[1],$
                             FORMAT='("    Doing Cut0:",F0.2,'+$
                             '" Cut1:",F0.2)'),/INF
           findpeakpix = KEYWORD_SET(usepeakpos)
           curr_cat = red_sourcefind_dofind( map250, map350, map500,$
                                             COEFFS=coeffs, SN=sn[ctr],$
                                             BGSUBSIZE=bgsubsize,$
                                             MIN_CORR=corr, FLUXRAT=ctvals,$
                                             ERRORS=errors, /NOWRITE,$
                                             SUCCESS=getcat_success,$
                                             USEPEAKPOS=usepeakpos,$
                                             FINDPEAKPIX=findpeakpix)
           ;MWRFITS,curr_cat,'red_cat.fits',/CREATE
           IF getcat_success THEN BEGIN
              retvar[i,ctr,ctr2].nfound = N_ELEMENTS(curr_cat)
              retvar[i,ctr,ctr2].min_df = MIN(curr_cat.f_sub,MAX=max_df)
              retvar[i,ctr,ctr2].max_df = max_df
           ENDIF 
        ENDFOR
     ENDFOR
  ENDFOR

  RETURN,retvar

END
