;+
;NAME
; redsource_getcat
;PURPOSE
; Gets catalog from a set of input maps and their linear combination
; map, assuming you have already built the background maps and done
; the combination.
;USAGE
;   cat = redsource_getcat(map250,map350,map500,mapdiff,bgmap250,bgmap350,$
;                          bgmap500, bgmapdiff, SIG=sig, CORR=corr,$
;                          FLUXRAT=, VERBOSE=, SUCCESS=)
;-
FUNCTION redsource_getcat, map250, map350, map500, mapdiff, $
                           bgmap250, bgmap350, bgmap500, bgmap, $
                           SIG=sig, CORR=corr, FLUXRAT=fluxrat,$
                           VERBOSE=verbose, SUCCESS=success
  COMPILE_OPT IDL2, HIDDEN
  success = 0b

  IF N_ELEMENTS(sig) EQ 0 THEN sig=6
  IF N_ELEMENTS(corr) EQ 0 THEN corr=0.85
  IF N_ELEMENTS(fluxrat) EQ 0 THEN i_fluxrat = [1.0,1.0] ELSE $
     i_fluxrat = fluxrat

  final_fwhm = 35.3*SQRT(2)
  npix_simple = 9 ;;size of simple phot box

  ;;Get the catalog (starfinder)
  IF KEYWORD_SET(verbose) THEN MESSAGE," Source finding",/INF
  cat = ps_extractor_doband(mapdiff, sig, 'multiband', FWHM=final_fwhm,$
                            PRE_SMOOTH=0b, ALG=2, MIN_CORR=corr,$
                            MAPMASKBITS=1uL, BGINPUT=bgmap, SUCCESS=csuccess)
  IF csuccess EQ 0 THEN RETURN,!VALUES.F_NAN
  

  ;;Now -- go back to original maps and check fluxes
  xy2ad,cat.x,cat.y,mapdiff.astrometry,ra,dec
  flux250 = smap_simplephot(map250, ra, dec, FWHM=final_fwhm, $
                            ERR=flux_err250, /USEMASK,$
                            NPIX=npix_simple, BGRND=bgmap250)
  flux350 = smap_simplephot(map350, ra, dec, FWHM=final_fwhm, $
                            ERR=flux_err350, /USEMASK,$
                            NPIX=npix_simple, BGRND=bgmap350)
  flux500 = smap_simplephot(map500, ra, dec, FWHM=final_fwhm, $
                            ERR=flux_err500, /USEMASK,$
                            NPIX=npix_simple, BGRND=bgmap500)
  
  ;;Errs are uber-correlated, so undo
  ;; we actually ignore these
  flux_err250 *= npix_simple/2
  flux_err350 *= npix_simple/2
  flux_err500 *= npix_simple/2
  
  ;;Prob check that each source is red and positive flux
  prob_red = GAUSS_TRIPLEREDCHECK( flux250, flux_err250, flux350,$
                                   flux_err350, flux500, flux_err500,$
                                   SEED=seed, FLUXPOS=fluxpos, $
                                   FLUXRAT=i_fluxrat, /NO250POS )

  ;;Apply cut metric based on i_fluxrat
  ;; Note we don't use the probability any more, we just record it
  wgood = WHERE( fluxpos GE 0.75 AND $
                 flux350 GE i_fluxrat[0]*flux250 AND $
                 flux500 GE i_fluxrat[1]*flux350,$
                 ncat, NCOMPLEMENT=nbad )
  

  IF ncat EQ 0 THEN RETURN,!VALUES.F_NAN
  IF nbad NE 0 THEN cat = cat[wgood]

  ;;Make catalog structure with this information
  final_cat = REPLICATE({ x_sub: !VALUES.F_NAN, y_sub: !VALUES.F_NAN,$
                          ra_sub: !VALUES.F_NAN, dec_sub: !VALUES.F_NAN,$
                          f_sub: !VALUES.F_NAN, df_sub: !VALUES.F_NAN,$
                          simple_f250: !VALUES.F_NAN, $
                          simple_df250: !VALUES.F_NAN, $
                          simple_f350: !VALUES.F_NAN, $
                          simple_df350: !VALUES.F_NAN, $
                          simple_f500: !VALUES.F_NAN, $
                          simple_df500: !VALUES.F_NAN, $
                          prob_red: !VALUES.F_NAN,$
                          fluxpos: !VALUES.F_NAN,$
                          corr: !VALUES.F_NAN }, ncat )
  final_cat.x_sub = cat.x
  final_cat.y_sub = cat.y
  final_cat.ra_sub = ra[wgood]
  final_cat.dec_sub = dec[wgood]
  final_cat.f_sub = cat.f*1e3 ;;to mJy
  final_cat.df_sub = cat.df*1e3
  final_cat.corr = cat.corr
  final_cat.simple_f250 = flux250[wgood]
  final_cat.simple_df250 = flux_err250[wgood]
  final_cat.simple_f350 = flux350[wgood]
  final_cat.simple_df350 = flux_err350[wgood]
  final_cat.simple_f500 = flux500[wgood]
  final_cat.simple_df500 = flux_err500[wgood]
  final_cat.prob_red = prob_red[wgood]
  final_cat.fluxpos = fluxpos[wgood]

  success = 1b
  RETURN,final_cat

END
