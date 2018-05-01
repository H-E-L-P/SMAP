;+
; NAME
;  smap_generate_bethermin_dual
; PURPOSE
;  Create a set of fake images using the Bethermin 2010 model based on
;  a set of input maps.  This does two maps -- one at high resolution,
;  one at low resolution using input maps.  The idea is to simulate
;  red sources as normal maps and in a form suitable for making
;  red-smoothed versions
; USAGE
;  smap_generate_bethermin_dual, dndlogldzdomega, cold, starburst, 
;                           mapbase1, mapbase2, outbase [, CATFILE=,
;                           CLUS= ]
; INPUTS
;  dndlogldzdomega Output of bethermin_dndlogldzdomega
;  cold          Holds information about IAS cold templates
;  starburst     Holds information about IAS starburst templates
;  mapbase1      Base name of map files to load, set 1
;  mapbase2      Base name of map files to load, set 2
;  outbase       Root name for output files
; OPTIONAL INPUTS
;  mapdir        Directory to look for maps in (def: !SMAP_MAPS)
;  nfluxes       Number of points to generate cumulative flux
;                 distribution at (def: 65536)
;  catfile       Write the catalog of sources to this file (fits)
;  seed          Seed for random number generator
;  localz        Set this to mask out sources at lower z than this
;                 on the assumption they wouldn't be included anyways
;  clus          Structure with clustering information.  Has two fields:
;                  .k (in inverse arcmin) and .pk
;  outdir        Directory path to prepend to output file name.
;  killred       A structure giving rules for removing red sources
;                 from the catalog.  Has fields .max (array of length
;                 nbeams) and .clr (length nbeams-1).  For each
;                 band, this results in throwing out all sources
;                 with flux[band] greater than .max[band] and
;                 flux[band+1] > .clr[band]*flux[band].  For example,
;                 if wave=[250,350,500], max=[0.01,0.02,0.03], clr=[0.1,0.8],
;                 then all sources that have 250 micron flux > 10 mJy,
;                 350 > 20 mJy, 500 > 30 mJy and f350 > 0.1*f250,
;                 f500>0.8*f350 will be removed.  Set any value to
;                 NaN to ignore.
;  maskfiles      List of mask files -- see construct_user_mapmask
;  maskdir        Directory to look for masks in
; KEYWORDS
;  verbose       Prints status messages
; SIDE EFFECTS
;  Writes 6 files, 3 for each set
; NOTES
;   The images are mean subtracted.  With rebinning, the output
;   catalog only gives the position to the nearest pixel.
; MODIFICATION HISTORY
;  Author: Alex Conley, August 17, 2012
;-

FUNCTION smap_generate_bethermin_dual_extent, map
  COMPILE_OPT IDL2, STRICTARRSUBS

  ;;Set up boundary box
  xpos = [ FINDGEN(map.xsize), REPLICATE(map.xsize-1, map.ysize),$
           REVERSE(FINDGEN(map.xsize)), REPLICATE(0, map.ysize) ]
  ypos = [ REPLICATE(0, map.xsize), FINDGEN(map.ysize),$
           REPLICATE(map.ysize-1, map.xsize), REVERSE(FINDGEN(map.ysize)) ]
  
  XY2AD, xpos, ypos, map.astrometry, ra, dec

  minra = MIN(ra, MAX=maxra)
  mindec = MIN(dec, MAX=maxdec)

  ;;Check for boundary crossing in ra assuming field smaller than, say
  ;; 60 deg across
  IF maxra - minra GT 60.0 THEN $
     MESSAGE,"Field is either too large or there is a RA=0 crossing"

  RETURN,[minra,maxra,mindec,maxdec]

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO smap_generate_bethermin_dual_domap, map, cat, minpixscale, outbase,$
                                        seed, VERBOSE=verbose, OUTDIR=outdir,$
                                        MASKFILES=maskfiles, MASKDIR=maskdir,$
                                        SMOOTH=smooth, NONOISE=nonoise
  COMPILE_OPT IDL2, STRICTARRSUBS

  IF KEYWORD_SET(verbose) THEN $
     MESSAGE,"Doing band: "+map.names,/INF
  
  CASE map.names OF
     'PSW' : fluxidx = 0
     'PMW' : fluxidx = 1
     'PLW' : fluxidx = 2
     ELSE : MESSAGE,"Unexpected map band: "+map.names
  ENDCASE
  beamsizes = [17.6, 23.9, 35.2] ;;same as in smap_redsource_smooth
  beamsize = beamsizes[fluxidx]

  ;;Get internal x/y positions at pixel scale
  scale = minpixscale / map.pixscale
  xpos = ROUND(cat.xpos * scale)
  ypos = ROUND(cat.ypos * scale)
  fluxes = cat.fluxes[fluxidx]

  ;;This can run off the edges
  wgd=WHERE( xpos GE 0 AND ypos GE 0 AND xpos LT map.xsize-1 AND $
           ypos LT map.ysize-1, nw, NCOMPLEMENT=nbad)
  IF nw EQ 0 THEN MESSAGE,"No sources in final map"
  IF nbad NE 0 THEN BEGIN
     xpos = xpos[wgd]
     ypos = ypos[wgd]
     fluxes = fluxes[wgd]
  ENDIF ELSE st = N_ELEMENTS(TEMPORARY(wgd))

  ;;Get list of all non-finite pixels in original image so we
  ;; can replicate later
  wnonfin = WHERE( ~ FINITE( map.image ), nnonfin )

  ;;Can't do this 'in parallel' because IDL can't handle
  ;; multiple sources ending up in the same pixel
  ;;That is,
  ;; image[cat.xpos,cat.ypos] += cat.flux
  ;;doesn't work, and worse, appears to work while
  ;; quietly doing the wrong thing.
  IF KEYWORD_SET(verbose) THEN MESSAGE,"  Adding sources",/INF
  map.image = 0.0
  FOR j=0,nw-1 DO map.image[xpos[j], ypos[j]] += fluxes[j]
  st = N_ELEMENTS(TEMPORARY(xpos))
  st = N_ELEMENTS(TEMPORARY(ypos))
  st = N_ELEMENTS(TEMPORARY(fluxes))

  ;;Convolve
  IF KEYWORD_SET(verbose) THEN MESSAGE,"  Convolving",/INF
  kernel1d = GET_SPIRE_BEAM(map.names, map.pixscale, FWHM=beamsize,$
                            /SILENT, /FACTOR)
  map.image = CONVOLVE_FACTOR(map.image, kernel1d, /WRAP)

  ;;Add errors
  IF ~ KEYWORD_SET(nonoise) THEN $
     map.image += map.error*RANDOMN(seed, map.xsize, map.ysize)

  IF nnonfin NE 0 THEN map.image[ TEMPORARY(wnonfin) ] = !VALUES.D_NAN

  ;;Add mask
  IF N_ELEMENTS(maskfiles) NE 0 THEN $
     ADD_USER_MAPMASK, map, maskfiles, MASKDIR=maskdir

  ;;Smooth
  IF KEYWORD_SET(smooth) THEN BEGIN
     IF KEYWORD_SET(verbose) THEN MESSAGE,"  Smoothing",/INF
     smap_redsource_smooth, map, /BRUTE, /NOPEAK
  ENDIF

  ;;Mean subtract
  w = WHERE(map.mask EQ 0, nw, NCOMPLEMENT=nn)
  IF nn EQ 0 THEN $
     mnval = MEAN(map.image,/NAN) ELSE mnval = MEAN(map.image[w],/NAN)
  map.image -= mnval

  ;;Write
  IF KEYWORD_SET(verbose) THEN MESSAGE,"  Writing",/INF
  success = WRITE_SMAP_FITSMAP(map,outbase,DIR=outdir,$
                               ERRMSG=errmsg,/SILENT)
  IF ~ success THEN MESSAGE,"Error writing output file1: "+errmsg
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO smap_generate_bethermin_dual, dndlogldzdomega, cold, starburst, $
                                  mapbase1, mapbase2, outbase,$
                                  MAPDIR1=mapdir1, MAPDIR2=mapdir2,$
                                  OUTDIR=outdir, SEED=seed, NFLUXES=nfluxes, $
                                  VERBOSE=verbose, CATFILE=catfile, $
                                  LOCALZ=localz, CLUS=clus, $
                                  KILLRED=killred, MASKFILES=maskfiles,$
                                  MASKDIR=maskdir, SMOOTH1=smooth1,$
                                  SMOOTH2=smooth2, NONOISE=nonoise
  
  COMPILE_OPT IDL2, STRICTARRSUBS

  IF N_ELEMENTS(nfluxes) EQ 0 THEN nfluxes = 65536
  IF nfluxes LE 0 THEN MESSAGE,"Invalid nfluxes "+STRING(nfluxes)
  IF N_ELEMENTS(outdir) EQ 0 THEN i_outdir='./' ELSE i_outdir=ADDSLASH(outdir)

  IF N_ELEMENTS(clus) NE 0 THEN BEGIN
     IF ~ TAG_EXIST(clus,'k',/TOP) THEN MESSAGE,"Clus doesn't have .k field"
     IF ~ TAG_EXIST(clus,'pk',/TOP) THEN MESSAGE,"Clus doesn't have .pk field"
     IF N_ELEMENTS(clus.pk) NE N_ELEMENTS(clus.k) THEN $
        MESSAGE,"clus.pk and clus.k must be same length"
     has_clustering = 1b
  ENDIF ELSE has_clustering=0b

  IF N_ELEMENTS(dndlogldzdomega) EQ 0 THEN $
     MESSAGE,"dndlogldzdomega not provided"
  IF N_ELEMENTS(cold) EQ 0 THEN MESSAGE,"Cold not provided"
  IF N_ELEMENTS(starburst) EQ 0 THEN MESSAGE,"Starburst not provided"
  IF N_ELEMENTS(mapbase1) EQ 0 THEN MESSAGE,"mapbase1 not provided"
  IF N_ELEMENTS(mapbase2) EQ 0 THEN MESSAGE,"mapbase2 not provided"
  IF N_ELEMENTS(outbase) EQ 0 THEN MESSAGE,"outbase not provided"

  ;;We have to figure out how large of an area to make
  ;;That means getting the ra/dec limits of each
  ;;The easiest, although maybe not more efficient thing, is just to
  ;; read them all in.  So, do that.
  IF KEYWORD_SET(verbose) THEN MESSAGE,"Reading input maps",/INF
  map1_psw = READ_SMAP_FITSMAP(mapbase1,'PSW',DIR=mapdir1, /NO_ABORT,$
                               SUCCESS=rdsuccess, ERRMSG=errmsg, /SILENT)
  IF rdsuccess EQ 0 THEN MESSAGE,errmsg
  map1_pmw = READ_SMAP_FITSMAP(mapbase1,'PMW',DIR=mapdir1, /NO_ABORT,$
                               SUCCESS=rdsuccess, ERRMSG=errmsg, /SILENT)
  IF rdsuccess EQ 0 THEN MESSAGE,errmsg
  map1_plw = READ_SMAP_FITSMAP(mapbase1,'PLW',DIR=mapdir1, /NO_ABORT,$
                               SUCCESS=rdsuccess, ERRMSG=errmsg, /SILENT)
  IF rdsuccess EQ 0 THEN MESSAGE,errmsg
  map2_psw = READ_SMAP_FITSMAP(mapbase2,'PSW',DIR=mapdir2, /NO_ABORT,$
                               SUCCESS=rdsuccess, ERRMSG=errmsg, /SILENT)
  IF rdsuccess EQ 0 THEN MESSAGE,errmsg
  map2_pmw = READ_SMAP_FITSMAP(mapbase2,'PMW',DIR=mapdir2, /NO_ABORT,$
                               SUCCESS=rdsuccess, ERRMSG=errmsg, /SILENT)
  IF rdsuccess EQ 0 THEN MESSAGE,errmsg
  map2_plw = READ_SMAP_FITSMAP(mapbase2,'PLW',DIR=mapdir2, /NO_ABORT,$
                               SUCCESS=rdsuccess, ERRMSG=errmsg, /SILENT)
  IF rdsuccess EQ 0 THEN MESSAGE,errmsg
  pixsize = [map1_psw.pixscale, map1_pmw.pixscale, map1_plw.pixscale,$
             map2_psw.pixscale, map2_pmw.pixscale, map2_plw.pixscale]
  minpixscale = MIN(pixsize)

  ;;Get max/min ra/dec for all the maps (minra,maxra,mindec,maxdec)
  extent = smap_generate_bethermin_dual_extent(map1_psw)
  extent2 = smap_generate_bethermin_dual_extent(map1_pmw)
  extent[0] <= extent2[0] & extent[1] >= extent2[1]
  extent[2] <= extent2[2] & extent[3] >= extent2[3]
  meandec = 0.5 * (extent[2] + extent[3])
  minpixsizedeg = minpixscale / 3600.0
  rapix = (extent[1] - extent[0]) * cos(!PI * meandec / 180.0) / minpixsizedeg
  decpix = (extent[3] - extent[2]) / minpixsizedeg

  IF rapix GT 10000 THEN $ ;;excludes helms!
     MESSAGE,"RA extent seems too large in pixel space"
  IF decpix GT 10000 THEN $ ;;excludes helms!
     MESSAGE,"DEC extent seems too large in pixel space"

  ;;Area estimate in sq deg.
  area = rapix*decpix*(minpixsizedeg)^2

  ;;Determine how many sources we will generate
  ntot = area * dndlogldzdomega.totsources * (!PI/180.0)^2 ;;to sq deg
  IF ntot LE 0 THEN MESSAGE,"Error: no sources to generate"
  ntot = RANDOMU(seed,POISSON=ntot,/DOUBLE)
  ntot = ROUND(ntot)
  IF ntot EQ 0 THEN MESSAGE,"Not generating any sources!"
  IF KEYWORD_SET(verbose) THEN $
     MESSAGE,STRING(ntot,FORMAT='("Generating ",I0," sources")'),/INF

  ;;Make catalog
  IF KEYWORD_SET(verbose) THEN MESSAGE,"Generating Bethermin catalog",/INF
  cat = REPLICATE( {xpos: 0, ypos: 0, z: !VALUES.F_NAN, $
                    loglum: !VALUES.F_NAN,$
                    type: 0b, fluxes: DBLARR(3) },$
                    ntot )
  bcat = bethermin_gencat( ntot, dndlogldzdomega, cold, starburst, $
                           SEED=seed, WAVE=[250.0,350.0,500.0], $
                           VERBOSE=verbose )
  cat.z = bcat.z
  cat.loglum = bcat.loglum
  cat.type = bcat.type
  cat.fluxes = bcat.obsflux*1d-3 ;;to Jy from mJy
  st = N_ELEMENTS(temporary(bcat))

  IF N_ELEMENTS(localz) NE 0 THEN BEGIN
     wgood = WHERE(cat.z GE localz[0],ntot,NCOMPLEMENT=nbad)
     IF ntot EQ 0 THEN MESSAGE,"Local z masked all sources"
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,STRING(nbad,FORMAT='("Local z cut masking: ",I0," sources")'),$
                /INF
     cat = cat[wgood]
  ENDIF

  IF N_ELEMENTS(killred) NE 0 THEN BEGIN
     arered = BYTARR(N_ELEMENTS(cat),/NOZERO)
     arered[*] = 1b ;;start with everything red, force them to pass everything
     IF N_ELEMENTS(killred.max) LT 3 THEN $
        MESSAGE,"Killread doesn't have enough .max elements"
     IF N_ELEMENTS(killred.clr) LT 3-1 THEN $
        MESSAGE,"Killread doesn't have enough .clr elements"
     ;;flux test
     FOR bnd = 0, 3-1 DO BEGIN
        maxval = killred.max[bnd]
        IF ~ FINITE(maxval) THEN CONTINUE
        arered AND= (cat.fluxes[bnd] GT killred.max[bnd])
     ENDFOR
     ;;clr test
     FOR bnd=0,3-2 DO BEGIN
        clr = killred.clr[bnd]
        IF ~ FINITE(clr) THEN CONTINUE
        arered AND= (cat.fluxes[bnd+1] GT clr*cat.fluxes[bnd])
     ENDFOR
     
     wnotred = WHERE( ~TEMPORARY(arered), nnotred, NCOMPLEMENT= nred )
     IF nnotred EQ 0 THEN MESSAGE,"Killred eliminates all sources"
     IF KEYWORD_SET(verbose) THEN $
       MESSAGE,STRING(nred,N_ELEMENTS(cat),$
                      FORMAT='("Killred eliminates ",I0," of ",I0," objs")'),$
               /INF
     IF nred NE 0 THEN cat = cat[wnotred]
     ntot = nnotred
  ENDIF


  ;;Get base positions (in most finely pixelated map) of each source
  IF has_clustering THEN BEGIN
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,"Generating clustered source positions",/INF
     prob_im = GENERATE_SOURCE_PROB( clus.k, clus.pk, rapix, $
                                     NUMPIX2=decpix, $
                                     PIXSIZE=minpixscale, SEED=seed, /USINGK )
     SMAP_POPULATE_SOURCES, TEMPORARY(prob_im), ntot, xpos, ypos, SEED=seed
     cat.xpos = TEMPORARY(xpos)
     cat.ypos = TEMPORARY(ypos)
  ENDIF ELSE BEGIN
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,"Generating un-clustered source positions",/INF
     cat.xpos = FLOOR( RANDOMU( seed, ntot ) * rapix )
     cat.ypos = FLOOR( RANDOMU( seed, ntot ) * decpix )
  ENDELSE


  ;;Now, generate the image in each band
  outbase1 = outbase+'_set1'
  IF KEYWORD_SET(verbose) THEN MESSAGE,"Doing set 1",/INF
  smap_generate_bethermin_dual_domap, TEMPORARY(map1_psw), $
                                      cat, minpixscale, outbase1, $
                                      seed, OUTDIR=outdir, VERBOSE=verbose,$
                                      MASKFILES=maskfiles, MASKDIR=maskdir,$
                                      NONOISE=nonoise,SMOOTH=smooth1
  smap_generate_bethermin_dual_domap, TEMPORARY(map1_pmw), $
                                      cat, minpixscale, outbase1, $
                                      seed, OUTDIR=outdir, VERBOSE=verbose,$
                                      MASKFILES=maskfiles, MASKDIR=maskdir,$
                                      NONOISE=nonoise, SMOOTH=smooth1
  smap_generate_bethermin_dual_domap, TEMPORARY(map1_plw), $
                                      cat, minpixscale, outbase1, $
                                      seed, OUTDIR=outdir, VERBOSE=verbose,$
                                      MASKFILES=maskfiles, MASKDIR=maskdir,$
                                      NONOISE=nonoise, SMOOTH=smooth1
  outbase2 = outbase+'_set2'
  IF KEYWORD_SET(verbose) THEN MESSAGE,"Doing set 2",/INF
  smap_generate_bethermin_dual_domap, TEMPORARY(map2_psw), $
                                      cat, minpixscale, outbase2, $
                                      seed, OUTDIR=outdir, VERBOSE=verbose,$
                                      MASKFILES=maskfiles, MASKDIR=maskdir,$
                                      NONOISE=nonoise, SMOOTH=smooth2
  smap_generate_bethermin_dual_domap, TEMPORARY(map2_pmw), $
                                      cat, minpixscale, outbase2, $
                                      seed, OUTDIR=outdir, VERBOSE=verbose,$
                                      MASKFILES=maskfiles, MASKDIR=maskdir,$
                                      NONOISE=nonoise, SMOOTH=smooth2
  smap_generate_bethermin_dual_domap, TEMPORARY(map2_plw), $
                                      cat, minpixscale, outbase2, $
                                      seed, OUTDIR=outdir, VERBOSE=verbose,$
                                      MASKFILES=maskfiles, MASKDIR=maskdir,$
                                      NONOISE=nonoise, SMOOTH=smooth2

  ;;Output catalog
  IF N_ELEMENTS(catfile) NE 0 THEN MWRFITS, cat, catfile, /CREATE

END
