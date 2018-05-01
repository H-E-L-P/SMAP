;+
; NAME
;  smap_generate_bethermin_gauss
; PURPOSE
;  Create a set of fake images using the Bethermin 2010 model assuming
;  Gaussian beams.
; USAGE
;  smap_generate_bethermin_gauss, dndlogldzdomega, cold, starburst, 
;                                 wave, fwhm, area, outbase [, CATFILE=,
;                                 CLUS=, REBINFAC=, PIXSIZE= ]
; INPUTS
;  dndlogldzdomega Output of bethermin_dndlogldzdomega
;  cold          Holds information about IAS cold templates
;  starburst     Holds information about IAS starburst templates
;  wave          Wavelengths of maps, in microns
;  fwhm          FWHM of maps, in arcsec
;  area          Area of the maps, in sq deg
;  outbase       Root name for output files
; OPTIONAL INPUTS
;  sigmas        Instrument noise, per pixel (def: 0).  Can be an array.
;  pixsize       Pixel size, in arcsec.  (def: FWHM/3 after rebinning)
;  nfluxes       Number of points to generate cumulative flux
;                 distribution at (def: 65536)
;  rebinfac      Factor to rebin output maps by.  Must be integral
;  catfile       Write the catalog of sources to this file (fits)
;  seed          Seed for random number generator
;  localz        Set this to mask out sources at lower z than this
;                 on the assumption they wouldn't be included anyways
;  clus          Structure with clustering information.  Has two fields:
;                  .k (in inverse arcmin) and .pk
;  bands         Name of bands (def: 'bandN' where N goes from 1 to
;                 the number of bands).  Used for output purposes
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
; KEYWORDS
;  verbose       Prints status messages
;  exposure      Add exposure information to output maps, all set to
;                 this value
; SIDE EFFECTS
;   Writes some files.
; NOTES
;   The images are mean subtracted.  With rebinning, the output
;   catalog only gives the position to the nearest pixel.
; MODIFICATION HISTORY
;  Author: Alex Conley, August 30, 2012
;-


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION smap_generate_bethermin_gauss_domap, cat, cat_pixscale, fluxidx, $
   pixscale, fwhm, band, wave, area, seed, racen, deccen, sigma,$
   REBINFAC=rebinfac, VERBOSE=verbose, EXPOSURE=exposure


  COMPILE_OPT IDL2, STRICTARRSUBS

  ;;Figure out how large a map to construct
  IF N_ELEMENTS(rebinfac) NE 0 && ABS(rebinfac-1.0) GT 1d-5 THEN $
     final_rebin = 1b ELSE final_rebin = 0b
  IF final_rebin THEN BEGIN
     rebinfac = FIX(rebinfac) ;;Make integer
     npix_x = ROUND(SQRT(area)*3600.0/pixscale)
     npix_y = npix_x
     genpix_x = npix_x * rebinfac
     genpix_y = genpix_x
     genpixscale = pixscale / rebinfac
  ENDIF ELSE BEGIN
     npix_x = ROUND(SQRT(area)*3600.0/pixscale)
     npix_y = npix_x
     genpix_x = npix_x
     genpix_y = npix_x
     genpixscale = pixscale
  ENDELSE
  
  fluxes = cat.fluxes[fluxidx]
  
  ;;Get internal x/y positions at pixel scale
  relscale = cat_pixscale / genpixscale
  xpos = FLOOR(cat.xpos * relscale)
  ypos = FLOOR(cat.ypos * relscale)

  wkeep = WHERE( xpos GE 0 AND ypos GE 0 AND $
                 xpos LE genpix_x-1 AND ypos LE genpix_y-1, nkeep,$
                 NCOMPLEMENT=nremove )
  IF nkeep EQ 0 THEN MESSAGE,"No points to keep"
  IF nremove NE 0 THEN BEGIN
     xpos = xpos[wkeep]
     ypos = ypos[wkeep]
     fluxes = fluxes[ TEMPORARY(wkeep) ]
  ENDIF

  IF genpixscale GT fwhm THEN MESSAGE,"Resulting image will be undersampled"

  ;;Fill in image
  IF KEYWORD_SET(verbose) THEN MESSAGE,"  Adding sources",/INF
  image = DBLARR(genpix_x, genpix_y)
  ;;Can't be done in parallel (that is, image[xpos,ypos] += fluxes
  ;; doesn't work correctly)
  FOR j=0,nkeep-1 DO image[ xpos[j], ypos[j] ] += fluxes[j]
  DELVARX, xpos, ypox, fluxes

  ;;Now get beam and convolve
  IF KEYWORD_SET(verbose) THEN MESSAGE,"  Convolving",/INF
  kernel1d = GET_SPIRE_BEAM('PSW', genpixscale, FWHM=fwhm,$
                            /SILENT, /FACTOR)
  image = CONVOLVE_FACTOR(image, kernel1d, /WRAP)

  ;;Rebin
  IF final_rebin THEN image = REBIN(image,npix_x,npix_y)

  ;;Create output structure
  noerr = ~ (sigma GT 0.0)
  noexp = N_ELEMENTS(exposure) EQ 0
  mapstruct = get_smap_mapstruct(NPIXX=npix_x,NPIXY=npix_y,$
                                 /NOMASK,NOERR=noerr,NOEXP=noexp,/SILENT,$
                                 ERRMSG=errmsg, SUCCESS=getmap_succ,$
                                 /NO_ABORT, BAND=band, LAMBDA=wave)
  IF ~ getmap_succ THEN MESSAGE,"Error getting structure: "+errmsg
  mapstruct.image = TEMPORARY(image)
  IF ~ noexp AND exposure GT 0 THEN mapstruct.exposure = DOUBLE(exposure)

  ;;add noise.  
  IF sigma GT 0.0 THEN BEGIN
     mapstruct.image += sigma*RANDOMN(seed,npix_x,npix_y)
     mapstruct.error = sigma
  ENDIF

  ;;Mean subtract
  mapstruct.image -= MEAN(mapstruct.image,/NAN)

  ;;Astrometry
  mapstruct.astrometry.cdelt = [1,1]
  mapstruct.astrometry.crpix = 0.5*[npix_x,npix_y]
  mapstruct.astrometry.crval = [racen,deccen]
  mapstruct.pixscale = pixscale
  mapstruct.astrometry.cd[0,0] = -pixscale/3600.0
  mapstruct.astrometry.cd[1,1] = pixscale/3600.0

  RETURN, mapstruct
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO smap_generate_bethermin_gauss, dndlogldzdomega, cold, starburst, $
                                   wave, fwhm, area, outbase, $
                                   OUTDIR=outdir, SEED=seed, NFLUXES=nfluxes, $
                                   VERBOSE=verbose, CATFILE=catfile, $
                                   LOCALZ=localz, CLUS=clus, $
                                   KILLRED=killred, PIXSIZE=pixsize,$
                                   REBINFAC=rebinfac, BANDS=bands, $
                                   RACEN=racen, DECCEN=deccen, SIGMAS=sigmas,$
                                   EXPOSURE=exposure
  
  COMPILE_OPT IDL2, STRICTARRSUBS

  IF N_ELEMENTS(racen) EQ 0 THEN racen = 120.0
  IF N_ELEMENTS(deccen) EQ 0 THEN deccen = 10.0
  IF N_ELEMENTS(nfluxes) EQ 0 THEN nfluxes = 65536
  IF nfluxes LE 0 THEN MESSAGE,"Invalid nfluxes "+STRING(nfluxes)
  IF area LE 0 THEN MESSAGE,"Invalid area"
  nbands = N_ELEMENTS(wave)
  IF nbands EQ 0 THEN MESSAGE,"No wave provided"
  IF N_ELEMENTS(fwhm) NE nbands THEN MESSAGE,"Wrong number of fwhm provided"
  IF N_ELEMENTS(sigmas) EQ 0 THEN i_sigmas = REPLICATE(0.0,nbands) ELSE BEGIN
     IF N_ELEMENTS(sigmas) EQ 1 THEN $
        i_sigmas = REPLICATE(sigmas,nbands) ELSE $
           i_sigmas = sigmas
  ENDELSE
  IF N_ELEMENTS(i_sigmas) NE nbands THEN MESSAGE,"Wrong number of sigmas"
  IF N_ELEMENTS(pixsize) EQ 0 THEN i_pixsize = fwhm / 3.0 ELSE $
     i_pixsize = pixsize
  IF N_ELEMENTS(i_pixsize) NE nbands THEN BEGIN
     IF N_ELEMENTS(i_pixsize) EQ 1 AND nbands GT 0 THEN $
        i_pixsize = REPLICATE(i_pixsize, nbands) ELSE $
           MESSAGE,"Wrong number of pixsizes"
  ENDIF
  IF N_ELEMENTS(bands) EQ 0 THEN $
     bands = STRING(INDGEN(nbands)+1,FORMAT='("bands",I0)')
  IF N_ELEMENTS(bands) NE nbands THEN MESSAGE,"Wrong number of bands"
  IF N_ELEMENTS(outdir) EQ 0 THEN i_outdir='./' ELSE i_outdir=ADDSLASH(outdir)
  IF N_ELEMENTS(rebinfac) NE 0 THEN BEGIN
     IF N_ELEMENTS(rebinfac) EQ 1 THEN $
        i_rebinfac = REPLICATE(rebinfac,nbands) ELSE $
           i_rebinfac = rebinfac
     IF MIN(i_rebinfac LT 1) THEN MESSAGE,"Invalid rebinning factor"
     IF N_ELEMENTS(i_rebinfac) NE nbands THEN $
        MESSAGE,"Wrong number of rebinfacs"
  ENDIF ELSE i_rebinfac = REPLICATE(1,nbands)

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
  IF N_ELEMENTS(outbase) EQ 0 THEN MESSAGE,"outbase not provided"

  minpixsize = MIN(i_pixsize / i_rebinfac)
  npix = ROUND(SQRT(area)*3600.0/minpixsize)
  truearea = npix^2 * (minpixsize/3600.0)^2

  ;;Determine how many sources we will generate
  ntot = truearea * dndlogldzdomega.totsources * (!PI/180.0)^2 ;;to sq deg
  IF ntot LE 0 THEN MESSAGE,"Error: no sources to generate"
  IF ~ KEYWORD_SET( fixntot ) THEN $
     ntot = RANDOMU(seed,POISSON=ntot,/DOUBLE)
  ntot = ROUND(ntot)
  IF ntot EQ 0 THEN MESSAGE,"Not generating any sources!"
  IF KEYWORD_SET(verbose) THEN $
     MESSAGE,STRING(ntot,FORMAT='("Generating ",I0," sources")'),/INF

  ;;Make catalog
  IF KEYWORD_SET(verbose) THEN MESSAGE,"Generating Bethermin catalog",/INF
  cat = REPLICATE( {xpos: 0, ypos: 0, z: !VALUES.F_NAN, $
                    loglum: !VALUES.F_NAN,$
                    type: 0b, fluxes: DBLARR(nbands) },$
                    ntot )
  bcat = bethermin_gencat( ntot, dndlogldzdomega, cold, starburst, $
                           SEED=seed, WAVE=wave, $
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
     IF KEYWORD_SET(verbose) THEN BEGIN
        fmt = '("Generating clustered source positions for ",I0," by ",I0,'+$
              '" image")'
        MESSAGE,STRING(npix,npix,FORMAT=fmt),/INF
     ENDIF
     prob_im = GENERATE_SOURCE_PROB( clus.k, clus.pk, npix, $
                                     NUMPIX2=npix, $
                                     PIXSIZE=minpixsize, SEED=seed, /USINGK )
     SMAP_POPULATE_SOURCES, TEMPORARY(prob_im), ntot, xpos, ypos, SEED=seed
     cat.xpos = TEMPORARY(xpos)
     cat.ypos = TEMPORARY(ypos)
  ENDIF ELSE BEGIN
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,"Generating un-clustered source positions",/INF
     cat.xpos = FLOOR( RANDOMU( seed, ntot ) * npix )
     cat.ypos = FLOOR( RANDOMU( seed, ntot ) * npix )
  ENDELSE

  ;;Now, generate the image in each band and write
  FOR i=0,nbands-1 DO BEGIN
     IF KEYWORD_SET(verbose) THEN MESSAGE,"Doing band: "+bands[i],/INF
     map = $
        smap_generate_bethermin_gauss_domap(cat, minpixsize, i,$
                                            i_pixsize[i], fwhm[i], bands[i],$
                                            wave[i], area, seed, racen,$
                                            deccen, i_sigmas[i], $
                                            REBINFAC=i_rebinfac[i], $
                                            VERBOSE=verbose, EXPOSURE=exposure )
                               
     success = WRITE_SMAP_FITSMAP(map,outbase,DIR=i_outdir,$
                                  ERRMSG=errmsg,/SILENT)
     IF ~ success THEN MESSAGE,"Error writing output file1: "+errmsg
  ENDFOR

  ;;Output catalog
  IF N_ELEMENTS(catfile) NE 0 THEN MWRFITS, cat, catfile, /CREATE

END
