;+
; NAME
;  smap_generate_bethermin_frommap
; PURPOSE
;  Create a set of fake images using the Bethermin 2010 model based on
;  a set of input maps.
; USAGE
;  smap_generate_bethermin_frommap, dndlogldzdomega, cold, starburst, 
;                           mapbase, bands, beamfiles, outfile [, CATFILE=,
;                           CLUS=, REBINFAC= ]
; INPUTS
;  dndlogldzdomega Output of bethermin_dndlogldzdomega
;  cold          Holds information about IAS cold templates
;  starburst     Holds information about IAS starburst templates
;  beamfiles     Array of FITS files with beams for all desired bands.
;                 They should all have the same pixel size!
;  mapbase       Base name of map files to load
;  bands         Bands of map files to load (e.g., ['PSW','PMW'])
;  outfile       Root name for output files
; OPTIONAL INPUTS
;  mapdir        Directory to look for maps in (def: !SMAP_MAPS)
;  rebinfac      Factor to rebin output maps by.  Must be integral
;  sigmas        The noise array in each band is multiplied by this 
;                 and that is used.
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
;  Writes nbeams fits files to outfile_band1.fits, outfile_band2.fits,etc.  
; NOTES
;   The images are mean subtracted.  With rebinning, the output
;   catalog only gives the position to the nearest pixel.
; MODIFICATION HISTORY
;  Author: Alex Conley, January 4, 2012
;-

FUNCTION smap_generate_bethermin_frommap_getpixsize, head, filename
  COMPILE_OPT IDL2, HIDDEN

  pixscale = SXPAR(head,'PIXSCALE',COUNT=cnt)
  IF cnt GT 0 THEN RETURN,pixscale[0]
  pixscale = SXPAR(head,'PIXSIZE',COUNT=cnt)
  IF cnt GT 0 THEN RETURN,pixscale[0]

  ;;Try ast
  EXTAST, head, astr, success
  IF success LT 0 THEN $
     MESSAGE,"Couldn't find plate scale info for "+filename
  GETROT, head, rot, cdelt, /SILENT
  RETURN, 3600.0*SQRT(ABS(cdelt[0]*cdelt[1]))
END

PRO smap_generate_bethermin_frommap, dndlogldzdomega, cold, starburst, $
                                     mapbase, bands, beamfiles, outfile,$
                                     MAPDIR=mapdir, REBINFAC=rebinfac, $
                                     SIGMAS=sigmas, OUTDIR=outdir, $
                                     SEED=seed, NFLUXES=nfluxes, $
                                     VERBOSE=verbose, CATFILE=catfile, $
                                     LOCALZ=localz, CLUS=clus, $
                                     KILLRED=killred, MASKFILES=maskfiles,$
                                     MASKDIR=maskdir
  
  COMPILE_OPT IDL2, STRICTARRSUBS

  IF N_ELEMENTS(nfluxes) EQ 0 THEN nfluxes = 65536
  IF nfluxes LE 0 THEN MESSAGE,"Invalid nfluxes "+STRING(nfluxes)
  nbeams = N_ELEMENTS(beamfiles)
  IF nbeams EQ 0 THEN MESSAGE,"No beam files provided"
  nbands = N_ELEMENTS(bands)
  IF nbeams GT nbands THEN $
     MESSAGE,"Mismatch in number of beams and number of maps"
  IF N_ELEMENTS(sigmas) EQ 0 THEN sigmas = REPLICATE(0.0,nbeams)
  IF N_ELEMENTS(sigmas) LT nbeams THEN $
     MESSAGE,"Number of sigmas must match number of beams"
  IF MIN(sigmas) LT 0.0 THEN MESSAGE,"Invalid (non-positive) sigma value"
  IF N_ELEMENTS(outdir) EQ 0 THEN i_outdir='./' ELSE i_outdir=ADDSLASH(outdir)

  IF N_ELEMENTS(clus) NE 0 THEN BEGIN
     IF ~ TAG_EXIST(clus,'k',/TOP) THEN MESSAGE,"Clus doesn't have .k field"
     IF ~ TAG_EXIST(clus,'pk',/TOP) THEN MESSAGE,"Clus doesn't have .pk field"
     IF N_ELEMENTS(clus.pk) NE N_ELEMENTS(clus.k) THEN $
        MESSAGE,"clus.pk and clus.k must be same length"
     has_clustering = 1b
  ENDIF ELSE has_clustering=0b

  ;;read in the first map, which we will use to compute the sizes.
  ;; later, we will check to make sure all the others have the same
  ;; pixel size
  mapstruct = READ_SMAP_FITSMAP( mapbase, bands[0], DIR=mapdir, /NO_ABORT,$
                                 SUCCESS=msuccess, ERRMSG=merrmsg, /SILENT )
  IF msuccess EQ 0 THEN MESSAGE,"Error reading first map: "+merrmsg
  wave = FLTARR( nbeams )
  naxis1 = LONARR( nbeams )
  naxis2 = LONARR( nbeams )
  wave[0] = mapstruct.lambda
  naxis1[0] = mapstruct.xsize
  naxis2[0] = mapstruct.ysize

  ;;We need the lambdas of the other maps as well -- get those from
  ;; their headers
  IF N_ELEMENTS( mapdir ) EQ 0 THEN idir = ADDSLASH(!SMAP_MAPS) ELSE $
     idir = ADDSLASH(mapdir)
  FOR i=1,nbeams-1 DO BEGIN
     mapname = idir + mapbase+'_'+STRUPCASE(bands[i]) + '.fits'
     IF ~ FILE_TEST( mapname, /READ ) THEN $
        MESSAGE,"Unable to read in map: "+mapname
     hd = HEADFITS(mapname,EXTEN=1,ERRMSG=herrmsg)
     IF herrmsg NE '' THEN MESSAGE,"Error reading header for: "+mapname
     wv = SXPAR(hd,'WAVELN',COUNT=cnt)
     IF cnt NE 1 THEN MESSAGE,"Can't determine wavelength of: "+mapname
     wave[i] = FLOAT(wv)
     naxis1[i] = SXPAR(hd,'NAXIS1')
     naxis2[i] = SXPAR(hd,'NAXIS2')
  ENDFOR

  ;;Check the image sizes are the same
  IF nbeams GT 1 THEN BEGIN
     wbad = WHERE( naxis1 NE naxis1[0], nbad1 )
     IF nbad1 NE 0 THEN MESSAGE,"Input maps don't match axis 1 extent"
     wbad = WHERE( naxis2 NE naxis2[0], nbad2 )
     IF nbad2 NE 0 THEN MESSAGE,"Input maps don't match axis 2 extent"
  ENDIF
     
  ;;read first beam, find pixel scale for comparison to other beams
  kernel1 = MRDFITS( beamfiles[0], 0, bmhead1, /SILENT, STATUS=status )
  IF status NE 0 THEN $
     MESSAGE,"Error reading user provided beam file: "+beamfiles[0]
  pixscale1 = smap_generate_bethermin_frommap_getpixsize(bmhead1,beamfiles[0])

  ;;Estimate final area based on extent of maps, pixel size of first
  ;; beam.  Later we will check to make sure the pixel sizes are the same
  IF N_ELEMENTS(rebinfac) NE 0 && ABS(rebinfac-1.0) GT 1d-5 THEN $
     final_rebin = 1b ELSE final_rebin = 0b

  npix_x = naxis1[0]
  npix_y = naxis2[0]
  IF final_rebin THEN BEGIN
     ;;in this case, the pixel size of the beam is smaller by rebinfac
     ;; than the input maps
     rebinfac = FIX(rebinfac) ;;Make integer
     genpix_x = npix_x * rebinfac
     genpix_y = npix_y * rebinfac
     truearea = genpix_x * genpix_y * (pixscale1/3600.0)^2
  ENDIF ELSE BEGIN
     genpix_x = npix_x
     genpix_y = npix_y
     truearea = genpix_x * genpix_y * (pixscale1/3600.0)^2
  ENDELSE

  ;;Determine how many sources we will generate
  ntot = truearea * dndlogldzdomega.totsources * (!PI/180.0)^2 ;;to sq deg
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
                    type: 0b, wave: wave, fluxes: DBLARR(nbeams) },$
                    ntot )
  bcat = bethermin_gencat( ntot, dndlogldzdomega, cold, starburst, $
                           SEED=seed, WAVE=wave, VERBOSE=verbose )
  cat.z = bcat.z
  cat.loglum = bcat.loglum
  cat.type = bcat.type
  cat.fluxes = bcat.obsflux*1d-3 ;;to Jy from mJy
  st = N_ELEMENTS(temporary(bcat))

  ;;Get positions of each source
  IF has_clustering THEN BEGIN
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,"Generating clustered source positions",/INF
     prob_im = GENERATE_SOURCE_PROB( clus.k, clus.pk, genpix_x, $
                                     NUMPIX2=genpix_y, $
                                     PIXSIZE=pixscale1, SEED=seed, /USINGK )
     SMAP_POPULATE_SOURCES, TEMPORARY(prob_im), ntot, xpos, ypos, SEED=seed
     cat.xpos = TEMPORARY(xpos)
     cat.ypos = TEMPORARY(ypos)
  ENDIF ELSE BEGIN
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,"Generating un-clustered source positions",/INF
     cat.xpos = FLOOR( RANDOMU( seed, ntot ) * genpix_x )
     cat.ypos = FLOOR( RANDOMU( seed, ntot ) * genpix_y )
  ENDELSE

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
     IF N_ELEMENTS(killred.max) LT nbeams THEN $
        MESSAGE,"Killread doesn't have enough .max elements"
     IF N_ELEMENTS(killred.clr) LT nbeams-1 THEN $
        MESSAGE,"Killread doesn't have enough .clr elements"
     ;;flux test
     FOR bnd = 0, nbeams-1 DO BEGIN
        maxval = killred.max[bnd]
        IF ~ FINITE(maxval) THEN CONTINUE
        arered AND= (cat.fluxes[bnd] GT killred.max[bnd])
     ENDFOR
     ;;clr test
     FOR bnd=0,nbeams-2 DO BEGIN
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

  ;;Now, generate the image in each band
  FOR i=0,nbeams-1 DO BEGIN
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,STRING(beamfiles[i],wave[i],$
                       FORMAT='("Doing beam: ",A0," with wave: ",I0)'),/INF

     ;;Get the beam and make sure it's the right size
     kernel = MRDFITS( beamfiles[i], 0, bmhead, /SILENT, STATUS=status )
     IF status NE 0 THEN $
        MESSAGE,"Error reading user provided beam file: "+beamfiles[i]
     pixscale = smap_generate_bethermin_frommap_getpixsize(bmhead,beamfiles[i])
     IF ABS( (pixscale1 - pixscale)/pixscale1 ) GT 1d-5 THEN $
        MESSAGE,"Input beams do not have same pixel scale for "+beamfiles[i]
     IF final_rebin THEN final_pixscale = pixscale1*rebinfac ELSE $
        final_pixscale = pixscale1

     ;;Read in map we will base our simulation on
     mapstruct = READ_SMAP_FITSMAP( mapbase, bands[i], DIR=mapdir, /NO_ABORT,$
                                    SUCCESS=msuccess, ERRMSG=merrmsg, /SILENT )
     IF msuccess EQ 0 THEN MESSAGE,"Error reading map: "+merrmsg

     ;;Make sure the pixel scale matches the beam and other maps
     IF ABS( (mapstruct.pixscale - final_pixscale) / $
             mapstruct.pixscale ) GT 1d-5 THEN $
        MESSAGE,"Map for band " + bands[i] + " doesn't match pixel scale"
     
     ;;not directly in mapstruct to allow for rebinning
     image = DBLARR(genpix_x,genpix_y) 

     ;;Can't do this 'in parallel' because IDL can't handle
     ;; multiple sources ending up in the same pixel
     ;;That is,
     ;; image[cat.xpos,cat.ypos] += cat.flux
     ;;doesn't work, and worse, appears to work while
     ;; quietly doing the wrong thing.
     FOR j=0,ntot-1 DO $
        image[ cat[j].xpos, cat[j].ypos ] += cat[j].fluxes[i]

     ;;Convolve
     IF KEYWORD_SET(verbose) THEN MESSAGE," Convolving",/INF
     image=CONVOLVE(image,kernel)

     ;;Rebin
     IF final_rebin THEN image = REBIN(image,npix_x,npix_y)
     mapstruct.image = TEMPORARY(image)

     ;;add noise.  Sigmas means different things if we are using
     ;; an input map or not
     IF sigmas[i] GT 0.0 THEN BEGIN
        mapstruct.image += sigmas[i]*mapstruct.error*RANDOMN(seed,npix_x,npix_y)
        IF mapstruct.has_error THEN mapstruct.error *= sigmas[i]
     ENDIF ELSE BEGIN
        mapstruct.error = 0.0
     ENDELSE

     ;;nan out non-finite pixels in input map
     wnonfin = WHERE( ~ FINITE( mapstruct.image ), nnonfin )
     IF nnonfin NE 0 THEN mapstruct.image[wnonfin] = !VALUES.D_NAN

     mnval = MEAN(mapstruct.image,/NAN)
     IF KEYWORD_SET(verbose) THEN BEGIN
        fmt = '("Realized mean per pixel, band : ",A0," is ",F0.4)'
        msg = STRING(bands[i],mnval,FORMAT=fmt)
        MESSAGE,msg,/INF
     ENDIF
     mapstruct.image -= mnval

     ;;Add mask
     IF N_ELEMENTS(maskfiles) NE 0 AND mapstruct.has_mask THEN $
        add_user_mapmask, mapstruct, maskfiles, /ANDPLUS, MASKDIR=maskdir

     success = WRITE_SMAP_FITSMAP(mapstruct,outfile,DIR=i_outdir,$
                                  ERRMSG=errmsg,/SILENT)
     IF ~ success THEN MESSAGE,"Error writing output file1: "+errmsg
  ENDFOR

  ;;Output catalog
  IF N_ELEMENTS(catfile) NE 0 THEN BEGIN
     IF final_rebin THEN BEGIN
        ;;this ignores fractional pixels
        cat.xpos /= rebinfac
        cat.ypos /= rebinfac
     ENDIF
     MWRFITS, cat, catfile, /CREATE
  ENDIF 

END
