;+
; NAME
;  smap_generate_bethermin
; PURPOSE
;  Create a set of fake images using the Bethermin 2010 model
; USAGE
;  smap_generate_bethermin, dndlogldzdomega, cold, starburst, 
;                           beamfiles, outfile [, WAVE=, AREA=, CATFILE=,
;                           CLUS=, REBINFAC=rebinfac ]
; INPUTS
;  dndlogldzdomega Output of bethermin_dndlogldzdomega
;  cold          Holds information about IAS cold templates
;  starburst     Holds information about IAS starburst templates
;  beamfiles     Array of FITS files with beams for all desired bands.
;                 They should all have the same pixel size!
;  outfile       Root name for output files
; OPTIONAL INPUTS
;  area          The area of the image (in sq. deg).  May not be
;                 perfectly realized on output
;  racen/deccen  RA/DEC of center in decimal degrees (def: 120/10)
;  rebinfac      Factor to rebin output maps by.  Must be integral
;  sigmas        Array of Gaussian noise added on a per-pixel basis in
;                 each band.  
;  nfluxes       Number of points to generate cumulative flux
;                 distribution at (def: 65536)
;  catfile       Write the catalog of sources to this file (fits)
;  seed          Seed for random number generator
;  bands         Name of bands (def: 'bandN' where N goes from 1 to
;                 the number of bands).  Used for output purposes
;  wave          Wavelength of bands in microns (def:first nbeams of 
;                 [250,350,500]).
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
; KEYWORDS
;  fixntot       Fix the total number of sources instead of treating
;                 it as poisson distributed
;  verbose       Prints status messages
; SIDE EFFECTS
;  Writes nbeams fits files to outfile_band1.fits, outfile_band2.fits, etc.
; NOTES
;   Using WAVE= and BANDS=, this is not limited to SPIRE bands.
;   The images are mean subtracted.
; MODIFICATION HISTORY
;  Author: Alex Conley, Mar 2, 2011
;-

FUNCTION smap_generate_bethermin_getpixsize, head, filename
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

PRO smap_generate_bethermin, dndlogldzdomega, cold, starburst, $
                             beamfiles, outfile,$
                             AREA=area, REBINFAC=rebinfac, $
                             SIGMAS=sigmas, OUTDIR=outdir, $
                             FIXNTOT=fixntot, SEED=seed, $
                             NFLUXES=nfluxes, VERBOSE=verbose, $
                             RACEN=racen, DECCEN=deccen, $
                             CATFILE=catfile, BANDS=bands,$
                             LOCALZ=localz, CLUS=clus, WAVE=wave,$
                             KILLRED=killred
  
  COMPILE_OPT IDL2, STRICTARRSUBS

  IF N_ELEMENTS(racen) EQ 0 THEN racen = 120.0
  IF N_ELEMENTS(deccen) EQ 0 THEN deccen = 10.0
  IF N_ELEMENTS(nfluxes) EQ 0 THEN nfluxes = 65536
  IF nfluxes LE 0 THEN MESSAGE,"Invalid nfluxes "+STRING(nfluxes)
  IF N_ELEMENTS(area) EQ 0 THEN area=1.0
  IF area LE 0 THEN MESSAGE,"Invalid area"
  nbeams = N_ELEMENTS(beamfiles)
  IF nbeams EQ 0 THEN MESSAGE,"No beam files provided"
  IF N_ELEMENTS(sigmas) EQ 0 THEN sigmas = REPLICATE(0.0,nbeams)
  IF N_ELEMENTS(sigmas) NE nbeams THEN $
     MESSAGE,"Number of sigmas must match number of beams"
  IF MIN(sigmas) LT 0.0 THEN MESSAGE,"Invalid (non-positive) sigma value"
  IF N_ELEMENTS(bands) EQ 0 THEN $
     bands = STRING(INDGEN(nbeams)+1,FORMAT='("bands",I0)')
  IF N_ELEMENTS(bands) NE nbeams THEN $
     MESSAGE,"Number of band names must match number of beams"
  IF N_ELEMENTS(clus) NE 0 THEN BEGIN
     IF ~ TAG_EXIST(clus,'k',/TOP) THEN MESSAGE,"Clus doesn't have .k field"
     IF ~ TAG_EXIST(clus,'pk',/TOP) THEN MESSAGE,"Clus doesn't have .pk field"
     IF N_ELEMENTS(clus.pk) NE N_ELEMENTS(clus.k) THEN $
        MESSAGE,"clus.pk and clus.k must be same length"
     has_clustering = 1b
  ENDIF ELSE has_clustering=0b
  IF N_ELEMENTS(outdir) EQ 0 THEN i_outdir='./' ELSE i_outdir=ADDSLASH(outdir)

  IF N_ELEMENTS( wave ) EQ 0 THEN BEGIN
     IF nbeams GT 3 THEN MESSAGE,"Must specify wave if more than 3 bands"
     wave = ([250.0,350,500])[0:nbeams-1]
  ENDIF
  IF N_ELEMENTS(wave) NE nbeams THEN $
     MESSAGE,"wave not same length as number of beams"

  IF N_ELEMENTS(UNIQ(bands,SORT(bands))) NE N_ELEMENTS(bands) THEN $
     MESSAGE,"bands must have unique names"
  IF KEYWORD_SET(verbose) THEN FOR i=0,nbeams-1 DO BEGIN
     IF FILE_TEST(outfile+'_'+bands[i]+'.fits') THEN $
        MESSAGE,"Warning -- output file "+outfile+'_'+bands[i]+'.fits'+$
                " will be overwritten",/INF
  ENDFOR

  ;;read first beam, find pixel scale for comparison to other beams
  kernel1 = MRDFITS( beamfiles[0], 0, bmhead1, /SILENT, STATUS=status )
  IF status NE 0 THEN $
     MESSAGE,"Error reading user provided beam file: "+beamfiles[0]
  pixscale1 = smap_generate_bethermin_getpixsize(bmhead1,beamfiles[0])

  ;;Now try to figure out how large to make the output maps
  ;; this depends if we are going to rebin or not, and if we are using
  ;; an input map
  IF N_ELEMENTS(rebinfac) NE 0 && ABS(rebinfac-1.0) GT 1d-5 THEN $
     final_rebin = 1b ELSE final_rebin = 0b
  IF final_rebin THEN BEGIN
     rebinfac = FIX(rebinfac) ;;Make integer
     npix_x = ROUND(SQRT(area)*3600.0/(rebinfac*pixscale1))
     npix_y = npix_x
     genpix_x = npix_x * rebinfac
     genpix_y = genpix_x
     truearea = npix_x*npix_y*(pixscale1/3600.0)^2
  ENDIF ELSE BEGIN
     npix_x = ROUND(SQRT(area)*3600.0/pixscale1)
     npix_y = npix_x
     genpix_x = npix_x
     genpix_y = npix_x
     truearea = (pixscale1/3600.0)^2 * npix_x * npix_y
  ENDELSE

  ;;Determine how many sources we will generate
  ntot = dndlogldzdomega.totsources * (!PI/180.0)^2 ;;to sq deg
  ntot *= truearea
  IF ntot EQ 0 THEN MESSAGE,"Error: no sources to generate"
  IF ~ KEYWORD_SET( fixntot ) THEN $
     ntot = RANDOMU(seed,POISSON=ntot,/DOUBLE)
  ntot = ROUND(ntot)
  IF ntot EQ 0 THEN MESSAGE,"Not generating any sources!"
  IF KEYWORD_SET(verbose) THEN $
     MESSAGE,STRING(ntot,FORMAT='("Generating ",I0," sources")'),/INF

  ;;Make catalog
  IF KEYWORD_SET(verbose) THEN MESSAGE,"Generating Bethermin catalog",/INF
  cat = REPLICATE( {xpos: 0, ypos: 0, z: !VALUES.F_NAN, loglum: !VALUES.F_NAN,$
                    type: 0b, wave: wave, fluxes: DBLARR(nbeams) },$
                    ntot )

  bcat = bethermin_gencat( ntot, dndlogldzdomega, cold, starburst, $
                           SEED=seed, WAVE=wave )

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
     pixscale = smap_generate_bethermin_getpixsize(bmhead,beamfiles[i])
     IF ABS( (pixscale1 - pixscale)/pixscale1 ) GT 1d-5 THEN $
        MESSAGE,"Input beams do not have same pixel scale"
     
     ;;Set up output structure
     mapstruct = get_smap_mapstruct(NPIXX=npix_x,NPIXY=npix_y,$
                                    /NOMASK,/NOEXP,/NOERR,/SILENT,$
                                    ERRMSG=errmsg, SUCCESS=getmap_succ,$
                                    /NO_ABORT, BAND=bands[i], LAMBDA=wave[i])
     IF ~ getmap_succ THEN MESSAGE,"Error getting structure: "+errmsg

     image = DBLARR(genpix_x,genpix_y) ;;not directly in mapstruct for rebinning

     ;;Can't do this 'in parallel' because IDL can't handle
     ;; multiple sources ending up in the same pixel
     ;;That is,
     ;; image[cat.xpos,cat.ypos] += cat.flux
     ;;doesn't work, and worse, appears to work while
     ;; quietly doing the wrong thing.
     FOR j=0l,ntot-1l DO $
        image[ cat[j].xpos, cat[j].ypos ] += cat[j].fluxes[i]

     ;;Convolve
     IF KEYWORD_SET(verbose) THEN MESSAGE," Convolving",/INF
     image=CONVOLVE(image,kernel)

     ;;Rebin
     IF final_rebin THEN image = REBIN(image,npix_x,npix_y)

     mapstruct.image = TEMPORARY(image)

     ;;add noise.  Sigmas means different things if we are using
     ;; an input map or not
     IF sigmas[i] GT 0.0 THEN $
        mapstruct.image += sigmas[i]*RANDOMN(seed,npix_x,npix_y)

     mnval = MEAN(mapstruct.image,/NAN)
     IF KEYWORD_SET(verbose) THEN BEGIN
        fmt = '("Realized mean per pixel, band : ",A0," is ",F0.4)'
        msg = STRING(bands[i],mnval,FORMAT=fmt)
        MESSAGE,msg,/INF
     ENDIF
     mapstruct.image -= mnval

     ;;setup astrometry
     mapstruct.astrometry.cdelt = [1,1]
     mapstruct.astrometry.crpix = 0.5*[npix_x,npix_y]
     mapstruct.astrometry.crval = [racen,deccen]
     mapstruct.pixscale = pixscale1
     IF final_rebin THEN BEGIN
        mapstruct.astrometry.cd[0,0] = -rebinfac*pixscale1/3600.0
        mapstruct.astrometry.cd[1,1] = rebinfac*pixscale1/3600.0
     ENDIF ELSE BEGIN
        mapstruct.astrometry.cd[0,0] = -pixscale1/3600.0
        mapstruct.astrometry.cd[1,1] = pixscale1/3600.0
     ENDELSE

     success = WRITE_SMAP_FITSMAP(mapstruct,outfile,DIR=i_outdir,$
                                  ERRMSG=errmsg,/SILENT)
     IF ~ success THEN MESSAGE,"Error writing output file1: "+errmsg
  ENDFOR

  ;;Output catalog
  IF final_rebin THEN BEGIN
     cat.xpos /= rebinfac
     cat.ypos /= rebinfac
  ENDIF

  IF N_ELEMENTS(catfile) NE 0 THEN BEGIN
     MWRFITS, cat, catfile, /CREATE
  ENDIF 

END
