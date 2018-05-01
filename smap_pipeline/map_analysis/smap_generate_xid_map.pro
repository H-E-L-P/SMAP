;+
;NAME
; smap_generate_xid_map
;PURPOSE
; To generate the SPIRE maps predicted by XID
;USAGE
; smap_generate_xid_map, xid, outfile [, REBINFAC=]
;INPUTS
; xid        XID catalog as IDL structure
; outfile    Base name of output maps
;OPTIONAL INPUTS
; rebinfac   If set, map is generated at higher binning and then
;             binned down to standard pixel sizes
;KEYWORDS
; verbose    Print informational messages as it runs
;SIDE EFFECTS
; Writes three fits files to current directory: outfile_P[SML]W.fits.
;MODIFICATION HISTORY
; Author: Alex Conley, June 2011
;-

PRO smap_generate_xid_map, xid, outfile, REBINFAC=rebinfac, VERBOSE=verbose
  COMPILE_OPT IDL2, STRICTARRSUBS

  IF N_ELEMENTS(xid) EQ 0 THEN MESSAGE,"No XID provided"
  IF SIZE(xid,/TNAME) NE 'STRUCT' THEN MESSAGE,"XID must be structure"
  reqtags = ['INRA','INDEC','F250','F350','F500']
  FOR i=0,N_ELEMENTS(reqtags)-1 DO $
     IF ~ TAG_EXIST(xid,reqtags[i],/TOP_LEVEL) THEN $
        MESSAGE,"Unable to find required structure tag: "+reqtags[i]+$
                " in XID"
  IF N_ELEMENTS(rebinfac) NE 0 THEN do_rebin=1b ELSE do_rebin=0b

  bands = ['PSW','PMW','PLW']

  ;;Setup astrometry
  ;; Based on stuff from smap_make_maps.  We will first make the
  ;; map at whatever scale, then rebin if needed

  ;;Pixel sizes
  pix_size = [6,8.333,12]
  IF rebinfac THEN gen_pix_size = pix_size/rebinfac ELSE $
     gen_pix_size = pix_size
  
  ;;RA range
  maxra = MAX(xid.inra,MIN=minra,/NAN)
  maxdec= MAX(xid.indec,MIN=mindec,/NAN)
  midra = 0.5*(maxra+minra)
  middec= 0.5*(maxdec+mindec)
  crval = [midra,middec]
  
  IF ( ABS(middec) GT 60.0 ) THEN BEGIN
     latpole = 0.0
     IF ( (midra GT 150 AND midra LT 210) OR $
          midra LT 30 OR midra GT 330) THEN BEGIN
        longpole = 90.0 
        cdbase = [ [0.0d0,1.0],[1.0,0.0] ]
     ENDIF ELSE BEGIN
        longpole = 180.0        
        cdbase = [ [-1.0d0,0.0],[0.0,1.0] ]
     ENDELSE
  ENDIF ELSE BEGIN
     latpole = 90.0
     longpole = 180.0
     cdbase = [ [-1.0d0,0.0],[0.0,1.0] ]
  ENDELSE

  FOR i=0,N_ELEMENTS(bands)-1 DO BEGIN
     IF KEYWORD_SET( verbose ) THEN MESSAGE,"Doing band: "+bands[i],/INF
     cdmat = gen_pix_size[i] * cdbase / 3600.0
     SMAP_GETMAPBOUNDARY, cdmat, crval, minra, maxra, $
                          mindec, maxdec, $
                          xlo, xhi, ylo, yhi, LATPOLE=latpole,$
                          LONGPOLE=longpole
     crpix = [-ROUND(xlo), -ROUND(ylo)] + 1 ; CRPIX is 1-indexed
     
     xrange = ROUND(xhi) + crpix[0]
     yrange = ROUND(yhi) + crpix[1]
     IF xrange GT 10000 OR yrange GT 10000 THEN $
        MESSAGE,STRING(xrange,yrange,FORMAT='("Map too large! ",I0," x ",I0)')

     IF do_rebin THEN BEGIN
        ;;Make sure it can be evenly rebinned
        xrange += rebinfac - (xrange MOD rebinfac)
        yrange += rebinfac - (yrange MOD rebinfac)
     ENDIF
     
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,STRING(xrange,yrange,FORMAT='(" Generating ",I0," by ",I0," image")'),/INF

     MAKE_ASTR,thisast,CD=cdmat,CRPIX=crpix,CRVAL=crval,$
               DELTA=[1.0,1.0], LATPOLE=latpole, LONGPOLE=longpole
     IF N_ELEMENTS(thisast) EQ 0 THEN MESSAGE,"MAKE_ASTR failed"
     
     map = GET_SMAP_MAPSTRUCT(NPIXX=xrange, NPIXY=yrange,$
                              BAND=bands[i],$
                              ASTROMETRY=thisast, SUCCESS=gsm_success,$
                              ERRMSG=gsm_errmsg, /SILENT)
     IF ~gsm_success THEN $
        MESSAGE,'GET_SMAP_MAPSTRUCT encountered an error: ' + gsm_errmsg

     ;;Now populate
     CASE bands[i] OF
        'PSW' : flux = xid.f250
        'PMW' : flux = xid.f350
        'PLW' : flux = xid.f500
     ENDCASE
     flux *= 1d-3 ;;SMAP maps in Jy

     AD2XY, xid.inra, xid.indec, thisast, xpos, ypos
     xpos = ROUND(xpos) > 0 < (xrange-1)
     ypos = ROUND(ypos) > 0 < (yrange-1)

     ;;must be done with a loop, not as array expression in case
     ;; two objects end up in the same pixel
     FOR j=0,N_ELEMENTS(flux)-1 DO $
        map.image[xpos[j],ypos[j]] += flux[j]

     ;;Get beam
     beam = get_spire_beam(bands[i], gen_pix_size[i],/SILENT)
     
     ;;Convolve
     IF KEYWORD_SET(verbose) THEN MESSAGE," Convolving",/INF
     map.image=CONVOLVE(map.image,beam)

     ;;Rebin
     IF do_rebin THEN BEGIN
        IF KEYWORD_SET(verbose) THEN MESSAGE," Rebinning",/INF
        map = smap_rebin( map, rebinfac )
     ENDIF

     ;;Zero-sub
     map.image -= MEAN(map.image,/NAN)

     ;;Write
     IF KEYWORD_SET(verbose) THEN MESSAGE," Writing",/INF
     st = write_smap_fitsmap( TEMPORARY(map), outfile, DIR='.', $
                              ERRMSG=errmsg,/NO_ABORT, /SILENT )
     IF st EQ 0b THEN MESSAGE,"Error writing map in "+bands[i]+": "+errmsg
  ENDFOR

END

