;+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  pro smap_write_fitsbeam.pro
;;  May 17, 2011
;;  Mike Zemcov
;;  This function calls get_spire_beam to generate a beam kernel and 
;;   then writes it to a fits file with correct header information.
;;  Inputs: 
;;          band (string)   = one of 'PSW', 'PMW', 'PLW' (def: PSW)
;;          file (string)   = filename of output
;;
;;  Keyword Inputs:
;;          PIXSIZE (float) = the pixel size in arcsec (def: 6/8.333/12)
;;          NPIXX (int)     = the number of pixels required in the x axis
;;                             This should be odd so the PSF is
;;                             centered. (def: 5 FWHM rounded to odd)
;;          NPIXY (int)     = the number of pixels required in the y axis.
;;                             This should be odd so the PSF is
;;                             centered. (def: npixx)
;;          XCENT (float)   = x pixel corresponding to center of the beam
;;                             (can be fractional, note that pixel
;;                             numbering starts at 0)  (def: npixx/2 using
;;                             integral division, so if npixx is 31,
;;                             this is 15)
;;          YCENT (float)   = y pixel corresponding to center of the beam 
;;                             (can be fractional).  (def: npixy/2, see
;;                             note for xcent for further info)
;;                             the future.
;;          RACENT (float)   = R.A. of center pixel in Deg.
;;          DECCENT (float)  = Dec. of center pixel in Deg.
;;          FWHM (float)     = The FWHM of the beam, in arcsec.
;;                              Normally this is determined by band.
;;          NORM (bit)       = normalize by Gaussian area?  (def: no)
;;          VERBOSE (bit)    = print useful informational messages (def: yes)
;;          ERRMSG (string)  = contains information error messages on bad exit
;;          SUCCESS (bit)    = is 1 if program completed successfully.
;;
;;  Keyword Outputs: 
;;          BEAMKERN (float) = array size npixx x npixy containing
;;                              beam kernel
;;          HEADER (float)   = copy of the fits header in file 
;;          
;;  Created by: M. Zemcov, 20110517
;;
;;  Changelog:
;;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-

PRO smap_write_fitsbeam,band,file,$
                        PIXSIZE=pixsize,NPIXX=npixx,NPIXY=npixy,$
                        XCENT=xcent,YCENT=ycent,RACENT=racent,DECCENT=deccent,$
                        FWHM=fwhm,NORM=norm,BEAMKERN=beamkern,HEADER=header,$
                        VERBOSE=verbose,ERRMSG=errmsg,SUCCESS=success

  ;; standard init stuff
  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  IF ~(N_ELEMENTS(verbose)) THEN verbose=1 ELSE verbose = verbose

  ;; check that we have been given a band; if not bail
  IF ~(N_ELEMENTS(band)) THEN BEGIN
     errmsg = 'Input band not set.'
     IF verbose THEN MESSAGE,errmsg,/INFORMATIONAL
     RETURN
  ENDIF

  ;; check that we have been given an output file name; if not bail
  IF ~(N_ELEMENTS(file)) THEN BEGIN
     errmsg = 'Output filename not given.'
     IF verbose THEN MESSAGE,errmsg,/INFORMATIONAL
     RETURN
  ENDIF

  ;; check if we've been given a pixel size; if not assume smap defaults
  IF N_ELEMENTS(pixsize) EQ 0 THEN BEGIN
     CASE STRUPCASE(band) OF
        'PSW' : pixsize = 6
        'PMW' : pixsize = 8d0 + 1.0d0/3.0d0
        'PLW' : pixsize = 12
        ELSE : MESSAGE,"Unknown band: "+band
     ENDCASE
     IF verbose THEN $
        MESSAGE,STRING(pixsize,FORMAT='("pixsize parameter not '+$
                       'supplied, assuming ",F0.4," arcsec")'),/INF
  ENDIF

  ;; these variables will get overwritten by nonsense if they 
  ;; exist so we need to copy
  IF N_ELEMENTS(xcent) THEN xcentp = xcent
  IF N_ELEMENTS(ycent) THEN ycentp = ycent

  ;; grab the beam with the input parameters
  beamkern = GET_SPIRE_BEAM(band,pixsize,npixx,npixy,xcentp,ycentp,$
                            FWHM=fwhm,NORM=norm,SILENT=~verbose)

  ;; now make a dummy header for that beam kernel
  header = GET_SMAP_HEADER(beamkern)

  ;; populate the header --
  ;; description keyword
  SXADDPAR,header,'DESC',STRING(band,' map')
  ;; wavelength keyword
  IF band[0] EQ 'P' THEN BEGIN
     CASE STRUPCASE(band) OF
        'PSW' : SXADDPAR,header,'WAVELN',250
        'PMW' : SXADDPAR,header,'WAVELN',350
        'PLW' : SXADDPAR,header,'WAVELN',500
     ENDCASE
  ENDIF
  ;; central pixel
  IF N_ELEMENTS(xcent) THEN SXADDPAR,header,'CRPIX1',xcent
  IF N_ELEMENTS(ycent) THEN SXADDPAR,header,'CRPIX2',ycent
  ;; pixel scalings
  SXADDPAR,header,'CD1_1',-pixsize/3600.
  SXADDPAR,header,'CD2_2',pixsize/3600.
  ;; central value
  IF N_ELEMENTS(racent) THEN SXADDPAR,header,'CRVAL1',racent
  IF N_ELEMENTS(deccent) THEN SXADDPAR,header,'CRVAL2',deccent
  ;; -- header is populated
  
  ;; write the fits file
  IF verbose THEN MESSAGE,'Writing fits file.',/INFORMATIONAL
  WRITEFITS,file,beamkern,header
  
  ;; and we're out
  RETURN

END
