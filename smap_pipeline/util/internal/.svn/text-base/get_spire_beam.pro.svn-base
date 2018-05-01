;+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  function get_spire_beam.pro
;;  May 22, 2009
;;  Mike Zemcov
;;  This function computes the spire beam given a band, pixel size and
;;   output map kernel.
;;  Inputs: 
;;          band (string)   = one of 'PSW', 'PMW', 'PLW' (def: PSW)
;;          pixsize (float) = the pixel size in arcsec (def: 6/8.333/12)
;;          npixx (int)     = the number of pixels required in the x axis
;;                             This should be odd so the PSF is
;;                             centered. (def: 5 FWHM rounded to odd)
;;          npixy (int)     = the number of pixels required in the y axis.
;;                             This should be odd so the PSF is
;;                             centered. (def: npixx)
;;          xcent (float)   = x pixel corresponding to center of the beam
;;                             (can be fractional, note that pixel
;;                             numbering starts at 0)  (def: npixx/2 using
;;                             integral division, so if npixx is 31,
;;                             this is 15).  This is in the
;;                             non-oversampled beam.
;;          ycent (float)   = y pixel corresponding to center of the beam 
;;                             (can be fractional).  (def: npixy/2, see
;;                             note for xcent for further info)
;;  Optional inputs:
;;          bolometer (string) = Optional argument specifying which bolometer
;;                             to return beam for (i.e., band='PSW', 
;;                             bolometer='A11': psf for 'PSWA11').
;;                             Not currently supported, but here for
;;                             when we have bolometer specific psfs in
;;                             the future.
;;          fwhm (float)     = The FWHM of the beam, in arcsec.
;;                              Normally this is determined by band.
;;          oversamp         = Amount to oversample pixels by before
;;                              convolving with pixel function.
;;                              Should be an odd integer (Def: 7)
;;  Outputs: 
;;          beamkern (float) = array size npixx x npixy containing
;;                              beam kernal 
;;  Keywords: norm           = normalize by Gaussian area?  (def: no)
;;            silent         = Don't output information messages
;;            factor         = Return the 1D beam factor rather than
;;                             the 2D beam.
;;
;;  Note that pixel positions mark the centres of pixels rather than
;;   the edges.  This follows sub-mm normalization conventions by default, so
;;   the default behavior is therefore that the central pixel 
;;   (xcent/ycent) has a value of one in an infinitely finely sampled
;;   beam.
;;  However, for a finitely sampled beam the situation is more complicated.
;;   The idea is to make an infinitely finely sampled beam (or at
;;   least one much better sampled than the final pixel scale) and
;;   convolve it with the pixel function.  This means that the central
;;   value will -not- be one, but will be slightly less by an amount
;;   that depends on the pixel size.  Note that this is the
;;   appropriate thing to do if the calibration is applied on a
;;   timestream level, as it is for the official SPIRE pipeline.
;;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-

FUNCTION get_spire_beam,band, pixsize, npixx, npixy, xcent, ycent,$
                        BOLOMETER=bolometer, FWHM=fwhm,$
                        NORM=norm, OVERSAMP=oversamp, SILENT=silent, $
                        FACTOR=factor

  COMPILE_OPT IDL2, STRICTARRSUBS

  IF ~KEYWORD_SET(norm) THEN norm = 0b
  IF N_ELEMENTS(bolometer) NE 0 AND ~ KEYWORD_SET(silent) THEN $
     MESSAGE,"PSFs for specific bolometer not yet supported -- ignoring",$
             /INFORMATIONAL

  ; check if we've been given a band, if not assume something
  ; and too bad for the user
  IF N_ELEMENTS(band) EQ 0 THEN BEGIN
     IF ~ KEYWORD_SET(silent) THEN $
        MESSAGE,'band parameter not supplied, assuming PSW',/INFORMATIONAL
     band = 'PSW'
  ENDIF
  ; check if we've been given a pixel size
  IF N_ELEMENTS(pixsize) EQ 0 THEN BEGIN
     CASE STRUPCASE(band) OF
        'PSW' : pixsize = 6
        'PMW' : pixsize = 8d0 + 1.0d0/3.0d0
        'PLW' : pixsize = 12
        ELSE : MESSAGE,"Unknown band: "+band
     ENDCASE
     IF ~ KEYWORD_SET( silent ) THEN $
        MESSAGE, STRING(pixsize,FORMAT='("pixsize parameter not '+$
                       'supplied, assuming ",F0.4," arcsec")'), /INF
  ENDIF

  ; figure out which color this is
  IF N_ELEMENTS(fwhm) EQ 0 THEN beamFWHM=get_spire_beam_fwhm(band) ELSE $
     beamFWHM=fwhm

  IF beamFWHM LE 0 THEN $
     MESSAGE, "Invalid beam FWHM value "+STRING(beamFWHM)

  ;; check if we've been given the map size, if not assume something
  ;; npixx/npixy will be the final number of pixels 
  IF N_ELEMENTS(npixx) EQ 0 THEN BEGIN
     npixx = ROUND(beamFWHM * 5.0 / pixsize)
     IF npixx MOD 2 NE 1 THEN npixx+=1
     IF ~ KEYWORD_SET(silent) THEN $
        MESSAGE,STRING(npixx, FORMAT='("npixx not supplied, using ",I0)'), /INF
  ENDIF
  ; if no y size then assume same as x
  IF N_ELEMENTS(npixy) EQ 0 THEN BEGIN
     npixy=npixx
  ENDIF
  
  ;; make sure that these have been cast from a float properly or we get errors
  npixx = CEIL(npixx)
  npixy = CEIL(npixy)

  IF npixx mod 2 NE 1 && ~ KEYWORD_SET(silent) THEN $
     MESSAGE,"WARNING: npixx not odd, so PSF will not be centered",/INF
  IF npixy mod 2 NE 1 && ~ KEYWORD_SET(silent) THEN $
     MESSAGE,"WARNING: npixy not odd, so PSF will not be centered",/INF
     
  ;;Now deal with oversampling
  IF N_ELEMENTS(oversamp) EQ 0 THEN ioversamp = 7 ELSE BEGIN
     ioversamp = ROUND(oversamp) ;;In case user provides float
     IF ioversamp MOD 2 NE 1 THEN $
        MESSAGE, "Oversamp must be an odd integer!"
  ENDELSE
  x_gen = npixx * ioversamp
  y_gen = npixy * ioversamp
  gen_pixsize = DOUBLE(pixsize) / ioversamp

  ; check if we've been given the center, if not assume middle
  IF N_ELEMENTS(xcent) EQ 0 THEN BEGIN
     IF ~ KEYWORD_SET(silent) THEN $
        MESSAGE,'xcent parameter not supplied, assuming array center',$
                /INFORMATIONAL
     ixcent = x_gen / 2
  ENDIF ELSE BEGIN
     ;; Adjust for oversampling
     ixcent = xcent * ioversamp
     IF ioversamp GT 1 THEN ixcent += ioversamp / 2
  ENDELSE
  IF N_ELEMENTS(ycent) EQ 0 THEN BEGIN
     IF ~ KEYWORD_SET(silent) THEN $
        MESSAGE,'ycent parameter not supplied, assuming array center',$
                /INFORMATIONAL
     iycent = y_gen / 2
  ENDIF ELSE BEGIN
     iycent = ycent * ioversamp
     IF ioversamp GT 1 THEN iycent += ioversamp / 2
  ENDELSE
  
  ; normalize FWHM to pixels
  beamFWHM /= gen_pixsize 
  
  ; if we want this normalized then call with /NORMALIZE set
  IF KEYWORD_SET( factor ) THEN BEGIN
     ;;1D beam
     beamkern = PSF_GAUSSIAN(NPIXEL=x_gen, FWHM=beamFWHM,$
                             CENTROID=ixcent,/DOUBLE,NDIMEN=1,$
                             NORMALIZE=norm)
     IF ioversamp GT 1 THEN $
        beamkern = REBIN(beamkern,npixx)
  ENDIF ELSE BEGIN
     beamkern = PSF_GAUSSIAN(NPIXEL=[x_gen, y_gen], FWHM=beamFWHM,$
                             CENTROID=[ixcent, iycent], /DOUBLE, NDIMEN=2,$
                             NORMALIZE=norm)
     IF ioversamp GT 1 THEN $
        beamkern = REBIN( beamkern, npixx, npixy )
  ENDELSE

  ; remove floating point errors
  error = check_math(MASK=32) 

  ; return the kernel
  RETURN,beamkern

; and we're out
END
