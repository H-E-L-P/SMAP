;+
;
; mapout = SMAP_MATCHED_FILTER(mapin, CONF=conf, INST=inst, NPAD=npad, $
;                              FILT=filt, EFFECTIVE=effective, $
;                              CONF_OUT=conf_out, WHITE_OUT=white_out)
;
; Wrapper script for applying matched_filter.pro (by E. Chapin,
; echapin@phas.ubc.ca) to smap-style map structure.
;
; Currently uses Gaussian beams with nominal value 18, 25 and
; 36". This can updated to real beams in the future. The default confusion
; noise values are taken from Nguyen et al. (2010).
;
; INPUTS:
;   mapin:  smap-style map structure
;
; OPTIONAL INPUTS:
;   conf:   override default confusion noise (same units as map)
;   inst:   Override error map instrumental noise value (same units as
;            map).  Will affect output noise map
;   npad:   number of pixels to pad map border with (border is
;           clipped, so output map has same dimensions as input)
;           default value is 100 pixels
;
; OUTPUT:
;   mapout: smap-style map structure containing filtered map
;
; OPTIONAL OUTPUT:
;   filt:      2-d array containing matched filter
;   effective: effective point source in filtered map (beam convolved by filt)
;   conf_out:  Confusion noise used
;   white_out: White noise used
;
; CREATED BY G. Marsden, 2011-03-16
;
; CHANGELOG:
;   2011-09-06 (gm) save filt and effective psf to mapstruct if available
;-

FUNCTION SMAP_MATCHED_FILTER, mapin, CONF=conf, INST=inst, NPAD=npad, $
                              FILT=filt, EFFECTIVE=effective, $
                              CONF_OUT=conf_out, WHITE_OUT=white_out

  COMPILE_OPT IDL2, STRICTARRSUBS

  ;; Jy / beam, updated from Nguyen et al. (2010)
  conf_def = [6.1, 6.7, 6.6] * 1e-3 
  ;; in arcsec
  fwhm_nom = [17.6, 23.9, 35.2]

  ;; could allow for no error map, but for now, abort
  IF N_ELEMENTS(inst) EQ 0 && ~ mapin.has_error THEN $
     MESSAGE, "input map structure must contain error map or user must "+$
              "provide instrumental noise"
  IF ~ mapin.has_exposure THEN $
     MESSAGE,"Input map structure must have exposure information"

  ;; default npad
  IF N_ELEMENTS(npad) EQ 0 THEN npad = 100

  map = mapin.image
  IF mapin.has_error THEN noise = mapin.error

  IF N_ELEMENTS(inst) NE 0 THEN BEGIN 
     IF N_ELEMENTS(inst) NE 1 THEN MESSAGE,"Must have single inst noise value"
     IF ~ FINITE(inst) THEN MESSAGE,"Non finite instrument noise"
     IF inst LE 0.0 THEN MESSAGE,"Invalid (non-positive) instrument noise"
     IF mapin.has_exposure THEN BEGIN
        wbad = WHERE( mapin.exposure EQ 0, nbad, NCOMPLEMENT=ngood )
        IF ngood EQ 0 THEN MESSAGE,"Map has no hits"
        IF nbad NE 0 THEN noise[wbad] = !VALUES.D_NAN
     ENDIF
  ENDIF

  ;; sanitize maps -- set missing pix to largeval * max(noise)
  badind = WHERE(~FINITE(noise) OR noise LE 0.0, nbad, COMPL=goodind,$
                 NCOMP=ngood)
  IF ngood EQ 0 THEN MESSAGE,"No good pixels in map"
  IF nbad GT 0 THEN BEGIN
     largeval = 100.0
     map[badind] = 0.0
     IF mapin.has_error THEN noise[badind] = largeval * MAX(noise[goodind])
  ENDIF

  ;; find band index
  CASE mapin.names OF
     'PSW': bandind = 0
     'PMW': bandind = 1
     'PLW': bandind = 2
  ENDCASE 

  ;; set fwhm
  fwhm_arc = fwhm_nom[bandind]

  ;; set confusion noise
  IF ~ KEYWORD_SET(conf) THEN conf = conf_def[bandind]

  ;; fwhm in pixels
  fwhm_pix = fwhm_arc / mapin.pixscale

  MATCHED_FILTER, fwhm_pix, conf, WHITE_IN=inst, PAD=npad, $
                  FILT=filt, EFFECTIVE=effective, $
                  MAP=TEMPORARY(map), NOISE=noise, $
                  SM_MAP=sm_map, SM_NOISE=sm_noise, $
                  WHITE_OUT=white_out
  conf_out = conf

  ;; output structure
  mapout = mapin
  mapout.image = TEMPORARY(sm_map)
  IF mapin.has_error THEN mapout.error = TEMPORARY(sm_noise)

  ;; set pixels with no data to NAN
  IF mapout.has_exposure THEN BEGIN
     badind = WHERE(mapin.exposure EQ 0, nbad)
     IF nbad GT 0 THEN BEGIN
        mapout.image[badind] = !VALUES.D_NAN
        IF mapout.has_error THEN mapout.error[badind] = !VALUES.D_NAN
     ENDIF
 ENDIF

  ; shift beam center to tan point
  IF TAG_EXIST(mapout, 'filter', /TOP_LEVEL) THEN $
     mapout.filter = SHIFT(filt, mapin.astrometry.crpix[0]-1, $
                           mapin.astrometry.crpix[1]-1)
  IF TAG_EXIST(mapout, 'effpsf', /TOP_LEVEL) THEN $
     mapout.effpsf = SHIFT(effective, mapin.astrometry.crpix[0]-1, $
                           mapin.astrometry.crpix[1]-1)

  RETURN, mapout

END
