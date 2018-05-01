;+
; NAME
;  get_firas_flux
; PURPOSE
;  To return the FIRAS background (per-beam) for SPIRE maps
; USAGE
;  bkg = get_firas_flux( band, FWHM= )
; INPUTS
;  band     Name of the band ('PSW','PMW','PLW')
; OPTIONAL INPUTS
;  fwhm     FWHM of the assumed Gaussian beam.  Otherwise the
;            official beam sizes from the SPIRE observers manual are
;            used
; RETURNS
;  The CFIRB flux from FIRAS per beam
; MODIFICATION HISTORY
;  Author: Alex Conley
FUNCTION get_firas_flux, band, FWHM=fwhm
  COMPILE_OPT IDL2, HIDDEN

  ;;bkg is FIRAS background in Jy/sq deg. from Lagach, Puget and Dole
  ;;96
  CASE band OF
     'PSW' : bkg = 258.9 
     'PMW' : bkg = 198.0 
     'PLW' : bkg = 118.8 
     ELSE : MESSAGE,"Unkown band for background: "+band
  ENDCASE

  IF N_ELEMENTS(fwhm) NE 0 THEN BEGIN
     ;;User specified Gaussian beam size
     IF fwhm LE 0 THEN MESSAGE,"Invalid (non-positive) beam FWHM"
     sig = fwhm / SQRT(8.0*ALOG(2))
     bmsize = 2 * !PI * fwhm^2 / (8*ALOG(2))
  ENDIF ELSE BEGIN
     CASE band OF
        'PSW' : bmsize = 423.0
        'PMW' : bmsize = 751.0
        'PLW' : bmsize = 1587.0
     ENDCASE
  ENDELSE

  RETURN,bkg*bmsize/(3600.0^2)

END
