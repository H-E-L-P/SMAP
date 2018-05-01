;+
;NAME
; get_spire_beam_fwhm
;PURPOSE
; Returns SPIRE beam FWHM in arcsec
;USAGE
; fwhm = get_spire_beam_fwhm(band)
;INPUTS
; band      'PSW','PMW','PLW'
;-

FUNCTION get_spire_beam_fwhm, band
  COMPILE_OPT IDL2
  ON_ERROR,2
  CASE STRUPCASE(band) OF
     'PSW': beamFWHM = 18.
     'PMW': beamFWHM = 25.
     'PLW': beamFWHM = 36.
     ELSE : MESSAGE,"Unknown band: "+band
  ENDCASE
  RETURN,beamFWHM
END
