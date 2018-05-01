;+
; NAME
;   gauss_tripleredcheck
; PURPOSE
;   Given 250/350/500 micron photometry and uncertainties, computes the
;   probability that f500 > f350 > f250 (assuming a multi-variate,
;   uncorrelated Gaussian distribution).
; USAGE
;   prob_red = gauss_tripleredcheck( flux250, flux_err250, flux350,$
;                                    flux_err350, flux500,
;                                    flux_err500, SEED=, NSIM=,
;                                    FLUXPOS=, /NO250POS, /NO350POS )
; INPUTS
;   flux250/flux350/flux500     Fluxes at 250,350,500 microns
;   flux_err250, etc.           Uncertainties in flux250, etc.
; KEYWORDS
;   no250pos                    Don't use 250 micron flux in fluxpos,
;                                if present
;   no350pos                    Don't use 350 micron flux in fluxpos,
;                                if present
; RETURNS
;   Probability that the source is red.
; OPTIONAL INPUTS
;   seed                        Random number generator seed
;   nsim                        Number of simulations used (def: 1000)
;   fluxrat                     Flux ratios in check.  The actual
;                                check is: f350 > fluxrat[0]*f250 and
;                                f500 > fluxrat[1]*f350.  The default
;                                is [1,1]
; OPTIONAL OUTPUTS
;   fluxpos                     The probability that the source has
;                                positive flux in all bands
; NOTES
;   This is done numerically, so you need to a lot of sims if you are
;   considering small probabilities
; MODIFICATION HISTORY
;   Author: Alex Conley, October 27, 2011
;-

FUNCTION gauss_tripleredcheck,flux250, flux_err250, flux350,$
                              flux_err350, flux500, flux_err500, $
                              FLUXPOS=fluxpos, NO250POS=no250POS,$
                              NO350POS=no350pos, SEED=seed, NSIM=nsim,$
                              FLUXRAT=fluxrat
  COMPILE_OPT IDL2, STRICTARRSUBS
  
  nsrcs = N_ELEMENTS(flux250)
  IF nsrcs EQ 0 THEN MESSAGE,"No sources!"
  IF nsrcs NE N_ELEMENTS(flux_err250) THEN $
     MESSAGE,"flux_err250 wrong number of elements"
  IF nsrcs NE N_ELEMENTS(flux350) THEN $
     MESSAGE,"flux_350 wrong number of elements"
  IF nsrcs NE N_ELEMENTS(flux_err350) THEN $
     MESSAGE,"flux_err350 wrong number of elements"
  IF nsrcs NE N_ELEMENTS(flux500) THEN $
     MESSAGE,"flux_500 wrong number of elements"
  IF nsrcs NE N_ELEMENTS(flux_err500) THEN $
     MESSAGE,"flux_err500 wrong number of elements"
  IF N_ELEMENTS(nsim) EQ 0 THEN i_nsim = 1000 ELSE i_nsim = nsim
  IF i_nsim LE 0 THEN MESSAGE,"Invalid (non-positive) number of sims"
  IF N_ELEMENTS(fluxrat) NE 0 THEN BEGIN
     IF N_ELEMENTS(fluxrat) EQ 1 THEN i_fluxrat = REPLICATE(fluxrat[0],2) ELSE $
        i_fluxrat = fluxrat[0:1]
  ENDIF ELSE i_fluxrat = [1.0,1.0]
  
  probarr = FLTARR(nsrcs)
  IF ARG_PRESENT(fluxpos) THEN BEGIN
     fluxpos = FLTARR(nsrcs)
     ;;set type code for positive check
     ;; code 0 : check all
     ;; code 1 : check 350, 500
     ;; code 2 : check 250, 500
     ;; code 3 : check 500 only
     pos_code = 0
     IF KEYWORD_SET( no250pos ) THEN BEGIN
        IF KEYWORD_SET( no350pos ) THEN pos_code = 3 ELSE pos_code=1
     ENDIF ELSE IF KEYWORD_SET( no350pos ) THEN pos_code=2
  ENDIF
  ;;Do in for loop since memory requirement could get ugly if we
  ;; do it in a big chunck
  insim = 1.0/i_nsim
  FOR i=0,nsrcs-1 DO BEGIN
     fluxgens = RANDOMN(seed,i_nsim,3)
     fluxgens[*,0] = flux_err250[i]*fluxgens[*,0] + flux250[i]
     fluxgens[*,1] = flux_err350[i]*fluxgens[*,1] + flux350[i]
     fluxgens[*,2] = flux_err500[i]*fluxgens[*,2] + flux500[i]

     wred = WHERE( fluxgens[*,2] GE i_fluxrat[1]*fluxgens[*,1] AND $
                   fluxgens[*,1] GE i_fluxrat[0]*fluxgens[*,0], nred )
     probarr[i] = nred * insim

     IF ARG_PRESENT(fluxpos) THEN BEGIN
        CASE pos_code OF
           0 : wpos = WHERE(fluxgens[*,2] GE 0.0 AND fluxgens[*,1] GE 0.0 AND $
                            fluxgens[*,0] GE 0.0, npos )
           1 : wpos = WHERE(fluxgens[*,2] GE 0.0 AND fluxgens[*,1] GE 0.0, npos)
           2 : wpos = WHERE(fluxgens[*,2] GE 0.0 AND fluxgens[*,0] GE 0.0, npos)
           3 : wpos = WHERE(fluxgens[*,2] GE 0.0, npos)
        ENDCASE
        fluxpos[i] = npos * insim
     ENDIF
  ENDFOR

  RETURN,probarr
END
