;NAME
; red_logistic_model
;PURPOSE
; Given catalog from red_sourcefind(_dofind), use logistic
; regression model to select red sources.  The model is hardwired
; to assumptions about the coefficients used to combine the maps.
;USAGE
; wred = red_logistic_model(fsub, dfsub, f250, f350, f500,
;                           corr, [N=n])
;INPUTS
; fsub     Combined map flux density [mJy]
; dfsub    Reported uncertainty in fsub
; f250     filtered map 250um flux density [mJy]
; f350     filtered map 350um flux density [mJy]
; f500     filtered map 500um flux density [mJy]
; corr     PSF correlation coefficient
;OUTPUTS
; A where array indexing which elements are red, or -1 if not found
;OPTIONAL OUTPUTS
; N        Number of sources selected
;NOTES:
; This is hardwired to the case k1=-0.650, k2=0.110 trained on B12
; data with a threshold of 20mJy and C=0.2 using a logistic linear
; model with L1 regularization.  
;MODIFICATION HISTORY
; Written: A. Conley, January 2015
;-

FUNCTION red_logistic_model, fsub, dfsub, f250, f350, f500, corr, N=n
  COMPILE_OPT IDL2, STRICTARRSUBS

  n = 0
  IF N_ELEMENTS(fsub) EQ 0 THEN RETURN, -1

  ;; Old k1 = -0.775 k2 = 0.483 model
  ;;u = 2.18419416 - 0.503927*(dfsub - 0.8802768) -$
  ;;    0.2463847*(f250 - 30.50877) -$
  ;;    2.04404432e-2*(f350 - 51.0699921) +$
  ;;    0.252701092*(f500 - 54.4681244) +$
  ;;    2.03998195*(corr - 0.93833828) -$
  ;;    6.057337e-4*(fsub^2 - 8.568408e2) -$
  ;;    8.2002138e-6*(f350^2 - 3.24788784e3) -$
  ;;    6.96945577e-6*(fsub^3 - 3.39166836e4)

  ;; k1 = -0.65 k2 = 0.350 thresh=30 C=0.05
  u = -0.83382513 + 4.38179830e-02*(fsub - 2.36889210e+01) -$
      1.35475900e+00*(dfsub - 9.35850024e-01) +$
      7.00586675e-02*(f250 - 4.02687531e+01) -$
      3.18775094e-01*(f350 - 4.80162163e+01) +$
      3.54239638e-01*(f500 - 4.48621063e+01) -$
      1.05469184e+00*(corr - 9.17310834e-01) -$
      2.20595287e-04*(f350^2 - 2.76666187e+03) -$
      2.07995361e-04*(fsub * f350 - 1.27160254e+03) -$
      4.31456042e-06*(fsub^3 - 2.54680176e+04)
  
  RETURN, WHERE(u > 0.0, n)
END
