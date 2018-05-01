;+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  function smap_getdefpixsize.pro
;;  Aug 30, 2009
;;  Mike Zemcov
;;  This function takes no input and returns a vector like [PSW,PMW,PLW]
;;   of various default parameters
;;  Inputs: none
;;  Options:
;;    pixsize = Array of three pixel sizes in degrees
;;  Outputs = params, a structure of various hard-wired default parameters
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-
FUNCTION SMAP_GETDEFPARAMS, PIXSIZE=pixsize

  ;; make the pixel size
  IF N_ELEMENTS(pixsize) EQ 0 THEN $
     pixsize = [18.0,25.0,36.0]/(3.0*3600.0) ;;1/3 beam
  IF N_ELEMENTS(pixsize) EQ 1 THEN pixsize = REPLICATE(pixsize,3)
  IF N_ELEMENTS(pixsize) NE 3 THEN MESSAGE,"Expected three pixel sizes"

  ;;Sanity check
  IF MAX(pixsize) GT 0.1 THEN $
     MESSAGE,"WARNING: very large pixel size.  Did you pass in arcsec"+$
             " instead of degrees?",/INF

  params = {pixsize:pixsize,$
            pixsize_units: 'Deg',$
            bands:['PSW','PMW','PLW']}

  ;return it
  RETURN,params

END
