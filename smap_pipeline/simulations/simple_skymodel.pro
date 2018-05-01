;+
; NAME:
;      simple_skymodel
; PURPOSE:
;      Draw source counts from a broken power law counts model 
; EXPLANATION:
;     Observed p(D) = histogram of observed map (careful to compute bins in the same way always!)  
;     Model p(D) = broken power law -> counts -> randomly distribute -> convolve with psf -> add noise (white only?) -> compute histogram --> chisq --> new params
;
; CALLING SEQUENCE:
;     sim_im = simple_skymodel(nsourceb, nsourcef, alpha, beta, fmin, fmax,IN_IMAGE=in_image,NPIX=npix,CONVOLVE=convolve)
;
; INPUTS:
;	nsourceb - Number of bright sources 
;	nsourcef - Number of faint sources
;	alpha - Bright power law index
;	beta - Faint power law index
;	fmin - Minimum source flux [Jy]
;	fmax - Maximum source flux [Jy]
;
; OPTIONAL KEYWORD INPUTS:
; 	CONVOLVE - FWHM of gaussian convolution kernel
; 	NOISE - FWHM white noise [Jy]
;	IN_IMAGE - input image , the dimensions of which will be reproduced in the simulated image
;	NPIX - If no input is given, the pixel dimension of the output image should be set here. 
;		EITHER NPIX OR IN_IMAGE AND IN_HEAD SHOULD BE SET, NOT BOTH. 
;
; RETURNS:
;	sim_im - an image with dimensions matching IN_IMAGE, or alternatively, [npix,npix] with the 
;		given source distribution, poisson distributed on the sky 
;
; EXAMPLE:
;        testsim=simple_skymodel(1034100,47712,-2.09,-2.66,0.0179,0.0005,1.,NPIX=100,CONVOLVE=2.5,NOISE=0.0002)     
;
; MODIFICATION HISTORY
;  Author: Louis Levenson 10/2009
;          Mike Zemcov 2/2010 - changed i/o calls to be more user 
;                               friendly and assumed some default parameters.
;-

FUNCTION SIMPLE_SKYMODEL,$
   modelparams,$
   in_image,$
   CONVOLVE=convolve,$
   NOISE=noise,$  
   VERBOSE=verbose,$
   SUCCESS=success,$
   ERRMSG=errmsg

  compile_opt idl2
  ;; set up the error handling variables
  success = 0b
  errmsg = ''
  
  ; set up the default model parameters in the form:
  ;  nsourceb (#/deg^2), nsourcef (#/deg^2), alpha, beta, 
  ;  break (Jy), fmin (Jy), fmax (Jy)
  defparams = [1034100,47712,-2.09,-2.66,0.0179,0.0005,1]
  
  ;; did you provide an input set of parameters?
  ;; if not, then make dummy array
  IF N_ELEMENTS(modelparams) EQ 0 THEN modelparams = defparams
  
  ;; ok, now modelparams for sure exists, $
  ;; but let's check if it has the correct number of entries
  IF N_ELEMENTS(modelparams) NE 7 THEN BEGIN
     ;; if it doesn't bail
     errmsg = 'MODEL PARAMETER ARRAY DOES NOT HAVE 7 ENTRIES, ABORTING.'
     GOTO,err_handler
  ENDIF ELSE BEGIN
                                ; if it does, find the NANs
     IF KEYWORD_SET(verbose) THEN $
                                ; tell me you found undefined things
        MESSAGE,'Found undefined entries in model param input, ' + $
        'using default values.',/INFORMATIONAL
     whnan = WHERE(FINITE(modelparams) EQ 0,count)
     ; over write nan values
     IF count GT 0 THEN $
        modelparams[whnan] = defparams[whnan]
  ENDELSE

  ; DID YOU PROVIDE AN INPUT MAP?
  IF N_ELEMENTS(in_image) EQ 0 THEN BEGIN
     errmsg='NO INPUT IMAGE PROVIDED, ABORTING.'
     GOTO, err_handler
  ENDIF 

  IF ~KEYWORD_SET(convolve) THEN BEGIN
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,'WARNING: CONVOLVE keyword not set, ' + $
                'assuming you want to convolve.',/INFORMATIONAL
     convolve = 1
  ENDIF

  sim_im          = in_image
  sim_im.image    = 0.0 * sim_im.image
  sim_im.error    = 0.0 * sim_im.error
  sim_im.exposure = 0.0 * sim_im.exposure + 1.0  
  sim_im.mask     = 0.0 * sim_im.mask

  band = sim_im.names

  ; set up modelparam variables
  nsourceb = modelparams[0]
  nsourcef = modelparams[1]
  alpha    = modelparams[2]
  beta     = modelparams[3] 
  sbreak   = modelparams[4]
  fmin     = modelparams[5]
  fmax     = modelparams[6]

  ;; NORMALIZE NSOURCE
  imangx = in_image.xsize * ABS(in_image.astrometry.cd[0,0])
  imangy = in_image.ysize * ABS(in_image.astrometry.cd[1,1])
  maparea = fix(imangx * imangy)

  nsourceb = nsourceb * maparea
  nsourcef = nsourcef * maparea

  ;; ok, finished set up, now we need to populate the map
  
  ;; GENERATE RANDOM PARAMETERS
  ;; DRAW SOURCE DISTRIBUTION FROM THE RESULTING POWER LAW 
  randomp,flux_b,alpha,nsourceb,range_x=[fmin,sbreak]
  randomp,flux_f,beta,nsourcef,range_x=[sbreak,fmax]
  flux=[flux_f,flux_b]

  ;; RANDOMLY DISTRIBUTE 
  ;; GENERATE RANDOM PIXEL VALUES
  gpixels=fltarr(nsourceb+nsourcef,2)
  gpixels[*,0]=fix(in_image.xsize*randomu(SEED,nsourceb+nsourcef))
  gpixels[*,1]=fix(in_image.ysize*randomu(SEED,nsourceb+nsourcef))

  FOR isrc=0L,nsourceb + nsourcef - 1L DO BEGIN
     sim_im.image[gpixels[isrc,0],gpixels[isrc,1]] = $
        sim_im.image[gpixels[isrc,0],gpixels[isrc,1]] + $
        flux[isrc]
  ENDFOR

  IF convolve THEN BEGIN
     pixsize = 3600. * SQRT(sim_im.astrometry.cd[0,0]^2 + $
                            sim_im.astrometry.cd[1,1]^2) / SQRT(2.)
     npix = 5 * GET_SPIRE_BEAM_FWHM(band) / pixsize
     psf=GET_SPIRE_BEAM(band,pixsize,npix,npix,/SILENT,/NORM)
     sim_im.image = CONVOL(sim_im.image,psf,/EDGE_WRAP)
  ENDIF

  ;; A WHITE NOISE GENERATOR
  IF N_ELEMENTS(noise) NE 0 THEN BEGIN
     noise_im=randomn(SEED,imsize[0],imsize[1])*noise 
     sim_im=sim_im+noise_im
  ENDIF

  RETURN,sim_im

  err_handler:
  IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF

END
