; $Id: psf_extract.pro, v 1.3 Aug 2004 e.d. $
;
;+
; NAME:
;	PSF_EXTRACT
;
; PURPOSE:
;	Estimate the PSF in a stellar field image by combination of a stack
;	of user-selected stars.
;	The "PSF stars" are cleaned from the contamination of secondary sources,
;	background-subtracted, centered with sub-pixel accuracy, normalized and
;	combined by median superposition.
;	It is possible to weigh the frames in the stack before superposition and
;	mask sub-regions above a pre-fixed threshold, which may suitably coincide
;	with the detector saturation level. Saturated stars are approximately
;	repaired, by replacing their core with a preliminary estimate of the PSF.
;
; CATEGORY:
;	Signal processing. Stellar photometry.
;
; CALLING SEQUENCE:
;	PSF_EXTRACT, X, Y, X_secondary, Y_secondary, $
;	             Image, Background, Psf_size, Psf, Psf_fwhm
;
; INPUTS:
;	X, Y:	Coordinates of stars to superpose
;
;	X_secondary, Y_secondary:	Coordinates of secondary sources to subtract
;
;	Image:	Image of the stellar field
;
;	Psf_size:	Size of output PSF in pixels
;
; OPTIONAL INPUTS:
;   Background: Array, with the same size as Image, containing an estimate of the
;		background emission.
;
; KEYWORD PARAMETERS:
;	NOBACKGROND:	Set this keyword to specify that the image has no
;		background (or the background in the input Image has already been
;		subtracted).
;
;	N_FWHM_BACK:	Size of box for background sampling (see IMAGE_BACKGROUND
;		for details) in units of the PSF FWHM. The default is N_FWHM_BACK = 5.
;		For other keywords related to background estimation (e.g. SKY_MEDBOX)
;		see the function IMAGE_BACKGROUND in the file "image_background.pro".
;		This keyword is applied only if NOBACKGROUND is not set and the optional
;		input Background is undefined.
;
;	SKY_MEDIAN: Set this keyword to estimate the background by median
;		smoothing of the input Image, instead of using IMAGE_BACKGROUND.
;
;	ITER:	The secondary sources around the PSF stars are fit and subtracted
;		using a preliminary estimate of the PSF. This procedure may be repeated
;		any number of times, as indicated  by the keyword ITER. The default is
;		ITER = 1. Set ITER = 0 to avoid subtracting the secondary sources.
;
;	N_FWHM_FIT:	Width of box used to fit secondary sources.
;		The box size must be specified in units of the PSF FWHM.
;		The default value is N_FWHM_FIT = 2.
;
;	INTERP_TYPE:	Set this keyword to a string identifying the interpolation
;		technique to use for sub-pixel centering. For details, see the function
;		IMAGE_SHIFT in the file "image_shift.pro". Recommended values for the
;		PSF extraction procedure are INTERP_TYPE = "S" or INTERP_TYPE = "I".
;
;	UPPER_LEVEL:	Use this keyword to provide the saturation threshold which
;		is used to mask the core of bright saturated stars when performing the
;		median superposition. Though saturated, these stars should not be
;		rejected, because they provide useful information on the PSF halo.
;		The value of the keyword UPPER_LEVEL may be:
;		a) a scalar, representing the unique threshold to be used all over
;		   the frame;
;		b) a 2D array, having the same size as the input Image, when the
;		   saturation level is not spatially uniform. This happens for
;		   example when the input Image has been background subtracted:
;		   if the removed contribution is not negligible with respect to
;		   the stellar peaks, the saturation threshold will be affected and
;		   should be defined as the original threshold minus the removed
;		   background contribution.
;
;	N_FWHM_MATCH:	Size of "search box", expressed in units of the PSF FWHM,
;		where the PSF must moved to optimize the correlation with a given
;		saturated star to repair. Notice that the correlation is maximized
;		after masking the pixels in the saturated core.
;		The default is N_FWHM_MATCH = 1.
;
;	N_WIDTH:	Size of the inner portion of a saturated star to replace.
;		It must be expressed in units of the saturated core diameter, which
;		is computed automatically by the program.
;		The default is N_WIDTH = 3.
;
;	MAG_FAC:	Integer representing the fractional sub-pixel step for
;		accurate positioning of the PSF estimate on the core of a saturated
;		star. This "magnification factor" is also used to optimize the
;		correlation.
;		The default is MAG_FAC = 2, corresponding to a positioning accuracy
;		of 1/2 pixel.
;
;	MAX_NORM:	Set this keyword to normalize the stars to unitary maximum
;		before combining them in a single frame. The default is to normalize
;     each sub-image to total unit flux.
;
;	RAD_NORM:	Set this keyword to normalize the stars using a circular
;     region centered on the stellar peak. The value of this keyword is the
;	   radius of the normalization region. The default is to normalize
;     each sub-image to total unit flux.
;
;	AVGTYPE:	Set this keyword to choose a combination algorithm among
;		the following possibilities:
;		AVGTYPE = 0 [default]: pixel-by-pixel average
;		        = 1: average with sigma-rejection of outliers
;		        = 2: median
;
;	WEIGHTED:	Set this keyword to weigh the stellar images, before averaging,
;		according to their signal-to-noise ratio in the photon noise case.
;
;	NONORM:	Set this keyword to avoid normalizing the final PSF estimate.
;
; OUTPUTS:
;	Image:	Same as input Image, with corrected saturated stars
;
;	Psf:	2D array of size Psf_size*Psf_size with the estimated PSF,
;		normalized to unit total flux
;
;	Psf_fwhm:	FWHM of estimated PSF
;
;	Background:	Estimate of the background emission
;
;	X, Y:	If there are saturated stars among the ones to superpose, their
;		position might slightly change after repair.
;
; SIDE EFFECTS:
;	1) May create modal widgets by means of DIALOG_MESSAGE.
;	2) Modify the input parameters Image, Clean_image, X, Y if there are
;		saturated stars to repair among the stars to superpose.
;
; RESTRICTIONS:
;	1) The PSF_EXTRACT routine is useful to extract an estimate of the PSF
;	provided the field of view is reasonably isoplanatic.
;	If this condition is not fulfilled, PSF_EXTRACT may still be used to
;	extract a PSF estimate from isoplanatic sub-sections in the image.
;	2) Centering and fitting stellar images is based on data interpolation.
;	Interpolation techniques are not suited to undersampled data.
;	3) Saturated stars may be useful to form a PSF estimate because they
;	provide information on the PSF halo. If there are saturated stars in the
;	Image, they should be repaired, to ensure proper normalization.
;	4) Saturated stars are identified as those peaks among the selected PSF
;	stars with an intensity greater than the saturation threshold.
;	5) Secondary sources around PSF stars are supposed to be unsaturated.
;
; PROCEDURE:
;	Before calling PSF_EXTRACT, the user has to select the candidate
;	"PSF stars" and the contaminating sources around each of them.
;	A preliminary estimate of the PSF is obtained superposing the unsaturated
;	stars with the routine SUPERPOSE_STARS (see the file "superpose_stars.pro"),
;	and is used to fit and subtract the secondary sources, thus cleaning the
;	PSF stars. This process may be repeated iteratively: at every step the
;	accuracy of the "fit + subtract" sequence is supposed to increase, even
;	though in practice one iteration is often enough.
;	A shifted scaled replica of the current PSF estimate is used to replace
;	the core of bright saturated stars: the optimal match is found by
;	cross-correlation maximization for the relative shift and by a least
;	squares fitting of the PSF wings for the scaling factor.
;	Finally all the candidate PSF stars, including the repaired ones, are
;	superposed to form the last PSF estimate, suitably masking the core of
;	saturated stars.
;	In every phase described above, the stellar images are background-
;	subtracted, centered with sub-pixel accuracy and normalized before
;	combination.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August-September 1999.
;	Updates:
;	1) Added keyword REFERENCE_PIX in call to IMAGE_MODEL
;	   (Emiliano Diolaiti, December 1999).
;	2) Modified some keywords, required by called routine SUPERPOSE_STARS
;	   (Emiliano Diolaiti, September 2001).
;	3) Added keyword NOBACKGROUND
;	   (Emiliano Diolaiti, September 2001).
;   4) Background turned to optional input: if it is set, it is not estimated
;	   by PSF_EXTRACT (E. D., August 2004).
;-



;;; Auxiliary procedures/functions.

; PSF_CLEAN: Subtract secondary sources around PSF stars.

FUNCTION psf_clean, image, psf, x, y, fitbox, _EXTRA = extra

	on_error, 2
	clean_image = image  &  siz = size52(image, /DIM)
	fitting_psf = sub_array(psf, 2*fitbox)
	reference_pix = size52(psf, /DIM) / 2
	fit_data = ptr_new(/ALLOCATE)  &  model_data = ptr_new(/ALLOCATE)
	nstars = n_elements(x)
	for  n = 0, nstars - 1  do begin
	   sub_image = sub_array(clean_image, fitbox, $
	   			 REFERENCE = [x[n], y[n]], LX = lx, LY = ly)
	   fitstars, sub_image, fitting_psf, x[n] - lx, y[n] - ly, $
	      		 PSF_DATA = fit_data, _EXTRA = extra, xn, yn, fn, b, fit_error
	   if  fit_error ge 0  then begin
	      xn = xn + lx  &  yn = yn + ly
	      clean_image = temporary(clean_image) - $
	      image_model(xn, yn, fn, siz[0], siz[1], psf, model_data, $
	      			  REFERENCE_PIX = reference_pix, _EXTRA = extra)
	   endif
	endfor
	ptr_free, fit_data, model_data
	return, clean_image
end



;;; The main routine.

PRO psf_extract, $
	x, y, x_secondary, y_secondary, $
	image, psf_size, psf, psf_fwhm, background, $
	NOBACKGROUND = nobackground, N_FWHM_BACK = n_fwhm_back, $
	N_FWHM_FIT = n_fwhm_fit, ITER = iter, UPPER_LEVEL = upper_lev, $
	RAD_NORM = norm_rad, NONORM = nonorm, _EXTRA = extra

	on_error, 2

	; Define unsaturated stars
	x_unsat = x  &  y_unsat = y  &  peaks = image[x, y]
	if  n_elements(upper_lev) ne 0  then begin
	   w = where(peaks lt upper_lev, count)
	   if  count eq 0  then begin
	      id = dialog_message(/ERROR, "Please select at least one unsaturated star.")
   	   return
   	endif
	   x_unsat = x_unsat[w]  &  y_unsat = y_unsat[w]  &  peaks = peaks[w]
	endif
	; Select the brightest unsaturated star
	m = max(peaks, w)  &  x0 = x_unsat[w]  &  y0 = y_unsat[w]
	; Use this star to estimate the PSF FWHM
	psf_fwhm = peak_fwhm(image, X = x0, Y = y0, MAG = 3, /CUBIC, _EXTRA = extra)
	; Estimate the image background, if necessary
	if  keyword_set(nobackground)  then begin
	   siz = size52(image, /DIM)
	   background = fltarr(siz[0], siz[1])
	endif else $
	if n_elements(background) eq 0 then begin
	   if  n_elements(n_fwhm_back) eq 0  then  n_fwhm_back = 5
	   backbox = round(n_fwhm_back * psf_fwhm)
	   background = estimate_background(image, backbox, /CUBIC, _EXTRA = extra)
	endif
	if not keyword_set(nobackground) then $
	   psf_fwhm = peak_fwhm(image - background, X = x0, Y = y0, MAG = 3, /CUBIC)

	; Compute preliminary estimate of the PSF using unsaturated stars
	if  n_elements(norm_rad) ne 0  then  rad = norm_rad * psf_fwhm
	psf = superpose_stars(image - background, x_unsat, y_unsat, psf_size, $
	   					  RAD_NORM = rad, CENTROID_BOX = round(psf_fwhm), _EXTRA = extra)

	; Fit and subtract secondary sources and refine PSF using unsaturated stars
	if  n_elements(iter) eq 0  then  iter = 0
	if  n_elements(n_fwhm_fit) eq 0  then  n_fwhm_fit = 2
	fitbox = round(n_fwhm_fit * psf_fwhm)	; fitting box
	if  iter eq 0  then  clean_image = image
	for  it = 0L, iter - 1  do begin
	   clean_image = psf_clean(image, psf, x_secondary, y_secondary, $
	                           fitbox, _EXTRA = extra)
	   psf = superpose_stars(clean_image - background, x_unsat, y_unsat, $
	                         psf_size, RAD_NORM = rad, CENTROID_BOX = round(psf_fwhm), $
	                         _EXTRA = extra)
	endfor
	psf_fwhm = fwhm(psf, MAG = 3, /CUBIC)

	; Repair saturated stars and compute last estimate of the PSF,
	; using all the selected stars
	n_satur = 0
	if  n_elements(upper_lev) ne 0  then begin
	   w = where(image[x, y] ge upper_lev, n_satur)
	   if  n_satur ne 0  then begin
	      x_satur = x[w]  &  y_satur = y[w]
	   endif
	endif
	if  n_satur ne 0  then begin
	   repair_saturated, image, clean_image, background, psf, psf_fwhm, $
	                     x_satur, y_satur, upper_lev, _EXTRA = extra
	   x = [x_satur, x_unsat]
	   y = [y_satur, y_unsat]
	   psf = superpose_stars(clean_image - background, x, y, psf_size, $
	                         SATURATION = upper_lev - background, $
	                         RAD_NORM = rad, CENTROID_BOX = round(psf_fwhm), $
	                         _EXTRA = extra)
	   psf_fwhm = fwhm(psf, MAG = 3, /CUBIC)
	endif

	if not keyword_set(nonorm) then psf = psf / total(psf)

	return
end
