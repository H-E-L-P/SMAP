; $Id: superpose_stars.pro, v 1.3 Sep 2001 e.d. $
;
;+
; NAME:
;	SUPERPOSE_STARS
;
; PURPOSE:
;	Given the coordinates of a set of stars in an image, extract sub-images
;	centered on the stars and combine them by a simple pixel-by-pixel average
;  or average with sigma-clipping or median operation. This routine may be
;  used to estimate the PSF in a stellar field image.
;	If the stars are known, e.g. after analyzing the field with a preliminary
;	PSF estimate, then it is possible to clean each selected star from all
;	the other point sources around before deriving a new PSF estimate.
;
; CATEGORY:
;	Signal processing.
;
; CALLING SEQUENCE:
;	Result = SUPERPOSE_STARS(Image, X, Y, Siz, Stack, Masks)
;
; INPUTS:
;	Image:	2D image of the stellar field
;
;	X, Y:	Coordinates of the stars to extract
;
;	Siz:	Scalar, representing the size of the output combined image
;
; KEYWORD PARAMETERS:
;	NO_SUB_PIX:	Set this keyword to avoid centering each star with sub-pixel
;		accuracy. For details on centering, see the function CENTROIDER in
;		the file 'centroider.pro'.
;
;	INTERP_TYPE:	Set this keyword to a string identifying the interpolation
;		technique to use for sub-pixel centering. For details, see the function
;		IMAGE_SHIFT in the file 'image_shift.pro'
;
;	NOISE_LEVEL:	Scalar, representing the background noise above the local
;		sky level. It may be expressed as N * Sigma, where N is any number > 0
;		and Sigma is the overall standard deviation of the Image gaussian noise.
;
;	SATURATION:	Use this keyword to provide the saturation threshold which
;		is used to mask the core of bright saturated stars when computing the
;		median superposition. Though saturated, these stars should not be
;		rejected, because they provide useful information on the PSF halo.
;		The value of the keyword SATURATION may be:
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
;	STARS:	Stellar field model, including a replica of the PSF for each known
;		star. This keyword, along with X, Y, FLUX, PSF, must be supplied when
;		the selected stars, identified by the positions X and Y (see INPUTS)
;		have to be cleaned from the contamination of secondary sources. This
;		will produce a better PSF estimate.
;
;	X_STARS, Y_STARS, FLUXES:	Positions and fluxes of the known stars
;		appearing in the image model supplied by the keyword STARS.
;
;	PSF:	Point Spread Function used to create the image model STARS.
;		It may be either a 2D array or a 3D stack of local PSF images, when
;		PSF is space-variant. In the latter case, it is necessary to supply
;		the bounds of the image domain partition (see LX, UX, LY, UY).
;
;	LX, UX, LY, UY:	Vectors specifying the bounds of the image domain
;		partition when the "space-variant PSF" option is used. In this case
;		the sub-domain  [LX[j]: Ux[j], LY[i]: UY[i]]  must correspond to the
;		(i * X_size + j)-th  PSF in the input stack. For more details, see
;		the documentation on the routine ARRAY_PARTITION.
;
; OUTPUTS:
;	Result:	2D array of size Siz*Siz containing the median combination of
;		the extracted stars, after suitable processing
;
; OPTIONAL OUTPUTS:
;	Stack:	Cube of stellar images, optionally centered and normalized,
;		before masking
;
;	Masks:	Cube of byte type, having the same size as the previous Stack.
;		The n-th plane in this cube is defined as follows:
;		Masks[j, i, n] = 1, if the pixel Stack[j, i, n] has been used to
;	                        form the output image
;		               = 0, if the same pixel has been masked
;		The Masks cube is used in median superposition
;
; RESTRICTIONS:
;	1) The input Image should be background subtracted.
;	2) Sub-pixel centering is based on interpolation and should be avoided
;	with undersampled data.
;	3) The extracted stellar images are normalized before superposition.
;  Saturated stars should have been previously repaired to ensure proper
;  normalization. However previous repair of saturated stars should not
;  prevent the use of the keyword SATURATION.
;
; PROCEDURE:
;	Extract a box centered on each star, suitably padded with 0 if the
;	source is near the Image edge. Additional 0-padded pixels will be
;	masked.
;	Optionally compute weights for median superposition, as the square root
;  of the peak of each star. Center each sub-image with sub-pixel accuracy,
;	then optionally mask pixels below the noise threshold and above the
;	saturation level. Notice that if a noise level is defined, the pixels
;	below this level are masked (set to 0!) and the central connected
;	component of the resulting image (see IMAGE_CORE for details) is
;	extracted.
;	Combine the frames in the stack by average or average with sigma-rejection
;  or median or weighted median.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;	Updates:
;	1) Fixed bug on normalization (Emiliano Diolaiti, January 2000).
;	2) Secondary stars subtraction (Emiliano Diolaiti, January 2000).
;	3) NOISE_LEVEL keyword (Emiliano Diolaiti, February 2000).
;	4) Renamed keyword NORM_MAX to MAX_NORM (Emiliano Diolaiti, September 2001).
;	5) Added keyword RAD_NORM (Emiliano Diolaiti, September 2001).
;	6) Replaced keyword UNWEIGHTED with WEIGHTED
;     (Emiliano Diolaiti, September 2001).
;-



; CLEAN_EXTRACT: extract stellar images after removing secondary sources.

PRO clean_extract, image, x_psf, y_psf, siz, _EXTRA = extra, $
                   stars, x_stars, y_stars, f_stars, psf, stack, masks

	on_error, 2
	imasiz = size52(image, /DIM)  &  nframes = n_elements(x_psf)
	stack = fltarr(siz, siz, nframes)  &  masks = bytarr(siz, siz, nframes)
	for  n = 0L, nframes - 1  do begin
	   compare_lists, x_psf[n], y_psf[n], x_stars, y_stars, $
	                  aux1, aux2, x, y, SUBSCRIPTS_2 = w
	   f = f_stars[w]
	   sub_arrays, image - stars + image_model(x, y, f, $
	               imasiz[0], imasiz[1], psf, _EXTRA = extra), $
	               x_psf[n], y_psf[n], siz, frame_n, mask_n
	   stack[*,*,n] = frame_n  &  masks[*,*,n] = mask_n
	endfor
	return
end

;;; The main routine.

FUNCTION superpose_stars, image, x, y, siz, stack, masks, _EXTRA = extra, $
                          NO_SUB_PIX = no_sub_pix, NOISE_LEVEL = noise_lev, $
                          SATURATION = satur_lev, MAX_NORM = max_norm, $
                          RAD_NORM = rad_norm, WEIGHTED = weighted, $
                          STARS = stars, PSF = psf, $
                          X_STARS = x_stars, Y_STARS = y_stars, FLUXES = f_stars

	on_error, 2

	; Extract sub-images
	if  n_elements(stars) ne 0 and n_elements(psf) ne 0 and $
	    n_elements(x_stars) ne 0 and n_elements(y_stars) ne 0 and $
	    n_elements(f_stars) ne 0  then $
	   clean_extract, image, x, y, siz, _EXTRA = extra, $
                      stars, x_stars, y_stars, f_stars, psf, stack, masks  else $
       sub_arrays, image, x, y, siz, stack, masks
	var_satur = n_elements(satur_lev) ne 0
	if  var_satur  then $
	   var_satur = max(abs(size52(satur_lev, /DIM) - size52(image, /DIM))) eq 0
	if  var_satur  then $	; define a stack of non-uniform saturation levels
	   sub_arrays, satur_lev, x, y, siz, satur_stack
	if  size52(stack, /N_DIM) eq 2  then $
	   nframes = 1  else  nframes = (size52(stack, /DIM))[2]

	; Sub-pixel centering
	if  not keyword_set(no_sub_pix)  then  for  n = 0L, nframes - 1  do $
	   stack[*,*,n] = centroider(stack[*,*,n], XC = siz/2, YC = siz/2, _EXTRA = extra)

	; Define weights
	if  keyword_set(weighted)  then begin
	   w = fltarr(nframes)
	   for  n = 0L, nframes - 1  do  w[n] = sqrt(stack[siz/2,siz/2,n] > 0)
	endif

	; For each star, mask pixels above saturation threshold
	if  n_elements(satur_lev) ne 0  then begin
	   satur = satur_lev
	   for  n = 0L, nframes - 1  do begin
	      if  var_satur  then  satur = satur_stack[*,*,n]
	      exclude = where(stack[*,*,n] gt satur, n_ex)
	      mask_n = make_array(siz, siz, /BYTE, VALUE = 1B)
	      if  n_ex ne 0  then  mask_n[exclude] = 0B
	      masks[*,*,n] = masks[*,*,n] and mask_n
	   endfor
	endif

	; For each star, extract central connected component above noise level
	if  n_elements(noise_lev) ne 0  then begin
	   eps = 1e-12
	   for  n = 0L, nframes - 1  do $
	      stack[*,*,n] = image_core(binary_array(stack[*,*,n], noise_lev) * $
	                                stack[*,*,n], eps, X = siz/2, Y = siz/2)
	endif

	; Normalize sub-images
	if  keyword_set(rad_norm)  then $
	   peak = where(radial_dist(siz, siz, siz/2, siz/2) le rad_norm)
	for  n = 0L, nframes - 1  do $
	   if  keyword_set(max_norm)  then $
	      stack[*,*,n] = stack[*,*,n] / stack[siz/2,siz/2,n]  else $
	   if  keyword_set(rad_norm)  then $
	      stack[*,*,n] = stack[*,*,n] / total((stack[*,*,n])[peak])  else $
	      stack[*,*,n] = stack[*,*,n] / total(stack[*,*,n])

	; Compute stack median
	if  nframes eq 1  then $
	   avg = stack[*,*,0] * masks[*,*,0]  else $
	   avg = stack_combine(stack, WEIGHTS = w, MASK = masks, _EXTRA = extra)

	return, avg
end
