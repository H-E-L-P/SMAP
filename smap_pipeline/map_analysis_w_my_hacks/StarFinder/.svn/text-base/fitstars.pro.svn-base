; $Id: fitstars.pro, v 1.2 Dec 2004 e.d. $
;
;+
; NAME:
;	FITSTARS
;
; PURPOSE:
;	Fit a multiple-component stellar image with a model given by a sum
;	of shifted scaled replicas of a reference image (one for each star),
;	superposed on a background represented by a slanting plane plus a
;	fixed additive contribution.
;	The parameters to be optimized are the stellar fluxes and sub-pixel
;	positions along with the coefficients of the slanting plane.
;	The optimization is performed by means of the Newton-Gauss iterative
;	method.
;	The reference image to be replicated (PSF) may either be a fixed
;	template ("input PSF" option) or a model computed by an auxiliary
;	procedure ("PSF model" option).
;
; CATEGORY:
;	Stellar astrometry and photometry.
;
; CALLING SEQUENCE:
;	FITSTARS, Image, Psf, X0, Y0, X, Y, F, B, Fit_error, $
;			  Sigma_x, Sigma_y, Sigma_f, Sigma_b
;
; INPUTS:
;	Image:	Stellar image
;
;	Psf:	When the "input PSF" option is used, this parameter must
;		contain a 2D image of the PSF.
;		When the "space-variant PSF" option is used, Psf must be a 3D stack
;		of PSF images. In this case it is necessary to supply the bounds of
;		image domain partition (see KEYWORDS LX, UX, LY, UY).
;		When the "PSF model" option is used, it must contain the string
;		which identifies the type of PSF model to apply, as defined in the
;		function IMAGE_MODEL (see the file 'image_model.pro').
;		Notice that when the first option is applied, the input template
;		is shifted by interpolation to match the stars in the Image to fit.
;		In this case it is possible to select an interpolation technique
;		with the keyword INTERP_TYPE (see below). For details on the
;		supported interpolation techniques see the functions IMAGE_SHIFT
;		or IMAGE_MODEL. When the second option is applied instead, the
;		procedure which computes the PSF model probably requires additional
;		information, which should be passed through with the keyword
;		PSF_DATA (see below).
;
;	X0, Y0:	Vectors of approximate positions of point sources
;
; KEYWORD PARAMETERS:
;	FIXED:	Fixed additive contribution, not optimized in the fitting
;		process. It must have the same size has the Image.
;		It may represent the contribution of point sources whose centers
;		lie outside the Image support
;
;	LX, UX, LY, UY:	Vectors specifying the bounds of the image domain
;		partition when the "space-variant PSF" option is used. In this case
;		the sub-domain  [LX[j]: Ux[j], LY[i]: UY[i]]  must correspond to the
;		(i * X_size + j)-th  PSF in the input stack.
;
;	PSF_DATA:	Use this keyword to pass a pointer to the auxiliary data
;		required either to shift the input PSF ("input PSF" option) or
;		to compute the PSF model ("PSF model" option).
;		Notice that when the "input PSF" option is applied, this keyword
;		may be undefined on input and may be used to retrieve on output
;		the auxiliary data which can be recycled in further calls. When
;		the "PSF model" option is applied instead, the keyword PSF_DATA
;		must generally be defined also on input (unless the user defines
;		a procedure for the PSF model based on COMMON blocks).
;
;	F0:	Initial guesses of scaling factors (i.e. fluxes) for each point
;		source. Fluxes must be referred to the normalization of the Psf.
;
;	BACKGROUND:	The image background is fitted with a slanting plane.
;		To obtain an initial guess of the stellar fluxes (if F0 is not
;		defined), it is better to have an approximate knowledge of the
;		image background. If no approximate background is provided and
;		F0 is not defined, the initial guesses of the stellar fluxes are
;		computed under the hypothesis of no background in the image.
;
;	NO_SLANT:	Set this keyword to a nonzero value to fit the stellar
;		image with a model made of a sum of stars (plus FIXED contribution)
;		but without slanting plane
;
;	NOISE_STD:	Noise standard deviation. It may be a scalar
;		('white noise' case) or an array, having the same size as Image
;
;	BAD_DATA:	1D vector of Image subscripts representing bad pixels in the
;		Image to be masked, i.e. excluded from the fitting process
;
;	POS_TOL:	Absolute tolerance on stars positions, used by the fitting
;		procedure for a convergence check. The default is 0.01 pixels
;
;	PHOT_TOL:	Relative tolerance on stars fluxes, used for the convergence
;		check. The default is 0.01, i.e. 1%
;
;	VERBOSE:	Set this keyword to a nonzero value to have some output at
;		the end of the fit
;
;	INTERP_TYPE:	String constant, indicating the interpolating technique
;		used by IMAGE_SHIFT to interpolate the input Psf in order to match
;		it to an observed star, which is generally off-centered.
;		For more details, see the function IMAGE_SHIFT in the file
;		'image_shift.pro'
;
;	_EXTRA:	Optional input keyword of NEWTON_GAUSS (e.g. MAXIT, MASK, etc.).
;		For more details, see the procedure NEWTON_GAUSS in the file
;		'newton_gauss.pro'
;
; OUTPUTS:
;	X, Y:	Vector of stars positions, estimated with sub-pixel accuracy
;
;	F:	Vector of stellar fluxes, referred to the normalization of the Psf
;
;	B:	3-components vector of [B0, B1, B2] background coefficients.
;		The background is defined as follows
;		B(x,y) = B0 + B1 * x + B2 * y
;
;	Fit_error:	Least squares fitting error between the best fit model and
;		the input Image. A negative value indicates that the fitting procedure
;		has not converged: the output results are not reliable.
;
;	Sigma_x, Sigma_y, Sigma_f, Sigma_b:	Vectors containing the formal errors
;		(standard deviation) on the estimated parameters (X, Y, F, B).
;		Available only if some information on the noise is given on input,
;		by means of the keywords GAUSSIAN_NOISE and/or PHOTON_COUNTS.
;
; OPTIONAL OUTPUTS:
;	PSF_DATA:	This input keyword may also be used as output keyword to
;		retrieve the additional data required for the PSF shift or the PSF
;		model computation, in case these data are undefined on input or have
;		been modified by IMAGE_MODEL, the function which actually shift the
;		PSF or compute the PSF model. For more details, see the section about
;		KEYWORD PARAMETERS above.
;
;	MODEL:	Best fit image model
;
;	IT:	Actual number of iterations performed by NEWTON_GAUSS
;
;	W_BAD:	Array of Image subscripts representing bad pixels which have
;		been masked. It coincides with the value of the input keyword
;		BAD_DATA, unless additional bad data masking has been performed
;		by NEWTON_GAUSS. For more details on data masking, see the procedure
;		NEWTON_GAUSS in the file 'newton_gauss.pro'
;
; RESTRICTIONS:
;	The main restriction concerns Psf interpolation, a necessary step to
;	match an observed off-centered star with a reference template if the
;	"input PSF" of "space-variant PSF" option is applied. Common interpolation
;	techniques (Fourier transform shift, spline interpolation) cannot be
;	applied to undersampled data. In this case suitable techiques should be
;	used instead, exploiting the "PSF model" option. For details on supported
;	PSF models, see the function IMAGE_MODEL in the file 'image_model.pro'.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;	Updates:
;	1) Fixed bug on output PSF_DATA (Emiliano Diolaiti, February 2000).
;	2) Added initial estimate on background (Emiliano Diolaiti, April 2000).
;   3) PSF_DATA again (Emiliano Diolaiti, June 2000).
;	4) Added space-variant PSF stack option (E.D., December 2004).
;-



;;; Auxiliary procedures/functions.

; FS_MODEL: compute the image model for the current set of parameters.

FUNCTION fs_model, parameters, DATA = data, PSF_DATA = psf_data

	if n_tags((*data).extra) ne 0 then extra = (*data).extra
	sx = (*data).sx  &  sy = (*data).sy
	nstar = (*data).nstar  &  b_nterms = (*data).b_nterms
	fs_deparam, parameters, x, y, f, b, nstar, b_nterms
	unit_flux = 1  &  model = fltarr(sx, sy)
	for  n = 0L, nstar - 1  do begin
	   (*data).psf_stack[*,*,n] = $
	      image_model(x[n], y[n], unit_flux, sx, sy, (*data).psf, $
	      			  psf_data, INTERP_TYPE = (*data).interp_type, _EXTRA = extra)
	   model = model + f[n] * (*data).psf_stack[*,*,n]
	endfor
	bplane = 0
	if  b_nterms ne 0  then  bplane = plane(b[0], b[1], b[2], sx, sy)
	model = model + (*data).fixed_image + bplane
	return, model
end

; FS_PARAM: store the model parameters (i.e. user parameters x, y, etc.)
;	into a single vector (program parameters).

FUNCTION fs_param, x, y, f, b, nstar, b_nterms

	parameters = fltarr(3*nstar + b_nterms)
	parameters[0] = f
	subs = nstar + 2*lindgen(nstar)
	parameters[subs] = x  &  parameters[subs+1] = y
	if  b_nterms ne 0  then  parameters[3*nstar] = b
	return, parameters
end

; FS_DEPARAM: convert program parameters vector to user parameters.

PRO fs_deparam, parameters, x, y, f, b, nstar, b_nterms

	f = parameters[0:nstar-1]
	subs = nstar + 2*lindgen(nstar)
	x = parameters[subs]  &  y = parameters[subs+1]
	if  b_nterms ne 0  then  b = parameters[3*nstar:3*nstar+b_nterms-1]
	return
end

; FS_CENTER_DERIVATIVE: compute the partial derivative of an image
; with respect to the x- or y- coordinate of its center (maximum).

FUNCTION fs_center_derivative, image, f, siz

	deriv = 2*!pi/siz * fft(image) * f
	deriv = complex(imaginary(deriv), -float(deriv))
	deriv = float(fft(deriv, /INVERSE))
	return, deriv
end

; FS_IACOBI: compute the Iacobi matrix of the parametric model.

FUNCTION fs_iacobi, parameters, DATA = data

	sx = (*data).sx  &  sy = (*data).sy
	nstar = (*data).nstar  &  b_nterms = (*data).b_nterms
	n_pix = sx * sy
	iacobi = fltarr(n_pix, 3*nstar + b_nterms)
	for  n = 0L, nstar - 1  do begin
	   iacobi[*,n] = reform((*data).psf_stack[*,*,n], n_pix)
	   deriv = fs_center_derivative((*data).psf_stack[*,*,n], (*data).x_frequency, sx)
	   iacobi[*,nstar+2*n] = reform(parameters[n] * deriv, n_pix)
	   deriv = fs_center_derivative((*data).psf_stack[*,*,n], (*data).y_frequency, sy)
	   iacobi[*,nstar+2*n+1] = reform(parameters[n] * deriv, n_pix)
	endfor
	if  b_nterms ne 0  then  iacobi[*,3*nstar] = 1
	if  b_nterms eq 3  then begin
	   iacobi[*,3*nstar+1] = reform(plane(0, 1, 0, sx, sy), n_pix)
	   iacobi[*,3*nstar+2] = reform(plane(0, 0, 1, sx, sy), n_pix)
	endif
	return, transpose(iacobi)
end

; FS_CONVERGENCE: check the convergence condition for the iterative estimation.

FUNCTION fs_convergence, p0, p, DATA = data

	fs_deparam, p0, x0, y0, f0, b0, (*data).nstar, (*data).b_nterms
	fs_deparam, p,  x,  y,  f,  b, (*data).nstar, (*data).b_nterms
	check = convergence(x0, x, (*data).pos_tol, /ABSOLUTE) and $
			convergence(y0, y, (*data).pos_tol, /ABSOLUTE) and $
			convergence(f0, f, (*data).phot_tol)
;	if  (*data).b_nterms ne 0  then $
;	   check = check and convergence(b0, b, (*data).phot_tol)
	return, check
end

; FS_WEIGHTS: compute inverse variances for weighted least squares fit.

 FUNCTION fs_weights, var

	min_var = min(var)
	if  min_var le 0.  then  min_var = max(var) * 1e-15
	return, 1 / (var > min_var)
end

; FS_ERROR: compute the least squares error between the model and the image.

FUNCTION fs_error, model, data, WEIGHTS = weights, W_BAD = w_bad

	if  n_elements(weights) ne 0  then  w = weights  else  w = 1
	d = w * (model - data)^2
	if  n_elements(w_bad) ne 0  then $
	   if  min(w_bad) ge 0  then  d[w_bad] = 0
	return, total(d)
end

; FS_OUT: print iteration no., fitting error, parameters.

PRO fs_out, it, fit_error, parameters, sigma_p, nstar, b_nterms

	print, ''
	print, 'no. of iterations ', it
	print, 'fitting error ', fit_error
	print, 'parameters: '
	fs_deparam, parameters, x, y, f, b, nstar, b_nterms
	if  n_elements(sigma_p) ne 0  then $
	   fs_deparam, sigma_p, sigma_x, sigma_y, sigma_f, sigma_b, nstar, b_nterms
	print, 'x coordinates:             ', x
	if  n_elements(sigma_x) ne 0  then $
	   	print, 'st. dev. on x coordinates: ', sigma_x
	print, 'y coordinates: ', y
	if  n_elements(sigma_y) ne 0  then $
	   	print, 'st. dev. on y coordinates: ', sigma_y
	print, 'fluxes	     : ', f ;* scale
	if  n_elements(sigma_f) ne 0  then $
	   	print, 'st. dev. on fluxes       : ', sigma_f
	if  b_nterms ne 0  then  print, 'background   : ', b
	if  n_elements(sigma_b) ne 0  then $
	   	print, 'st. dev. on background   : ', sigma_b
   	return
end

;;; The main routine.

PRO fitstars, $
	image, FIXED = fixed_image, psf, PSF_DATA = psf_data, $
	x0, y0, F0 = f0, BACKGROUND = background, NO_SLANT = no_slant, $
   	POS_TOL = pos_tol, PHOT_TOL = phot_tol, VERBOSE = verbose, $
   	_EXTRA = extra, INTERP_TYPE = interp_type, NOISE_STD = noise_std, $
   	x, y, f, b, fit_error, sigma_x, sigma_y, sigma_f, sigma_b, $
   	MODEL = model, IT = it, BAD_DATA = bad_data, W_BAD = w_bad

	catch, error
	if  error ne 0  then begin
	   if  ptr_valid(data)  then  ptr_free, data
	   if  ptr_valid(pdata)  then  ptr_free, pdata
	   fit_error = -1
	   return
	endif
	; Define pointers to 'global' variables
	s = size52(image, /DIM)  &  sx = s[0]  &  sy = s[1]
	if  n_elements(fixed_image) eq 0  then  fixed_image = 0
	if  n_elements(background) eq 0  then $
	   if  n_elements(f0) eq 0  then  background = 0
	if  not keyword_set(no_slant)  then begin
	   b_nterms = 3  &  b = fltarr(b_nterms)
	   if  n_elements(background) ne 0  then $
	      b[0] = total(background) / n_elements(background)
	endif else begin
	   b_nterms = 0  &  b = 0
	endelse
	if  n_elements(interp_type) eq 0  then  interp_type = ''
	nstar = n_elements(x0)
	psf_stack = fltarr(sx, sy, nstar)
	if n_elements(extra) ne 0 then extra_data = extra else extra_data = 0
	if n_elements(pos_tol)  eq 0  then  pos_tol = 0.01
	if n_elements(phot_tol) eq 0  then  phot_tol = 0.01
	x_frequency = frequency(sx) # (fltarr(sy) + 1)
	y_frequency = (fltarr(sx) + 1) # frequency(sy)
	data = {image: image, fixed_image: fixed_image, psf: psf, $
			psf_stack: psf_stack, x_frequency: x_frequency,	  $
			y_frequency: y_frequency, sx: sx, sy: sy, nstar: nstar,   $
			b_nterms: b_nterms, pos_tol: pos_tol, phot_tol: phot_tol, $
			interp_type: interp_type, extra: extra_data}
	data = ptr_new(data, /NO_COPY)	; pointer to data structure
	pdata = ptr_new(/ALLOCATE)		; pointer to PSF auxiliary data
	if  ptr_valid(psf_data)  then $
	   if  n_elements(*psf_data) ne 0  then  *pdata = *psf_data

	; Initial estimates of parameter
	ndim_psf = size52(psf, /N_DIM)
	if ndim_psf lt 2 then $
	   tot_psf = total(image_model(sx/2, sy/2, 1, sx, sy, psf, pdata)) else $
	if ndim_psf eq 2 then $
	   tot_psf = total(sub_array(psf, sx, sy)) else $
	begin
	   npsf = (size52(psf, /DIM))[2]
	   avg_psf = total(psf, 3) / npsf
	   tot_psf = total(sub_array(avg_psf, sx, sy))
	endelse
	x = x0  &  y = y0
	if  n_elements(f0) eq 0  then begin
	   sub = image - fixed_image - background
	   f = sub[round(x)>0<(sx - 1), round(y)>0<(sy - 1)]	; intensities
	   f = f / total(f) * total(sub) / tot_psf		; fluxes
	endif else  f = f0
	p0 = fs_param(x, y, f, b, nstar, b_nterms)

	; Define noise weights
	if  n_elements(noise_std) ne 0  then  weights = fs_weights(noise_std^2)

	; Iterative estimation of parameters
	newton_gauss, 'fs_model', 'fs_iacobi', 'fs_convergence', p0, image, $
	              _EXTRA = extra, NOISE_STD = noise_std, INVERSE_DATA_VAR = $
	              weights, converged, p, sigma_p, model, IT = it, $
	              BAD_DATA = bad_data, W_BAD = w_bad, DATA = data, $
	              PSF_DATA = pdata
	if  converged  then begin
	   fit_error = fs_error(model, image, WEIGHTS = weights, W_BAD = w_bad)
	   fs_deparam, p, x, y, f, b, nstar, b_nterms
	   if  n_elements(sigma_p) ne 0  then $
	   fs_deparam, sigma_p, sigma_x, sigma_y, sigma_f, sigma_b, nstar, b_nterms
	   if  (size52(x0))[0] eq 0  then begin
	      x = x[0]  &  y = y[0]  &  f = f[0]
	   endif
	   if  keyword_set(verbose)  then begin
	      print, 'FITSTARS: converged'
	      fs_out, it, fit_error, p, sigma_p, nstar, b_nterms
	   endif
	endif else begin
	   fit_error = -1
	   if  keyword_set(verbose)  then  print, 'FITSTARS: not converged'
	endelse
	ptr_free, data
	if  ptr_valid(psf_data)  then $
	   if  n_elements(*pdata) ne 0  then  *psf_data = *pdata
	if  ptr_valid(pdata)  then  ptr_free, pdata
	return
end
