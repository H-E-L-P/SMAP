; $Id: image_model.pro, v 1.5 Jan 2005 e.d. $
;
;+
; NAME:
;	IMAGE_MODEL
;
; PURPOSE:
;	Create a synthetic image given by a sum of shifted scaled replicas
;	of a PSF. The PSF may either be a replica of the input template
;	("fixed PSF" option) or may be extracted from a set of local PSFs
;	("space-variant PSF" option) or computed for each location, according
;	to some user-defined model ("PSF model" option).
;
; CATEGORY:
;	Models.
;
; CALLING SEQUENCE:
;	Result = IMAGE_MODEL(X, Y, F, X_size, Y_size, Psf, Data)
;
; INPUTS:
;	X, Y:	Vectors of stars positions
;
;	F:	Vector of stellar fluxes
;
;	X_size, Y_size:	First and second size of the output array
;
;	Psf:	When the "fixed PSF" option is used, Psf must be a 2D array
;		containing he image of the PSF to be replicated in the output image
;		model.
;		When the "space-variant PSF" option is used, Psf must be a 3D stack
;		of PSF images. In this case it is necessary to supply the bounds of
;		image domain partition (see KEYWORDS LX, UX, LY, UY).
;		When the "PSF model" option is used, Psf must be a string identifying
;		the desired type of PSF model. The following PSF models are available:
;		- gaussian PSF. Select it by setting Psf = "gaussian"
;		- Airy diffraction pattern. Set Psf = "airy"
;		- Sum of scaled gaussians. Set Psf = "multigaussian"
;		If the "PSF model" option is applied, maybe the user has to supply
;		additional information with the parameter Data (see OPTIONAL INPUTS),
;		depending on how the procedure which actually computes the PSF model
;		is defined.
;
; OPTIONAL INPUTS:
;	Data: Use this variable to provide a pointer to the additional information
;		required by the "model" option.
;		In general the heap variable pointed by Data is a structure, containing
;		miscellaneous information.
;		For the supported model types, the structure pointed by Data must be
;		defined as follows:
;		1) Psf type = 'gaussian'
;			*Data = {X_size: x_size, Y_size: y_size, $
;                    Sigma_x: sigma_x, Sigma_y: sigma_y, Angle: angle},
;			where X_size and Y_size represent the size of the PSF model,
;			Sigma_x and Sigma_y are the standard deviations of the gaussian,
;			Angle is the position angle of the gaussian
;		2) Psf type = 'airy'
;			*Data = {X_size: x_size, Y_size: y_size, $
;                    Sampling_factor: sampling_factor},
;			where Sampling_factor is the ratio of the actual sampling step
;			to the critical step size
;		3) Psf type = 'multigaussian'
;		When the "fixed PSF" options is chosen, the variable Data may also be
;		used to provide additional information for the Psf shift, which has
;		been released on output in a previous call to IMAGE_MODEL (see OPTIONAL
;		OUTPUTS below).
;		The parameter Data must NOT be supplied when a stack of space-variant
;		PSFs is supplied.
;
; KEYWORD PARAMETERS:
;	LX, UX, LY, UY:	Vectors specifying the bounds of the image domain
;		partition when the "space-variant PSF" option is used. In this case
;		the sub-domain  [LX[j]: Ux[j], LY[i]: UY[i]]  must correspond to the
;		(i * X_size + j)-th  PSF in the input stack.
;
;	INTERP_TYPE:	Set this keyword to a string identifying one of the
;		interpolation techniques supported by the function IMAGE_SHIFT
;		(for more details see the file 'image_shift.pro'). This keyword
;		is neglected if the "PSF model" option is used.
;
;	REFERENCE_PIX:	Set this keyword to a two-elements integer vector with
;		the coordinates of the reference pixel in the Psf array which must
;		be placed at the positions (X, Y) in the output array.
;		The default is the Psf maximum.
;
; OUTPUTS:
;	Result:	2D array containing the synthetic model
;
; OPTIONAL OUTPUTS:
;	Data:	The heap variable pointed by Data may be modified by the
;		procedures called by IMAGE_MODEL to shift the input Psf ("input PSF"
;		option) or to compute a PSF model ("PSF model" option).
;		This pointer can be used as an useful input/output variable to
;		provide or retrieve information for subsequent calls to IMAGE_MODEL.
;		Let us consider the following example: suppose IMAGE_MODEL is
;		called the first time with the heap variable (*Data) undefined and
;		using the "input PSF" option. The pointer variable Data released on
;		output will reference an anonymous structure (created by IMAGE_SHIFT)
;		with useful information which can be recycled in a further call to
;		IMAGE_MODEL (provided all the options are the same, of course).
;
; RESTRICTIONS:
;	The "fixed" and "space-variant" PSF options are suited to a well sampled
;	PSF: in this case it is possible to interpolate the template when a
;	fractional shift is required for sub-pixel positioning. When the data are
;	sub-sampled, the "model" option should be used.
;
; PROCEDURE:
;	For each input position and stellar flux, put one PSF in the output image.
;	The PSF may be simply a replica of the input template or may be extracted
;	from an stack of local PSFs or even computed for each location. In the
;	first two cases, sub-pixel positioning is performed by interpolating the
;	input PSF (see the function IMAGE_SHIFT in the file 'image_shift.pro' for
;	more details). In the third case, the computed PSF model should already
;	account for any sub-pixel offsets.
;	If the user wishes to define new model options, he/she must write a new
;	procedure according to the following template:
;
;	PRO model_psf, X, Y, Aux, Psf, Reference_pixel
;		dx = X - round(X)  &  dy = Y - round(Y)
;		"function call" to define the PSF model,
;			having its maximum at (x_size/2,y_size/2)
;		Psf = Psf / total(Psf)
;		Reference_pixel = [Aux.x_size, Aux.y_size] / 2
;		return
;	end
;
;	where X and Y represent the location of a star in the image, Aux is a
;	structure passed to IMAGE_MODEL through the pointer Data (see OPTIONAL
;	INPUTS above), Psf is the output PSF model and Reference_pixel will be
;	used by IMAGE_MODEL to position the computed PSF in the output array.
;	The line "function call" in the above template is a call to some
;	procedure/function which actually computes the PSF model. The maximum
;	intensity pixel should lie at (x_size/2,y_size/2). For more details,
;	see the procedures 'gaussian_psf' and 'airy_psf' in this file.
;	After defining the new procedure, a line of code should be added in the
;	CASE instruction of the IMAGE_MODEL function, like the following
;
;		case  strlowcase(psf)  of
;	       'gaussian':	psf_pro = 'gaussian_psf'
;	       'airy':	psf_pro = 'airy_psf'
;		   'new_model_identifier':	psf_pro = 'new_procedure_name'
;		endcase
;
;	To use the new user-defined PSF model, set the input parameter Psf to
;	the string 'new_model_identifier'
;
;
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;	Updates:
;	1) Added REFERENCE_PIX keyword (Emiliano Diolaiti, December 1999)
;	2) Space-Variant PSF option (Emiliano Diolaiti, January 2000)
;   3) Added MODEL keyword (Emiliano Diolaiti, December 2000)
;   4) Space-Variant PSF option: fixed error in call to PICK_REGION
;      and adjusted call to IMAGE_SHIFT (Emiliano Diolaiti, December 2004)
;	5) Added "multigaussian_psf" and "powergau_psf" options
;	   (E. D., January 2005)
;-


;;; Auxiliary procedures to compute PSF model.

; GAUSSIAN_PSF: 2D (elliptical) gaussian PSF.

PRO gaussian_psf, x, y, data, psf, ref_pix

	on_error, 2
	dx = x - round(x)  &  dy = y - round(y)
	psf = gaussian2d(data.x_size, data.y_size, $
					 data.x_size/2+dx, data.y_size/2+dy, $
					 data.sigma_x, data.sigma_y, data.angle)
	psf = psf / total(psf)
	ref_pix = [data.x_size, data.y_size] / 2
	return
end

; AIRY_PSF: Airy diffraction pattern.

PRO airy_psf, x, y, data, psf, ref_pix

	on_error, 2
	dx = x - round(x)  &  dy = y - round(y)
	psf = airy_pattern(data.x_size, data.y_size, $
					   data.x_size/2+dx, data.y_size/2+dy, data.sampling_factor)
	psf = psf / total(psf)
	ref_pix = [data.x_size, data.y_size] / 2
	return
end

; MULTIGAUSSIAN_PSF: Sum of 2D gaussians.
; All gaussians have same center and position angle.

PRO multigaussian_psf, x, y, data, psf, ref_pix

	on_error, 2
	dx = x - round(x)  &  dy = y - round(y)
	ref_pix = [data.x_size/2, data.y_size/2]
	npar = data.ngau * data.npar_gau + 1
	psf = fltarr(data.x_size, data.y_size)
	for g = 0, data.ngau-1 do begin
	   k = g * data.npar_gau
	   psf = temporary(psf) + data.p[k] * $
	   gaussian2d(data.x_size, data.y_size, ref_pix[0] + dx, ref_pix[1] + dy, $
	              data.p[k + 1], data.p[k + 2], data.p[npar - 1])
	endfor
	return
end

; MULTIGAUSSIAN_PSF: Sum of 2D gaussians.
; Each gaussian may have different center and position angle.

PRO multigaussian_psf_, x, y, data, psf, ref_pix

	on_error, 2
	psf = fltarr(data.x_size, data.y_size)
	dx = x - round(x)  &  dy = y - round(y)
	for g = 0, data.ngau-1 do begin
	   k = g * data.npar_gau
	   psf = temporary(psf) + data.p[k] * $
	   gaussian2d(data.x_size, data.y_size, $
	   data.x_size/2 + data.p[k + 3] + dx, data.y_size/2 + data.p[k + 4] + dy, $
	   data.p[k + 1], data.p[k + 2], data.p[k + 5])
	endfor
	ref_pix = [data.x_size/2, data.y_size/2]
	return
end

; POWERGAU_PSF: Gaussian with power profile.

PRO powergau_psf, x, y, data, psf, ref_pix

	on_error, 2
	psf = fltarr(data.x_size, data.y_size)
	dx = x - round(x)  &  dy = y - round(y)
	psf = powergau(data.x_size, data.y_size, data.x_size/2 + dx, data.y_size/2 + dy, $
	               data.p[0], data.p[1], data.p[2], data.p[3])
	psf = psf / total(psf)
	ref_pix = [data.x_size/2, data.y_size/2]
	return
end

; MULTIPOWERGAU_PSF: Sum of 2D gaussians with power profile.
; All gaussians have same center and position angle.

PRO multipowergau_psf, x, y, data, psf, ref_pix

	on_error, 2
	dx = x - round(x)  &  dy = y - round(y)
	ref_pix = [data.x_size/2, data.y_size/2]
	npar = data.ngau * data.npar_gau + 2
	phi = data.p[npar - 2]
	pow = data.p[npar - 1]
	psf = fltarr(data.x_size, data.y_size)
	for g = 0, data.ngau-1 do begin
	   k = g * data.npar_gau
	   psf = temporary(psf) + data.p[k] * $
	         powergau(data.x_size, data.y_size, ref_pix[0] + dx, ref_pix[1] + dy, $
	                  data.p[k + 1], data.p[k + 2], phi, pow)
	endfor
	psf = psf / total(psf)
	return
end

; TEMPLATE_PSF: template procedure for user-written PSF model procedures.

PRO template_psf, x, y, data, psf, ref_pix

	on_error, 2
	dx = x - round(x)  &  dy = y - round(y)
;	psf = "function call"
	psf = psf / total(psf)
	ref_pix = [data.x_size, data.y_size] / 2
	return
end



;;; The main routine.

FUNCTION image_model, x, y, f, x_size, y_size, psf, data, $
	                  REFERENCE_PIX = psf_ref_pix, _EXTRA = extra, $
	                  LX = lx, UX = ux, LY = ly, UY = uy, MODEL = image

	on_error, 2
	; Define output image model
	if  n_elements(image) eq 0  then  image = fltarr(x_size, y_size)
	nstar = n_elements(f)
	; Define PSF option ("fixed" or "space variant" or "model")
	fixed_psf = size52(psf, /N_DIM) eq 2
	space_var = not fixed_psf
	if  not fixed_psf  then $
	   space_var = size52(psf, /N_DIM) eq 3 and $
	               n_elements(lx) ne 0 and n_elements(ux) ne 0 and $
	               n_elements(ly) ne 0 and n_elements(uy) ne 0
	if  fixed_psf or space_var  then begin
	   psf_size = (size52(psf, /DIM))[0:1]
	   if  n_elements(psf_ref_pix) eq 0  then $
	      psf_ref_pix = get_max(psf[*, *, 0])
	endif else $
	case  strlowcase(psf)  of
	   "gaussian":	psf_pro = "gaussian_psf"
	   "airy":	psf_pro = "airy_psf"
	   "multigaussian": psf_pro = "multigaussian_psf"
	   "powergau": psf_pro = "powergau_psf"
	   "multipowergau": psf_pro = "multipowergau_psf"
	endcase
	; If there are some additional data defined, de-reference them
	if  ptr_valid(data)  then $
	   if  n_elements(*data) ne 0  then  aux = *data
	; Compute image model
	for  n = 0L, nstar - 1  do begin
	   ix = round(x[n])  &  iy = round(y[n])
	   if  fixed_psf  then $
	      psf_xy = image_shift(psf, x[n] - ix, y[n] - iy, _EXTRA = extra, aux) $
	   else  if  space_var  then $
	      psf_xy = image_shift(psf[*,*,pick_region(lx, ux, ly, uy, x[n], y[n])], $
	                           x[n] - ix, y[n] - iy, _EXTRA = extra) $
	   else $
	      call_procedure, psf_pro, x[n], y[n], aux, psf_xy, psf_ref_pix
	   add_overlap, image, f[n] * psf_xy, [ix, iy], psf_ref_pix
	endfor
	; Update additional data if necessary
	if  ptr_valid(data)  then $
	   if  n_elements(aux) ne 0  then  *data = aux
	return, image
end
