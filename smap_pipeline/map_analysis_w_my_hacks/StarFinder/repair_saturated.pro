; $Id: repair_saturated.pro, v 1.1 Jul 2000 e.d. $
;
;+
; NAME:
;	REPAIR_SATURATED
;
; PURPOSE:
;	Repair saturated stars in an image, by replacing their core with a
;	template representing an estimate of the PSF. Accurate positioning
;	of the template onto each star is performed by correlation
;	maximization, whereas the scaling factor is computed by fitting the
;	wings of the saturated source.
;	This procedure gives acceptable results when the size of the
;	saturated region of a given star does not exceed the diameter of
;	the central core of the PSF.
;
; CATEGORY:
;	Signal processing. Stellar photometry.
;
; CALLING SEQUENCE:
;	REPAIR_SATURATED, Image, Clean_image, Background, $
;	                  Psf, Psf_fwhm, X, Y, Upper_lev
;
; INPUTS:
;	Image:	Stellar field image containing saturated stars.
;
;	Clean_image:	Stellar field image, after subtraction of non-saturated
;		sources. It may coincide with Image, if there are no important
;		sources other than the saturated ones.
;
;	Background:	2D array, with the same size as Image, containing an
;		estimate of the background emission.
;
;	Psf:	2D array, containing an estimate of the PSF, to be used as a
;		template to repair the core of the saturated stars.
;
;	Psf_fwhm:	FWHM of the PSF.
;
;	X, Y:	X- and Y- coordinates of saturated stars.
;
;	Upper_lev:	Scalar, representing the presumed saturation threshold.
;
; KEYWORD PARAMETERS:
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
;		Notice that the box size for repair is limited by the size of the
;		Psf array.
;
;	MAG_FAC:	Integer representing the fractional sub-pixel step for
;		accurate positioning of the PSF estimate on the core of a saturated
;		star. This "magnification factor" is also used to optimize the
;		correlation.
;		The default is MAG_FAC = 2, corresponding to a positioning accuracy
;		of 1/2 pixel.
;
;	INTERP_TYPE:	Use this keyword to choose an interpolation technique
;		for the PSF fractional shift when MAG_FAC > 1. See IMAGE_SHIFT in
;		the file "image_shift.pro" for more details. The default is to
;		use cubic convolution interpolation.
;
; OUTPUTS:
;	Image:	Image array with repaired saturated stars.
;
;	Clean_image:	Input Clean_image with repaired saturated stars.
;
;	X, Y:	Coordinates of saturated stars after repair.
;
; SIDE EFFECTS:
;	The input variables Image, Clean_image, X and Y are overwritten.
;
; RESTRICTIONS:
;	It is assumed that the saturated stars are separated by a distance
;	at least (N_WIDTH * Width), where N_WIDTH is the input keyword described
;	above and Width is the maximum diameter in pixels of a saturated core.
;
; PROCEDURE:
;	The input image is temporarily cleaned from the contamination of
;	secondary sources and background emission. Then the saturated sources
;	are isolated and repaired with a scaled replica of the PSF template.
;	Accurate positioning of the template onto each star is performed by
;	correlation maximization, whereas the scaling factor is computed by
;	fitting the wings of the saturated source. Of course the saturated core
;	is masked when computing the correlation coefficient and the scaling
;	factor.
;	When all the saturated sources specified on input have been repaired,
;	the image is restored adding the previously subtracted stars and
;	the background emission.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August-September 1999.
;	Updates:
;	1) Fixed bug on correlation maximization in module MATCH_REPLACE
;	   (Emiliano Diolaiti, July 2000).
;-




;;; Auxiliary routines.

; MATCH_REPLACE: repair the core of a saturated star,
; given an unsaturated template.

PRO match_replace, image, template, x, y, box, search_box, $
				   upper_surface, MAG_FAC = mag_fac, _EXTRA = extra

	on_error, 2
	; Define saturated star, template, upper surface for matching
	star = sub_array(image, box + 2*(search_box/2), REF = [x, y], $
					 LX = lx, UX = ux, LY = ly, UY = uy)
	star_ref = [x - lx, y - ly]  &  temp_ref = get_max(template)
	upper = upper_surface[lx:ux,ly:uy]
	if  n_elements(mag_fac) eq 0  then  mag_fac = 2
	mag = round(mag_fac > 1)
	if  mag gt 1  then $
	   shifted_templates, template, mag, _EXTRA = extra, templates, dx, dy
	; Find optimal position by cross-correlation maximization
	w = where(star ge upper, count)
	subs_to_coord, w, (size52(star, /DIM))[0], x_core, y_core
	correlate_max, star, template, star_ref[0], star_ref[1], search_box, $
   				   XT = temp_ref[0], YT = temp_ref[1], $
   				   X_BAD = x_core, Y_BAD = y_core, $
   				   TEMPLATES = templates, DX = dx, DY = dy, c, x, y
	; Compute optimal scaling factor
	if  mag gt 1  then begin
	   dx_opt = round(mag * (x - round(x)))
	   dy_opt = round(mag * (y - round(y)))
	   w = where(round(mag * dx) eq dx_opt and round(mag * dy) eq dy_opt)
	   temp = templates[*,*,w]
	endif else  temp = template
	extract_overlap, star, temp, round([x, y]), temp_ref, $
					 star, temp, lxs, uxs, lys, uys
   	x = x + lx  &  y = y + ly
	lx = lx + lxs  &  ly = ly + lys  &  upper = upper[lxs:uxs,lys:uys]
	w = where(star lt upper)
	scale = total(temp[w]*star[w])/total(temp[w]^2)
	; Repair image
	w = where(star ge upper)
	star[w] = scale * temp[w]
	image[lx,ly] = star
	return
end



;;; The main routine.

PRO repair_saturated, image, clean_image, background, psf, psf_fwhm, x, y, $
					  upper_lev, N_FWHM_MATCH = n_fwhm, N_WIDTH = n_width, $
					  _EXTRA = extra

	on_error, 2
	; The image to use to repair saturated stars must be:
	; 1) background removed
	; 2) cleaned from secondary stars around saturated ones.
	sec_sources = image - clean_image
	clean_image = temporary(clean_image) - background
	upper_surf = upper_lev - background - sec_sources
	; Match and repair saturated stars in clean_image
	n_satur = n_elements(x)
	if  n_elements(n_fwhm) eq 0  then  n_fwhm = 1
	if  n_elements(n_width) eq 0  then  n_width = 3
	for  n = 0L, n_satur - 1  do begin
	   width = peak_width(clean_image, MAG = 1, X = x[n], Y = y[n], $
	   					  ABS_THRESH = upper_surf)
	   box = round(n_width * width) < min(size52(psf, /DIM))
	   search_box = round(n_fwhm * psf_fwhm)
	   x_n = x[n]  &  y_n = y[n]
	   match_replace, clean_image, psf, x_n, y_n, box, search_box, upper_surf, _EXTRA = extra
	   x[n] = x_n  &  y[n] = y_n
	endfor
	; Define clean_image = input image with repaired stars - secondary sources
	clean_image = temporary(clean_image) + background
	; Define image = original image with repaired stars
	image = clean_image + sec_sources
	return
end
