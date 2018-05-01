; $Id: resample.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	RESAMPLE
;
; PURPOSE:
;	Resample a bivariate function defined on a rectangular domain by
;	integrating it over square regions, defining pixels of the resampled
;	array. The integration is performed by means of IDL REBIN or with
;	the Simpson rule. The size of the input array need not be an integer
;	multiple of the resampling factor.
;	This routine is particularly useful to integrate over pixels an
;	oversampled PSF.
;
; CATEGORY:
;	Mathematics. Integration routines.
;
; CALLING SEQUENCE:
;	Result = RESAMPLE(F, N_sub)
;
; INPUTS:
;	F:	Sampled bivariate function to be resampled
;
;	N_sub:	Resampling factor
;
; KEYWORD PARAMETERS:
;	X_SHIFT, Y_SHIFT:	X- and Y- fractional offsets, expressed in pixel
;		units after resampling. The default is X_SHIFT = Y_SHIFT = 0.
;
;	X_REF, Y_REF:	Use this keyword to provide the position of a pixel
;		in the input array to be used as a reference for centering.
;		In general the pixel of interest is the array maximum.
;		Suppose that X_SHIFT = 0. and Y_SHIFT = 0. and that the keywords
;		X_REF and Y_REF are not supplied: the maximum of the resampled array
;		may appear "not well centered", i.e. it may present a visual
;		off-centering different from X_SHIFT, Y_SHIFT.
;		In practice all the relative positions in the output array are always
;		preserved. X_REF and Y_REF should be provided when the visual
;		appearance of the resampled array is important.
;
;	SIMP:	Set this keyword to a nonzero value to perform the integration
;		by means of the Simpson rule instead of REBIN (neighbor averaging).
;		The results is much more accurate.
;
; OUTPUTS:
;	Result:	Resampled array
;
; RESTRICTIONS:
;	1) When the input array has to be shifted to realize the desired offset,
;	missing values at the array edges are actually replaced with 0s.
;	2) The actual off-centering of the resampled array can only be an
;	integer multiple of the smallest "theoretical" shift, given by
;	(1. / N_sub).
;	In practice the input offsets X_SHIFT and Y_SHIFT are rounded.
;	For example, if N_sub = 4 and X_SHIFT = 0.4, the actual shift will be
;	0.5; if N_sub = 5 and X_SHIFT = 0.5, the actual shift will be 0.6
;	pixels of the resampled array.
;	Of course this rounding effect is more and more negligible as the
;	oversampling factor of the input array and the resampling factor
;	N_sub increase.
;
; PROCEDURE:
;	Suitably resize the input array, according to the initial size and the
;	resampling factor. Apply the desired shift and then resample with REBIN
;	or SIMPSON_PIX_INTEGRAL.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

FUNCTION resample, f, n_sub, X_SHIFT = x_shift, Y_SHIFT = y_shift, $
				   SIMP = simp, X_REF = x_ref, Y_REF = y_ref

	on_error, 2
	nsub = round(n_sub)
	if  n_elements(x_shift) eq 0  then  x_shift = 0.
	if  n_elements(y_shift) eq 0  then  y_shift = 0.
	xshift = round(x_shift*nsub)  &  yshift = round(y_shift*nsub)
	; Shift the array in order to:
	;	1) apply the requested shift (x_shift, y_shift)
	;	2) prevent spurious off-centering due to resampling
	;	   when a reference pixel is provided
	size_f = size52(f, /DIM)  &  e = 1 - nsub mod 2
	if  n_elements(x_ref) * n_elements(y_ref) ne 0  then begin
	   xr = round(x_ref)  &  yr = round(y_ref)
	   xshift = xshift + (xr/n_sub) * nsub + (nsub + e)/2 - xr
	   yshift = yshift + (yr/n_sub) * nsub + (nsub + e)/2 - yr
	endif
	a = extend_shift(f, xshift, yshift)
	; Resize the array, if necessary
	if  not keyword_set(simp)  then  e = 0
	size_a = (size_f / nsub) * nsub + e
	if  size_a[0] lt size_f[0]  then $
	   a = a[0:size_a[0]-1,*]  else $
	if  size_a[0] gt size_f[0]  then $
	   a = extend_array(a, size_a[0], size_f[1], /NO_OFF)
	if  size_a[1] lt size_f[1]  then $
	   a = a[*,0:size_a[1]-1]  else $
	if  size_a[1] gt size_f[1]  then $
	   a = extend_array(a, size_a[0], size_a[1], /NO_OFF)
	; Integrate over pixels
	if  keyword_set(simp)  then $
	   a = simpson_pix_integral(a, nsub)  else $
	   a = rebin(a, size_f[0]/nsub, size_f[1]/nsub)
	return, a
end
