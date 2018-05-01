; $Id: halo_smooth.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	HALO_SMOOTH
;
; PURPOSE:
;	Smooth the halo of a stellar image by means of a variable box size
;	median filtering technique. The width of the smoothing box for a
;	given pixel increases with the radial distance of the pixel itself
;	from the center of the image.
;	This routine may be used to smooth the halo of a PSF image,
;	exctracted from a stellar field.
;
; CATEGORY:
;	Signal processing.
;
; CALLING SEQUENCE:
;	Result = HALO_SMOOTH(Image, R0)
;
; INPUTS:
;	Image:	2D image to smooth
;
;	R0:	Scalar, indicates the radius of the region around the Image
;		maximum which is not processed by median filtering.
;		In practice the halo smoothing starts from this distance.
;
; KEYWORD PARAMETERS:
;	R_WIDTH:	Use this keyword to specify the radial width of the
;		smoothing box at a distance of 2*R0 from the Image maximum.
;		The default is  R_WIDTH = FLOAT(R0)/2 pixels.
;
;	A_WIDTH:	Use this keyword to specify the azimuthal width of the
;		smoothing box at a distance of 2*R0 from the Image maximum.
;		The default is  A_WIDTH = !pi/8 rad
;
;	R_EXP, A_EXP:	The radial and azimuthal widths of the smoothing box
;		increase as power-law functions of the radial distance from the
;		Image maximum. The keywords R_EXP and A_EXP allow the user to
;		specify the power degree.
;		The default values are  R_EXP = 2, A_EXP = 3.
;
;	PAD_0:	Set this keyword to a nonzero value to have the Image padded
;		with a frame of 0s before smoothing. The use of this strategy may
;		prevent the onset of undesired edge effects and force the smoothed
;		image to approach 0 at the edge. The default is no padding.
;
;	SHOW:	Set this keyword to a nonzero value to show the shape and
;		area of the smoothing box for each radial distance from the Image
;		maximum, along the main diagonal of the input array.
;		In practice a binary image is displayed, equal to 1 on the support
;		of the smoothing box. The image is displayed on the currently
;		active graphic window.
;
; OUTPUTS:
;	Result:	Halo-smoothed image, with the same size as the input array
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-



;;; Auxiliary functions.

;RAD_WIDTH:	compute radial width of smoothing neighborhood for a given pixel.

FUNCTION rad_width, r, r0, rad_coeff, rad_exp

	return, rad_coeff * (r / (2.* r0)) ^rad_exp
end

;AZI_WIDTH:	compute azimuthal width of smoothing neighborhood for a given pixel.

FUNCTION azi_width, r, r0, azi_coeff, azi_exp

	return, azi_coeff * (r / (2.* r0)) ^azi_exp
end


;;; The main routine.

FUNCTION halo_smooth, image, r0, PAD_0 = pad_0, SHOW = show, $
					  R_WIDTH = rw, A_WIDTH = aw, R_EXP = re, A_EXP = ae

	on_error, 2
	if  size52(image, /N_DIM) ne 2  then  return, image
	siz = size52(image, /DIM)  &  sx = siz[0]  &  sy = siz[1]
	smooth_image = image
	; Pad with 0s?
	if  keyword_set(pad_0)  then begin
	   siz = 2 * siz  &  sx = siz[0]  &  sy = siz[1]
	   smooth_image = extend_array(smooth_image, sx, sy)
	endif
	saved_image = smooth_image
	; Define default width of filtering box at 2*r0 from the center
	if  n_elements(rw) eq 0  then  rw = r0 / 2.
	if  n_elements(aw) eq 0  then  aw = !pi / 8
	if  n_elements(re) eq 0  then  re = 2
	if  n_elements(ae) eq 0  then  ae = 3
	rad_coeff = rw  &  azi_coeff = 2 * r0 * aw
	; For each pixel, compute distance from the center and azimuthal angle
	m = get_max(smooth_image)  &  x0 = m[0]  &  y0 = m[1]
	x = findgen(sx) # make_array(sy, VALUE = 1)
	y = make_array(sx, VALUE = 1) # findgen(sy)
	r_distance = round(distance(x0, y0, x, y))
	azi = angle(x0, y0, x, y)
	outer_rad = max(r_distance)
	; Define parameters for display
	if  keyword_set(show)  then begin
	   dsiz = min([!D.x_vsize, !D.y_vsize]) * siz/max(siz)
	   dxy = m[1] - m[0]
	endif
	; Iterate on annular regions of increasing radius, from inner to outer
	for  r = r0, outer_rad  do begin
	   ; Identify pixels at this distance from the center
	   wr = where(r_distance eq r, nr)
	   if  nr ne 0  then begin
	      ; Compute radial and azimuthal width of smoothing box
		  w = rad_width(r, r0, rad_coeff, re)
		  l = azi_width(r, r0, azi_coeff, ae) / r
		  rad_range = abs(r_distance - r) le w/2
		  for  n = 0, nr - 1  do begin
		  	 ; Define the smoothing 'box' of each pixel
			 delta_azi = abs(azi - azi[wr[n]])
			 azi_range = delta_azi le l/2  or  2*!pi - delta_azi le l/2
		     neigh = where((rad_range and azi_range) and 1B)
		     if  keyword_set(show)  then $
		     if  y[wr[n]] eq (x[wr[n]] + dxy) and x[wr[n]] gt x0  then begin
			    b = bytarr(sx, sy)  &  b[neigh] = 1
			    tvscl, congrid(b, dsiz[0], dsiz[1])
		     endif
			 ; Compute median of box
			 smooth_image[x[wr[n]], y[wr[n]]] = median(saved_image[neigh], /EVEN)
		  endfor
	   endif
	endfor
	if  keyword_set(pad_0)  then $
	   smooth_image = sub_array(smooth_image, sx/2, sy/2)
	return, smooth_image
end