; $Id: correlate_max.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	CORRELATE_MAX
;
; PURPOSE:
;	Given an object, a reference template and an estimate of the object's
;	center, obtain a better estimate of the object's position by maximizing
;	its correlation with the template.
;
; CATEGORY:
;	Image statistics.
;
; CALLING SEQUENCE:
;	CORRELATE_MAX, Image, Template, X_i, Y_i, Search_box, $
;   			   Max_correl, X_opt, Y_opt
;
; INPUTS:
;	Image:	2D array containing the object to be correlated
;
;	Template:	reference template to compute the correlation
;
;	X_i, Y_i: x- and y- coordinates of the object's presumed center
;
;	Search_box:	width of box centered at (X-i, Y_i) inside which the template
;		must be ideally moved to find the best match with the object
;
; KEYWORD PARAMETERS:
;	XT, YT:	reference pixel in Template, i.e. pixel to be ideally superposed
;		to the presumed object's center. The default is the central pixel
;		of the Template array
;
;	X_BAD, Y_BAD: x- and y- coordinates of bad pixels, i.e. unreliable data
;		points to be excluded from the computation. Default: no bad pixels
;
;	TEMPLATES: stack of sub-pixel-shifted templates, used to maximize the
;		correlation with sub-pixel accuracy positioning
;
;	DX, DY:	1D arrays of fractional shifts corresponding to the shifted
;		templates in the Templates stack
;
; OUTPUTS:
;	Max_correl:	maximum correlation coefficient
;
;	X_opt, Y_opt:	x- and y- coordinates of the object position's
;		guess yielding the largest correlation coefficient
;
; RESTRICTIONS:
;	1) The correlation between the object and the template is computed
;	on the overlap area of the two arrays, which is found after ideally
;	superposing their reference pixels. The template must be 'moved'
;	within the range specified by the input parameter Search_box in order
;	to optimize the correlation coefficient. To ensure that the area of
;	the overlapping region be the same for all possible shifts, the Image
;	array must be larger than the Template array, by an amount depending
;	on the width of the search box. The CORRELATE_MAX procedure doesn't
;	check this condition! If for some reason the overlapping region is
;	less than 3 x 3 pixels in size, the correlation is set to 0.
;	2) Sub-pixel positioning is performed only if all the input keywords
;	TEMPLATES, DX and DY are defined. These parameters may be defined by
;	means of the function SHIFT_TEMPLATES in the file 'shift_templates.pro'
;
; PROCEDURE:
;	Find the intersection between Template and Image after superposing
;	their reference pixels and compute the correlation coefficient by
;	means of CORRELATION_COEFF. The operation is performed for all possible
;	shifts within the range specified by the parameter Search_box; the
;	position yielding the largest match is taken as the best estimate of
;	the object's center.
;	The shift + match + correlate procedure may then be repeated for sub-
;	pixel shifts around the optimal position previously found, in order
;	to determine a better estimate of the object's center.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

PRO correlate_max, image, template, x_i, y_i, search_box, $
   				   XT = x_t, YT = y_t, X_BAD = xb, Y_BAD = yb, $
   				   TEMPLATES = templates, DX = dx, DY = dy, $
   				   max_correl, x_opt, y_opt

	on_error, 2
	; optimize correlation applying integer shifts,
	; within the range specified by search_box
	lo_xy = [0, 0]  &  up_xy = size52(image, /DIM) - 1  &  min_size = 3
	h = round(search_box > 1) / 2  &  nshift = (2*h + 1)^2
	x = lonarr(nshift)  &  y = lonarr(nshift)  &  correl = fltarr(nshift)
	xi = round(x_i)  &  yi = round(y_i)
	if  n_elements(x_t) eq 0 or n_elements(y_t) eq 0  then begin
	   s = size52(template, /DIM)  &  xt = s[0]/2  &  yt = s[1]/2
	endif else begin
	   xt = round(x_t)  &  yt = round(y_t)
	endelse
	n = 0L
	for  j = -h, +h  do  for  i = -h, +h  do begin
	   x[n] = (xi + j) > lo_xy[0] < up_xy[0]
	   y[n] = (yi + i) > lo_xy[1] < up_xy[1]
	   extract_overlap, image, template, [x[n], y[n]], [xt, yt], ima, tem, $
						lxi, uxi, lyi, uyi
	   sx = uxi - lxi + 1  &  sy = uyi - lyi + 1
	   if  min([sx, sy]) ge min_size  then begin
	      if  n_elements(xb) ne 0 and n_elements(yb) ne 0  then begin
	         wb = where(xb ge lxi and xb le uxi and yb ge lyi and yb le uyi, nb)
	         if  nb ne 0  then $
	            wb = coord_to_subs(xb[wb] - lxi, yb[wb] - lyi, sx)
	      endif
	      correl[n] = correlation_coeff(ima, tem, EXCLUDE = wb)
	   endif
	   n = n + 1
	endfor
	max_correl = max(correl, w)  &  x_opt = x[w]  &  y_opt = y[w]
	; optimize correlation applying sub-pixel shifts
	mag = n_elements(templates) * n_elements(dx) * n_elements(dy) ne 0
	if  mag  then  mag = size52(templates, /N_DIM) eq 3
	if  mag  then begin
	nshift = (size52(templates, /DIM))[2]
	x = x_opt + dx  &  y = y_opt + dy  &  correl = fltarr(nshift)
	for  n = 0L, nshift - 1  do begin
	   correlate_max, image, templates[*,*,n], x_opt, y_opt, 1, $
	   				  XT = xt, YT = yt, X_BAD = xb, Y_BAD = yb, correl_n
	   correl[n] = correl_n
	endfor
	max_correl = max(correl, w)  &  x_opt = x[w]  &  y_opt = y[w]
	endif
	return
end
