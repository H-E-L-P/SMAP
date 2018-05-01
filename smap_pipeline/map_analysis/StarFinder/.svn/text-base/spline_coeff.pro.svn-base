; $Id: spline_coeff.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	SPLINE_COEFF
;
; PURPOSE:
;	Given a set of observations on a rectangular grid of points,
;	compute the coefficients of the 2D interpolating spline.
;
; CATEGORY:;
;	Mathematics. Interpolation
;
; CALLING SEQUENCE:
;	SPLINE_COEFF, Data, Coefficients, X_knots, Y_knots, $
;	              X, Y, Lo_x, Up_x, Lo_y, Up_y
;
; INPUTS:
;	Data:	2D array of data to be interpolated
;
; OPTIONAL INPUTS:
;	X, Y:	Vectors of abscissae and ordinates, in increasing order.
;		If not provided, a default grid is defined by the function
;		SAMPLING_GRID
;
;	Lo_x, Up_x, Lo_y, Up_y:	Lower and Upper, X- and Y- bounds of
;		rectangular domain where the observations are assigned.
;		The following conditions must be fulfilled:
;		Lo_x <= X <= Up_x, Lo_y <= Y <= Up_y
;
; KEYWORD PARAMETERS:
;	DEGREE:	Integer odd degree of spline. The default is DEGREE = 3.
;
; OUTPUTS:
;	Coefficients:	2D array of spline coefficients, with the same size
;		size as the input Data. Return a scalar if an error occurs or
;		if the input parameters are not acceptable.
;
;	X_knots, Y_knots:	Vectors of spline knots
;
; OPTIONAL OUTPUTS:
;	X, Y:	Vectors of abscissae and ordinates
;
;	Lo_x, Up_x, Lo_y, Up_y:	Lower and Upper, X- and Y- bounds of
;		rectangular domain where the observations are assigned
;
; RESTRICTIONS:
;	Apply only to 2D data.
;
; PROCEDURE:
;	Apply the procedures described in
;	Paul Dierckx, "Curve and surface fitting with splines",
;		Clarendon Press, Oxford (1995)
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;	Adapted in IDL from the sofware FITPACK, written in FORTRAN by P.Dierckx.
;	Complete references may be found in
;	Paul Dierckx, "Curve and surface fitting with splines",
;		Clarendon Press, Oxford (1995)
;-



;;; Auxiliary procedures/functions.

; SPL_ASCENDING: given the vector x of size n, check the condition
; x[i] < x[i+1], for i = 0, n - 2.

FUNCTION spl_ascending, x

	on_error, 2
	if  size52(x, /N_DIM) gt 1  then  return, 0B
	n = n_elements(x)
	check = (x lt shift(x, -1) )[0:n-2] and 1B
	return, min(check)
end

; SPL_CHECK_DATA: check input coordinates. If not set, define a default grid.

PRO spl_check_data, data, x, y, lo_x, up_x, lo_y, up_y, error

	on_error, 2
	error = size52(data, /N_DIM) ne 2  &  if  error  then  return
	sx = n_elements(x)  &  sy = n_elements(y)
	if  sx eq 0 or sy eq 0 or $
	    n_elements(lo_x) eq 0 or n_elements(up_x) eq 0 or $
	    n_elements(lo_y) eq 0 or n_elements(up_y) eq 0  then begin
	   s = size52(data, /DIM)  &  sx = s[0]  &  sy = s[1]
	   x = sampling_grid(sx, 1, lo_x, up_x)
	   y = sampling_grid(sy, 1, lo_y, up_y)
	endif else $
	   error = min(x) lt lo_x  or  max(x) gt up_x  or $
	   		   min(y) lt lo_y  or  max(y) gt up_y  or $
	   		   not spl_ascending(x) or not spl_ascending(y)
	return
end

; SPL_COMPUTE_KNOTS: 1-D set of knots for spline interpolation of odd degree.

FUNCTION spl_compute_knots, x, a, b, degree, error

	on_error, 2
	n = n_elements(x)
	error = n le degree or degree mod 2 eq 0  &  if  error  then  return, 0
	k = fltarr(n + degree + 1)
	k[0:degree] = a  &  k[n:n+degree] = b	; additional knots
	if  n gt degree + 1  then $				; interior knots
	   k[degree + 1:n - 1] = x[degree/2 + 1:degree/2 + n - degree - 1]
	return, k
end

; SPL_GIVPAR: compute parameters for a Givens transformation.

PRO spl_givpar, piv, r, c, s

	on_error, 2
	abs_piv = abs(piv)
	if  abs_piv ge r  then $
	   aux_r = abs_piv * sqrt(1 + (r/piv)^2)  else $
	   aux_r = r * sqrt(1 + (piv/r)^2)
	c = r / aux_r  &  s = piv / aux_r  &  r = aux_r
	return
end

; SPL_G_ROTATE: rotate vectors v1 and v2 applying a Givens rotation (c,s).

PRO spl_g_rotate, c, s, v1, v2

	on_error, 2
	u1 = v1  &  u2 = v2
	v1 = c * u1 - s * u2
	v2 = s * u1 + c * u2
	return
end

; SPL_GIV_TRANSFORM: rotate observation array a and data array d
; applying Givens rotations.

PRO spl_giv_transform, a, npt, d, degree

	on_error, 2
	n_points = n_elements(npt)
	aux_a = a  &  a = a - a
	aux_d = transpose(d)  &  d = aux_d - aux_d
	for  n = 0L, n_points - 1  do begin
	   this = npt[n]  &  va = aux_a[*,n]  &  vd = aux_d[*,n]
	   for  i = 0, degree  do begin
	      piv = va[i]
	      if  piv ne 0  then begin
	         temp = a[0,this]  &  spl_givpar, piv, temp, c, s   &  a[0,this] = temp
	         temp = d[*,this]  &  spl_g_rotate, c, s, vd, temp  &  d[*,this] = temp
	         if  i lt degree  then begin
	            temp = va[i+1:degree]  &  temp1 = a[1:degree-i,this]
	            spl_g_rotate, c, s, temp, temp1
	            va[i+1:degree] = temp  &  a[1:degree-i,this] = temp1
	         endif
	      endif
	      this = this + 1
	   endfor
	endfor
	return
end

; SPL_BACK_SUB: solve the set of m linear algebraic systems  a x = b,
; where a is an n*n upper triangular matrix of bandwidth w,
; x and b are m*n arrays.

FUNCTION spl_back_sub, a, b, w

	on_error, 2
	s = size52(b, /DIM)  &  m = s[0]  &  n = s[1]
	x = fltarr(m, n + w - 1)
	for  i = n - 1, 0, -1  do $
	   x[*,i] = (b[*,i] -  x[*,i+1:i+w-1] # a[1:w-1,i]) / a[0,i]
	return, x[*,0:n-1]
end

; SPL_SOLVE_SYS: compute the coefficients of the interpolating spline
; as the solution of the linear system  (ay) c (ax)' = data

FUNCTION spl_solve_sys, ax, nptx, ay, npty, data, degree

	on_error, 2
	d = data
	; Reduce (ax) to upper triangular form. Apply the same transformation
	; to the data.
	spl_giv_transform, ax, nptx, d, degree
	; Reduce (ay) to upper triangular form. Apply the same transformation
	; to the data.
	spl_giv_transform, ay, npty, d, degree
	; Solve the linear system  (ay) c (ax)' = d
	temp = spl_back_sub(ay, d, degree + 1)
	c = spl_back_sub(ax, transpose(temp), degree + 1)
	return, transpose(c)	; c is (nx) * (ny)
end

;;; The main routine.

PRO spline_coeff, data, DEGREE = degree, coefficients, $
   				  x_knots, y_knots, x, y, lo_x, up_x, lo_y, up_y

	on_error, 2
	coefficients = 0
	spl_check_data, data, x, y, lo_x, up_x, lo_y, up_y, error
	if  error  then  return
	if  n_elements(degree) eq 0  then  degree = 3
	if  degree mod 2 eq 0  then  degree = degree + 1
	x_knots = spl_compute_knots(x, lo_x, up_x, degree, error)
	if  error  then  return
	y_knots = spl_compute_knots(y, lo_y, up_y, degree, error)
	if  error  then  return
	b_splines, x, x_knots, degree, ax, nptx
	b_splines, y, y_knots, degree, ay, npty			; delta_t = 2.6
	coefficients = spl_solve_sys(ax, nptx, ay, npty, data, degree)
													; delta_t = 10.6
	return
end
