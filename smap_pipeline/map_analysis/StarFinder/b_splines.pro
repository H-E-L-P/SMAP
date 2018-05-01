; $Id: b_splines.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	B_SPLINES
;
; PURPOSE:
;	Compute the 1-D observation matrix to determine the coefficients
;	of an interpolating spline in the B-splines representation.
;
; CATEGORY:
;	Mathematics. Interpolation.
;
; CALLING SEQUENCE:
;	B_SPLINES, Points, Knots, Degree, A, Npt
;
; INPUTS:
;	Points:	1D vector of observation points
;
;	Knots:	Knots of the spline function
;
;	Degree:	Degree of the spline (integer, odd)
;
; KEYWORD PARAMETERS:
;	BOUNDS:	Option used by SPLINE_INTERP (see the function SPLINE_INTERP
;		in the file 'splie_interp.pro') to compute the observation matrix.
;
;	FULL:	The observation array has a band structure of width (Degree+1).
;		Normally only the nonzero entries are returned. Set the keyword
;		FULL to return the full observation matrix.
;
; OUTPUTS:
;	A:	Observation matrix of size (Degree+1)*N, where N is the number
;		of observation points. If the keyword FULL is set, A is returned
;		as a N*N array.
;
;	Npt:	1D Vector of subscripts defining the relative position of
;			the observation points with respect to the knots of the spline
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


;;; Auxiliary function.

; SPL_BSPL: given a set of knots k and a point x, with k[s] <= x < k[s+1],
; compute the (d + 1) non-zero B-splines of degree d at x, using
; the recurrence relations of de Boor and Cox.

FUNCTION spl_bspl, k, d, x, s

	on_error, 2
	v = fltarr(d + 1)  &  aux = fltarr(d)
	v[0] = 1
	for  j = 1, d  do begin
	   aux[0:j-1] = v[0:j-1]  &  v[0] = 0
	   for  i = 1, j  do begin
	      si = s + i  &  sj = si - j
	      f = aux[i-1] / (k[si] - k[sj])
	      v[i-1] = v[i-1] + f * (k[si] - x)
	      v[i] = f * (x - k[sj])
	   endfor
	endfor
	return, v
end

;;; The main routine.

PRO b_splines, points, knots, degree, a, npt, $
   			   BOUNDS = chk_bounds, FULL = full

	on_error, 2
	npoints = n_elements(points)  &  nk = n_elements(knots)
	if  keyword_set(full)  then $
	   a = fltarr(nk - degree - 1, npoints)  else $
	   a = fltarr(degree + 1, npoints)
	npt = lonarr(npoints)
	l = degree  &  r = l + 1  &  num = 0
	for  n = 0L, npoints - 1  do begin
	   point = points[n]
	   if  keyword_set(chk_bounds)  then $
	      point = (point > knots[degree]) < knots[nk-degree-1]
	   while  point ge knots[r] and l lt nk - degree - 2  do begin
	      l = r  &  r = l + 1  &  num = num + 1
	   endwhile
	   npt[n] = num
	   ; knots[degree + npt[n]] <= points[n] < knots[degree + npt[n] + 1]
	   b = spl_bspl(knots, degree, point, l)
	   if  keyword_set(full)  then  a[num,n] = b  else  a[*,n] = b
	endfor
	return
end
