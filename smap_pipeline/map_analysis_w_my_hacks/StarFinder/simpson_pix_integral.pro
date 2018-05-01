; $Id: simpson_pix_integral.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	SIMPSON_PIX_INTEGRAL
;
; PURPOSE:
;	Resample a bivariate function defined on a rectangular domain by
;	integrating it over square regions. The integration is performed
;	by means of the Simpson rule.
;
; CATEGORY:
;	Mathematics. Integration routines.
;
; CALLING SEQUENCE:
;	Result = SIMPSON_PIX_INTEGRAL(F, Nsteps)
;
; INPUTS:
;	F:	2D array containing the values of the bivariate function
;
;	Nsteps:	Number of samples / size of integration interval. The following
;		relationship must exist between Nsteps and the size S of the input
;		array (we consider only one dimension for simplicity):
;		S = FIX(S / Nsteps) * Nsteps + e, where
;		e = 1 - Nsteps MOD 2.
;
; KEYWORD PARAMETERS:
;	PIX_SIZE:	Set this keyword to the physical value of the pixel size.
;		The default is PIX_SIZE = 1.
;
; OUTPUTS:
;	Result:	2D array containing the input function integrated
;		over square sub-regions.
;
; PROCEDURE:
;	Integrate the input function over square sub-domains of N*N points, where
;	N is related to the input parameter Nsteps by the following relation:
;	N = Nsteps, if Nsteps is odd
;	N = Nsteps + 1, if Nsteps is even.
;	The output array may be thought of as the input function integrated over
;	larger pixels. The value of each output pixel is the result of the integral
;	computed over N*N points. If Nsteps is even, two adjacent output pixels
;	will share one input point: this strategy is necessary when using Simpson
;	rule (at least its simplest version), which requires an odd number of
;	points per integration interval.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-



; SIMPSON_COEFF: auxiliary function, compute coefficients for Simpson rule.

FUNCTION simpson_coeff, n, l

	on_error, 2
	w = fltarr(n)  &  s = lindgen((n - 1)/2)
	w[2*s] = 2.  &  w[2*s+1] = 4.
	w[0] = 1.  &  w[n-1] = 1.
	w = w / 3. * float(l) / (n - 1)
	return, w
end


FUNCTION simpson_pix_integral, f, nsteps, PIX_SIZE = pixsize

	on_error, 2
	s = size52(f, /DIM)  &  nstep = round(nsteps)
	nx = s[0]/nstep  &  ny = s[1]/nstep  &  e = 1 - nstep mod 2
	if  s[0] ne (nx*nstep + e) or s[1] ne (ny*nstep + e)  then $
	   message, 'wrong array size'
	if  n_elements(pixsize) eq 0  then  pixsize = 1.
	npt = nstep + e  &  half = npt/2
	w = simpson_coeff(npt, pixsize)  &  wt = transpose(w)
	lx = half  &  ly = half  &  ux = s[0]-1-half  &  uy = s[1]-1-half
	intf = fltarr(nx, ny)
	i = -1L
	for  y = ly, uy, nstep  do begin
	   i = i + 1  &  j = -1L
	   for  x = lx, ux, nstep  do begin
	      j = j + 1
		  intf[j,i] = wt # f[x-half:x+half,y-half:y+half] # w
	   endfor
	endfor
	return, intf
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Matrix implementation (for nstep even). More beautiful but slower!

;   FUNCTION simp_coeff_array, nstep, nint
;
;	c = simpson_coeff(nstep+1, 1.)
;	a = fltarr( nstep*nint+1, nint )
;	for  n = 0, nint-1  do  a[n*nstep,n] = c
;	return, a
;  end

;   FUNCTION simpson_pix_integral, f, nstep

;	s = size52(f, /DIM)  &  nstep = round(nstep)
;	nx = s[0]/nstep  &  ny = s[1]/nstep
;	if  size52(f, /N_DIM) ne 2  or $
;		s[0] mod 2 eq 0 or s[1] mod 2 eq 0  or $
;		nstep mod 2 ne 0  or nx mod 2 ne 0 or ny mod 2 ne 0 $
;		then return, f
;	cx = simp_coeff_array( nstep, nx )
;	if  ny ne nx  then $
;	   cy = simp_coeff_array( nstep, ny )  else  cy = cx
;	return, transpose(cx) # f # cy
;  end



