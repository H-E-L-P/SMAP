; $Id: centroider.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	CENTROIDER
;
; PURPOSE:
;	Sub-pixel centering of an image by iterative fractional shift.
;
; CATEGORY:
;	Signal processing. Interpolation.
;
; CALLING SEQUENCE:
;	Result = CENTROIDER(Image)
;
; INPUTS:
;	Image:	2D array to be centered
;
; KEYWORD PARAMETERS:
;	XC, YC:	Reference pixel for centroid computation. The default is the
;		Image maximum.
;
;	CENTROID_BOX:	Width of box centered at the reference pixel used to
;		compute the Image centroid. The default is equal to the (rounded)
;		FWHM of the peak at the reference position. A minimum value of
;		3 pixels is fixed.
;
;	CENTROID_TOL:	This keyword defines the stopping condition for the
;		iterative algorithm, i.e. the maximum allowed off-centering of
;		the Image centroid. The default is  CENTROID_TOL = 0.05 pixels.
;
;	CENTROID_IT:	Use this keyword to fix the maximum number of
;		iterations. The default is 20, even though centering is achieved
;		in fewer iterations (tipically 5).
;
;	INTERP_TYPE:	Set this keyword to a string identifying one of the
;		interpolation techiques supported by IMAGE_SHIFT. For more details
;		see the function IMAGE_SHIFT in the file 'image_shift.pro'.
;
; OUTPUTS:
;	Result:	Centered array
;
; OPTIONAL OUTPUTS:
;	XSHIFT, YSHIFT:	Use these output keywords to retrieve the total shifts
;		performed to center the Image, starting from the reference position.
;		The quantities XC + X_SHIFT and YC + Y_SHIFT represent and estimate
;		of the true centroid location in the original Image
;
; RESTRICTIONS:
;	The Image is shifted by interpolation techniques, which are suited to
;	well sampled data. This method should not applied to undersampled
;	images.
;
; PROCEDURE:
;	Compute first the offset of the image centroid and shift the array in
;	order to cancel the off-centering. The operation is iterated until the
;	offset is smaller than a pre-fixed tolerance.
;	The centroid is computed on a small box centered at a reference position,
;	which generally coincides with the image maximum.
;	The image shift is performed by interpolation.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;	Adapted from an algorithm described in:
;	Christou J.C., Bonaccini D., "Technical Report ESO VLT",
;		Doc. No. GEN-TRE-ESO-11620-1261  (1996)
;-

FUNCTION centroider, image, XC = xc, YC = yc, CENTROID_BOX = box, $
   					 CENTROID_TOL = tol, CENTROID_IT = maxit,     $
   					 XSHIFT = xs, YSHIFT = ys, _EXTRA = extra

	on_error, 2
	; reference pixel
	if  n_elements(xc) * n_elements(yc) eq 0  then begin
	   m = get_max(image)  &  x = m[0]  &  y = m[1]
	endif else begin
	   x = xc  &  y = yc
	endelse
	x = round(x)  &  y = round(y)
	; box size to compute centroid
	if  n_elements(box) eq 0  then $
	   b = fwhm(image, X = x, Y = y)  else  b = box
	b = round(b)
	if  n_elements(b) eq 1  then  b = [b, b]
	b = b < size52(image, /DIM)  &  b = b + 1 - b mod 2  &  b = b > 3
	; other parameters
	if  n_elements(tol)   eq 0  then  tol = 0.05
	if  n_elements(maxit) eq 0  then  maxit = 20
	imag = image  &  it = 0  &  xs = 0.  &  ys = 0.
	; iteration
	repeat begin
	   it = it + 1
	   ; compute centroid
	   c = centroid(sub_array(imag, b[0], b[1], $
	   					REFERENCE = [x, y], LX = lx, LY = ly))
	   dx = x - lx - c[0]  &  dy = y - ly - c[1]
	   ; shift image to center centroid
	   xs = xs + dx   &  ys = ys + dy
	   imag = image_shift(imag, xs, ys, _EXTRA = extra, shift_data)
	   convergence = abs(dx) lt tol and abs(dy) lt tol
	endrep until  convergence or it eq maxit
	if  it eq maxit and not convergence  then begin
	   imag = image  &  xs = 0.  &  ys = 0.
	endif
	return, imag
end
