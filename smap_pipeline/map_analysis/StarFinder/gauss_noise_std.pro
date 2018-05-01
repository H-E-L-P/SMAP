; $Id: gaussian_noise_std.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	GAUSSIAN_NOISE_STD
;
; PURPOSE:
;	Given a 2D image, compute standard deviation of normally distributed
;	noise by histogram fitting.
;
; CATEGORY:
;	Statistics.
;
; CALLING SEQUENCE:
;	GAUSSIAN_NOISE_STD, Data, Mode, Std, H, V, Vmean, Hfit
;
; INPUTS:
;	Data: 2D data array
;
; KEYWORD PARAMETERS:
;	PATCH: 	Box size for median smoothing of the data. The median smoothed
;		image is subtracted from the input data, to remove all the features
;		which would cause an anomalous spread of the histogram
;
;	POINT_FRAC:	The histogram is computed on a subset of the input data.
;		The number of points in the sample is given by the number of
;		elements in the image / the value set by this keyword. The default
;		is 1, i.e. all the input data are used.
;
;	NOSUB:	Set this keyword to a nonzero value to skip median subtraction
;
;	N_STD:	After extracting a sample from the original image, a further
;		restriction is applied to exclude the so-called "outliers", i.e.
;		pixels whose value is too distant from the sample median.
;		The keyword N_STD fixes a tolerance for outliers identification.
;		The rejection condition for a given element called "value" is
;		abs("value" - "sample median") >
;				N_STD * "standard deviation of (sample - sample median)"
;		The default is 3
;
;	HIST_MINSIZE:	The data histogram is optimized in order to have at
;		least a minimum number of bins in the histogram's Half Width at
;		Half Maximum (HWHM). The minimum number of bins in one histogram's
;		HWHM is fixed by this keyword. The default is 5.
;		Notice this keyword is just a hint and may be overridden.
;
;	HIST_MAXSIZE:	The data histogram is fitted with a gaussian curve, to
;		estimate the noise standard deviation. In practice only the central
;		part of the histogram is considered, around the mode. The size of
;		the useful section of the histogram may be defined in units of the
;		approximate histogram's FWHM by means of this keyword.
;		The default is 5.
;		Notice this keyword is just a hint and may be overridden.
;
;	MAXIT:	The gaussian fit to the histogram is performed in the range
;		[MIN(V), Mode + Std], because the right tail of the histogram may
;		be contaminated by the photon noise due to spatially non-uniform
;		sources. The first fit is performed starting from an initial guess
;		of the quantities Mode and Std and is then repeated iteratively,
;		until the estimated Std converges to a stable value or a maximum
;		number of iterations reached. Use the keyword MAXIT to fix the
;		maximum number of iterations. The default is 10, even though 2-3
;		are generally sufficient
;
;	TOL:	Tolerance for the convergence of Std in the iteration of the
;		gaussian fit. The default is 0.01
;
;	NTERMS_FIT:	Same as the input keyword NTERMS of the library routine
;		GAUSSFIT. The default is 3, i.e. the histogram is fitted with a
;		pure gaussian curve (having 3 parameters: a normalization constant,
;		the center and the standard deviation). Set NTERMS = 4 to add a
;		constant terms to the fitting curve, NTERMS = 5 to add a linear
;		background and NTERMS = 6 to add a quadratic background.
;
; OUTPUTS:
;	Mode:	Histogram mode, determined as the center of the best fit
;		gaussian. If median subtraction is performed, Mode should be near 0.
;		When median subtraction is skipped, Mode represents the mean level
;		of the image gaussian noise
;
;	Std:	Standard deviation of gaussian noise, estimated as the "sigma"
;		of the best fit gaussian. A negative value indicates an error
;
;	H:	Histogram (1D array)
;
;	V:	1D array of data values, corresponding to the center of each
;		histogram bin
;
;	Vmean: 1D array of data values, corresponding to the mean of the
;		pixels entering each histogram bin
;
;	Hfit:	Best fit gaussian. The right tail of the gaussian is truncated
;		at 1 Std from the histogram mode
;
; OPTIONAL OUTPUTS:
;	COEFF:	Vector of best fit model coefficients, returned by GAUSSFIT.
;
; RESTRICTIONS:
;	1) This procedure assumes the image noise is normally distributed.
;	Normally distributed noise includes for instance read-out-noise, but
;	also the photon (Poisson) noise associated to nearly spatially uniform
;	signals contributing to the image (e.g. dark current, smooth nebulosities
;	in the sky, etc.). A Poisson-distributed 2-D signal may be considered as
;	distributed according to a unique gaussian probability law if:
;	a) the signal is not too faint
;	b) the signal is approximately spatially uniform
;	If the image contains both gaussian and photon noise due to non-smooth
;	sources (e.g. stars), the noise standard deviation will be of course
;	over-estimated.
;
;	2) A maximum number of bins is fixed when computing the histogram.
;	This threshold is equal to number of elements in the data sub-sample,
;	which corresponds to having (in the mean) one value per histogram bin!
;	In practice the computation of the histogram starts with a minimum
;	number of bins. The bin number is then iteratively increased (by a
;	factor of 2 at every iteration) until there is a minimum number of bins
;	(fixed by the keyword HIST_MINSIZE) in one histogram's HWHM or the
;	maximum number of bins is reached. In the latter case the former
;	condition may not be fulfilled.
;
; PROCEDURE:
;	Extract a sample of spatially uniformly distributed image pixels,
;	remove outliers and compute the sample histogram. When median removal
;	is performed, the histogram bin corresponding to the data values around
;	0 may be strongly over-populated, due to the large number of small
;	positive values produced by median subtraction. This bin is thus
;	replaced with the mean of the two adjacent ones. The data histogram is
;	then fit with a gaussian curve, whose sigma represents the standard
;	deviation of the image gaussian noise. The fit is performed by means of
;	the library routine GAUSSFIT.
;
; EXAMPLE:
;	Compute the mean and standard deviation of an array of normally
;	distributed pesudo random numbers.
;		Noise = RANDOMN(Seed, 512, 512)
;		GAUSSIAN_NOISE_STD, Noise, Mode, Std, H, V, Vmean, Hfit
;		PRINT, Mode, Std
;		PLOT, PSYM = 10, V, H  &  OPLOT, LINESTYLE = 1, V, Hfit
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-





;;; Auxiliary procedures / functions.

; OPT_HISTO: auxiliary procedure. Compute the optimal histogram
; by adjusting iteratively the bin size so that the histogram FWHM
; contains at least a minimum number of bins.

; INPUT
;	data: data to compute histogram
;	[HIST_MINSIZE = ]: min. no. of bins in histogram HWHM (default = 5)
;	[HIST_MAXSIZE = ]: size of output histogram in FWHM units (default = 5)
;	[/REPLACE_MAX]: replace most populated histogram bin, which may be
;		an artifact (e.g. if the original data are median-subtracted)
; OUTPUT
;	h: histogram
;	x: center of each bin
;	xmean: vector containing mean data value for each histogram bin

; REMOVE_SPIKE: remove spike (corresponding to 0-bin).

FUNCTION remove_spike, h

	on_error, 2
	m = max(h, w)
	if  w gt 0 and w lt n_elements(h)-1  then $
	   h[w] = round(( h[w-1]+h[w+1] ) / 2.)
	return, h
end

PRO opt_histo, data, HIST_MINSIZE = min_size, HIST_MAXSIZE = max_size, $
   			   REPLACE_MAX = replace, h, v, vmean

	on_error, 2
	if  n_elements(min_size) eq 0  then  min_size = 5	; bin units
	if  n_elements(max_size) eq 0  then  max_size = 5	; FWHM units
	max_nbin = n_elements(data)	; max total no. of histogram bins
	range = float(max([ abs(min(data)), abs(max(data)) ]))
	dmin = -range  &  dmax = +range  &  range = 2*range  &  nbin = 1L
	; compute the histogram, adjusting iteratively the bin size
	repeat begin
	   nbin = nbin * 2  &  bin = range / nbin
	   histo, data, dmin, dmax, bin, h, v, vmean
	   if  keyword_set(replace)  then  h = remove_spike(h)
	   l = histo_hwhm(h, -1, mode_sub)
	   r = histo_hwhm(h, +1, mode_sub)
	endrep until min([l, r]) ge min_size or 2*nbin ge max_nbin
	; reduce histogram size
	l = (mode_sub - max_size * l) > 0
	r = (mode_sub + max_size * r) < (n_elements(h) - 1)
	h = h[l:r]  &  v = v[l:r]  &  vmean = vmean[l:r]
	return
end

;;; The main routine.

PRO gauss_noise_std, data, PATCH = patch_, POINT_FRAC = point_frac, $
					 NOSUB = nosub, N_STD = n_std, _EXTRA = extra,  $
					 MAXIT = maxit, TOL = tol, NTERMS_FIT = nterms, $
					 mode, std, h, v, vmean, hfit, COEFF = c

	on_error, 2
	; remove median
	if  n_elements(patch_) eq 0  then  patch = 3  else  patch = patch_ > 3
	d = data
	if  not keyword_set(nosub)  then  d = d - median_filter(d, patch)
	; extract a sample of uniformly distributed points
	if  n_elements(point_frac) eq 0  then  point_frac = 1
	if  point_frac ne 1  then begin
	   npoints = round(n_elements(d) / float(point_frac))
	   s = float(size52(d, /DIM))
	   nx = round(sqrt( s[0]/s[1] * npoints ))
	   ny = round(sqrt( s[1]/s[0] * npoints ))
	   x = round(sampling_grid(nx, s[0] / nx))
	   y = round(sampling_grid(ny, s[1] / ny))
	   x = x # (lonarr(ny) + 1)  &  y = (lonarr(nx) + 1) #  y
	   d = d[x, y]
	endif
	; extract data within suitable intensity range
	if  n_elements(n_std) eq 0  then  n_std = 3
	d_med = median(d)  &  m = moment(d - d_med, SDEV = std)
	w = where(abs(d - d_med) lt n_std*std)  &  d = d[w]
	; compute histogram with suitable bin size
	opt_histo, d, h, v, vmean, _EXTRA = extra, $
			   REPLACE_MAX = not keyword_set(no_sub) and 1B
	; iterative gaussian fit
	if  n_elements(maxit) eq 0  then  maxit = 10
	if  n_elements(tol) eq 0  then  tol = 1e-2
	if  n_elements(nterms) eq 0  then  nterms = 3
	it = 0  &  converging = 0B
	m = max(h, mode)  &  mode = v[mode]
	std = max([ mode-min(v), max(v)-mode ])
	while  not converging and it lt maxit  do begin
;	   w = where(v le mode + std)	; this line to fit left-tail
	   w = lindgen(n_elements(h))	; this line to fit all histogram
	   hfit = gaussfit(vmean[w], h[w], NTERMS = nterms, c)
	   std0 = std  &  mode = c[1]  &  std = abs(c[2])
	   converging = abs((std - std0) / std0) lt tol
	   it = it + 1
	endwhile
	if  not converging  then  std = -1
	return
end
