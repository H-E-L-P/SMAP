; $Id: svpsf_extract.pro, v 1.0 Dec 2004 e.d. $
;
;+
; NAME:
;	SVPSF_EXTRACT
;
; PURPOSE:
;	Partition an image into a regular grid and derive a PSF estimate
;	for each sub-region by combining a set of point sources.
;	The PSF stars are found automatically as significant peaks above
;	a detection threshold or may be selected among a set supplied by
;	the user. The extracted set of PSFs may be used for the identification
;	and the analysis of the stars in the field by the procedure STARFINDER.
;	After this analysis, SVPSF_EXTRACT may be iterated using the information
;	retrieved by STARFINDER in order to improve the PSF estimate.
;
; CATEGORY:
;	Stellar field photometry.
;
; CALLING SEQUENCE:
;	SVPSF_EXTRACT, Image, Background, Threshold, Nx, Ny, Psfsize, Stack, Sv_par
;
;
; INPUTS:
;	Image:	2D image array
;
;	Background:	2D array, with the same size of Image, representing the
;		background contribution
;
;	Threshold:	Scalar, representing the detection threshold for the
;		presumed stellar peaks.
;
;	Nx, Ny:	Number of sub-regions along the X- and Y- axis respectively.
;		The higher these numbers, the finer the partitioning of the image.
;
;	Psfsize:	Scalar, giving the size of each local PSF.
;
; KEYWORD PARAMETERS:
;	SMOOTHBOX:	Size of box for median smoothing before search for local peaks.
;
;	SEARCHBOX:	Define the size of the box where each significant stellar
;		peak is the relative image maximum. The default is 9.
;
;	NSTAR:	Number of stars to combine for each sub-region. By definition,
;		the NSTAR brightest stars in a given sub-region are selected.
;
;	SATUR_LEV:	Upper linearity threshold of the detector. The stars whose
;		peak is higher than this level are not included in the PSF estimation.
;
;	NORMRAD:	The stars in a given sub-region, before being combined to
;		estimate the local PSF, are normalized to the same flux integrated
;		over a circular region of radius given by the value of this keyword.
;		The default is 3.
;
;	NOISE_LEVEL:	Each stellar image, selected for PSF estimation, may be
;		"cleaned" by masking the pixels below some threshold related to the
;		noise level. A useful choice of this parameter is a few times the
;		background noise standard deviation.
;
;	X_STARS, Y_STARS:	Input coordinates of stars to use for PSF extraction.
;		If set, no search for local peaks is performed. The stars to use for
;		the estimation of the PSF are selected among these positions, according
;		to the constraints set by the keywords NSTAR and SATUR_LEV.
;
;	FLUXES, STARS, PSF, SV_PAR:	When this procedure is used after a run of
;		STARFINDER, the available information on the image may be used
;		to improve the PSF estimate. The new stack of PSF estimates is obtained
;		by removing from each used star the contamination due to the surrounding
;		sources previously identified by STARFINDER. In order to apply this
;		option, the coordinates of the known stars must be supplied with the
;		keywords X_STARS, Y_STARS described above and, in addition, it is
;		necessary to supply also the fluxes, the image model, the previously
;		used PSF and corresponding SV_PAR. The input PSF may also be a 2D array,
;		if the previous analysis has been performed assuming a space-invariant
;		PSF; in this case, the input SV_PAR must not be supplied.
;
; OUTPUTS:
;	Stack:	3D array, of size  Psfsize * Psfsize * (Nx * Ny), with the
;		local PSF estimates.
;
;	Sv_par:	Structure to use as input parameter of the STARFINDER procedure
;		to make photometry with spatially variable PSF.
;		This structure has four fields: Lx, Ux, Ly, Uy. The value of each
;		field is a vector containing the lower (Lx and Ly) and upper (Ux and
;		Uy) bounds of the image sub-regions, along the X- and Y- directions.
;		The k-th PSF in the Stack corresponds to the sub-region bounded by
;		j*Nx + i, where i = k MOD Nx and j = k / Nx.
;
; RESTRICTIONS:
;
; PROCEDURE:
;	Find all the significant peaks in the image, defined as the relative
;	maxima above the detection threshold and not saturated. Each peak is the
;	relative maximum within a certain sub-region of size given by the keyword
;	parameter SEARCHBOX. The larger this figure, the lower the probability of
;	selecting two close stars, whose images might be spatially overlapped.
;	The image is then partitioned into a grid of sub-region and for each
;	region the local PSF is estimated considering the brightest stars among
;	the previously identified peaks.
;	If the keywords X_STARS, Y_STARS are supplied, then the peaks are chosen
;	among those defined by these keywords.
;	If ALL the keywords X_STARS, Y_STARS, FLUXES, STARS, PSF, SV_PAR are supplied,
;	then the peaks are chosen among those defined by X_STARS, Y_STARS and each
;	stellar image selected for PSF extraction is cleaned from the contamination
;	of the already known surrounding stars.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, December 2004.
;	Updates:
;-


PRO svpsf_extract, image, background, threshold, nx, ny, psfsize, $
	               stack, sv_par, $
	               SMOOTHBOX = smoothbox, SEARCHBOX = searchbox, NSTAR = nstar, $
	               SATUR_LEV = satur, NORMRAD = norm_rad, $
	               NOISE_LEVEL = noise, CCTHRESHOLD = ccthreshold, _EXTRA = extra, $
	               X_STARS = x_in, Y_STARS = y_in, FLUXES = f_in, $
	               STARS = stars_in, PSF = psf_in, SV_PAR = sv_par_in, $
	               SILENT = silent

	; Default values of keywords
	if n_elements(smoothbox) eq 0 then smoothbox = 3
;	if n_elements(searchbox) eq 0 then searchbox = 9
	if n_elements(nstar) eq 0 then nstar = 20
	if n_elements(norm_rad) eq 0 then norm_rad = 3

	; Find peaks
	s = size(image, /DIM)  &  sx = s[0]  &  sy = s[1]
	if n_elements(x_in) ne 0 and n_elements(y_in) ne 0 then begin
	   x = round(x_in) > 0 < (sx - 1)
	   y = round(y_in) > 0 < (sy - 1)
	endif else $
	   all_max, median_filter(image - background, smoothbox), x, y, BOX = searchbox
	f = image[x,y] - background[x,y]
	; Reject peaks below detection threshold
	w = where(f gt threshold)
	x = x[w]  &  y = y[w]  &  f = f[w]
	; Reject peaks above saturation threshold
	if  n_elements(satur) ne 0  then begin
	   w = where(image[x,y] lt satur)
	   x = x[w]  &  y = y[w]  &  f = f[w]
	endif

	; Define sub-regions
	array_partition, sx, nx, lx, ux
	array_partition, sy, ny, ly, uy
	sv_par = {lx: lx, ux: ux, ly: ly, uy: uy}
	if n_elements(sv_par_in) ne 0 then begin
	   sv_lx = sv_par_in.lx
	   sv_ux = sv_par_in.ux
	   sv_ly = sv_par_in.ly
	   sv_uy = sv_par_in.uy
	endif

	; For each sub-region, estimate PSF
	stack = fltarr(psfsize, psfsize, nx*ny)
	for  i = 0L, ny - 1  do for  j = 0L, nx - 1  do begin
	   if not keyword_set(silent) then begin
	      print, ""
	      print, "Estimating PSF in sub-region centered at ", $
	             (lx[j] + ux[j]) / 2, (ly[i] + uy[i]) / 2
	   endif
	   ; Find stars in sub-region
	   w = where(x gt lx[j] and x lt ux[j] and y gt ly[i] and y lt uy[i], n_ji)
	   if  n_ji eq 0  then $
	      message, "No star in sub-region " + $
	      strcompress(string(j), /REMOVE_ALL) + ", " + strcompress(string(i), /REMOVE_ALL)
	   x_ji = x[w] ;- lx[j]  &
	   y_ji = y[w] ;- ly[i]  &
	   f_ji = f[w]
	   ; Select brightest stars
	   n_ji = min([n_ji, nstar])
	   s = (reverse(sort(f_ji)))[0:n_ji-1]
	   x_ji = x_ji[s]  &  y_ji = y_ji[s]  &  f_ji = f_ji[s]
	   ; Estimate local PSF
	   psf_extract, x_ji, y_ji, dummy1, dummy2, $
	                image, psfsize, psf, fw, background, $
	                RAD_NORM = norm_rad, AVGTYPE = 2, $
	                NOISE_LEVEL = noise, /NONORM, _EXTRA = extra, $
	                X_STARS = x_in, Y_STARS = y_in, FLUXES = f_in, $
	                STARS = stars_in, PSF = psf_in, LX = sv_lx, UX = sv_ux, LY = sv_ly, UY = sv_uy
	   if not keyword_set(silent) then begin
	      print, "Used ", n_ji, " stars"
;	      print, "Stars magnitudes between ", -2.5*alog10(max(f_ji)), " and ", -2.5*alog10(min(f_ji))
	      print, "PSF FWHM = ", fw
	   endif
	   if n_elements(ccthreshold) ne 0 then psf = image_core(psf, X = psfsize/2, Y = psfsize/2, ccthreshold)
	   psf = psf / total(psf)
	   stack[*,*,i*nx+j] = psf
	endfor

	return
end
