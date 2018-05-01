; $Id: histo.pro, v 1.1 Jan 2000 e.d. $
;
;+
; NAME:
;	HISTO
;
; PURPOSE:
;	Compute histogram of input data and array of data values
;	corresponding to histogram bins.
;
; CATEGORY:
;	Mathematics. Statistics.
;
; CALLING SEQUENCE:
;	HISTO, Data, Dmin, Dmax, Bin, H, X, Xmean
;
; INPUTS:
;	Data: Data to compute histogram (any number of IDL dimensions)
;
;	Dmin, Dmax:	Min. and max. data value to be considered in the histogram
;
;	Bin:	Size of histogram bin
;
; OUTPUTS:
;	H: Histogram
;
;	X: 1D array of data values corresponding to the center of each bin
;
;	Xmean: 1D array corresponding to the mean of the data values entering
;		each histogram bin
;
; PROCEDURE:
;	Compute the histogram with the IDL intrinsic function HISTOGRAM, using
;	the input options specified by the parameters Dmin, Dmax, Bin. All
;	the computations are performed in floating-point arithmetics.
;	Then compute arrays of values corresponding to each histogram bin,
;	useful for plots, fitting, etc.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;	Updates:
;	1) long integer loop variable (Emiliano Diolaiti, January 2000).
;-

PRO histo, data, dmin, dmax, bin, h, x, xmean

	on_error, 2
	; compute histogram
	h = histogram(float(data), BINSIZE = float(bin), MIN = float(dmin), $
				  MAX = float(dmax), REVERSE_INDICES = r)
	; compute center of each bin
	range = float(dmax - dmin)  &  nbin = long(range/bin) + 1
	x = findgen(nbin) * bin + dmin + bin/2.
	; compute mean data value for each bin
	n_el = n_elements(h)  &  xmean = fltarr(n_el)
	for  n = 0L, n_el - 1  do begin
	   lo = r[n]  &  up = r[n+1] - 1
	   if  lo lt up  then $
	      xmean[n] = mean(data[r[lo:up]])  else  xmean[n] = x[n]
	endfor
   return
end
