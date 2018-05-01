; $Id: sampling_grid.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	SAMPLING_GRID
;
; PURPOSE:
;	Define a set of sampling points along an axis.
;
; CATEGORY:
;	Mathematics. Interpolation.
;
; CALLING SEQUENCE:
;	Result = SAMPLING_GRID(N, Dx, Lx, Ux)
;
; INPUTS:
;	N:	Number of sampling points
;
;	Dx:	Sampling step
;
; OUTPUTS:
;	Result:	N-components vector of equispaced points
;
; OPTIONAL OUTPUTS:
;	Lx, Ux:	Lower and upper bounds of sampling region, useful for
;		spline interpolation
;
; PROCEDURE:
;	Call FINDGEN to define a set of pixels along a 1D axis. Each pixel
;	has a physical step size and corresponds to a sampling point, ideally
;	located in the middle of the pixel itself.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

FUNCTION sampling_grid, n, dx, lx, ux

	on_error, 2
	x = findgen(n) * dx + dx / 2.
	lx = min(x) - dx/2.  &  ux = max(x) + dx/2.
	return, x
end


