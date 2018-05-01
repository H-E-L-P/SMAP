; $Id: shifted_templates.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	SHIFTED_TEMPLATES
;
; PURPOSE:
;	Shift a template image by an integer multiple of pre-fixed sub-pixel
;	shift. The basic fractional offset is defined by a magnification factor
;	(> 1). All the possible fractional shifts whose absolute value is < 1/2
;	pixel are applied to the input array and the resulting shifted templates
;	are collected into an output stack.
;
; CATEGORY:
;	Image processing, spatial transformations
;
; CALLING SEQUENCE:
;	SHIFTED_TEMPLATES, Template, Magfac, Templates, Dx, Dy
;
; INPUTS:
;	Template:	image to be shifted
;
;	Magfac:	magnification factor (integer, > 1)
;
;	EDGE:	width (in pixel units) of a frame around the shifted template
;		which is excluded after applying the shift. This is useful to
;		reject possible edge effects due to interpolation.
;		All the shifted templates will have x- and y- size given by
;		(sx - 2*edge) and (sy - 2*edge) respectively.
;		The default is EDGE = 0.
;
;	_EXTRA: keyword parameters of IMAGE_SHIFT (see file 'image_shift.pro')
;
; OUTPUTS:
;	Templates:	stack of shifted templates
;
;	Dx, Dy:	1D arrays of fractional shifts. Templates[*,*,n] is the
;		input template shifted by (Dx[n], Dy[n])
;
; RESTRICTIONS:
;	The shift of the input template is performed by IMAGE_SHIFT applying
;	some interpolation techniques. Undersampled data should not be shifted
;	by interpolation. For details, see IMAGE_SHIFT
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999
;-

PRO shifted_templates, template, magfac, _EXTRA = extra, EDGE = edge, $
					   templates, dx, dy

	on_error, 2
	mag = round(magfac) > 1
	; define shifts
	nshift = mag + 1L - mag mod 2  &  one = make_array(nshift, VALUE = 1)
	dx = ((findgen(nshift) - nshift/2) / mag) # one
	dy = one # ((findgen(nshift) - nshift/2) / mag)
	nshift = nshift^2  &  dx = reform(dx, nshift)  &  dy = reform(dy, nshift)
	; define stack of templates
	if  n_elements(edge) eq 0  then  edge = 0
	s = size52(template, /DIM) - 2*edge  &  l = [edge, edge]  &  u = l + s - 1
	templates = make_array(s[0], s[1], nshift, TYPE = size52(template, /TYPE))
	; compute shifted templates
	for  n = 0L, nshift - 1  do begin
	   template_n = image_shift(template, dx[n], dy[n], aux, _EXTRA = extra)
	   templates[*,*,n] = template_n[l[0]:u[0],l[1]:u[1]]
	endfor
	return
end
