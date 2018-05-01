; $Id: crosses.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	CROSSES
;
; PURPOSE:
;	Mark interesting points in an image with crosses.
;
; CATEGORY:
;	Data visualization.
;
; CALLING SEQUENCE:
;	CROSSES, image, x, y
;
; INPUTS:
;	Image:	2D data array
;	X, Y: x- and y- coordinates of points to mark (with a '+' sign)
;
; KEYWORD PARAMETERS:
;	X2, Y2:	x- and y- coordinates of another set of interesting points,
;		to be marked with a different symbol ('x' sign)
;
;	EXISTING: Set this keyword when the image is already displayed on
;		some graphic window
;
;	_EXTRA: Input keywords of DISPLAY_IMAGE (namely display options).
;		Neglected if the keyword EXISTING is set.
;
;	DARK: Set this keyword to specify that the color of the marks must
;		be the IDL default background color (dark marks). The default is
;		to use the highest color number available (bright marks).
;
;	DEVICE:	Set this keyword to a nonzero value to specify that the input
;		coordinates are device coordinates. The default is data coordinates.
;
;	SYMSIZE:	Keyword of the IDL routine PLOTS: specifies symbol size.
;
; OUTPUTS:
;	Graphic output.
;
; SIDE EFFECTS:
;	Open a new graphic window if the image is displayed for the first time.
;
; RESTRICTIONS:
;	1) If the keyword EXISTING is set, the procedure assumes that the image
;	is already display in the currently active window.
;	2) If the keyword EXISTING is not set, the image is displayed on a new
;	window, using the default display options (see the function
;	DEFAULT_DISPLAY_OPT in the file 'default_display_opt.pro') if the input
;	keyword OPTIONS is not used.
;	3) The color of the marks (bright by default, dark if the keyword DARK
;	is set) is correct assuming the IDL system variables !D and !P have not
;	been previously modified.
;
; PROCEDURE:
;	Display the image on a new window if the keyword EXISTING is not set,
;	using the display options defined by the keyword OPTIONS or the default
;	options if the keyword is not defined. Then put a sign on each point.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

PRO crosses, image, x, y, X2 = x2, Y2 = y2, EXISTING = existing, $
	         _EXTRA = extra, DARK = dark, DEVICE = device, SYMSIZE = symsize

	on_error, 2
	if  not keyword_set(existing)  then $
	   display_image, image, _EXTRA = extra
	if  keyword_set(dark)  then $
	   color = !P.background  else $
	   color = !D.n_colors - 1
	if  keyword_set(device)  then  scale = [1, 1]  else $
	   scale = [!D.x_size, !D.y_size] / float(size52(image, /DIM) - 1)
	plots, x * scale[0], y * scale[1], PSYM = 1, /DEVICE, $
	       COLOR = color, SYMSIZE = symsize
	if  n_elements(x2) * n_elements(y2) ne 0  then $
	   plots, x2 * scale[0], y2 * scale[1], PSYM = 7, /DEVICE, $
	          COLOR = color, SYMSIZE = symsize
	return
end
