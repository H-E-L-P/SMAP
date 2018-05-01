; $Id: display_image.pro, v 1.2 Sep 2001 e.d. $
;
;+
; NAME:
;	DISPLAY_IMAGE
;
; PURPOSE:
;	Display a 2D image, according to a pre-fixed or default set of options.
;
; CATEGORY:
;	Data visualization.
;
; CALLING SEQUENCE:
;	DISPLAY_IMAGE, Image [, wnum]
;
; INPUTS:
;	Image: 2D data array
;
; OPTIONAL INPUT PARAMETERS:
;	Wnum: number of existing graphic window
;
; KEYWORD PARAMETERS:
;	OPTIONS: structure containing display options,
;		as defined by the function DEFAULT_DISPLAY_OPT
;
;	/MODIFY_OPT: set this keyword to modify display options
;
; OUTPUTS:
;	Graphic output on the window identified by the parameter wnum
;
; OPTIONAL OUTPUT PARAMETERS:
;	Wnum: number of new graphic window, if undefined on input
;
;	OPTIONS: display options
;
; SIDE EFFECTS:
;	1) Use WSET to activate the graphic window identified by the parameter wnum.
;	2) If wnum is undefined, open a new graphic window. The size of this window
;	is proportional to the image size and however smaller than the screen size.
;	3) If /MODIFY_OPT is set, call XDisplayOpt to modify the display options.
;
; RESTRICTIONS:
;	If the parameter Wnum is a window number < 32 but the corresponding
;	window does not exist (or has been deleted), the new window size is set
;	by IDL and may not fit the image x- and y size. If Wnum >=32 and the
;	corresponding window does not exists, an error occurs.
;
; PROCEDURE:
;	Activate the window identified by Wnum (or create a new one) and
;	display the input image, using the display options passed with the
;	keyword OPTIONS or the default ones defined by DEFAULT_DISPLAY_OPT.
;	NOTE on intensity stretch. Possible value are:
;	square: display  Image^2
;	linear: display  Image
;	square root: display  sqrt(Image > 0)
;	logarithm: display  alog10(Image > max(Image)*1e-15)
;
; MODIFICATION HISTORY:
;	Written by: Emiliano Diolaiti, August 1999.
;	Updates:
;	1) Replaced TVScl with scaling + TV;
;	   Corrected stretch with reverse option.
;     (Emiliano Diolaiti, November 1999).
;	2) Replaced again TV with TVScl
;     (Emiliano Diolaiti, September 2001).
;-

PRO display_image, image, wnum, OPTIONS = options, MODIFY_OPT = modify_opt

	on_error, 2
	; define graphic window if necessary
	if  n_elements(wnum) eq 0  then begin
	   smax = get_screen_size()  &  smax = min(smax)*2./3.
	   s = size52(image, /DIM)  &  s = s / max(s) * smax  &  s = round(s)
	   window, /FREE, XSIZE = s[0], YSIZE = s[1]  &  wnum = !D.window
	endif
	; define default display options if necessary
	if  n_elements(options) eq 0  then $
	   options = default_display_opt(image)
	; process image according to display options
	data = image
	data = (data > options.range[0]) < options.range[1]
	w = where(data gt options.chop, count)
	if  count ne 0  then  data[w] = min(data)
	case options.stretch of
	   'square': data = data^2
	   'linear': ; do nothing
	   'square root': data = sqrt(data > 0)
	   'logarithm': data = alog10(data > max(data) * 1e-15)
	endcase
	if  options.reverse  then  data = -data
	; display image
	wset, wnum  &  erase, wnum  &  loadct, options.color_table, /SILENT
	tvscl, congrid(data, !D.x_size, !D.y_size)
	if  keyword_set(modify_opt)  then $
	   options = xdisplayopt(image, wnum, OPTIONS = options, /NODISPLAY)
	return
end
