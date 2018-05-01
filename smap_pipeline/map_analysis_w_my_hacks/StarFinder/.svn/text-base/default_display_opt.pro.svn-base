; $Id: default_display_opt.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	DEFAULT_DISPLAY_OPT
;
; PURPOSE:
;	Define default options to display a 2D image.
;
; CATEGORY:
;	Data visualization
;
; CALLING SEQUENCE:
;	Result = DEFAULT_DISPLAY_OPT(Image)
;
; INPUTS:
;	Image: 2D image
;
; OUTPUTS:
;	Return structure of default options. The structure fields are:
;	range: 2-components vector, containing the min. and max. level to display
;		( default = [min(image), max(image)] )
;	chop: chopping threshold; all the gray-levels above this threshold will be
;		replaced with the min. level. The default value is greater than the
;		image maximum, i.e. no chopping
;	reverse: boolean, true if the gray-levels must be reversed (default false)
;	stretch: string, representing the stretch to be used. Possible values are
;		'square', 'linear' (default), 'square root', 'logarithm'
;	color_table: long integer, representing the color table number (default 0)
;
; MODIFICATION HISTORY:
;	Written by: Emiliano Diolaiti, August 1999.
;-

FUNCTION default_display_opt, image

	range = [min(image), max(image)]
	chop = 1e6  &  while  chop le range[1]  do  chop = 10 * chop
	return, { range: range, chop: chop, reverse: 0B, $
			  stretch: 'linear', color_table: 0L }
end
