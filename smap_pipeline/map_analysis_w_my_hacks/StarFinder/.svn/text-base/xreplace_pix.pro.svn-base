; $Id: xreplace_pix, v 1.1 Apr 2000 e.d. $
;
;+
; NAME:
;	XREPLACE_PIX
;
; PURPOSE:
;	Widget interface for the REPLACE_PIX function.
;	Replace given pixels in a 2D array with the median of a suitable
;	neighborhood. The pixels to replace are excluded from the computation
;	of the local median.
;
; CATEGORY:
;	Widgets. Signal processing.
;
; CALLING SEQUENCE:
;	Result = XREPLACE_PIX(Array, X_pix, Y_pix)
;
; INPUTS:
;	Array: 2D data array, containing pixels to replace
;
; OPTIONAL INPUTS:
;	X_pix, Y_pix:	Coordinates of pixels to replace
;
; KEYWORD PARAMETERS:
;	PATH:	Initial path for file browsing when reading bad pixels mask.
;		If the argument of the keyword is a named variable, its value
;		is overwritten.
;
;	WNUM:	Number of existing graphic window.
;
;	DISPLAY_OPT:	Display options for Array.
;
;	GROUP: Xreplace_Pix group leader.
;
;	UVALUE: Xreplace_Pix user value.
;
; OUTPUTS:
;	Result:	Array with replaced pixels
;
; OPTIONAL OUTPUTS:
;	X_pix, Y_pix:	Coordinates of pixels to replace
;
;	WNUM: window number of new window, if undefined on input.
;
;	DISPLAY_OPT:	Display options, as defined and/or modified by
;		Xreplace_Pix.
;
; SIDE EFFECTS:
;	1) Initiates the XMANAGER if it is not already running.
;	2) Call DISPLAY_IMAGE to activate the graphic window identified by Wnum.
;	3) Create a new graphic window if not defined on input (see DISPLAY_IMAGE
;	and XDISPLAYOPT for details).
;	4) Any image displayed on the window Wnum is overwritten first by the
;	input array and then by the result of the processing.
;
; RESTRICTIONS:
;	1) The pixels to replace must be marked by 0s in a binary mask which
;	is read from a FITS file by Xreplace_Pix.
;	2) The Help menu opens the file
;	'/starfinder/xreplace_pix_help.txt'.
;
; PROCEDURE:
;	Create and register the widget as a modal widget.
;	Then let the user define and or/modify the smoothing options and apply
;	them to the input array. As a new processed array is obtained, it is
;	displayed on the graphic window.
;
; MODIFICATION HISTORY:
;	Written by: Emiliano Diolaiti, September 1999
;	Updates:
;	1) Enhanced error handling in event-handler
;	   (Emiliano Diolaiti, April 2000).
;-

; XREPLACE_PIX_EVENT: XReplace_Pix event handler.

PRO xreplace_pix_event, event

	catch, error
	if  error ne 0  then begin
	   msg = dialog_message(/ERROR, !err_string)
	   widget_control, event.id, SET_UVALUE = data, /NO_COPY
	   return
	endif
	widget_control, event.id, GET_UVALUE = data, /NO_COPY
	event_type = strlowcase(event.tag)
	case  event_type  of
	   'replace': begin
	      if  n_elements(*(*data).x_pix) ne 0  then begin
	         form = cw_form(['0,INTEGER,3,LABEL_LEFT=Enter box size to compute median,TAG=box', $
	         				 '2, BUTTON,OK,QUIT,NO_RELEASE'], TITLE = 'Box size to compute median')
	         box = form.box > 3
	         widget_control, /HOURGLASS
	         *(*data).array_out = replace_pix(*(*data).array_in, $
	         					  *(*data).x_pix, *(*data).y_pix, BOXSIZE = box)
	         display_image, *(*data).array_out, *(*data).wnum, OPT = *(*data).display_opt
	      endif else $
	         msg = dialog_message(/ERROR, 'Please read bad pixels mask.')
	      end
	   'load': begin
	      file = dialog_pickfile(/READ, FILTER = '*.fits', $
	      						 PATH = *(*data).path, GET_PATH = path)
	      if  file ne ''  then begin
	         widget_control, /HOURGLASS
	         *(*data).path = path
	         fits_read, file, mask
	         w = where(mask eq 0, count)
	         if  count ne 0  then $
	            subs_to_coord, w, (size52(mask, /DIM))[0], *(*data).x_pix, *(*data).y_pix
	      endif
	      end
	   'display': *(*data).display_opt = $
	      xdisplayopt(*(*data).array_out, *(*data).wnum, /NODISPLAY, $
	      			  OPTIONS = *(*data).display_opt, GROUP = event.top)
	   'help': $
	      xdispfile, file_name('starfinder', 'xreplace_pix_help.txt'), $
	      		 TITLE = 'XReplace_Pix help', /MODAL
	   'exit': begin
	      widget_control, event.id, SET_UVALUE = data, /NO_COPY
	      widget_control, event.top, /DESTROY
	      end
	   else:
	endcase
	if  event_type ne 'exit'  then $
	   widget_control, event.id, SET_UVALUE = data, /NO_COPY
	return
end

; XREPLACE_PIX_DEF: define data structure.

FUNCTION xreplace_pix_def, array, x_pix, y_pix, path, wnum, display_opt

	return, {array_in: ptr_new(array), path: ptr_new(path, /NO_COPY), $
			 wnum: ptr_new(wnum, /NO_COPY), $
			 display_opt: ptr_new(display_opt, /NO_COPY), $
			 array_out: ptr_new(array), $
			 x_pix: ptr_new(x_pix, /NO_COPY), y_pix: ptr_new(y_pix, /NO_COPY)}
end

; XREPLACE_PIX_DEL: de-reference output data.

PRO xreplace_pix_del, data, path, wnum, display_opt, array_out, x_pix, y_pix

	if  n_elements(*(*data).path) ne 0  then  path = *(*data).path
	if  n_elements(*(*data).wnum) ne 0  then  wnum = *(*data).wnum
	if  n_elements(*(*data).display_opt) ne 0  then  display_opt = *(*data).display_opt
	if  n_elements(*(*data).array_out) ne 0  then  array_out = *(*data).array_out
	if  n_elements(*(*data).x_pix) ne 0  then begin
	   x_pix = *(*data).x_pix  &  y_pix = *(*data).y_pix
	endif
	ptr_free, (*data).array_in, (*data).path, (*data).wnum, (*data).display_opt, $
			  (*data).array_out, (*data).x_pix, (*data).y_pix
	ptr_free, data
	return
end

; XREPLACE_PIX: XReplace_Pix widget definition module.

FUNCTION xreplace_pix, array, x_pix, y_pix, PATH = path, $
					   WNUM = wnum, DISPLAY_OPT = display_opt, $
					   GROUP = group, UVALUE = uvalue

	on_error, 2
	if  n_elements(array) eq 0  then begin
	   mgs = dialog_message(/ERROR, 'Missing data array.')
	   return, array
	endif
	; Display image and define display options
	display_image, array, wnum, OPTIONS = display_opt
	; Create group leader if necessary
	if  n_elements(group) eq 0  then $
	   group_id = widget_base()  else  group_id = group
	; Create modal base
	if  n_elements(uvalue) eq 0  then  uvalue = 0B
	base = widget_base(TITLE = 'XReplace_Pix', /MODAL, UVALUE = uvalue, $
					   GROUP_LEADER = group_id)
	; Define form
	desc = [ $
	'1, BASE,,ROW', $
	'1, BASE,,COLUMN', $
	'0, BUTTON,Read bad pixels,NO_RELEASE,TAG=load', $
	'2, BUTTON,Replace,NO_RELEASE,TAG=replace', $
	'1, BASE,,COLUMN', $
	'2, BUTTON,Display Options,NO_RELEASE,TAG=display', $
	'2, BASE,,', $
	'1, BASE,,ROW', $
	'1, BASE,,ROW', $
	'0, BUTTON,Help,NO_RELEASE,TAG=help', $
	'2, BUTTON,Exit,QUIT,NO_RELEASE,TAG=exit', $
	'2, BASE,,']
	form = cw_form(base, desc, /COLUMN)
	; Define pointer to auxiliary/output data
	data = ptr_new(xreplace_pix_def(array, x_pix, y_pix, path, wnum, display_opt), /NO_COPY)
	widget_control, form, SET_UVALUE = data
	; Realize, register, etc.
	widget_control, base, /REALIZE
	xmanager, 'xreplace_pix', base, EVENT_HANDLER = 'xreplace_pix_event'
	; De-reference output data
	xreplace_pix_del, data, path, wnum, display_opt, array_out, x_pix, y_pix
	; Destroy group leader if necessary
	if  n_elements(group) eq 0  then $
	   widget_control, group_id, /DESTROY
	return, array_out
end
