; $Id: ximage_support, v 1.1 Apr 1999 e.d. $
;
;+
; NAME:
;	XIMAGE_SUPPORT
;
; PURPOSE:
;	Widget interface for the CIRC_MASK and IMAGE_CORE functions.
;	Modify the support of an image by applying a circular mask and/or
;	extracting the principal connected component above a pre-fixed threshold.
;	The reference pixel for CIRC_MASK and IMAGE_CORE is assumed to be the
;	maximum intensity pixel of the input array.
;
; CATEGORY:
;	Widgets. Signal processing.
;
; CALLING SEQUENCE:
;	Result = XIMAGE_SUPPORT(Psf, Wnum, Display_opt)
;
; INPUTS:
;	Psf: 2D data array, containing the image of the point source to smooth
;
; OPTIONAL INPUTS:
;	Wnum: Window number of an existing window
;
;	Display_opt:	Structure of current display options;
;		the structure must be defined as in DEFAULT_DISPLAY_OPT
;
; KEYWORD PARAMETERS:
;	DEFAULT_PAR:	Structure of default parameters for the widget's form.
;
;	GROUP: XImage_Support group leader.
;
;	UVALUE: XImage_Support user value.
;
; OUTPUTS:
;	Result:	Input Image with processed support
;
; OPTIONAL OUTPUTS:
;	Wnum: window number of new window, if undefined on input
;
;	Display_opt:	Display options, as defined and/or modified by
;		X_Psf_smooth
;
;	DEFAULT_PAR:	Set this keyword to a named variable to get the
;		structure of parameters set by the widget's user.
;
; SIDE EFFECTS:
;	1) Initiates the XMANAGER if it is not already running.
;	2) Call DISPLAY_IMAGE to activate the graphic window identified by Wnum.
;	3) Create a new graphic window if not defined on input (see DISPLAY_IMAGE
;	and XDISPLAYOPT for details).
;	4) Any image displayed on the window Wnum is overwritten first by the
;	input image and then by the processed array.
;
; RESTRICTIONS:
;	The Help menu opens the file
;	'/starfinder/ximage_support_help.txt'.
;
; PROCEDURE:
;	Create and register the widget as a modal widget.
;	Then let the user define and or/modify the support options and apply
;	them to the input image. As a new processed image is obtained applying
;	the current options, it is displayed on the graphic window.
;
; MODIFICATION HISTORY:
;	Written by: Emiliano Diolaiti, September 1999
;	Updates:
;	1) Enhanced error handling in event-handler
;	   (Emiliano Diolaiti, April 2000).
;-

; XIMAGE_SUPPORT_EVENT: XImage_Support event handler.

PRO ximage_support_event, event

	catch, error
	if  error ne 0  then begin
	   msg = dialog_message(/ERROR, !err_string)
	   widget_control, event.id, SET_UVALUE = data, /NO_COPY
	   return
	endif
	widget_control, event.id, GET_UVALUE = data, /NO_COPY
	event_type = strlowcase(event.tag)
	case  event_type  of
	   'mask': $
	      widget_control, (*data).ids[0], SENSITIVE = (event.value eq 1) and 1B
	   'comp': $
	      widget_control, (*data).ids[1], SENSITIVE = (event.value eq 1) and 1B
	   'proc': begin
	      widget_control, /HOURGLASS
	      widget_control, event.id, GET_VALUE = form
	      (*data).par.mask = form.mask
	      (*data).par.rad = form.rad
	      (*data).par.comp = form.comp
	      (*data).par.thresh = form.thresh
	      mask = (form.mask eq 1) and 1B
	      if  mask  then  rad = form.rad
	      comp = (form.comp eq 1) and 1B
	      if  comp  then  thresh = form.thresh
	      (*data).image_out = (*data).image_in
	      m = get_max((*data).image_out)
	      if  mask  then $
	         (*data).image_out = circ_mask((*data).image_out, m[0], m[1], rad)
	      if  comp  then $
	         (*data).image_out = image_core((*data).image_out, thresh, X = m[0], Y = m[1])
	      display_image, (*data).image_out, (*data).wnum, OPT = (*data).display_opt
	      end
	   'display': $
	      (*data).display_opt = xdisplayopt((*data).image_out, (*data).wnum, $
	      		/NODISPLAY, OPTIONS = (*data).display_opt, GROUP = event.top)
	   'help': $
	      xdispfile, file_name('starfinder', 'ximage_support_help.txt'), $
	      		 TITLE = 'XImage_Support help', /MODAL
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

; XIMAGE_SUPPORT_PAR: define default parameters.

PRO ximage_support_par, id, maxsiz, par

	if  n_elements(par) ne 0  then begin
	   mask = par.mask
	   rad = par.rad
	   comp = par.comp
	   thresh = par.thresh
	endif else begin
	   mask = 0
	   rad = maxsiz
	   comp = 0
	   thresh = 0
	   par = {mask: mask, rad: rad, comp: comp, thresh: thresh}
	endelse
	init = {mask: mask, rad: strcompress(string(rad), /REMOVE_ALL), $
			comp: comp, thresh: strcompress(string(thresh), /REMOVE_ALL)}
	widget_control, id, SET_VALUE = init
	return
end

; XIMAGE_SUPPORT: XImage_Support widget definition module.

FUNCTION ximage_support, image, wnum, display_opt, DEFAULT_PAR = par, $
						 GROUP = group, UVALUE = uvalue

	on_error, 2
	; Display image and define display options
	display_image, image, wnum, OPTIONS = display_opt
	; Create group leader if necessary
	if  n_elements(group) eq 0  then $
	   group_id = widget_base()  else  group_id = group
	; Create modal base
	if  n_elements(uvalue) eq 0  then  uvalue = 0B
	base = widget_base(TITLE = 'XImage_Support', /MODAL, UVALUE = uvalue, $
					   GROUP_LEADER = group_id)
	; Define form
	desc = [ $
	'0, LABEL,Circular mask:', $
	'1, BASE,,COLUMN,FRAME', $
	'0, BUTTON,No|Yes,EXCLUSIVE,TAG=mask,ROW', $
	'2, INTEGER,,LABEL_LEFT=Circular mask radius,TAG=rad', $
	'0, LABEL,Principal component extraction:', $
	'1, BASE,,COLUMN,FRAME', $
	'0, BUTTON,No|Yes,EXCLUSIVE,TAG=comp,ROW', $
	'2, FLOAT,,LABEL_LEFT=Threshold for principal component extraction,TAG=thresh', $
	'1, BASE,,ROW', $
	'0, BUTTON,Processing...,NO_RELEASE,TAG=proc', $
	'2, BUTTON,Display Options,NO_RELEASE,TAG=display', $
	'1, BASE,,ROW', $
	'0, BUTTON,Help,NO_RELEASE,TAG=help', $
	'2, BUTTON,Exit,QUIT,NO_RELEASE,TAG=exit']
	form = cw_form(base, desc, IDS = ids, /COLUMN)
	ximage_support_par, form, max(size52(image, /DIM)), par
	widget_control, ids[3], SENSITIVE = par.mask eq 1 and 1B
	widget_control, ids[7], SENSITIVE = par.comp eq 1 and 1B
	; Define pointer to auxiliary/output data
	data = {par: par, image_in: image, image_out: image, $
			wnum: wnum, display_opt: display_opt, ids: [ids[3], ids[7]]}
	data = ptr_new(data, /NO_COPY)
	widget_control, form, SET_UVALUE = data
	; Realize, register, etc.
	widget_control, base, /REALIZE
	xmanager, 'ximage_support', base, EVENT_HANDLER = 'ximage_support_event'
	; De-reference output data
	par = (*data).par  &  image_out = (*data).image_out  &  display_opt = (*data).display_opt
	ptr_free, data
	; Destroy group leader if necessary
	if  n_elements(group) eq 0  then $
	   widget_control, group_id, /DESTROY
	return, image_out
end
