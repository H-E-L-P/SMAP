; $Id: xpsf_smooth, v 1.1 Apr 2000 e.d. $
;
;+
; NAME:
;	XPSF_SMOOTH
;
; PURPOSE:
;	Widget interface for the HALO_SMOOTH function.
;	Apply a variable box size median filter to the halo of a PSF image.
;
; CATEGORY:
;	Widgets. Signal processing.
;
; CALLING SEQUENCE:
;	Result = XPSF_SMOOTH(Psf, Wnum, Display_opt)
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
;	GROUP: XPsf_smooth group leader.
;
;	UVALUE: XPsf_smooth user value.
;
; OUTPUTS:
;	Result:	Halo-smoothed Psf
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
;	input image to smooth and then by the result of the processing performed
;	by XPsf_Smooth.
;
; RESTRICTIONS:
;	The Help menu opens the file
;	'/starfinder/xpsf_smooth_help.txt'.
;
; PROCEDURE:
;	Create and register the widget as a modal widget.
;	Then let the user define and or/modify the smoothing options and apply
;	them to the input image. As a new processed image is obtained applying
;	the current options, it is displayed on the graphic window.
;
; MODIFICATION HISTORY:
;	Written by: Emiliano Diolaiti, September 1999
;	Updates:
;	1) Enhanced error handling in event-handler
;	   (Emiliano Diolaiti, April 2000).
;-

; XPSF_SMOOTH_EVENT: XPsf_Smooth event handler.

PRO xpsf_smooth_event, event

	catch, error
	if  error ne 0  then begin
	   msg = dialog_message(/ERROR, !err_string)
	   widget_control, event.id, SET_UVALUE = data, /NO_COPY
	   return
	endif
	widget_control, event.id, GET_UVALUE = data, /NO_COPY
	event_type = strlowcase(event.tag)
	case  event_type  of
	   'process': begin
	      widget_control, /HOURGLASS
	      widget_control, event.id, GET_VALUE = form
	      (*data).par.rad = form.rad
	      (*data).par.rw = form.rw
	      (*data).par.rp = form.rp
	      (*data).par.aw = form.aw
	      (*data).par.ap = form.ap
	      (*data).par.pad = form.pad
	      r = form.rad  &  rw = form.rw  &  rp = form.rp
	      aw = form.aw * !pi / 180.  &  ap = form.ap
	      pad = (form.pad eq 1) and 1B
	      (*data).psf_out = halo_smooth((*data).psf_in, r, PAD_0 = pad, $
					        R_WIDTH = rw, A_WIDTH = aw, R_EXP = rp, A_EXP = ap)
	      display_image, (*data).psf_out, (*data).wnum, OPT = (*data).display_opt
	      end
	   'display': (*data).display_opt = $
	      xdisplayopt((*data).psf_out, (*data).wnum, /NODISPLAY, $
	      			  OPTIONS = (*data).display_opt, GROUP = event.top)
	   'help': $
	      xdispfile, file_name('starfinder', 'xpsf_smooth_help.txt'), $
	      		 TITLE = 'XPsf_Smooth help', /MODAL
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

; XPSF_SMOOTH_PAR: define default parameters.

PRO xpsf_smooth_par, id, minsiz, par

	if  n_elements(par) ne 0  then begin
	   rad = par.rad  &  rw = par.rw  &  rp = par.rp
	   aw = par.aw  &  ap = par.ap  &  pad = par.pad
	endif else begin
	   rad = round(minsiz/4.)  &  rw = round(rad/2.)  &  rp = 2
	   aw = 22.5  &  ap = 3  &  pad = 0
	   par = {rad: rad, rw: rw, rp: rp, aw: aw, ap: ap, pad: pad}
	endelse
	init = {rad: strcompress(string(rad), /REMOVE_ALL), $
			rw: strcompress(string(rw), /REMOVE_ALL), $
			rp: strcompress(string(rp), /REMOVE_ALL), $
			aw: strcompress(string(aw), /REMOVE_ALL), $
			ap: strcompress(string(ap), /REMOVE_ALL), $
			pad: pad}
	widget_control, id, SET_VALUE = init
	return
end

; XPSF_SMOOTH: XPsf_Smooth widget definition module.

FUNCTION xpsf_smooth, psf, wnum, display_opt, DEFAULT_PAR = par, $
					  GROUP = group, UVALUE = uvalue

	on_error, 2
	; Display image and define display options
	display_image, psf, wnum, OPTIONS = display_opt
	; Create group leader if necessary
	if  n_elements(group) eq 0  then $
	   group_id = widget_base()  else  group_id = group
	; Create modal base
	if  n_elements(uvalue) eq 0  then  uvalue = 0B
	base = widget_base(TITLE = 'XPsf_Smooth', /MODAL, UVALUE = uvalue, $
					   GROUP_LEADER = group_id)
	; Define form
	desc = [ $
	'0, INTEGER,,LABEL_LEFT=Inner radius,TAG=rad', $
	'0, INTEGER,,LABEL_LEFT=Radial width of smoothing box,TAG=rw', $
	'0, INTEGER,,LABEL_LEFT=Radial power,TAG=rp', $
	'0, FLOAT,,LABEL_LEFT=Angular width of smoothing box (deg.),TAG=aw', $
	'0, INTEGER,,LABEL_LEFT=Angular power,TAG =ap', $
	'0, BUTTON,No|Yes,EXCLUSIVE,LABEL_LEFT=Pad,NO_RELEASE,ROW,TAG=pad', $
	'1, BASE,,ROW', $
	'0, BUTTON,Processing...,NO_RELEASE,TAG=process', $
	'2, BUTTON,Display Options,NO_RELEASE,TAG=display', $
	'1, BASE,,ROW', $
	'0, BUTTON,Help,NO_RELEASE,TAG=help', $
	'2, BUTTON,Exit,QUIT,NO_RELEASE,TAG=exit']
	form = cw_form(base, desc, /COLUMN)
	xpsf_smooth_par, form, min(size52(psf, /DIM)), par
	; Define pointer to auxiliary/output data
	data = {par: par, psf_in: psf, wnum: wnum, $
			display_opt: display_opt, psf_out: psf}
	data = ptr_new(data, /NO_COPY)
	widget_control, form, SET_UVALUE = data
	; Realize, register, etc.
	widget_control, base, /REALIZE
	xmanager, 'xpsf_smooth', base, EVENT_HANDLER = 'xpsf_smooth_event'
	; De-reference output data
	par = (*data).par  &  psf_out = (*data).psf_out
	display_opt = (*data).display_opt
	ptr_free, data
	; Destroy group leader if necessary
	if  n_elements(group) eq 0  then $
	   widget_control, group_id, /DESTROY
	return, psf_out
end
