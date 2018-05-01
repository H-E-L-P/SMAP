; $Id: xdisplayopt.pro, v 1.1 Apr 2000 e.d. $
;
;+
; NAME:
;	XDISPLAYOPT
;
; PURPOSE:
;	Modify display options for a 2D image.
;
; CATEGORY:
;	Widgets. Data visualization.
;
; CALLING SEQUENCE:
;	Result = XDISPLAYOPT(Image, Wnum)
;
; INPUTS:
;	Image: 2D data array
;
; OPTIONAL INPUT PARAMETERS:
;	Wnum: window number of an existing window
;
; KEYWORD PARAMETERS:
;	OPTIONS: structure of current display options; the structure must be
;		defined as in DEFAULT_DISPLAY_OPT
;
;	GROUP: group leader of top level base
;
;	NODISPLAY: avoid first image display. This keyword should be set only
;		if the image is already displayed in its window (as in a call from
;		DISPLAY_IMAGE)
;
;	UVALUE: xdisplayopt user value
;
; OUTPUTS:
;	Return structure of modified display options
;
; OPTIONAL OUTPUT PARAMETERS:
;	Wnum: window number of new window, if undefined on input
;
; SIDE EFFECTS:
;	1) Initiates the XMANAGER if it is not already running.
;	2) Use WSET to activate the graphic window identified by Wnum.
;	3) Create a new graphic window if the input parameter Wnum is undefined.
;
; RESTRICTIONS:
;	1) If the parameter Wnum is a window number < 32 but the corresponding
;	window does not exist (or has been deleted), the new window size is set
;	by IDL and may not fit the image x- and y size. If Wnum >=32 and the
;	corresponding window does not exists, an error occurs.
;	2) The Help menu opens the file '/starfinder/xdisplay_opt_help.txt'.
;
; PROCEDURE:
;	Create and register the widget as a modal widget.
;	Then let the user modify the image display options.
;
; MODIFICATION HISTORY:
;	Written by: Emiliano Diolaiti, August 1999
;	Updates:
;	1) Enhanced error handling in event-handler
;	   (Emiliano Diolaiti, April 2000).
;-


; XDISPLAYOPT_EVENT: XDisplayOpt event handler.

PRO xdisplayopt_event, event

	catch, error
	if  error ne 0  then begin
	   msg = dialog_message(/ERROR, !err_string)
	   child = widget_info(event.top, /CHILD)
	   widget_control, child, SET_UVALUE = info, /NO_COPY
	   return
	endif
	; Get user value
	child = widget_info(event.top, /CHILD)
	widget_control, child, GET_UVALUE = info, /NO_COPY
	; Event case
	event_id = event.id  &  id = (*info).id
	case event_id of
	   id.min_level: (*info).options.range[0] = event.value
	   id.max_level: (*info).options.range[1] = event.value
	   id.stretch: (*info).options.stretch = event.value
	   id.reverse: (*info).options.reverse = not (*info).options.reverse and 1B
	   id.chop: (*info).options.chop = event.value
	   id.ct: (*info).options.color_table = event.index
	   id.apply: begin
	      widget_control, /HOURGLASS
	      display_image, (*info).image, (*info).wnum, OPTIONS = (*info).options
	      end
	   id.hlp: $
	      xdispfile, file_name('starfinder', 'xdisplayopt_help.txt'), $
	                 TITLE = 'XDisplayOpt help', /MODAL
	   id.done: begin
	      widget_control, child, SET_UVALUE = info, /NO_COPY
	      widget_control, event.top, /DESTROY
	      end
	endcase
	; Set user value
	if  event_id ne id.done  then $
	   widget_control, child, SET_UVALUE = info, /NO_COPY
	return
end


; XDISPLAYOPT: XDisplayOpt widget definition module.

FUNCTION xdisplayopt, image, wnum, OPTIONS = options, NODISPLAY = nodisplay, $
					  GROUP = group, UVALUE = uvalue

	catch, display_error
	if  display_error ne 0  then begin
	   if  n_elements(options) eq 0  then  options = 0
	   return, options
	endif
	; Display image, retrieving default options if undefined
	if  not keyword_set(nodisplay)  then $
	   display_image, image, wnum, OPTIONS = options  else $
	if  n_elements(options) eq 0  then $
	   options = default_display_opt(image)
	; Define group leader, if necessary
	if  n_elements(group) eq 0  then $
	   group_id = widget_base()  else  group_id = group
	; Define modal top level base
	if  n_elements(uvalue) eq 0  then  uvalue = 0B
	base = widget_base(TITLE = 'XDisplayOpt', GROUP_LEADER = group_id, $
					   /MODAL, /COLUMN, UVALUE = uvalue)
	; Define child, to store data structure
	child = widget_base(base)
	; Define 'Intensity range' input fields
	min_level = cw_form(base, '0, FLOAT,,WIDTH=12,LABEL_LEFT=Minimum intensity ')
	widget_control, min_level, SET_VALUE = $
					{tag0: strcompress(string(options.range[0]), /REMOVE_ALL)}
	max_level = cw_form(base, '0, FLOAT,,WIDTH=12,LABEL_LEFT=Maximum intensity ')
	widget_control, max_level, SET_VALUE = $
					{tag0: strcompress(string(options.range[1]), /REMOVE_ALL)}
	; Define Stretch buttons
	slabel = widget_label(base, VALUE = 'Stretch:', /ALIGN_LEFT)
	stretch_opt = ['square', 'linear', 'square root', 'logarithm']
	stretch = cw_bgroup(base, stretch_opt, /EXCLUSIVE, /NO_RELEASE, $
			    SET_VALUE = (where(stretch_opt eq options.stretch))[0], $
			    BUTTON_UVALUE = stretch_opt)
	reverse = cw_bgroup(base, ['reverse scale'], /NONEXCLUSIVE, $
						SET_VALUE = options.reverse and 1B,	$
						BUTTON_UVALUE = 'reverse')
	; Define 'Chopping threshold' input field
	chop = cw_form(base, '0, FLOAT,,WIDTH=12,LABEL_LEFT=Chopping threshold ')
	widget_control, chop, SET_VALUE = $
					{tag0: strcompress(string(options.chop), /REMOVE_ALL)}
	; Define 'Color Table' list
	ct_names = ''	; define ct_names for LoadCT
	loadct, GET_NAMES = ct_names, /SILENT
	ctlabel = widget_label(base, VALUE = 'Color Table:', /ALIGN_LEFT)
	ct = widget_list(base, VALUE = ct_names, YSIZE = 5)
	; Define other buttons
	base2 = widget_base(base, /ROW)
	apply = widget_button(base2, VALUE = 'Apply options', /NO_RELEASE)
	base3 = widget_base(base, /ROW)
	hlp = widget_button(base3, VALUE = 'Help', /NO_RELEASE)
	done = widget_button(base3, VALUE = 'Exit', /NO_RELEASE)
	; Realize widget, set user value, register, etc.
	id = {min_level: min_level, max_level: max_level, $
		  stretch: stretch, reverse: reverse, chop: chop, ct: ct, $
		  apply: apply, hlp: hlp, done: done} ; IDs structure
	data = {id: id, wnum: wnum, image: image, options: options}	; data structure
	data = ptr_new(data, /NO_COPY)	; pointer to data structure
	widget_control, child, SET_UVALUE = data
		; NOTE: /NO_COPY is not set here, to avoid losing the pointer to
		; the global data stored in the user-value of the 'child' base
	widget_control, base, /REALIZE
	xmanager, 'xdisplayopt', base, EVENT_HANDLER = 'xdisplayopt_event'
	options = (*data).options  &  ptr_free, data
	; Destroy group leader base, if necessary
	if  n_elements(group) eq 0  then $
	   widget_control, group_id, /DESTROY
	return, options
end
