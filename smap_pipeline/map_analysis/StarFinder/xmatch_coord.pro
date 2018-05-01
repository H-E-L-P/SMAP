; $Id: xmatch_coord, v 1.1 Apr 2000 e.d. $
;
;+
; NAME:
;	XMATCH_COORD
;
; PURPOSE:
;	Widget interface for the MATCH_COORD procedure.
;	Given two sets of coordinates on a plane, representing the same points
;	in two different reference frames supposed to be reciprocally translated
;	and rotated, find the optimal transformation between the two sets and
;	map one of them onto the other.
;
; CATEGORY:
;	Widgets. Spatial transformations.
;
; CALLING SEQUENCE:
;	XMATCH_COORD, X1, Y1, X2, Y2, X_ref, Y_ref, X2_out, Y2_out
;
; INPUTS:
;	X1, Y1:	Coordinates of points in reference frame no. 1
;
;	X2, Y2:	Coordinates of points in reference frame no. 2
;
;	X_ref, Y_ref:	Coordinates of reference points, used to find the
;		optimal transformation. (X_ref, Y_ref) may be a subset of (X1, Y1).
;
; KEYWORD PARAMETERS:
;	GROUP: XMatch_Coord group leader.
;
;	UVALUE: XMatch_Coord user value.
;
; OUTPUTS:
;	X2_out, Y2_out:	Coordinates (X2, Y2) mapped onto reference frame 1.
;		These coordinates may be directly compared to (X1, Y1).
;		A negative scalar value indicates an error condition
;
; OPTIONAL OUTPUTS:
;	ORIGIN:	Set this keyword to a named variable to retrieve a 2-components
;		vector, representing the estimated position of the origin of reference
;		frame 2 in reference frame 1.
;
;	ANGLE:	Set this keyword to a named variable to retrieve a scalar,
;		representing the estimated angle (in radians) between the x- axis of
;		reference frame 1 and the x- axis of reference frame 2, measured
;		counter-clockwise from 1 to 2.
;
; SIDE EFFECTS:
;	Initiates the XMANAGER if it is not already running.
;
; RESTRICTIONS:
;	The Help menu opens the file
;	'/starfinder/xmatch_coord_help.txt'.
;
; PROCEDURE:
;	Create and register the widget as a modal widget.
;
; MODIFICATION HISTORY:
;	Written by: Emiliano Diolaiti, September 1999
;	Updates:
;	1) Enhanced error handling in event-handler
;	   (Emiliano Diolaiti, April 2000).
;-

; XMATCH_COORD_EVENT: XMatch_Coord event handler.

PRO xmatch_coord_event, event

	catch, error
	if  error ne 0  then begin
	   msg = dialog_message(/ERROR, !err_string)
	   widget_control, event.id, SET_UVALUE = data, /NO_COPY
	   return
	endif
	widget_control, event.id, GET_UVALUE = data, /NO_COPY
	event_type = strlowcase(event.tag)
	case  event_type  of
	   'match': begin
	      widget_control, /HOURGLASS
	      ; Match coordinates
	      widget_control, event.id, GET_VALUE = form
	      origin = [form.ox, form.oy]
	      angle = form.angle * !pi/180.
	      match_coord, (*data).x1, (*data).y1, (*data).x2, (*data).y2, $
	      			   (*data).x_ref, (*data).y_ref, x2_out, y2_out, $
	      			   ORIGIN_0 = origin, ANGLE_0 = angle, origin, angle
	      ; Save results
	      if  n_elements(x2_out) eq n_elements((*data).x2)  then begin
	         msg = dialog_message(['The parameters of the transformation are: ', $
	         					   'x-shift = ' + strcompress(string(origin[0])), $
	         					   'y-shift = ' + strcompress(string(origin[1])), $
	         					   'rotation angle (deg.) = ' + $
	         					   strcompress(string(angle/!pi*180))], /INFO)
	         *(*data).x2_out = x2_out  &  *(*data).y2_out = y2_out
	         (*data).origin = origin  &  (*data).angle = angle
	      endif else $
		     msg = dialog_message('Unsuccessful matching', /ERROR)
	      end
	   'help': $
	      xdispfile, file_name('starfinder', 'xmatch_coord_help.txt'), $
	      		 TITLE = 'XMatch_Coord help', /MODAL
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

; XMATCH_COORD_DEF: define data structure.

FUNCTION xmatch_coord_def, x1, y1, x2, y2, x_ref, y_ref

	data = {x1: x1, y1: y1, x2: x2, y2: y2, $
			x_ref: x_ref, y_ref: y_ref, $
			x2_out: ptr_new(/ALLOCATE), y2_out: ptr_new(/ALLOCATE), $
			origin: [0., 0.], angle: 0.}
	return, data
end

; XMATCH_COORD_DEL: de-reference and de-allocate heap variables.

PRO xmatch_coord_del, data, x2_out, y2_out, origin, angle

	if  n_elements(*(*data).x2_out) ne 0 and n_elements(*(*data).y2_out) ne 0  then begin
	   x2_out = *(*data).x2_out  &  y2_out = *(*data).y2_out
	endif
	origin = (*data).origin  &  angle = (*data).angle
	ptr_free, (*data).x2_out, (*data).y2_out
	ptr_free, data
	return
end

; XMATCH_COORD: XMatch_Coord widget definition module.

PRO xmatch_coord, x1, y1, x2, y2, x_ref, y_ref, x2_out, y2_out, $
				  ORIGIN = origin, ANGLE = angle, $
				  GROUP = group, UVALUE = uvalue

	on_error, 2
	if  n_elements(x1) eq 0 or n_elements(y1) eq 0 or $
		n_elements(x2) eq 0 or n_elements(y2) eq 0  then begin
	   msg = dialog_message('XMatch_Coord: missing data', /ERROR)
	   return
	endif
	if  n_elements(x_ref) lt 2 or n_elements(y_ref) lt 2  then begin
	   msg = dialog_message('XMatch_Coord: at least 2 reference ' + $
	   						'points must be supplied.', /ERROR)
	   return
	endif
	; Create group leader if necessary
	if  n_elements(group) eq 0  then $
	   group_id = widget_base()  else  group_id = group
	; Create modal base
	if  n_elements(uvalue) eq 0  then  uvalue = 0B
	base = widget_base(TITLE = 'XMatch_Coord', /MODAL, UVALUE = uvalue, $
					   GROUP_LEADER = group_id)
	; Define form
	desc = [ $
	'0, LABEL,Initial guess:,LEFT', $
	'1, BASE,,COLUMN,FRAME', $
	'0, FLOAT,0,LABEL_LEFT=x-axis translation,TAG=ox', $
	'0, FLOAT,0,LABEL_LEFT=y-axis translation,TAG=oy', $
	'2, FLOAT,0,LABEL_LEFT=rotation angle (deg.),TAG=angle', $
	'1, BASE,,ROW', $
	'2, BUTTON,Match,NO_RELEASE,TAG=match', $
	'1, BASE,,ROW', $
	'0, BUTTON,Help,NO_RELEASE,TAG=help', $
	'2, BUTTON,Exit,QUIT,NO_RELEASE,TAG=exit']
	form = cw_form(base, desc, /COLUMN)
	; Define pointer to auxiliary/output data
	data = xmatch_coord_def(x1, y1, x2, y2, x_ref, y_ref)
	data = ptr_new(data, /NO_COPY)
	widget_control, form, SET_UVALUE = data
	; Realize, register, etc.
	widget_control, base, /REALIZE
	xmanager, 'xmatch_coord', base, EVENT_HANDLER = 'xmatch_coord_event'
	; De-reference output data and de-allocate heap variables
	xmatch_coord_del, data, x2_out, y2_out, origin, angle
	; Destroy group leader if necessary
	if  n_elements(group) eq 0  then $
	   widget_control, group_id, /DESTROY
	return
end
