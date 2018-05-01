; $Id: xdispfile.pro, v 1.1 Apr 2000 e.d. $
;
;+
; NAME:
;	XDISPFILE
;
; PURPOSE:
;	Display an ASCII text file.
;	This is a simplified version of the IDL library routine
;	XDisplayFile, with the only additional feature that it
;	can be called by other modal widgets.
;
; CATEGORY:
;	Widgets.
;
; CALLING SEQUENCE:
;	XDISPFILE, Filename
;
; INPUTS:
;     Filename:	String, containing the name of the file.
;
; KEYWORD PARAMETERS:
;	BLOCK:	Set this keyword to have XMANAGER block the IDL
;		command line when this application is registered.
;		The default is 'not blocked'.
;
;	GROUP:	The widget identifier of the group leader of the widget.
;		If this keyword is specified and the keyword MODAL (see
;		below) is not set, the death of the group leader results
;		in the death of XDispFile.
;
;	MODAL:	Set this keyword to any nonzero value to create the widget
;		as a modal widget, blocking the IDL command line and every
;		other previously created widget applications.
;
;	TITLE:	Widget title. The default is Filename.
;
;	WIDTH:	Width of the text widget in characters. The default is 80.
;
;	HEIGHT:	Number of lines to be displayed in the text widget.
;		Scroll bars are available to see the hidden text.
;		The default height is 24 lines.
;
; OUTPUTS:
;	None. Text is displayed on the widget.
;
; SIDE EFFECTS:
;	Initiates the XMANAGER if it is not already running.
;
; PROCEDURE:
;	Read the specified ASCII file and display its content
;	on a text widget.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, November 1999.
;	Updates:
;	1) Enhanced error handling (Emiliano Diolaiti, April 2000).
;-

; XDISPFILE_EVENT: XDispFile event handler.

PRO xdispfile_event, event

	widget_control, event.id, GET_UVALUE = ev_type
	if  strlowcase(ev_type) eq 'quit'  then $
	   widget_control, event.top, /DESTROY
	return
end

; XDISPFILE: XDispFile widget definition module.

PRO xdispfile, filename, GROUP = group, MODAL = modal, BLOCK = block, $
	           TITLE = title, WIDTH = width, HEIGHT = height

	catch, error
	if  error ne 0  then begin
	   msg = dialog_message(/ERROR, !err_string)
	   close, /ALL
	   return
	endif
	; Open file and read its content
	openr, unit, filename, /GET_LUN
	text = ''  &  line = text
	while  not eof(unit)  do begin
	   readf, unit, line  &  text = [text, line]
	endwhile
	free_lun, unit
	; Define default values
	if  n_elements(title) eq 0  then  title = filename
	if  n_elements(group) ne 0  then  group_leader = group
	if  keyword_set(modal)  then $
	   if  n_elements(group) eq 0  then  group_leader = widget_base()
	if  n_elements(width) eq 0  then  width = 80
	if  n_elements(height) eq 0  then  height = 24
	; Define widget
    base = widget_base(TITLE = title, /COLUMN, $
                       MODAL = modal, GROUP_LEADER = group_leader)
	text_id = widget_text(base, /SCROLL, VALUE = text, $
	                      XSIZE = width, YSIZE = height)
	lo_base = widget_base(base)
	quit_id = widget_button(lo_base, VALUE = 'Close', UVALUE = 'quit')
	; Realize widget, register it, etc.
	widget_control, base, /REALIZE
	xmanager, 'xdispfile', base, $
                  EVENT_HANDLER = 'xdispfile_event', $
                  NO_BLOCK = (not keyword_set(block)) and 1B
	if  keyword_set(modal)  then  if  n_elements(group) eq 0  then $
	   widget_control, group_leader, /DESTROY
	return
end
