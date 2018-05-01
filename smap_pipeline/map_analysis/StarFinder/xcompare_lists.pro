; $Id: xcompare_lists, v 1.3 Jun 2000 e.d. $
;
;+
; NAME:
;	XCOMPARE_LISTS
;
; PURPOSE:
;	Widget interface to read and compare two lists of objects.
;	Each list is supposed to include floating point data ordered in rows
;	of 7 elements. Such a list is produced by XStarFinder, as a result
;	of the analysis of a stellar field. XCOMPARE_LISTS may be used to
;	compare the results obtained for two images of the same field, observed
;	at different wavelenghts. This task requires matching the coordinates,
;	which are assumed to be reciprocally translated and rotated, and finding
;	the common objects in the two lists.
;	The output lists may be saved to a file. It is possible to plot the
;	magnitudes of the common objects, producing a Color Magnitude Diagram.
;
; CATEGORY:
;	Widgets. Stellar astrometry and photometry.
;
; CALLING SEQUENCE:
;	XCOMPARE_LISTS, Wnum
;
; OPTIONAL INPUTS:
;	Wnum:	Number of an existing graphic window where the widget must
;		produce a Color Magnitude Diagram.
;		If undefined, the plot is displayed on a new graphic window.
;
; KEYWORD PARAMETERS:
;	PATH:	Initial path for file browsing when reading lists or saving
;		results. If the argument of the keyword is a named variable,
;		its value is overwritten.
;
;	GROUP: XCompare_Lists group leader.
;
;	UVALUE: XCompare_Lists user value.
;
; SIDE EFFECTS:
;	1) Initiates the XMANAGER if it is not already running.
;	2) If the window number Wnum is undefined and the user wants to plot the
;	Color Magnitude Diagram, a new window is created.
;
; RESTRICTIONS:
;	The Help menu opens the file
;	'/starfinder/xcompare_lists_help.txt'.
;
; PROCEDURE:
;	Create and register the widget as a modal widget.
;
; MODIFICATION HISTORY:
;	Written by: Emiliano Diolaiti, September 1999
;	Updates:
;	1) Fixed bug on output roto-translated coordinates
;	   (Emiliano Diolaiti, March 2000)
;	2) Enhanced error handling in event-handler
;	   (Emiliano Diolaiti, April 2000).
;	3) Reviewed handling of lists (Emiliano Diolaiti, June 2000).
;-

; XCOMPARE_LISTS_EVENT: XCompare_Lists event handler.

PRO xcompare_lists_event, event

	catch, error
	if  error ne 0  then begin
	   msg = dialog_message(/ERROR, !err_string)
	   child = widget_info(event.top, /CHILD)
	   widget_control, child, SET_UVALUE = data, /NO_COPY
	   close, /ALL
	   return
	endif
	; Get user value
	child = widget_info(event.top, /CHILD)
	widget_control, child, GET_UVALUE = data, /NO_COPY
	; Event case
	event_id = event.id  &  id = (*data).id
	case  event_id  of
	   id.fil: $
	      case  event.value  of
	         'File.Load.List 1': begin
	            file = dialog_pickfile(/READ, FILTER = '*.txt', $
	            					   PATH = *(*data).path, GET_PATH = path)
	            if  file ne ''  then begin
	               widget_control, /HOURGLASS
	               *(*data).path = path
	               in = transpose(read_float_data(file, 7))
	               *(*data).x1 = in[*,0]  &  *(*data).sx1 = in[*,3]
	               *(*data).y1 = in[*,1]  &  *(*data).sy1 = in[*,4]
	               *(*data).f1 = in[*,2]  &  *(*data).sf1 = in[*,5]
	               *(*data).c1 = in[*,6]
	               ptr_free, (*data).x1c, (*data).sx1c, (*data).y1c, (*data).sy1c, $
	                         (*data).f1c, (*data).sf1c, (*data).c1c, $
	                         (*data).x_ref, (*data).y_ref
	               (*data).x1c = ptr_new(/ALLOCATE)
	               (*data).sx1c = ptr_new(/ALLOCATE)
	               (*data).y1c = ptr_new(/ALLOCATE)
	               (*data).sy1c = ptr_new(/ALLOCATE)
	               (*data).f1c = ptr_new(/ALLOCATE)
	               (*data).sf1c = ptr_new(/ALLOCATE)
	               (*data).c1c = ptr_new(/ALLOCATE)
	               (*data).x_ref = ptr_new(/ALLOCATE)
	               (*data).y_ref = ptr_new(/ALLOCATE)
	            endif
	            end
	         'File.Load.List 2': begin
	            file = dialog_pickfile(/READ, FILTER = '*.txt', $
	            					   PATH = *(*data).path, GET_PATH = path)
	            if  file ne ''  then begin
	               widget_control, /HOURGLASS
	               *(*data).path = path
	               in = transpose(read_float_data(file, 7))
	               *(*data).x2 = in[*,0]  &  *(*data).sx2 = in[*,3]
	               *(*data).y2 = in[*,1]  &  *(*data).sy2 = in[*,4]
	               *(*data).f2 = in[*,2]  &  *(*data).sf2 = in[*,5]
	               *(*data).c2 = in[*,6]
	               ptr_free, (*data).x2c, (*data).sx2c, (*data).y2c, (*data).sy2c, $
	                         (*data).f2c, (*data).sf2c, (*data).c2c, $
	                         (*data).x2rt, (*data).y2rt
	               (*data).x2c = ptr_new(/ALLOCATE)
	               (*data).sx2c = ptr_new(/ALLOCATE)
	               (*data).y2c = ptr_new(/ALLOCATE)
	               (*data).sy2c = ptr_new(/ALLOCATE)
	               (*data).f2c = ptr_new(/ALLOCATE)
	               (*data).sf2c = ptr_new(/ALLOCATE)
	               (*data).c2c = ptr_new(/ALLOCATE)
	               (*data).x2rt = ptr_new(/ALLOCATE)
	               (*data).y2rt = ptr_new(/ALLOCATE)
	            endif
	            end
	         'File.Load.Reference list': begin
	            file = dialog_pickfile(/READ, FILTER = '*.txt', $
	            					   PATH = *(*data).path, GET_PATH = path)
	            if  file ne ''  then begin
	               widget_control, /HOURGLASS
	               *(*data).path = path
	               in = transpose(read_float_data(file, 2))
	               *(*data).x_ref = in[*,0]  &  *(*data).y_ref = in[*,1]
	            endif
	            end
	         'File.Save.List 1': begin
	            if  n_elements(*(*data).f1c) ne 0  then begin
	               file = dialog_pickfile(/WRITE, FILTER = '*.txt', $
	            					      PATH = *(*data).path, GET_PATH = path)
	               if  file ne ''  then begin
	                  widget_control, /HOURGLASS
	                  if  strpos(file, '.txt') lt 0  then  file = file + '.txt'
	                  *(*data).path = path
	                  out = [transpose(*(*data).x1c), $
	               		     transpose(*(*data).y1c), $
	               		     transpose(*(*data).f1c), $
	               		     transpose(*(*data).sx1c), $
	               		     transpose(*(*data).sy1c), $
	            	         transpose(*(*data).sf1c), $
	            	         transpose(*(*data).c1c)]
	                  openw, lun, file, /GET_LUN
	                  printf, lun, out  &  free_lun, lun
	               endif
	            endif
	            end
	         'File.Save.List 2': begin
	            if  n_elements(*(*data).f2c) ne 0 or $
	                n_elements(*(*data).x2rt) ne 0  then begin
	               file = dialog_pickfile(/WRITE, FILTER = '*.txt', $
	            					      PATH = *(*data).path, GET_PATH = path)
	               if  file ne ''  then begin
	                  widget_control, /HOURGLASS
	                  if  strpos(file, '.txt') lt 0  then  file = file + '.txt'
	                  *(*data).path = path
	                  if  n_elements(*(*data).f2c) ne 0  then begin
	                     x = *(*data).x2c  &  sx = *(*data).sx2c
	                     y = *(*data).y2c  &  sy = *(*data).sy2c
	                     f = *(*data).f2c  &  sf = *(*data).sf2c
	                     c = *(*data).c2c
	                  endif else $
	                  if  n_elements(*(*data).x2rt) ne 0  then begin
	                     x = *(*data).x2rt  &  sx = *(*data).sx2
	                     y = *(*data).y2rt  &  sy = *(*data).sy2
	                     f = *(*data).f2  &  sf = *(*data).sf2
	                     c = *(*data).c2
	                  endif
	                  out = [transpose(x), transpose(y), transpose(f), $
	               	  	     transpose(sx), transpose(sy), transpose(sf), $
	            	         transpose(c)]
	                  openw, lun, file, /GET_LUN
	                  printf, lun, out  &  free_lun, lun
	               endif
	            endif
	            end
	      end
	   id.match: begin
	      if  n_elements(*(*data).x1) eq 0 or n_elements(*(*data).y1) eq 0  then $
	         msg = dialog_message(/ERROR, 'Load list 1.')  else $
	      if  n_elements(*(*data).x2) eq 0 or n_elements(*(*data).y2) eq 0  then $
	         msg = dialog_message(/ERROR, 'Load list 2.')  else $
	      if  n_elements(*(*data).x_ref) eq 0 or n_elements(*(*data).y_ref) eq 0  then $
	         msg = dialog_message(/ERROR, 'Load reference points for list 1.') $
	      else begin
	         xmatch_coord, *(*data).x1, *(*data).y1, *(*data).x2, *(*data).y2, $
	         			   *(*data).x_ref, *(*data).y_ref, x2_out, y2_out, $
	         			   GROUP = event.top
	         if  n_elements(x2_out) ne 0 and n_elements(y2_out) ne 0  then begin
	            *(*data).x2rt = x2_out  &  *(*data).y2rt = y2_out
	         endif
	      endelse
	      end
	   id.find: begin
	      if  n_elements(*(*data).x1) eq 0 or n_elements(*(*data).y1) eq 0  then $
	         msg = dialog_message(/ERROR, 'Load list 1.')  else $
	      if  n_elements(*(*data).x2) eq 0 or n_elements(*(*data).y2) eq 0  then $
	         msg = dialog_message(/ERROR, 'Load list 2.')  $
	      else begin
	         if  n_elements(*(*data).x2rt) le 1  then begin
	            *(*data).x2rt = *(*data).x2  &  *(*data).y2rt = *(*data).y2
	         endif
	         form = cw_form(['0,FLOAT,1,LABEL_LEFT=Distance,TAG=dist,WIDTH=12', $
	         				 '2, BUTTON,OK,QUIT,NO_RELEASE'], TITLE = 'Matching distance')
	         widget_control, /HOURGLASS
	         compare_lists, *(*data).x1, *(*data).y1, *(*data).x2rt, *(*data).y2rt, $
	      				    MAX_DISTANCE = form.dist, x1c, y1c, x2c, y2c, $
						    SUBSCRIPTS_1 = sub1, SUBSCRIPTS_2 = sub2
			 if  n_elements(sub1) ne 0  then  begin
	            *(*data).x1c = x1c  &  *(*data).y1c = y1c
	            *(*data).x2c = x2c  &  *(*data).y2c = y2c
	            *(*data).f1c = (*(*data).f1)[sub1]
	            *(*data).sx1c = (*(*data).sx1)[sub1]
	            *(*data).sy1c = (*(*data).sy1)[sub1]
	            *(*data).sf1c = (*(*data).sf1)[sub1]
	            *(*data).c1c = (*(*data).c1)[sub1]
	            *(*data).f2c = (*(*data).f2)[sub2]
	            *(*data).sx2c = (*(*data).sx2)[sub2]
	            *(*data).sy2c = (*(*data).sy2)[sub2]
	            *(*data).sf2c = (*(*data).sf2)[sub2]
	            *(*data).c2c = (*(*data).c2)[sub2]
	         endif
	         msg = dialog_message(/INFO, 'Found ' + $
	         					  strcompress(string(n_elements(sub1)), /REMOVE_ALL) + ' common stars.')
	      endelse
	      end
	   id.cmd: begin
	      if  n_elements(*(*data).f1c) ne 0 and n_elements(*(*data).f2c) ne 0  then begin
	         mag1 = *(*data).f1c  &  mag2 = *(*data).f2c
	      endif else $
	      if  n_elements(*(*data).f1) ne 0 and n_elements(*(*data).f2) ne 0  then begin
	         mag1 = *(*data).f1  &  mag2 = *(*data).f2
	      endif else $
	         msg = dialog_message(/ERROR, 'No data to plot.')
	      if  n_elements(mag1) ne 0 and n_elements(mag2) ne 0  then begin
	         w = where(mag1 gt 0 and mag2 gt 0, count)
	         if  count ne 0  then begin
	            mag1 = -2.5*alog10(mag1[w])  &  mag2 = -2.5*alog10(mag2[w])
	            form = cw_form(TITLE = 'Magnitude zero point: set 1', $
	         				['0,FLOAT,0,LABEL_LEFT=Magnitude zero point,TAG=z,WIDTH=12', $
	         				 '2, BUTTON,OK,QUIT,NO_RELEASE'])
	            mag1 = mag1 + form.z
	            form = cw_form(TITLE = 'Magnitude zero point: set 2', $
	         				['0,FLOAT,0,LABEL_LEFT=Magnitude zero point,TAG=z,WIDTH=12', $
	         				 '2, BUTTON,OK,QUIT,NO_RELEASE'])
	            mag2 = mag2 + form.z
	            xplot, mag1, mag1 - mag2, WNUM = (*data).wnum, $
	            	   PATH = *(*data).path, DEFAULT_PAR = *(*data).plot_par
	         endif
	      endif
	      end
	   id.hlp: $
	      xdispfile, file_name('starfinder', 'xcompare_lists_help.txt'), $
	      		 TITLE = 'XCompare_Lists help', /MODAL
	   id.done: begin
	      widget_control, child, SET_UVALUE = data, /NO_COPY
	      widget_control, event.top, /DESTROY
	      end
	endcase
	if  event_id ne id.done  then $
	   widget_control, child, SET_UVALUE = data, /NO_COPY
	return
end

; XCOMPARE_LISTS_DEF: define data structure.

FUNCTION xcompare_lists_def, wnum, path, id

	on_error, 2
	if  n_elements(wnum) eq 0  then  w = -1  else  w = wnum
	data = {x1: ptr_new(/ALLOCATE), sx1: ptr_new(/ALLOCATE), $
			y1: ptr_new(/ALLOCATE), sy1: ptr_new(/ALLOCATE), $
			f1: ptr_new(/ALLOCATE), sf1: ptr_new(/ALLOCATE), $
			c1: ptr_new(/ALLOCATE), $	; input parameters of set 1
			x2: ptr_new(/ALLOCATE), sx2: ptr_new(/ALLOCATE), $
			y2: ptr_new(/ALLOCATE), sy2: ptr_new(/ALLOCATE), $
			f2: ptr_new(/ALLOCATE), sf2: ptr_new(/ALLOCATE), $
			c2: ptr_new(/ALLOCATE), $	; input parameters of set 2
			x_ref: ptr_new(/ALLOCATE), y_ref: ptr_new(/ALLOCATE), $
			x2rt: ptr_new(/ALLOCATE), y2rt: ptr_new(/ALLOCATE), $
			id: id, wnum: w, path: ptr_new(path), $ ; other parameters
			x1c: ptr_new(/ALLOCATE), sx1c: ptr_new(/ALLOCATE), $
			y1c: ptr_new(/ALLOCATE), sy1c: ptr_new(/ALLOCATE), $
			f1c: ptr_new(/ALLOCATE), sf1c: ptr_new(/ALLOCATE), $
			c1c: ptr_new(/ALLOCATE), $	; output parameters of set 1
			x2c: ptr_new(/ALLOCATE), sx2c: ptr_new(/ALLOCATE), $
			y2c: ptr_new(/ALLOCATE), sy2c: ptr_new(/ALLOCATE), $
			f2c: ptr_new(/ALLOCATE), sf2c: ptr_new(/ALLOCATE), $
			c2c: ptr_new(/ALLOCATE), $	; output parameters of set 2
			plot_par: ptr_new(/ALLOCATE)}
	return, data
end

; XCOMPARE_LISTS_DEL: de-reference and de-allocate heap variables.

PRO xcompare_lists_del, data, path

	if  n_elements(*(*data).path) ne 0  then  path = *(*data).path
	ptr_free, (*data).x1, (*data).sx1, (*data).y1, (*data).sy1, $
	   		  (*data).f1, (*data).sf1, (*data).c1, $
	   		  (*data).x2, (*data).sx2, (*data).y2, (*data).sy2, $
	   		  (*data).f2, (*data).sf2, (*data).c2, $
	   		  (*data).x_ref, (*data).y_ref, (*data).x2rt, (*data).y2rt, $
	   		  (*data).x1c, (*data).sx1c, (*data).y1c, (*data).sy1c, $
	   		  (*data).f1c, (*data).sf1c, (*data).c1c, $
	   		  (*data).x2c, (*data).sx2c, (*data).y2c, (*data).sy2c, $
	   		  (*data).f2c, (*data).sf2c, (*data).c2c, $
	   		  (*data).path, (*data).plot_par
	ptr_free, data
	return
end

; XCOMPARE_LISTS: XCompare_Lists widget definition module.

PRO xcompare_lists, wnum, PATH = path, GROUP = group, UVALUE = uvalue

	on_error, 2
	; Create group leader if necessary
	if  n_elements(group) eq 0  then $
	   group_id = widget_base()  else  group_id = group
	; Create modal base
	if  n_elements(uvalue) eq 0  then  uvalue = 0B
	base = widget_base(TITLE = 'XCompare_Lists', /MODAL, UVALUE = uvalue, $
					   GROUP_LEADER = group_id, /COLUMN)
	; Define child of top level base to store data
	child = widget_base(base)
	; Define File menu
	desc = ['1\File', '1\Load', '0\List 1', '0\List 2', $
			'2\Reference list', '3\Save', '0\List 1', '2\List 2']
	fil = cw_pdmenu(base, desc, /RETURN_FULL_NAME)
	; Define 'processing' buttons
	pbase = widget_base(base, /COLUMN)
	match = widget_button(pbase, VALUE = 'Match coordinates', /NO_RELEASE)
	find = widget_button(pbase, VALUE = 'Find coincident', /NO_RELEASE)
	cmd = widget_button(pbase, VALUE = 'Color Magnitude Diagram', /NO_RELEASE)
	; Define other buttons
	ubase = widget_base(base, /ROW)
	hlp = widget_button(ubase, VALUE = 'Help', /NO_RELEASE)
	done = widget_button(ubase, VALUE = 'Exit', /NO_RELEASE)
	; Define pointer to auxiliary/output data
	id = {fil: fil, match: match, find: find, cmd: cmd, hlp: hlp, done: done}
	data = xcompare_lists_def(wnum, path, id)
	data = ptr_new(data, /NO_COPY)
	widget_control, child, SET_UVALUE = data
	; Realize, register, etc.
	widget_control, base, /REALIZE
	xmanager, 'xcompare_lists', base, EVENT_HANDLER = 'xcompare_lists_event'
	; De-reference output data and de-allocate heap variables
	xcompare_lists_del, data, path
	; Destroy group leader if necessary
	if  n_elements(group) eq 0  then $
	   widget_control, group_id, /DESTROY
	return
end
