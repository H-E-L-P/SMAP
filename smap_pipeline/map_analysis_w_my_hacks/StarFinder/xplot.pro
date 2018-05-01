; $Id: xplot, v 1.1 Apr 2000 e.d. $
;
;+
; NAME:
;	XPLOT
;
; PURPOSE:
;	Widget interface to execute simple plots.
;
; CATEGORY:
;	Widgets. Graphics.
;
; CALLING SEQUENCE:
;	XPLOT, Y, X
;
; INPUTS:
;	Y:	1D vector of ordinates to plot.
;
; OPTIONAL INPUTS:
;	X:	1D vector of abscissae. The default is
;		X = [0, 1, ..., N - 1], where N is the number of elements in Y.
;
; KEYWORD PARAMETERS:
;	OVERPLOT1:	1D vector of additional values to plot over Y.
;
;	OVERPLOT2:	1D vector of additional values to plot over Y.
;
;	WNUM:	Window number of an existing window.
;
;	PATH:	Initial path for file browsing to save plot.
;
;	DEFAULT_PAR:	Structure of default parameters for the widget's form.
;
;	GROUP: XPlot group leader.
;
;	UVALUE: XPlot user value.
;
; OUTPUTS:
;	No particular output, except the plot which may be saved on a file.
;
; OPTIONAL OUTPUTS:
;	WNUM:	Set this keyword to a named variable to get the window number
;		of the new created window, if not existing on input.
;
;	DEFAULT_PAR:	Set this keyword to a named variable to get the
;		structure of parameters defined by the widget's user.
;
;	PATH:	Set this keyword to a named variable to get the path of the
;		file selected by the user to save the plot.
;
; SIDE EFFECTS:
;	1) Initiates the XMANAGER if it is not already running.
;	2) Call WSET to activate the graphic window identified by Wnum.
;	3) Create a new graphic window if not defined on input.
;	4) Any previous display on the window Wnum is overwritten.
;
; RESTRICTIONS:
;	1) The Help menu opens the file
;	'/starfinder/xplot_help.txt'.
;	2) The plot may be saved only as a GIF file.
;
; PROCEDURE:
;	Create and register the widget as a modal widget. Then let the user
;	define and or/modify the plotting options and apply them.
;
; MODIFICATION HISTORY:
;	Written by: Emiliano Diolaiti, September 1999.
;	Updates:
;	1) Changed output from PS to GIF (Emiliano Diolaiti, Apr 2000).
;	2) Enhanced error handling in event-handler
;	   (Emiliano Diolaiti, April 2000).
;-

; XPLOT_LP: determine line and point style.

PRO xplot_lp, type, line, psym

	case  type  of
	   0: line = 0
	   1: line = 2
	   2: line = 1
	   3: psym = 10
	   4: psym = 3
	   5: psym = 1
	endcase
	return
end

; XPLOT_DO: plot data for XPlot.

PRO xplot_do, x, y, y1, y2, psym1, line1, psym2, line2, $
			  xmin, xmax, ymin, ymax, title, xtitle, ytitle, $
			  chsize, symsize

	on_error, 2
	font = !P.Font  &  !P.Font = -1
	plot, x, y, PSYM = psym1, LINE = line1, $
	      XRANGE = [xmin, xmax], YRANGE = [ymin, ymax], $
	      TITLE = title, XTITLE = xtitle, YTITLE = ytitle, $
	      CHARSIZE = chsize, SYMSIZE = symsize
	if  n_elements(y1) gt 1  then  oplot, x, y1, PSYM = psym2, LINE = line2
	if  n_elements(y2) gt 1  then  oplot, x, y2, PSYM = psym2, LINE = line2
	!P.Font = font
	return
end

; XPLOT_EVENT: XPlot event handler.

PRO xplot_event, event

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
	      xmin = form.xmin  &  xmax = form.xmax
	      ymin = form.ymin  &  ymax = form.ymax
	      title = form.title  &  xtitle = form.xtitle  &  ytitle = form.ytitle
	      chsize = form.chsize > 0  &  symsize = form.symsize > 0
	      type1 = form.type1  &  type2 = form.type2
	      xplot_lp, type1, line1, psym1
	      if  n_elements(*(*data).y1) ne 0 or n_elements(*(*data).y2) ne 0  then $
	         xplot_lp, type2, line2, psym2
	      *(*data).par = {xmin: xmin, xmax: xmax, ymin: ymin, ymax: ymax, $
	   					  chsize: chsize, symsize: symsize, title: title, $
	   					  xtitle: xtitle, ytitle: ytitle, type1: type1, type2: type2}
	      if  n_elements(*(*data).wnum) eq 0  then begin
	         window, /FREE  &  *(*data).wnum = !D.window
	      endif else  wset, *(*data).wnum
	      xplot_do, *(*data).x, *(*data).y, *(*data).y1, *(*data).y2, $
	      			psym1, line1, psym2, line2, xmin, xmax, ymin, ymax, $
	      			title, xtitle, ytitle, chsize, symsize
	      end
	   'save': begin
	      if  n_elements(*(*data).wnum) ne 0 and n_elements(*(*data).par) ne 0  then begin
	         file = dialog_pickfile(/WRITE, FILTER = '*.gif', $
	         						PATH = *(*data).path, GET_PATH = path)
	         if  file ne ''  then begin
	            widget_control, /HOURGLASS
	            if  strpos(file, '.gif') lt 0  then  file = file + '.gif'
	            *(*data).path = path
	            write_gif, file, bytscl(255 - tvrd())
	         endif
	      endif
	      end
	   'help': $
	      xdispfile, file_name('starfinder', 'xplot_help.txt'), $
	                 TITLE = 'XPlot help', /MODAL
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

; XPLOT_DEF: define XPlot data structure.

FUNCTION xplot_def, par, x, y, y1, y2, wnum, path

	return, {par: ptr_new(par, /NO_COPY), $
			 x: ptr_new(x, /NO_COPY), y: ptr_new(y, /NO_COPY), $
			 y1: ptr_new(y1, /NO_COPY), y2: ptr_new(y2, /NO_COPY), $
			 wnum: ptr_new(wnum, /NO_COPY), path: ptr_new(path, /NO_COPY)}
end

; XPLOT_DEL: de-reference XPlot data structure.

PRO xplot_del, data, par, x, y, y1, y2, wnum, path

	if  n_elements(*(*data).par) ne 0  then  par = *(*data).par
	if  n_elements(*(*data).x) ne 0  then  x = *(*data).x
	if  n_elements(*(*data).y) ne 0  then  y = *(*data).y
	if  n_elements(*(*data).y1) ne 0  then  y1 = *(*data).y1
	if  n_elements(*(*data).y2) ne 0  then  y2 = *(*data).y2
	if  n_elements(*(*data).wnum) ne 0  then  wnum = *(*data).wnum
	if  n_elements(*(*data).path) ne 0  then  path = *(*data).path
	ptr_free, (*data).par, (*data).x, (*data).y, (*data).y1, (*data).y2, $
			  (*data).wnum, (*data).path
	ptr_free, data
	return
end

; XPLOT_PAR: define default parameters.

PRO xplot_par, id, par, y, x

	if  n_elements(par) ne 0  then begin
	   xmin = par.xmin
	   xmax = par.xmax
	   ymin = par.ymin
	   ymax = par.ymax
	   chsize = par.chsize
	   symsize = par.symsize
	   title = par.title
	   xtitle = par.xtitle
	   ytitle = par.ytitle
	   type1 = par.type1
	   type2 = par.type2
	endif else begin
	   xmin = min(x)
	   xmax = max(x)
	   ymin = min(y)
	   ymax = max(y)
	   chsize = 1.
	   symsize = 1.
	   title = ''
	   xtitle = ''
	   ytitle = ''
	   type1 = 0
	   type2 = 1
	   par = {xmin: xmin, xmax: xmax, ymin: ymin, ymax: ymax, $
	   		  chsize: chsize, symsize: symsize, title: title, $
	   		  xtitle: xtitle, ytitle: ytitle, type1: type1, type2: type2}
	endelse
	init = {xmin: strcompress(string(xmin), /REMOVE_ALL), $
			xmax: strcompress(string(xmax), /REMOVE_ALL), $
			ymin: strcompress(string(ymin), /REMOVE_ALL), $
			ymax: strcompress(string(ymax), /REMOVE_ALL), $
			type1: type1, type2: type2, $
			chsize: strcompress(string(chsize), /REMOVE_ALL), $
			symsize: strcompress(string(symsize), /REMOVE_ALL), $
			title: title, xtitle: xtitle, ytitle: ytitle}
	widget_control, id, SET_VALUE = init
	return
end

; XPLOT: XPlot widget definition module.

PRO xplot, y, x, OVERPLOT1 = y1, OVERPLOT2 = y2, WNUM = wnum, $
		   PATH = path, DEFAULT_PAR = par, GROUP = group, UVALUE = uvalue

	on_error, 2
	if  n_elements(y) eq 0  then begin
	   msg = dialog_message(/ERROR, 'XPlot: missing data.')
	   return
	endif
	if  n_elements(x) eq 0  then  x = findgen(n_elements(y))
	; Create group leader if necessary
	if  n_elements(group) eq 0  then $
	   group_id = widget_base()  else  group_id = group
	; Create modal base
	if  n_elements(uvalue) eq 0  then  uvalue = 0B
	base = widget_base(TITLE = 'XPlot', /MODAL, UVALUE = uvalue, $
					   GROUP_LEADER = group_id)
	; Define form
	desc = [ $
	'0, LABEL,Range:,LEFT', $
	'1, BASE,,COLUMN,FRAME', $
	'1, BASE,,ROW', $
	'0, FLOAT,,LABEL_LEFT=X lower,WIDTH=12,TAG=xmin', $
	'2, FLOAT,,LABEL_LEFT=X upper,WIDTH=12,TAG=xmax', $
	'1, BASE,,ROW', $
	'0, FLOAT,,LABEL_LEFT=Y lower,WIDTH=12,TAG=ymin', $
	'2, FLOAT,,LABEL_LEFT=Y upper,WIDTH=12,TAG=ymax', $
	'2, BASE,,', $
	'0, LABEL,Labels:,LEFT', $
	'1, BASE,,COLUMN,FRAME', $
	'0, TEXT,,LABEL_LEFT=Main title,WIDTH=12,TAG=title', $
	'0, TEXT,,LABEL_LEFT=X-title,WIDTH=12,TAG=xtitle', $
	'2, TEXT,,LABEL_LEFT=Y-title,WIDTH=12,TAG=ytitle', $
	'0, LABEL,Type:,LEFT', $
	'1, BASE,,ROW,FRAME', $
	'1, BASE,,COLUMN', $
	'0, LABEL,Main plot:,LEFT', $
	'2, BUTTON,Continuous line|Dashed line|Dotted line|Steps|Dot|Plus sign,' + $
	   'EXCLUSIVE,SET_VALUE=0,COLUMN,NO_RELEASE,TAG=type1', $
	'1, BASE,,COLUMN', $
	'0, LABEL,Overplot:,LEFT', $
	'2, BUTTON,Continuous line|Dashed line|Dotted line|Steps|Dot|Plus sign,' + $
	   'EXCLUSIVE,SET_VALUE=2,COLUMN,NO_RELEASE,TAG=type2', $
	'2, BASE,,', $
	'0, LABEL,Style:,LEFT', $
	'1, BASE,,COLUMN,FRAME', $
	'0, FLOAT,,LABEL_LEFT=Character size,WIDTH=8,TAG=chsize', $
	'2, FLOAT,,LABEL_LEFT=Symbol size,WIDTH=8,TAG=symsize', $
	'1, BASE,,ROW', $
	'2, BUTTON,Plot,NO_RELEASE,TAG=process', $
	'1, BASE,,ROW', $
	'0, BUTTON,Save,NO_RELEASE,TAG=save', $
	'0, BUTTON,Help,NO_RELEASE,TAG=help', $
	'2, BUTTON,Exit,QUIT,NO_RELEASE,TAG=exit']
	form = cw_form(base, desc, IDS = ids, /COLUMN)
	xplot_par, form, par, y, x
	widget_control, ids[21], $
	   SENSITIVE = (n_elements(y1) ne 0 or n_elements(y2) ne 0) and 1B
	; Define pointer to auxiliary/output data
	data = ptr_new(xplot_def(par, x, y, y1, y2, wnum, path), /NO_COPY)
	widget_control, form, SET_UVALUE = data
	; Realize, register, etc.
	widget_control, base, /REALIZE
	xmanager, 'xplot', base, EVENT_HANDLER = 'xplot_event'
	; De-reference output data
	xplot_del, data, par, x, y, y1, y2, wnum, path
	; Destroy group leader if necessary
	if  n_elements(group) eq 0  then $
	   widget_control, group_id, /DESTROY
	return
end
