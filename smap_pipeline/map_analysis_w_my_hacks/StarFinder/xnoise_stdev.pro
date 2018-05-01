; $Id: xnoise_stdev, v 1.1 Apr 2000 e.d. $
;
;+
; NAME:
;	XNOISE_STDEV
;
; PURPOSE:
;	Widget interface for the GAUSSIAN_NOISE_STD procedure.
;	Compute the standard deviation of the gaussian noise in an image.
;
; CATEGORY:
;	Widgets. Signal processing.
;
; CALLING SEQUENCE:
;	XNOISE_STDEV, Image, Stdev
;
; INPUTS:
;	Image: 2D data array
;
; KEYWORD PARAMETERS:
;	WNUM: Window number of an existing window to plot the data histogram
;		and the gaussian fit. It is used only if the 'Plot' button is
;		pressed.
;
;	PATH:	Initial path for file browsing to save the histogram plot.
;
;	DEFAULT_PAR:	Structure of default parameters for the widget's form.
;
;	PLOT_PAR:	Structure of default parameters to plot histogram.
;
;	GROUP: XNoise_StDev group leader.
;
;	UVALUE: XNoise_StDev user value.
;
; OUTPUTS:
;	Stdev:	Scalar value of noise standard deviation.
;
; OPTIONAL OUTPUTS:
;	WNUM:	Set this keyword to a named variable to get the number of
;		the new graphic window created by XPlot to plot the histogram if
;		no window is defined on input.
;
;	PATH:	Set this keyword to a named variables to get the path of
;		the file selected by the user to save the histogram plot.
;
;	DEFAULT_PAR:	Set this keyword to a named variables to get the
;		structure of parameters set by the widget's user.
;
;	PLOT_PAR:	Set this keyword to a named variables to get the
;		structure of parameters used to plot the histogram (see XPlot
;		for details).
;
; SIDE EFFECTS:
;	1) Initiates the XMANAGER if it is not already running.
;	2) Call WSET to activate the graphic window identified by Wnum.
;	3) Create a new graphic window if not defined on input.
;	4) Any image displayed on the window Wnum is overwritten.
;
; RESTRICTIONS:
;	The Help menu opens the file
;	'/starfinder/xnoise_stdev_help.txt'.
;
; PROCEDURE:
;	Create and register the widget as a modal widget.
;	Then let the user define and or/modify the noise estimation options and
;	apply them to the input image. As a new estimate is obtained applying
;	the current options a dialog message appears reporting the value of the
;	estimated noise standard deviation.
;	The histogram and its fit may be plotted.
;
; MODIFICATION HISTORY:
;	Written by: Emiliano Diolaiti, September 1999
;	Updates:
;	1) Enhanced error handling in event-handler
;	   (Emiliano Diolaiti, April 2000).
;-

; XNOISE_STDEV_EVENT: XNoise_StDev event handler.

PRO xnoise_stdev_event, event

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
	      patch = form.patch
	      frac = form.frac > 0  &  if  frac ne 0  then  frac = 1/frac
	      n_std = form.n_std
	      hminsize = form.hmin
	      hmaxsize = form.hmax
	      nterms = 3 + form.nterms
	      (*(*data).par).patch = form.patch
	      (*(*data).par).frac = form.frac
	      (*(*data).par).n_std = form.n_std
	      (*(*data).par).hmin = form.hmin
	      (*(*data).par).hmax = form.hmax
	      (*(*data).par).nterms = form.nterms
	      gauss_noise_std, *(*data).image, PATCH = patch, POINT_FRAC = frac, $
	         N_STD = n_std, HIST_MINSIZE = hminsize, HIST_MAXSIZE = hmaxsize, $
	         NTERMS = nterms, mode, stdev, h, v, vm, hfit, COEFF = c
	      if  stdev ge 0  then begin
	         *(*data).stdev = stdev
	         *(*data).h = h  &  *(*data).hfit = hfit  &  *(*data).v = v
	         *(*data).b = 0
	         if  nterms gt 3  then  *(*data).b = replicate(c[3], n_elements(v))
	         if  nterms gt 4  then  *(*data).b = *(*data).b + c[4] * v
	         if  nterms gt 5  then  *(*data).b = *(*data).b + c[5] * v^2
	         msg = dialog_message(/INFO, ['Mode = ', strcompress(string(mode)), '', $
	         					   'Standard deviation = ', strcompress(string(stdev))])
	      endif else $
	         msg = dialog_message(/INFO, ['Computation failed.', $
	         					   'Try with a different fitting model.'])
	      end
	   'plot': begin
	      if  n_elements(*(*data).h) ne 0  then begin
	         if  n_elements(*(*data).b) gt 1  then  b = *(*data).b
	         xplot, *(*data).h, *(*data).v, OVERPLOT1 = *(*data).hfit, $
	         		OVERPLOT2 = b, WNUM = *(*data).wnum, PATH = *(*data).path, $
	         		DEFAULT_PAR = *(*data).plot_par, GROUP = event.top
	      endif
	      end
	   'help': $
	      xdispfile, file_name('starfinder', 'xnoise_stdev_help.txt'), $
	      		 TITLE = 'XNoise_StDev help', /MODAL
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

; XNOISE_STDEV_DEF: define XNoise_StDev data structure.

FUNCTION xnoise_stdev_def, par, plot_par, image, wnum, path

	return, {par: ptr_new(par, /NO_COPY), $
			 plot_par: ptr_new(plot_par, /NO_COPY), $
			 image: ptr_new(image, /NO_COPY), $
			 wnum: ptr_new(wnum, /NO_COPY), $
			 path: ptr_new(path, /NO_COPY), $
			 stdev: ptr_new(/ALLOCATE), $
			 h: ptr_new(/ALLOCATE), hfit: ptr_new(/ALLOCATE), $
			 v: ptr_new(/ALLOCATE), b: ptr_new(/ALLOCATE)}
end

; XNOISE_STDEV_DEL: de-reference XNoise_StDev data structure.

PRO xnoise_stdev_del, data, par, plot_par, image, wnum, stdev, path

	if  n_elements(*(*data).par) ne 0  then  par = *(*data).par
	if  n_elements(*(*data).plot_par) ne 0  then  plot_par = *(*data).plot_par
	if  n_elements(*(*data).image) ne 0  then  image = *(*data).image
	if  n_elements(*(*data).wnum) ne 0  then  wnum = *(*data).wnum
	if  n_elements(*(*data).stdev) ne 0  then  stdev = *(*data).stdev
	if  n_elements(*(*data).path) ne 0  then  path = *(*data).path
	ptr_free, (*data).par, (*data).plot_par, (*data).image, (*data).wnum, $
			  (*data).stdev, (*data).path, (*data).h, (*data).hfit, (*data).v, (*data).b
	ptr_free, data
	return
end

; XNOISE_STDEV_PAR: define default parameters.

PRO xnoise_stdev_par, id, par

	if  n_elements(par) ne 0  then begin
	   patch = par.patch
	   frac = par.frac
	   n_std = par.n_std
	   hmin = par.hmin
	   hmax = par.hmax
	   nterms = par.nterms
	endif else begin
	   patch = 3
	   frac = 1.
	   n_std = 3
	   hmin = 5
	   hmax = 5
	   nterms = 0
	   par = {patch: patch, frac: frac, n_std: n_std, $
			  hmin: hmin, hmax: hmax, nterms: nterms}
	endelse
	init = {patch: strcompress(string(patch), /REMOVE_ALL), $
			frac: strcompress(string(frac), /REMOVE_ALL), $
			n_std: strcompress(string(n_std), /REMOVE_ALL), $
			hmin: strcompress(string(hmin), /REMOVE_ALL), $
			hmax: strcompress(string(hmax), /REMOVE_ALL), $
			nterms: nterms}
	widget_control, id, SET_VALUE = init
	return
end

; XNOISE_STDEV: XNoise_StDev widget definition module.

PRO xnoise_stdev, image, stdev, WNUM = wnum, PATH = path, $
				  DEFAULT_PAR = par, PLOT_PAR = plot_par, $
				  GROUP = group, UVALUE = uvalue

	on_error, 2
	; Create group leader if necessary
	if  n_elements(group) eq 0  then $
	   group_id = widget_base()  else  group_id = group
	; Create modal base
	if  n_elements(uvalue) eq 0  then  uvalue = 0B
	base = widget_base(TITLE = 'XNoise_StDev', /MODAL, UVALUE = uvalue, $
					   GROUP_LEADER = group_id)
	; Define form
	desc = [ $
	'0, LABEL,Pre-processing:,LEFT', $
	'1, BASE,,COLUMN,FRAME', $
	'0, INTEGER,,LABEL_LEFT=Patch size for median subtraction,TAG=patch', $
	'0, FLOAT,,LABEL_LEFT=Fraction of data point to use,TAG=frac', $
	'2, FLOAT,,LABEL_LEFT=Threshold to reject out-liers (St. Dev. units),TAG=n_std', $
	'0, LABEL,Histogram computation:,LEFT', $
	'1, BASE,,COLUMN,FRAME', $
	'0, FLOAT,,LABEL_LEFT=Minimum number of bins in one HWHM,TAG=hmin', $
	'2, FLOAT,,LABEL_LEFT=Size of histogram (FWHM units),TAG=hmax', $
	'0, LABEL,Histogram fitting:,LEFT', $
	'1, BASE,,COLUMN,FRAME', $
	'2, BUTTON,gaussian|gaussian + costant|gaussian + linear|gaussian + quadratic,' + $
	   'EXCLUSIVE,TAG=nterms,COLUMN', $
	'1, BASE,,ROW', $
	'0, BUTTON,Processing...,NO_RELEASE,TAG=process', $
	'2, BUTTON,Plot histogram,NO_RELEASE,TAG=plot', $
	'1, BASE,,ROW', $
	'0, BUTTON,Help,NO_RELEASE,TAG=help', $
	'2, BUTTON,Exit,QUIT,NO_RELEASE,TAG=exit']
	form = cw_form(base, desc, /COLUMN)
	xnoise_stdev_par, form, par
	; Define pointer to auxiliary/output data
	data = ptr_new(xnoise_stdev_def(par, plot_par, image, wnum, path), /NO_COPY)
	widget_control, form, SET_UVALUE = data
	; Realize, register, etc.
	widget_control, base, /REALIZE
	xmanager, 'xnoise_stdev', base, EVENT_HANDLER = 'xnoise_stdev_event'
	; De-reference output data
	xnoise_stdev_del, data, par, plot_par, image, wnum, stdev, path
	; Destroy group leader if necessary
	if  n_elements(group) eq 0  then $
	   widget_control, group_id, /DESTROY
	return
end
