; $Id: xnoise, v 1.1 Apr 2000 e.d. $
;
;+
; NAME:
;	XNOISE
;
; PURPOSE:
;	Widget interface to estimate the noise standard deviation for each
;	pixel of a given image.
;	The overall variance is the sum of the photon noise and of the
;	variances due to the following noise sources:
;	1) read-out noise
;	2) dark current
;	3) thermal background
;	4) sky
;	Noise source 1) follows a gaussian distribution of 0 mean.
;	Noise sources 2), 3) and 4) strictly follow a Poisson distribution,
;	but since they may be often considered roughly constant over the frame
;	the Poisson distribution may be approximated by a gaussian one,
;	spatially uniform across the frame itself.
;	While noise source 1) introduces no positive bias in the recorded signal,
;	sources 2), 3) and 4) have nonzero mean. In general the mean dark
;	current level, the overall thermal background and the mean sky are
;	subtracted in the pre-processing phase.
;	The photon noise is estimated on the basis of the recorded signal:
;	for this reason, the dark current level and the thermal background
;	should be removed beforehand. The gaussian noise standard deviation is
;	estimated as a function of the noise parameters 1) to 4), but it may be
;	optionally computed by means of the XNoise_StDev widget program.
;	The simplest way to use this widget program is to estimate the gaussian
;	noise in the data, by means of XNoise_StDev, without considering the
;	contribution of Poisson noise: in this way no other parameters must be
;	supplied on input in the widget form. The retrieved noise estimate
;	roughly represents the standard deviation of the 'mean intensity level'
;	in the image: in a crowded stellar field this quantity is the major
;	source of noise and may be associated to the background emission due
;	to nebulosities, bright stars haloes and light from faint unresolved
;	sources.
;
; CATEGORY:
;	Widgets. Signal processing.
;
; CALLING SEQUENCE:
;	XNOISE, Image, Noise_std
;
; INPUTS:
;	Image: 2D data array
;
; KEYWORD PARAMETERS:
;	WNUM: Window number of an existing window used by XNoise_StDev
;		when the gaussian noise standard deviation must be computed
;		since no input estimate is available (see keyword STDEV).
;
;	PATH:	Initial path for file browsing. If the argument is a named
;		variable, its value is overwritten.
;
;	DEFAULT_PAR:	Structure of default parameters for the widget's form.
;
;	G_NOISE_PAR:	Structure of default parameters to compute the gaussian
;		noise standard deviation (see XNoise_StDev for details).
;
;	PLOT_PAR:	Structure of default parameters to plot the data histogram
;		(see XNoise_StDev and XPlot for details).
;
;	GROUP: XNoise group leader.
;
;	UVALUE: XNoise user value.
;
; OUTPUTS:
;	Noise_std:	2D array, with the same size as the input Image, containing
;		the noise standard deviation for each Image pixel.
;
; OPTIONAL OUTPUTS:
;	STDEV:	Standard deviation of gaussian noise, computed by XNoise,
;		either as a function of the input values of read-out noise, dark
;		current, etc. or as computed by means of XNoise_StDev.
;
;	WNUM: Window number of new graphic window, created by XNoise_StDev if
;		undefined on input.
;
;	DEFAULT_PAR:	Set this keyword to a named variable to get the
;		structure of parameters set by the widget's user.
;
;	G_NOISE_PAR:	Set this keyword to a named variable to get the
;		structure of default parameters used by XNoise_StDev.
;
;	PLOT_PAR:	Set this keyword to a named variable to get the
;		structure of parameters used by XPlot through XNoise_StDev.
;
; SIDE EFFECTS:
;	1) Initiates the XMANAGER if it is not already running.
;	2) Might create a new graphic window, if WNUM is undefined on input
;	   (see XNoise_StDev for details).
;
; RESTRICTIONS:
;	The Help menu opens the file
;	'/starfinder/xnoise_help.txt'.
;
; PROCEDURE:
;	Create and register the widget as a modal widget.
;	Then let the user define the parameters used to estimate the noise
;	standard deviation. The resulting array of noise may be saved to a
;	FITS file.
;
; MODIFICATION HISTORY:
;	Written by: Emiliano Diolaiti, October 1999
;	1) Enhanced error handling in event-handler
;	   (Emiliano Diolaiti, April 2000).
;-

; XNOISE_EVENT: XNoise event handler.

PRO xnoise_event, event

	catch, error
	if  error ne 0  then begin
	   msg = dialog_message(/ERROR, !err_string)
	   widget_control, event.id, SET_UVALUE = data, /NO_COPY
	   return
	endif
	widget_control, event.id, GET_UVALUE = data, /NO_COPY
	event_type = strlowcase(event.tag)
	case  event_type  of
	   'eval': begin
	      widget_control, event.id, GET_VALUE = form
	      for  id = 3, 6  do $
	         widget_control, (*data).ids[id], $
	         SENSITIVE = (form.eval eq 0) and 1B
	      for  id = 12, 14  do $
	         widget_control, (*data).ids[id], $
	         SENSITIVE = (form.eval eq 0 or form.phnoise eq 1) and 1B
	      end
	   'phnoise': begin
	      widget_control, event.id, GET_VALUE = form
	      for  id = 3, 6  do $
	         widget_control, (*data).ids[id], $
	         SENSITIVE = (form.eval eq 0) and 1B
	      for  id = 12, 14  do $
	         widget_control, (*data).ids[id], $
	         SENSITIVE = (form.eval eq 0 or form.phnoise eq 1) and 1B
	      end
	   'compute': begin
	      ; Read out form
	      widget_control, event.id, GET_VALUE = form
	      nexp = form.nexp > 1
	      el_per_adu = form.el_per_adu > 0
	      ron = form.ron > 0
	      dark = form.dark > 0
	      therm = form.therm > 0
	      sky = form.sky > 0
	      avg = form.avg eq 0 and 1B
	      norm = 1.  &  if  avg  then  norm = norm/nexp
	      eval = form.eval eq 1 and 1B
	      phnoise = (form.phnoise eq 1) and 1B
	      ; Store parameters
	      (*(*data).par).nexp = form.nexp
	      (*(*data).par).el_per_adu = form.el_per_adu
	      (*(*data).par).ron = form.ron
	      (*(*data).par).dark = form.dark
	      (*(*data).par).therm = form.therm
	      (*(*data).par).sky = form.sky
	      (*(*data).par).avg = form.avg
	      (*(*data).par).eval = form.eval
	      (*(*data).par).phnoise = form.phnoise
	      ; Compute noise
	      if  eval  then begin
	         msg = dialog_message(/INFO, $
	         ['Calling XNnoise_StDev to estimate gaussian noise.', $
	         							  'Press OK to continue.'])
	         xnoise_stdev, *(*data).image, stdev, WNUM = *(*data).wnum, $
	         			   DEFAULT_PAR = *(*data).g_noise_par, $
	         			   PLOT_PAR = *(*data).plot_par, $
	         			   PATH = *(*data).path, GROUP = event.top
	      endif else $
	         stdev = instr_noise(ron, dark, therm, sky, el_per_adu, nexp, AVG = avg)
	      if  n_elements(stdev) ne 0  then  *(*data).stdev = stdev  else  stdev = 0.
	      if  phnoise  then begin
	         widget_control, /HOURGLASS
	         phn = photon_noise(*(*data).image, el_per_adu, nexp, AVG = avg)
	         *(*data).noise_std = sqrt(phn^2 + stdev^2)
	         if  not eval  then  msg = dialog_message(/INFO, 'Done.')
	      endif else $
	      if  n_elements(*(*data).stdev) ne 0  then begin
	         widget_control, /HOURGLASS
	         siz = size52(*(*data).image, /DIM)
	         *(*data).noise_std = replicate(stdev[0], siz[0], siz[1])
	         if  not eval  then  msg = dialog_message(/INFO, 'Done.')
	      endif
	      end
	   'save': $
	      if  n_elements(*(*data).noise_std) ne 0  then begin
	         file = dialog_pickfile(/WRITE, FILTER = '*.fits', $
	      						    PATH = *(*data).path, GET_PATH = path)
	         if  file ne ''  then begin
	            widget_control, /HOURGLASS
	            if  strpos(file, '.fits') lt 0  then  file = file + '.fits'
	            *(*data).path = path
	            writefits, file, *(*data).noise_std
	         endif
	      endif
	   'help': $
	      xdispfile, file_name('starfinder', 'xnoise_help.txt'), $
	                 TITLE = 'XNoise help', /MODAL
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

; XNOISE_DEF: define XNoise data structure.

FUNCTION xnoise_def, par, g_noise_par, plot_par, ids, image, wnum, path

	return, {par: ptr_new(par, /NO_COPY), $
			 g_noise_par: ptr_new(g_noise_par, /NO_COPY), $
			 plot_par: ptr_new(plot_par, /NO_COPY), $
			 image: ptr_new(image, /NO_COPY), $
			 wnum: ptr_new(wnum, /NO_COPY), $
			 path: ptr_new(path, /NO_COPY), $
			 stdev: ptr_new(/ALLOCATE), $
			 noise_std: ptr_new(/ALLOCATE), $
			 ids: ids}
end

; XNOISE_DEL: de-reference XNoise data structure.

PRO xnoise_del, data, par, g_noise_par, plot_par, image, wnum, stdev, noise_std, path

	if  n_elements(*(*data).par) ne 0  then  par = *(*data).par
	if  n_elements(*(*data).g_noise_par) ne 0  then  g_noise_par = *(*data).g_noise_par
	if  n_elements(*(*data).plot_par) ne 0  then  plot_par = *(*data).plot_par
	if  n_elements(*(*data).image) ne 0  then  image = *(*data).image
	if  n_elements(*(*data).wnum) ne 0  then  wnum = *(*data).wnum
	if  n_elements(*(*data).stdev) ne 0  then  stdev = *(*data).stdev
	if  n_elements(*(*data).noise_std) ne 0  then  noise_std = *(*data).noise_std
	if  n_elements(*(*data).path) ne 0  then  path = *(*data).path
	ptr_free, (*data).par, (*data).g_noise_par, (*data).plot_par, $
			  (*data).image, (*data).wnum, (*data).stdev, $
			  (*data).noise_std, (*data).path
	ptr_free, data
	return
end

; XNOISE_PAR: define default parameters.

PRO xnoise_par, id, par

	if  n_elements(par) ne 0  then begin
	   nexp = par.nexp
	   el_per_adu = par.el_per_adu
	   ron = par.ron
	   dark = par.dark
	   therm = par.therm
	   sky = par.sky
	   avg = par.avg
	   eval = par.eval
	   phnoise = par.phnoise
	endif else begin
	   nexp = 1
	   el_per_adu = 1.
	   ron = 0.
	   dark = 0.
	   therm = 0.
	   sky = 0.
	   avg = 0
	   eval = 1
	   phnoise = 0
	   par = {nexp: nexp, el_per_adu: el_per_adu, ron: ron, dark: dark, $
			  therm: therm, sky: sky, avg: avg, eval: eval, phnoise: phnoise}
	endelse
	init = {nexp: strcompress(string(nexp), /REMOVE_ALL), $
			el_per_adu: strcompress(string(el_per_adu), /REMOVE_ALL), $
			ron: strcompress(string(ron), /REMOVE_ALL), $
			dark: strcompress(string(dark), /REMOVE_ALL), $
			therm: strcompress(string(therm), /REMOVE_ALL), $
			sky: strcompress(string(sky), /REMOVE_ALL), $
			avg: avg, eval: eval, phnoise: phnoise}
	widget_control, id, SET_VALUE = init
	return
end

; XNOISE: XNoise widget definition module.

PRO xnoise, image, noise_std, WNUM = wnum, STDEV = stdev, $
			PATH = path, DEFAULT_PAR = par, G_NOISE_PAR = g_noise_par, $
			PLOT_PAR = plot_par, GROUP = group, UVALUE = uvalue

	on_error, 2
	; Create group leader if necessary
	if  n_elements(group) eq 0  then $
	   group_id = widget_base()  else  group_id = group
	; Create modal base
	if  n_elements(uvalue) eq 0  then  uvalue = 0B
	base = widget_base(TITLE = 'XNoise', /MODAL, UVALUE = uvalue, $
					   GROUP_LEADER = group_id)
	; Define form
	desc = [ $
	'0, LABEL,Gaussian noise:,LEFT', $
	'1, BASE,,COLUMN,FRAME', $
	'0, BUTTON,no|yes,EXCLUSIVE,LABEL_LEFT=Evaluate from data:,TAG=eval,ROW', $
	'0, FLOAT,,LABEL_LEFT=Read-out-noise (electrons),TAG=ron,WIDTH=8', $
	'0, FLOAT,,LABEL_LEFT=Dark current (electrons),TAG=dark,WIDTH=8', $
	'0, FLOAT,,LABEL_LEFT=Thermal background (electrons),TAG=therm,WIDTH=8', $
	'2, FLOAT,,LABEL_LEFT=Sky (ADU),TAG=sky,WIDTH=8', $
	'0, LABEL,Photon noise:,LEFT', $
	'1, BASE,,COLUMN,FRAME', $
	'2, BUTTON,no|yes,EXCLUSIVE,,LABEL_LEFT=Consider photon noise,TAG=phnoise,ROW', $
	'0, LABEL,General parameters:,LEFT', $
	'1, BASE,,COLUMN,FRAME', $
	'0, INTEGER,,LABEL_LEFT=Number of exposures,TAG=nexp,WIDTH=8', $
	'0, BUTTON,mean|sum,EXCLUSIVE,,LABEL_LEFT=Exposures combined by:,TAG=avg,ROW', $
	'2, FLOAT,,LABEL_LEFT=Electrons/ADU,TAG=el_per_adu,WIDTH=8', $
	'1, BASE,,ROW', $
	'0, BUTTON,Compute,NO_RELEASE,TAG=compute', $
	'2, BUTTON,Save,NO_RELEASE,TAG=save', $
	'1, BASE,,ROW', $
	'0, BUTTON,Help,NO_RELEASE,TAG=help', $
	'2, BUTTON,Exit,QUIT,NO_RELEASE,TAG=exit']
	form = cw_form(base, desc, IDS = ids, /COLUMN)
	xnoise_par, form, par
	for  id = 3, 6  do $
	   widget_control, ids[id], SENSITIVE = (par.eval eq 0) and 1B
	for  id = 12, 14  do $
	   widget_control, ids[id], SENSITIVE = (par.eval eq 0 or par.phnoise eq 1) and 1B
	; Define pointer to auxiliary/output data
	data = ptr_new(xnoise_def(par, g_noise_par, plot_par, ids, image, wnum, path), /NO_COPY)
	widget_control, form, SET_UVALUE = data
	; Realize, register, etc.
	widget_control, base, /REALIZE
	xmanager, 'xnoise', base, EVENT_HANDLER = 'xnoise_event'
	; De-reference output data
	xnoise_del, data, par, g_noise_par, plot_par, image, wnum, stdev, noise_std, path
	; Destroy group leader if necessary
	if  n_elements(group) eq 0  then $
	   widget_control, group_id, /DESTROY
	return
end
