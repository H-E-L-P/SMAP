; $Id: xstarfinder_run, v 1.2 Aug 2004 e.d. $
;
;+
; NAME:
;	XSTARFINDER_RUN
;
; PURPOSE:
;	Widget interface for the STARFINDER procedure.
;	Detect stars in a stellar field and determine astrometry and photometry.
;
; CATEGORY:
;	Widgets. Signal processing.
;
; CALLING SEQUENCE:
;	XSTARFINDER_RUN, Image, Psf, Psf_fwhm, Background, X, Y, F, Sx, Sy, Sf, C
;
; INPUTS:
;	Image:	Stellar field
;
;	Psf:	2D array, containing the Point Spread Function.
;
;	Psf_fwhm:	PSF FWHM.
;
;	Background:	2D array, containing an initial guess of the image background
;
;	Back_Box:	Box size to compute the background.
;
; KEYWORD PARAMETERS:
;	X_BAD, Y_BAD:	Coordinates of bad pixels.
;
;	PATH:	Initial path for file browsing when saving the results.
;		If the argument of the keyword is a named variable, its value
;		is overwritten.
;
;	DEFAULT_PAR:	Structure of default parameters for the widget's form.
;
;	GROUP: XPsf_Extract group leader.
;
;	UVALUE: XPsf_Extract user value.
;
; OUTPUTS:
;	Background:	2D array, containing the updated image background
;
;	X, Y:	Positions of detected stars. Origin is at (0, 0).
;
;	F:	Stellar fluxes, referred to the normalization of the input Psf.
;
;	Sx, Sy, Sf:	Formal errors on astrometry and photometry. Available
;		only if the necessary information on the image noise are supplied.
;
;	C:	Correlation coefficient of each detected star. The objects detected
;		and analyzed as 'blends' have the correlation coefficient set to
;		-1 by default.
;
; OPTIONAL OUTPUTS:
;	NOISE_STD:	2D array, containing an estimate of the noise standard
;		deviation for each pixel in the input image. If undefined on input,
;		it can be read from file.
;
;	STARS:	Image model released by StarFinder, given by a superposition
;		of shifted scaled PSFs, one for eaach detected star.
;
;	DEFAULT_PAR:	Set this keyword to a named variable to get the
;		structure of parameters set by the widget's user.
;
; SIDE EFFECTS:
;	Initiates the XMANAGER if it is not already running.
;
; RESTRICTIONS:
;	The Help menu opens the file
;	'/starfinder/xstarfinder_run_help.txt'.
;
; PROCEDURE:
;	Create and register the widget as a modal widget.
;	Then let the user define and or/modify the analysis options and apply
;	them to the input image. The results may be saved on a text a file.
;
; MODIFICATION HISTORY:
;	Written by: Emiliano Diolaiti, September 1999
;	Updates:
;	1) Enhanced error handling in event-handler
;	   (Emiliano Diolaiti, April 2000).
;   2) BACKGROUND and BACK_BOX are now parameters (E. D., August 2004).
;   3) Removed DEBLEND options (E. D., August 2004).
;-

; XSTARFINDER_RUN_EVENT: XStarFinder_Run event handler.

PRO xstarfinder_run_event, event

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
	case event_id of
	   id.noise: begin
	      widget_control, id.noise, GET_VALUE = noise
	      (*data).par.noise = noise
	      widget_control, id.rel_thresh, GET_VALUE = rel_thresh
	      rel_thresh = (rel_thresh eq 1 and noise eq 1) and 1B
	      (*data).par.rel_thresh = rel_thresh
	      widget_control, id.rel_thresh, SET_VALUE = rel_thresh, $
	      				  SENSITIVE = (noise eq 1) and 1B
	      end
	   id.proc: begin
	      ; Read out form and store parameters
	      widget_control, id.dthresh, GET_VALUE = threshold
	      str_threshold = threshold.tag0
	      (*data).par.dthresh = str_threshold
	      threshold = float(str_sep(strcompress(str_threshold, /REMOVE_ALL), ','))
	      widget_control, id.rel_thresh, GET_VALUE = rel_thresh
	      (*data).par.rel_thresh = rel_thresh
	      rel_thresh = (rel_thresh eq 1) and 1B
	      widget_control, id.cthresh, GET_VALUE = min_correlation
	      min_correlation = min_correlation.tag0
	      (*data).par.cthresh = min_correlation
	      widget_control, id.csub, GET_VALUE = correl_mag
	      correl_mag = correl_mag.tag0 > 1
	      (*data).par.csub = correl_mag
	      widget_control, id.noise, GET_VALUE = noise
	      (*data).par.noise = noise
	      if  noise eq 1  then  noise_std = *(*data).noise_std
	      widget_control, id.bbox, GET_VALUE = bbox
	      bbox = bbox.tag0
	      (*data).par.bbox = bbox
	      (*data).back_box = bbox
;	      widget_control, id.deblend, GET_VALUE = deblend
;	      (*data).par.deblend = deblend
;	      deblend = (deblend eq 1) and 1B
	      widget_control, id.niter, GET_VALUE = niter
	      niter = niter.tag0
	      (*data).par.niter = niter
	      ; Call STARFINDER and store results
	      if  str_threshold eq ''  then $
	         msg = dialog_message(/ERROR, ['Enter one or more detection thresholds', $
	         			       'separated by commas.']) $
	      else begin
	         widget_control, /HOURGLASS
	         minif = round((*data).psf_fwhm / 2)
	         starfinder, *(*data).image, *(*data).psf, $
	               X_BAD = *(*data).x_bad, Y_BAD = *(*data).y_bad, $
	               BACKGROUND = *(*data).background, BACK_BOX = round(bbox * (*data).psf_fwhm), /SKY_MEDIAN, $
	               threshold, REL_THRESHOLD = rel_thresh, /PRE_SMOOTH, $
	               NOISE_STD = *(*data).noise_std, min_correlation, $
	               CORREL_MAG = correl_mag, INTERP_TYPE = 'I', $
	               DEBLEND = deblend, N_ITER = niter, /SILENT, $
	               x, y, f, sx, sy, sf, c, STARS = *(*data).stars
	         nstars = n_elements(f)
	         msg = dialog_message(/INFO, strcompress(string(nstars)) + ' detected stars.')
	         if  nstars ne 0  then begin
	            *(*data).x = [x]  &  *(*data).sx = [sx]
	            *(*data).y = [y]  &  *(*data).sy = [sy]
	            *(*data).f = [f]  &  *(*data).sf = [sf]
	            *(*data).c = [c]
	         endif
	      endelse
	      end
	   id.sav: begin
	      if  n_elements(*(*data).f) ne 0  then begin
	         file = dialog_pickfile(/WRITE, FILTER = '*.txt', $
	         			PATH = *(*data).path, GET_PATH = path)
	         if  file ne ''  then begin
	            widget_control, /HOURGLASS
	            out = [transpose(*(*data).x), $
	            	   transpose(*(*data).y), $
	            	   transpose(*(*data).f), $
	            	   transpose(*(*data).sx), $
	            	   transpose(*(*data).sy), $
	            	   transpose(*(*data).sf), $
	            	   transpose(*(*data).c)]
	            if  strpos(file, '.txt') lt 0  then  file = file + '.txt'
	            *(*data).path = path
	            openw, lun, file, /GET_LUN
	            printf, lun, out  &  free_lun, lun
	         endif
	      endif
	      end
	   id.hlp: $
	      xdispfile, file_name('starfinder', 'xstarfinder_run_help.txt'), $
	      		 TITLE = 'XStarFinder_Run help', /MODAL
	   id.ex: begin
	      widget_control, child, SET_UVALUE = data, /NO_COPY
	      widget_control, event.top, /DESTROY
	      end
	   else:
	endcase
	if  event_id ne id.ex  then $
	   widget_control, child, SET_UVALUE = data, /NO_COPY
	return
end

; XSTARFINDER_RUN_DEF: define data structure.

FUNCTION xstarfinder_run_def, id, par, image, psf, background, back_box, noise_std, $
	                          x_bad, y_bad, psf_fwhm, path

	data = {id: id, par: par, path: ptr_new(path), $
			image: ptr_new(image, /NO_COPY), $
			psf: ptr_new(psf, /NO_COPY), psf_fwhm: psf_fwhm, $
			background: ptr_new(background, /NO_COPY), back_box: back_box, $
			noise_std: ptr_new(noise_std, /NO_COPY), $
			stars: ptr_new(/ALLOCATE), $
			x_bad: ptr_new(x_bad, /NO_COPY), y_bad: ptr_new(y_bad, /NO_COPY), $
			x: ptr_new(/ALLOCATE), sx: ptr_new(/ALLOCATE), $
			y: ptr_new(/ALLOCATE), sy: ptr_new(/ALLOCATE), $
			f: ptr_new(/ALLOCATE), sf: ptr_new(/ALLOCATE), c: ptr_new(/ALLOCATE)}
	return, data
end

; XSTARFINDER_RUN_DEL: de-reference and de-allocate heap variables.

PRO xstarfinder_run_del, data, image, psf, background, back_box, noise_std, x_bad, y_bad, $
						 stars, x, y, f, sx, sy, sf, c, path, par

	if  not ptr_valid(data)  then begin
	   heap_gc  &  return
	endif
	if  n_elements(*(*data).image) ne 0  then $
	   image = *(*data).image
	if  n_elements(*(*data).psf) ne 0  then $
	   psf = *(*data).psf
	if  n_elements(*(*data).background) ne 0  then $
	   background = *(*data).background
	back_box = (*data).back_box
	if  n_elements(*(*data).noise_std) ne 0  then $
	   noise_std = *(*data).noise_std
	if  n_elements(*(*data).x_bad) ne 0  then begin
	   x_bad = *(*data).x_bad  &  y_bad = *(*data).y_bad
	endif
	if  n_elements(*(*data).stars) ne 0  then $
	   stars = *(*data).stars
	if  n_elements(*(*data).f) ne 0  then begin
	   x = *(*data).x  &  sx = *(*data).sx
	   y = *(*data).y  &  sy = *(*data).sy
	   f = *(*data).f  &  sf = *(*data).sf
	   c = *(*data).c
	endif
	if  n_elements(*(*data).path) ne 0  then  path = *(*data).path
	par = (*data).par
	ptr_free, (*data).image, (*data).psf, (*data).background, $
			  (*data).noise_std, (*data).x_bad, (*data).y_bad, $
			  (*data).stars, (*data).x, (*data).y, (*data).f, $
			  (*data).sx, (*data).sy, (*data).sf, (*data).c, $
			  (*data).path
	ptr_free, data
	return
end

; XSTARFINDER_RUN_PAR: define default parameters.

PRO xstarfinder_run_par, id, noise_def, back_box, par

	if  n_elements(par) ne 0  then begin
	   dthresh = par.dthresh
	   cthresh = par.cthresh
	   csub = par.csub
	   noise = (par.noise eq 1 and noise_def) and 1B
	   rel_thresh = (par.rel_thresh eq 1 and noise) and 1B
	   bbox = back_box
;	   deblend = par.deblend
	   niter = par.niter
	endif else begin
	   if  noise_def  then  dthresh = '3., 3.'  else  dthresh = '0., 0.'
	   rel_thresh = noise_def
	   cthresh = 0.7
	   csub = 2
	   noise = noise_def
	   bbox = back_box
;	   deblend = 0
	   niter = 2
	   par = {dthresh: dthresh, rel_thresh: rel_thresh, $
	   		  cthresh: cthresh, csub: csub, noise: noise, $
	   		  bbox: bbox, niter: niter}
;	   		  bbox: bbox, deblend: deblend, niter: niter}
	endelse
	widget_control, id.dthresh, SET_VALUE = {tag0: strcompress(string(dthresh), /REMOVE_ALL)}
	widget_control, id.rel_thresh, SET_VALUE = rel_thresh, SENSITIVE = noise and 1B
	widget_control, id.cthresh, SET_VALUE = {tag0: strcompress(string(cthresh), /REMOVE_ALL)}
	widget_control, id.csub, SET_VALUE = {tag0: strcompress(string(csub), /REMOVE_ALL)}
	widget_control, id.noise, SET_VALUE = noise, SENSITIVE = noise_def
	widget_control, id.bbox, SET_VALUE = {tag0: strcompress(string(bbox), /REMOVE_ALL)}
;	widget_control, id.deblend, SET_VALUE = deblend
	widget_control, id.niter, SET_VALUE = {tag0: strcompress(string(niter), /REMOVE_ALL)}
	return
end

; XSTARFINDER_RUN: XStarFinder_Run widget definition module.

PRO xstarfinder_run, image, psf, psf_fwhm, background, back_box, $
					 X_BAD = x_bad, Y_BAD = y_bad, NOISE_STD = noise_std, $
					 STARS = stars, x, y, f, sx, sy, sf, c, DEFAULT_PAR = par, $
					 PATH = path, GROUP = group, UVALUE = uvalue

	catch, error
	if  error ne 0  then begin
	   xstarfinder_run_del, data, image, psf, background, back_box, noise_std, x_bad, y_bad, $
						    stars, x, y, f, sx, sy, sf, c, path, par
	   if  n_elements(group) eq 0  then $
	      widget_control, group_id, /DESTROY
	   return
	endif
	; Create group leader if necessary
	if  n_elements(group) eq 0  then $
	   group_id = widget_base()  else  group_id = group
	; Create modal base
	if  n_elements(uvalue) eq 0  then  uvalue = 0B
	base = widget_base(TITLE = 'XStarFinder_Run', /MODAL, UVALUE = uvalue, $
					   GROUP_LEADER = group_id, /COLUMN)
	; Define child, to store data structure
	child = widget_base(base)
	; Define Search parameters
	lab_s = widget_label(base, VALUE = 'Search:', /ALIGN_LEFT)
	s_base = widget_base(base, /FRAME, /COLUMN)
	dthresh = cw_form(s_base, '0,TEXT,,WIDTH=16,LABEL_LEFT=Detection threshold(s)')
	rel_thresh = cw_bgroup(s_base, 'Relative threshold', /NONEXCLUSIVE, /ROW)
	; Define Correlation parameters
	lab_c = widget_label(base, VALUE = 'Correlation:', /ALIGN_LEFT)
	c_base = widget_base(base, /FRAME, /COLUMN)
	cthresh = cw_form(c_base, '0,FLOAT,,WIDTH=8,LABEL_LEFT=Correlation threshold')
	csub = cw_form(c_base, '0,INTEGER,,WIDTH=8,LABEL_LEFT=No. of sub-pixel offsets')
	; Define other parameters
	lab_o = widget_label(base, VALUE = 'Other parameters:', /ALIGN_LEFT)
	o_base = widget_base(base, /FRAME, /COLUMN)
	noise = cw_bgroup(o_base, 'Noise', /NONEXCLUSIVE)
	bbox = cw_form(o_base, '0,INTEGER,,WIDTH=8,LABEL_LEFT=Box size for background estimation (FWHM units)')
;	deblend = cw_bgroup(o_base, 'Apply deblender', /NONEXCLUSIVE)
	niter = cw_form(o_base, '0,INTEGER,,WIDTH=8,LABEL_LEFT=Final re-fitting iterations')
	u_base1 = widget_base(base, /ROW)
	proc = widget_button(u_base1, VALUE = 'Processing')
	sav = widget_button(u_base1, VALUE = 'Save results')
	u_base2 = widget_base(base, /ROW)
	hlp = widget_button(u_base2, VALUE = 'Help')
	ex = widget_button(u_base2, VALUE = 'Exit')
	; Define pointer to auxiliary/output data
	id = {dthresh: dthresh, rel_thresh: rel_thresh, $
		  cthresh: cthresh, csub: csub, noise: noise, $
		  bbox: bbox, niter: niter, $
;		  bbox: bbox, deblend: deblend, niter: niter, $
		  proc: proc, sav: sav, hlp: hlp, ex: ex}
	xstarfinder_run_par, id, n_elements(noise_std) ne 0 and 1B, back_box, par
	data = xstarfinder_run_def(id, par, image, psf, background, back_box, noise_std, $
							   x_bad, y_bad, psf_fwhm, path)
	data = ptr_new(data, /NO_COPY)
	widget_control, child, SET_UVALUE = data
	; Realize, register, etc.
	widget_control, base, /REALIZE
	xmanager, 'xstarfinder_run', base, EVENT_HANDLER = 'xstarfinder_run_event'
	; De-reference output data and de-allocate heap variables
	xstarfinder_run_del, data, image, psf, background, back_box, noise_std, x_bad, y_bad, $
						 stars, x, y, f, sx, sy, sf, c, path, par
	; Destroy group leader if necessary
	if  n_elements(group) eq 0  then $
	   widget_control, group_id, /DESTROY
	return
end
