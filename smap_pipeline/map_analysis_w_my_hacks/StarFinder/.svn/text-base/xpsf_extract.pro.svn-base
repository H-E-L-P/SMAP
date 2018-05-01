; $Id: xpsf_extract, v 1.3 Aug 2004 e.d. $
;
;+
; NAME:
;	XPSF_EXTRACT
;
; PURPOSE:
;	Widget interface for the PSF_EXTRACT procedure.
;	Given a stellar field image, extract an estimate of the PSF
;	by combination of a set of stars selected by the user.
;
; CATEGORY:
;	Widgets. Signal processing.
;
; CALLING SEQUENCE:
;	XPSF_EXTRACT, Image, Psf, Psf_fwhm, X, Y, Background, Back_Box
;
; INPUTS:
;	Image:	Stellar field
;
; KEYWORD PARAMETERS:
;	DISPLAYIMAGE:	 Structure of current image display options;
;		the structure must be defined as in DEFAULT_DISPLAY_OPT.
;
;	DISPLAYPSF:	Structure of current PSF display options;
;		the structure must be defined as in DEFAULT_DISPLAY_OPT.
;
;	PATH:	Initial path for file browsing when saving reference stars.
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
;	Image:	Same as input Image if no saturated stars are present.
;		Otherwise it is the input Image with corrected saturated stars.
;
;	Psf:	PSF estimate. The size must be specified by the user filling
;		the interactive form
;
;	Psf_fwhm:	FWHM of output PSF
;
;	X, Y:	Coordinates of 'PSF stars'
;
;	Background:	2D array, with the same size as Image, containing an estimate
;		of the background emission
;
;   Back_Box: Size of box used for estimation of the Background array (see also
;       the file IMAGE_BACKGROUND.PRO)
;
; OPTIONAL OUTPUTS:
;	IMADISPLAYOPT:	 Set this keyword to a named variable to get
;		the structure of image display options, as defined and/or
;		modified by XPSF_EXTRACT.
;
;	PSFDISPLAYOPT:	 Set this keyword to a named variable to get
;		the structure of PSF display options, as defined and/or
;		modified by XPSF_EXTRACT.
;
;	DEFAULT_PAR:	Set this keyword to a named variable to get the
;		structure of parameters set by the widget's user.
;
; SIDE EFFECTS:
;	Initiates the XMANAGER if it is not already running.
;
; RESTRICTIONS:
;	The Help menu opens the file
;	'/starfinder/xpsf_extract_help.txt'.
;
; PROCEDURE:
;	Create and register the widget as a modal widget.
;	Then let the user define and or/modify the PSF extraction options and apply
;	them to the input image. As a PSF estimate is obtained applying the current
;	options, it is displayed on the graphic window. Then the user may exit or
;	repeat the extraction procedure with different parameters.
;
; MODIFICATION HISTORY:
;	Written by: Emiliano Diolaiti, September 1999
;	Updates:
;	1) Enhanced error handling in event-handler
;	   (Emiliano Diolaiti, April 2000).
;	2) Removed call to obsolete routine APPEND_ELEMENTS
;	   (Emiliano Diolaiti, June 2001).
;	3) Modified widget parameters
;	   (Emiliano Diolaiti, September 2001).
;	4) Renamed keywords IMAGE_DISPLAY_OPT and PSF_DISPLAY_OPT
;	   (Emiliano Diolaiti, September 2001).
;   5) Added parameter Back_Box (E. D., August 2004).
;   6) Skip 'secondary stars selection' step (E. D., August 2004).
;-

; XPSF_EXTRACT_GET_PRINCIPAL: auxiliary routine to select 'PSF stars'.

PRO xpsf_extract_get_principal, image, wnum, display_opt, upper_lev, $
	                             x_in, y_in, x, y, same_stars

	on_error, 2
	same_stars = 0B
	if  n_elements(x_in) ne 0 and n_elements(y_in) ne 0  then begin
	   msg = dialog_message('Do you want to use the same stars as before?', /QUESTION)
	   if  strlowcase(msg) eq 'yes'  then begin
	      x = x_in  &  y = y_in  &  same_stars = 1B  &  return
	   endif
	endif
	; Select stars
	display_image, image, wnum, OPTIONS = display_opt
   	id = dialog_message(['Select the stars to form the PSF.', '', $
   	   		     'Use the left button of your mouse; ' + $
   	  		     'push the right button to exit.'], /INFO)
	if  n_elements(upper_lev) ne 0  then  upper = upper_lev
	click_on_max, image, /MARK, /SILENT, UPPER = upper, $
	              SYMSIZE = 3, /DARK, x_click, y_click
	nstars = n_elements(x_click)
	if  nstars eq 0  then  return
	; Is there at least one unsaturated star?
	if  n_elements(upper_lev) ne 0  then begin
	   w = where(image[x_click, y_click] lt upper_lev, count)
	   if  count eq 0  then begin
   	      id = dialog_message(/ERROR, 'Please select at least one unsaturated star.')
   	      return
   	   endif
	endif
	; Sort them in order of decreasing intensity
	x = x_click  &  y = y_click
	if  nstars ne 0  then begin
	   sorted = reverse(sort(image[x, y]))  &  x = x[sorted]  &  y = y[sorted]
	endif
	return
end

; XPSF_EXTRACT_CONFIRM: auxiliary routine to select secondary sources.

PRO xpsf_extract_confirm, image, wnum, display_opt, x_in, y_in, $
	                       same_stars, psfsize, x_out, y_out

	on_error, 2
	if  n_elements(x_in) eq 0 or n_elements(y_in) eq 0  then  return
   	if  same_stars  then begin
   	   x_out = x_in  &  y_out = y_in  &  return
   	endif
   	sub_arrays, image, x_in, y_in, psfsize, stack
   	nstars = n_elements(x_in)
   	for  n = 0L, nstars - 1  do begin
   	   xn = -1  &  yn = -1
   	   opt = default_display_opt(stack[*,*,n])
   	   opt.reverse = display_opt.reverse
   	   opt.stretch = display_opt.stretch
   	   opt.color_table = display_opt.color_table
   	   display_image, stack[*,*,n], wnum, OPTIONS = opt
   	   msg = dialog_message('Confirm this star?', /QUESTION)
   	   if  strlowcase(msg) eq 'no'  then begin
   	      x_in[n] = -1  &  y_in[n] = -1
   	   endif
   	endfor
   	w = where(x_in ge 0 and y_in ge 0, n_confirm)
   	if  n_confirm ne 0  then begin
   	   x_out = x_in[w]  &  y_out = y_in[w]
   	endif
	display_image, image, wnum, OPTIONS = display_opt
	return
end

; XPSF_EXTRACT_GET_SECONDARY: auxiliary routine to select secondary sources.

PRO xpsf_extract_get_secondary, image, wnum, display_opt, x, y, $
	                             same_stars, psfsize, x2_in, y2_in, x2, y2

  	on_error, 2
	if  n_elements(x) eq 0 or n_elements(y) eq 0  then  return
	msg = dialog_message(['Do you want to select and subtract the ', $
			      'secondary sources around the selected stars?'], /QUESTION)
	if  strlowcase(msg) eq 'no'  then  return
	if  same_stars and n_elements(x2_in) ne 0 and n_elements(y2_in) ne 0  then begin
	   msg = dialog_message('Select the same secondary sources as before?', /QUESTION)
	   if  strlowcase(msg) eq 'yes'  then begin
	      x2 = x2_in  &  y2 = y2_in  &  return
	   endif
	endif
   sub_arrays, image, x, y, psfsize, stack
   nstars = n_elements(x)
   for  n = 0L, nstars - 1  do begin
      xn = -1  &  yn = -1
      opt = default_display_opt(stack[*,*,n])
      opt.reverse = display_opt.reverse
      opt.stretch = display_opt.stretch
      opt.color_table = display_opt.color_table
      display_image, stack[*,*,n], wnum, OPTIONS = opt
      msg = dialog_message(['Select the main secondary sources around ' +  $
   	   			 'the displayed star.', '', 'Use the left '  +  $
   	   			 'button of your mouse; push the right button ' + $
   	   			 'to exit.'], /INFO)
	   click_on_max, stack[*,*,n], /MARK, /SILENT, $
	                 BOXSIZE = 3, SYMSIZE = 3, xn, yn
	   if  xn[0] ne -1 and yn[0] ne -1  then begin
	      xn = xn + x[n] - psfsize/2  &  yn = yn + y[n] - psfsize/2
	      if  n_elements(x2) eq 0  then begin
	         x2 = xn  &  y2 = yn
	      endif else begin
	         x2 = [x2, xn]
	         y2 = [y2, yn]
	      endelse
	   endif
   endfor
	; Sort secondary stars in order of decreasing intensity
   if  n_elements(x2) ne 0  then begin
	   sorted = reverse(sort(image[x2, y2]))
	   x2 = x2[sorted]  &  y2 = y2[sorted]
	endif
	; Restore previous display
	display_image, image, wnum, OPTIONS = display_opt
	return
end

; XPSF_EXTRACT_EVENT: XPsf_Extract event handler.

PRO xpsf_extract_event, event

	catch, error
	if  error ne 0  then begin
	   msg = dialog_message(/ERROR, !err_string)
	   widget_control, event.id, SET_UVALUE = data, /NO_COPY
	   return
	endif
	widget_control, event.id, GET_UVALUE = data, /NO_COPY
	event_type = strlowcase(event.tag)
	case  event_type  of
	   'upper_lev': begin
	      widget_control, event.id, GET_VALUE = form
	      image_max = max(*(*data).image)
	      for  id = 11, 13  do $
	         widget_control, (*data).ids[id], $
	         SENSITIVE = form.upper_lev le image_max and 1B
	      end
	   'process': begin
	      widget_control, event.id, GET_VALUE = form
	      if  form.psf_size ne 0  then begin
	         ; Define input parameters of PSF_EXTRACT
	         psf_size = form.psf_size
	         back_box = form.back_box
;	         n_fwhm_fit = form.n_fwhm_fit
	         norm_rad = form.norm_rad
	         avg = form.avg
	         satur = form.upper_lev le max(*(*data).image)
	         if  satur  then  upper_lev = form.upper_lev
	         n_fwhm_match = form.n_fwhm_match
	         n_width = form.n_width
	         mag_fac = form.mag_fac
	         ; Save parameters
	         (*data).par.psf_size = psf_size
	         (*data).par.back_box = back_box
	         (*data).back_box = back_box
;	         (*data).par.n_fwhm_fit = n_fwhm_fit
	         (*data).par.norm_rad = norm_rad
	         (*data).par.avg = avg
	         (*data).par.upper_lev = form.upper_lev
	         (*data).par.n_fwhm_match = n_fwhm_match
	         (*data).par.n_width = n_width
	         (*data).par.mag_fac = mag_fac
	         ; Select 'PSF stars'
	         xpsf_extract_get_principal, *(*data).image, (*data).wnum, *(*data).ima_disp, $
	            upper_lev, *(*data).x, *(*data).y, x_0, y_0, same_stars
	         xpsf_extract_confirm, *(*data).image, (*data).wnum, *(*data).ima_disp, $
	            x_0, y_0, same_stars, psf_size, x, y
;	         xpsf_extract_get_secondary, *(*data).image, (*data).wnum, *(*data).ima_disp, $
;	            x, y, same_stars, psf_size, *(*data).x2, *(*data).y2, x2_0, y2_0
	         ; Estimate PSF
	         if  n_elements(x_0) ne 0 and n_elements(y_0) ne 0  then begin
	            x = x_0  &  y = y_0
	            *(*data).x = x  &  *(*data).y = y
	            if  n_elements(x2_0) ne 0 and n_elements(y2_0) ne 0  then begin
	               compare_lists, x, y, x2_0, y2_0, MAX_DISTANCE = sqrt(2) * 1.5, $
	                              x_1, y_1, x2_1, y2_1, SUB2 = s2
	               if  s2[0] ge 0  then begin
	                  x2 = x2_0[s2]  &  y2 = y2_0[s2]
	                  *(*data).x2 = x2  &  *(*data).y2 = y2
	               endif
	            endif
	            avgtype = 2 - avg
	            if avgtype ne 2 then avgtype = 0
	            widget_control, /HOURGLASS
	            psf_extract, *(*data).x, *(*data).y, x2, y2, *(*data).image, $
	               psf_size, *(*data).psf, psf_fwhm, *(*data).background, $
	               N_FWHM_BACK = back_box, $;N_FWHM_FIT = n_fwhm_fit, $
	               INTERP_TYPE = 'I', UPPER_LEVEL = upper_lev, $
	               N_FWHM_MATCH = n_fwhm_match, N_WIDTH = n_width, $
	               MAG_FAC = mag_fac, AVGTYPE = avgtype, $
	               RAD_NORM = norm_rad, _EXTRA = *(*data).extra
		        ; Save outputs
		        if  n_elements(psf_fwhm) ne 0  then begin
		           *(*data).psf_fwhm = psf_fwhm
	               msg = dialog_message(/INFO, 'Done.')
		        endif
	         endif
	      endif else $
	         msg = dialog_message(/ERROR, 'Please select a PSF size.')
	      end
	   'disp_ima': begin
	      widget_control, /HOURGLASS
	      display_image, *(*data).image, (*data).wnum, OPTIONS = *(*data).ima_disp
	      (*data).last_disp = (*data).image
	      (*data).last_disp_opt = (*data).ima_disp
	      end
	   'disp_psf': if  n_elements(*(*data).psf) ne 0  then begin
	      widget_control, /HOURGLASS
	      display_image, *(*data).psf, (*data).wnum, OPTIONS = *(*data).psf_disp
	      (*data).last_disp = (*data).psf
	      (*data).last_disp_opt = (*data).psf_disp
	      endif
	   'disp_opt': if  n_elements(*(*data).last_disp) ne 0  then $
	      *(*data).last_disp_opt = xdisplayopt(*(*data).last_disp, (*data).wnum, $
	      			OPTIONS = *(*data).last_disp_opt, GROUP = event.top)
	   'help': $
	      xdispfile, file_name('starfinder', 'xpsf_extract_help.txt'), $
	      		 TITLE = 'XPsf_Extract help', /MODAL
	   'exit': begin
	      if  n_elements(*(*data).x) ne 0  then begin
	         msg = dialog_message(/QUESTION, 'Save PSF stars?')
	         if  strlowcase(msg) eq 'yes'  then begin
	            file = dialog_pickfile(/WRITE, FILTER = '*.txt', $
	            					   PATH = *(*data).path, GET_PATH = path)
	            if  file ne ''  then begin
	               widget_control, /HOURGLASS
	               if  strpos(file, '.txt') lt 0  then  file = file + '.txt'
	               *(*data).path = path
	               out = [transpose(*(*data).x), transpose(*(*data).y)]
	               openw, lun, file, /GET_LUN
	               printf, lun, out  &  free_lun, lun
	            endif
	         endif
	      endif
	      widget_control, event.id, SET_UVALUE = data, /NO_COPY
	      widget_control, event.top, /DESTROY
	      end
	   else:
	endcase
	if  event_type ne 'exit'  then $
	   widget_control, event.id, SET_UVALUE = data, /NO_COPY
	return
end

; XPSF_EXTRACT_DEF: define data structure.

FUNCTION xpsf_extract_def, ids, par, image, psf, psf_fwhm, x, y, background, back_box, $
	                       wnum, ima_disp, psf_disp, path, extra

	return, { ids: ids, par: par, $
			  image: ptr_new(image, /NO_COPY), $
			  psf: ptr_new(psf, /NO_COPY), psf_fwhm: ptr_new(psf_fwhm, /NO_COPY), $
			  x: ptr_new(x, /NO_COPY), y: ptr_new(y, /NO_COPY), $
			  x2: ptr_new(/ALLOCATE), y2: ptr_new(/ALLOCATE), $
			  background: ptr_new(background, /NO_COPY), back_box: back_box, $
			  wnum: wnum, $
			  ima_disp: ptr_new(ima_disp, /NO_COPY), $
			  psf_disp: ptr_new(psf_disp, /NO_COPY), $
			  last_disp: ptr_new(/ALLOCATE), last_disp_opt: ptr_new(/ALLOCATE), $
			  path: ptr_new(path, /NO_COPY), $
			  extra: ptr_new(extra, /NO_COPY) }
end

; XPSF_EXTRACT_DEL: de-reference and de-allocate heap variables.

PRO xpsf_extract_del, data, par, image, psf, psf_fwhm, x, y, background, back_box, $
	                  ima_disp, psf_disp, path, extra

	par = (*data).par
	image = *(*data).image
	if  n_elements(*(*data).psf) ne 0  then  psf = *(*data).psf
	if  n_elements(*(*data).psf_fwhm) ne 0  then  psf_fwhm = *(*data).psf_fwhm
	if  n_elements(*(*data).x) ne 0 and n_elements(*(*data).y) ne 0  then begin
	   x = *(*data).x  &  y = *(*data).y
	endif
	if  n_elements(*(*data).background) ne 0  then  background = *(*data).background
	back_box = (*data).back_box
	if  n_elements(*(*data).ima_disp) ne 0  then  ima_disp = *(*data).ima_disp
	if  n_elements(*(*data).psf_disp) ne 0  then  psf_disp = *(*data).psf_disp
	if  n_elements(*(*data).path) ne 0  then  path = *(*data).path
	if  n_elements(*(*data).extra) ne 0  then  extra = *(*data).extra
	ptr_free, (*data).image, (*data).psf, (*data).psf_fwhm, $
	          (*data).x, (*data).y, (*data).x2, (*data).y2, $
	          (*data).background, (*data).ima_disp, (*data).psf_disp, $
	          (*data).last_disp, (*data).last_disp_opt, (*data).path, (*data).extra
	ptr_free, data
	heap_gc
	return
end

; XPSF_EXTRACT_PAR: define default parameters.

PRO xpsf_extract_par, id, par, image_max, back_box

	if  n_elements(par) ne 0  then begin
	   psf_size = par.psf_size
	   if n_elements(back_box) eq 0 then back_box = par.back_box
;	   n_fwhm_fit = par.n_fwhm_fit
	   norm_rad = par.norm_rad
	   avg = par.avg
	   upper_lev = par.upper_lev
	   n_fwhm_match = par.n_fwhm_match
	   n_width = par.n_width
	   mag_fac = par.mag_fac
	endif else begin
	   psf_size = 0
	   if n_elements(back_box) eq 0 then back_box = 9
;	   n_fwhm_fit = 2
	   norm_rad = 1.
	   avg = 0
	   upper_lev = 1e6
	   while  upper_lev le image_max  do  upper_lev = 10 * upper_lev
	   n_fwhm_match = 1
	   n_width = 3
	   mag_fac = 2
	endelse
	par = {psf_size: psf_size, back_box: back_box, $;n_fwhm_fit: n_fwhm_fit, $
	       norm_rad: norm_rad, avg: avg, upper_lev: upper_lev, $
	       n_fwhm_match: n_fwhm_match, n_width: n_width, mag_fac: mag_fac}
	init = {psf_size: strcompress(string(psf_size), /REMOVE_ALL), $
	        back_box: strcompress(string(back_box), /REMOVE_ALL), $
;	        n_fwhm_fit: strcompress(string(n_fwhm_fit), /REMOVE_ALL), $
	        norm_rad: strcompress(string(norm_rad), /REMOVE_ALL), $
	        avg: avg, $
	        upper_lev: strcompress(string(upper_lev), /REMOVE_ALL), $
	        n_fwhm_match: strcompress(string(n_fwhm_match), /REMOVE_ALL), $
	        n_width: strcompress(string(n_width), /REMOVE_ALL), $
	        mag_fac: strcompress(string(mag_fac), /REMOVE_ALL)}
	widget_control, id, SET_VALUE = init
	return
end

; XPSF_EXTRACT: XPsf_Extract widget definition module.

PRO xpsf_extract, image, psf, psf_fwhm, x, y, background, back_box, _EXTRA = extra, $
	              DISPLAYIMAGE = ima_disp, DISPLAYPSF = psf_disp, $
	              PATH = path, DEFAULT_PAR = par, GROUP = group, UVALUE = uvalue

	catch, error
	if  error ne 0  then begin
	   xpsf_extract_del, data, par, image, psf, psf_fwhm, x, y, background, back_box, $
	   				     ima_disp, psf_disp, path, extra
	   if  n_elements(group) eq 0  then  widget_control, group_id, /DESTROY
	   return
	endif
	; Create group leader if necessary
	if  n_elements(group) eq 0  then $
	   group_id = widget_base()  else  group_id = group
	; Create modal base
	if  n_elements(uvalue) eq 0  then  uvalue = 0B
	base = widget_base(TITLE = 'XPsf_Extract', GROUP_LEADER = group_id, $
					   /MODAL, UVALUE = uvalue, COLUMN = 2)
	left_base = widget_base(base, /GRID_LAYOUT)
	right_base = widget_base(base, /GRID_LAYOUT)
	; Define draw window (in the right part of base)
	s = round(0.7 * min(get_screen_size()))
	draw = widget_draw(right_base, SCR_XSIZE = s, SCR_YSIZE = s, $
					   /ALIGN_CENTER, RETAIN = 2)
	; Define form with PSF extraction parameters
	desc = [ $
	'0, LABEL,Boxes:,LEFT', $
	'1, BASE,,FRAME,COLUMN', $
	'0, INTEGER,,LABEL_LEFT=Size of output PSF,TAG=psf_size', $
	'2, FLOAT,,LABEL_LEFT=Box size for background estimation (FWHM units),' + $
	   'TAG=back_box', $
;	'2, FLOAT,,LABEL_LEFT=Fitting box size (FWHM units),TAG=n_fwhm_fit', $
	'0, LABEL,Point Sources Combination:,LEFT', $
	'1, BASE,,FRAME,COLUMN', $
	'0, FLOAT,,LABEL_LEFT=Normalization radius (FWHM units),TAG=norm_rad', $
	'2, BUTTON,median|mean,EXCLUSIVE, ' + $
	   'LABEL_LEFT=Average type:,NO_RELEASE,ROW,TAG=avg', $
	'0, LABEL,Saturated stars:,LEFT', $
	'1, BASE,,FRAME,COLUMN', $
	'0, FLOAT,,LABEL_LEFT=Saturation threshold,WIDTH=12,TAG=upper_lev', $
	'0, FLOAT,,LABEL_LEFT=Search box to optimize correlation (FWHM units),' + $
	   'TAG=n_fwhm_match', $
	'0, FLOAT,,LABEL_LEFT=Repair box (saturated core units),TAG=n_width', $
	'2, INTEGER,,LABEL_LEFT=Sub-pixel positioning accuracy,TAG=mag_fac', $
	'1, BASE,,ROW', $
	'2, BUTTON,Processing...,NO_RELEASE,TAG=process', $
	'1, BASE,,COLUMN', $
	'0, BUTTON,Display Image,NO_RELEASE,TAG=disp_ima', $
	'0, BUTTON,Display PSF,NO_RELEASE,TAG=disp_psf', $
	'2, BUTTON,Display Options,NO_RELEASE,TAG=disp_opt', $
	'1, BASE,,ROW', $
	'0, BUTTON,Help,NO_RELEASE,TAG=help', $
	'2, BUTTON,Exit,QUIT,NO_RELEASE,TAG=exit']
	form = cw_form(left_base, desc, /COLUMN, IDS = ids)
	image_max = max(image)
	xpsf_extract_par, form, par, image_max, back_box
	widget_control, ids[3], SENSITIVE = n_elements(background) eq 0 and 1B
	for  id = 11, 13  do $
	   widget_control, ids[id], SENSITIVE = par.upper_lev le image_max and 1B
	; Realize widget
	widget_control, base, /REALIZE
	; Display image and define display options
	widget_control, draw, GET_VALUE = wnum
	display_image, image, wnum, OPTIONS = ima_disp
	; Define pointer to auxiliary/output data
	data = xpsf_extract_def(ids, par, image, psf, psf_fwhm, x, y, background, back_box, $
	                        wnum, ima_disp, psf_disp, path, extra)
	data = ptr_new(data, /NO_COPY)
	(*data).last_disp = (*data).image
	(*data).last_disp_opt = (*data).ima_disp
	widget_control, form, SET_UVALUE = data
	; Register
	xmanager, 'xpsf_extract', base, EVENT_HANDLER = 'xpsf_extract_event'
	; De-reference output data and de-allocate heap variables
	xpsf_extract_del, data, par, image, psf, psf_fwhm, x, y, background, back_box, $
	   				  ima_disp, psf_disp, path, extra
	; Destroy group leader if necessary
	if  n_elements(group) eq 0  then $
	   widget_control, group_id, /DESTROY
	return
end
