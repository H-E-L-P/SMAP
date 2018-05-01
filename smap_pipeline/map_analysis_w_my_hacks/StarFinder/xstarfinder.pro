; $Id: xstarfinder.pro, v 1.5 Aug 2004 e.d. $
;
;+
; NAME:
;	XSTARFINDER
;
; PURPOSE:
;	Package for stellar fields analysis.
;
; CATEGORY:
;	Widgets.
;
; CALLING SEQUENCE:
;	XSTARFINDER
;
; KEYWORD PARAMETERS:
;	GROUP:	Group leader, i.e. identifier of the calling widget program
;
;	BLOCK:	Set this keyword to a nonzero value to have the IDL command
;		line blocked when this application is registered
;
;	UVALUE:	User value to be assigned to XStarFinder
;
; SIDE EFFECTS:
;	Initiates the XMANAGER if it is not already running.
;
; PROCEDURE:
;	Create and register the widget and then exit. The additional data
;	required by the program are stored as a structure of pointers in the
;	user value of a child of the top level base.
;
; MODIFICATION HISTORY:
;	Written by: Emiliano Diolaiti, August-September 1999.
;	Updates:
;	1) Enhanced error handling in event-handler
;	   (Emiliano Diolaiti, April 2000).
;	2) Modified 'Repeat PSF extraction' task
;	   (Emiliano Diolaiti, September 2001).
;	3) 'Repeat PSF extraction' task replaced by a call to the
;	   PSF extraction procedure, to allow a more flexible tuning
;	   of the parameters (Emiliano Diolaiti, January 2002).
;   4) Fixed problem with parameter 'Box for Background estimation'
;      (E. D., August 2004).
;   5) When a new image is loaded, all the existing data are erased, while
;      all the configuration parameters are preserved (E. D., August 2004).
;   6) The PSF is automatically normalized whenever it is loaded, extracted or
;      processed (E. D., August 2004).
;-



;;; EVENT PROCEDURES

; LOAD_IMAGE_EV: load FITS format image data from input file and display it.

PRO load_image_ev, data_p, path_p, HDR = hdr_p, $
	               DISPLAY = display, drawid, display_par_p, $
	               LOADED = loaded, FILE = file_p

	on_error, 2
	file = dialog_pickfile(/READ, FILTER = '*.fits', $
	                       PATH = *path_p, GET_PATH = path)
	loaded = file ne ''
	if  not loaded  then  return
	widget_control, /HOURGLASS
	fits_read, file, data, hdr
	if n_elements(path) ne 0 then *path_p = path
	if n_elements(data) ne 0 then *data_p = data
    if ptr_valid(file_p) then *file_p = file
	if ptr_valid(hdr_p) then *hdr_p = hdr
	if keyword_set(display) then begin
;	   *display_par_p = default_display_opt(data)
	   display_image_ev, drawid, path_p, display_par_p, data_p
	endif
	return
end

; DISPLAY_IMAGE_EV: display image data using current options.

PRO display_image_ev, drawid, path_p, display_par_p, data_p, $
                      HDR = hdr_p, FILE = file_p

	on_error, 2
	no_data = not ptr_valid(data_p)
	if  no_data  then  data_p = ptr_new(/ALLOCATE)
	if  n_elements(*data_p) eq 0  then begin
	   if  not ptr_valid(hdr_p)  then  hdr_p = ptr_new(/ALLOCATE)
	   load_image_ev, data_p, path_p, HDR = hdr_p, FILE = file_p
	endif
	if  size52(*data_p, /N_DIM) ne 2  then  return
	widget_control, /HOURGLASS
	if  n_elements(*display_par_p) eq 0  then $
	   *display_par_p = default_display_opt(*data_p)
	display_image, *data_p, drawid, OPTIONS = *display_par_p
	return
end

; DISPLAY_OPTIONS_EV: modify display options by means of XDisplayOpt.

PRO display_options_ev, drawid, last_data_p, display_par_p, group_leader

	on_error, 2
	if  n_elements(*last_data_p) eq 0  then begin
	   msg = dialog_message('Please select data', /ERROR)
	   return
	endif
	widget_control, /HOURGLASS
	if  size52(*last_data_p, /TYPE) eq 7  then $
	   fits_read, *last_data_p, data  else  data = *last_data_p
	*display_par_p = xdisplayopt(data, drawid, /NODISPLAY, $
				     OPTIONS = *display_par_p, GROUP = group_leader)
	return
end

; SAVE_IMAGE_EV: save image data in FITS-format output file.

PRO save_image_ev, data_p, path_p, HDR = hdr_p

	on_error, 2
	if  n_elements(*data_p) eq 0  then  return
	file = dialog_pickfile(/WRITE, FILTER = '*.fits',  $
						   PATH = *path_p, GET_PATH = path)
	if  file eq ''  then  return
	widget_control, /HOURGLASS
	if  strpos(file, '.fits') lt 0  then  file = file + '.fits'
	*path_p = path
	if  ptr_valid(hdr_p)  then $
	   if  n_elements(*hdr_p) ne 0  then  header = *hdr_p
	writefits, file, *data_p, header
	return
end

; NOISE_STD_EV: compute st. dev. of gaussian noise in the image.

PRO noise_std_ev, data, group_leader

	on_error, 2
	if  n_elements(*data.image.image) eq 0  then begin
	   msg = dialog_message('Please select data', /ERROR)
	   return
	endif
	xnoise, *data.image.image, *data.image.noise_std, WNUM = *data.draw, $
			PATH = *data.path, DEFAULT_PAR = *data.image.noise_par, $
			G_NOISE_PAR = *data.image.g_noise_par, PLOT_PAR = $
			*data.image.plot_par, GROUP = group_leader
	return
end

; REPLACE_BAD_EV: replace bad pixels in the input image.

PRO replace_badpix_ev, data, group_leader

	on_error, 2
	if  n_elements(*data.image.image) eq 0  then begin
	   msg = dialog_message('Please select data', /ERROR)
	   return
	endif
	*data.image.image = xreplace_pix(*data.image.image, $
						*data.image.x_badpix, *data.image.y_badpix, $
						PATH = *data.path, WNUM = *data.draw, $
						DISPLAY_OPT = *data.image.image_display_par, $
						GROUP = group_leader)
	return
end

; PSF_EXTRACT_EV: extract PSF from image.

PRO psf_extract_ev, data, group_leader

	on_error, 2
	if  n_elements(*data.image.image) eq 0  then begin
	   msg = dialog_message('Please select data', /ERROR)
	   return
	endif
	xpsf_extract, *data.image.image, *data.psf.psf, *data.psf.psf_fwhm, $
	              *data.psf.x_psf, *data.psf.y_psf, *data.image.background, *data.image.backbox, $
	              STARS = *data.image.stars, PSF = *data.psf.psf, $
	              X_STARS = *data.image.x_stars, Y_STARS = *data.image.y_stars, $
	              FLUXES = *data.image.f_stars, $
	              DEFAULT_PAR = *data.psf.extract_par, $
	              DISPLAYIMAGE = *data.image.image_display_par, $
	              DISPLAYPSF = *data.psf.psf_display_par, $
	              PATH = *data.path, GROUP = group_leader
	if n_elements(*data.psf.psf) ne 0 then $
	   for rep = 1, 2 do *data.psf.psf = *data.psf.psf / total(*data.psf.psf)
;	xpsf_extract, *data.image.image, *data.psf.psf, *data.psf.psf_fwhm, $
;	              *data.psf.x_psf, *data.psf.y_psf, *data.image.background, $
;	              DEFAULT_PAR = *data.psf.extract_par, $
;	              DISPLAYIMAGE = *data.image.image_display_par, $
;	              DISPLAYPSF = *data.psf.psf_display_par, $
;	              PATH = *data.path, GROUP = group_leader
;	if  n_elements(*data.psf.psf_fwhm) ne 0  then begin
;	   if  n_elements(*data.psf.extract_par) ne 0  then $
;	      n_fwhm = (*data.psf.extract_par).n_fwhm_back  else  n_fwhm = 9
;	   *data.image.backbox = round(n_fwhm * *data.psf.psf_fwhm)
;	endif
	return
end

; PSF_REPEAT_EV: repeat extraction procedure after 1st analysis.

PRO psf_repeat_ev, data

	on_error, 2
	if  n_elements(*data.image.image) eq 0  then begin
	   msg = dialog_message('Load image first', /ERROR)
	   return
	endif
	if  n_elements(*data.image.stars) eq 0  then begin
	   msg = dialog_message('Analyze the image before', /ERROR)
	   return
	endif
	if  n_elements(*data.psf.extract_par) * $
		n_elements(*data.psf.x_psf) eq 0  then begin
	   msg = dialog_message('Run PSF extraction procedure first', /ERROR)
	   return
	endif
	widget_control, /HOURGLASS
	xpsf_extract, *data.image.image, *data.psf.psf, *data.psf.psf_fwhm, $
	              *data.psf.x_psf, *data.psf.y_psf, *data.image.background, *data.image.backbox, $
	              STARS = *data.image.stars, PSF = *data.psf.psf, $
	              X_STARS = *data.image.x_stars, Y_STARS = *data.image.y_stars, $
	              FLUXES = *data.image.f_stars, $
	              DEFAULT_PAR = *data.psf.extract_par, $
	              DISPLAYIMAGE = *data.image.image_display_par, $
	              DISPLAYPSF = *data.psf.psf_display_par, $
	              PATH = *data.path, GROUP = group_leader
	if n_elements(*data.psf.psf) ne 0 then $
	   for rep = 1, 2 do *data.psf.psf = *data.psf.psf / total(*data.psf.psf)
	return
end

; PSF_SUPPORT_EV: PSF support event.

PRO psf_support_ev, data, group_leader

	on_error, 2
	if  n_elements(*data.psf.psf) eq 0  then begin
	   msg = dialog_message('Please define PSF', /ERROR)
	   return
	endif
	*data.psf.psf = ximage_support(*data.psf.psf, *data.draw, $
								   *data.psf.psf_display_par, $
								   DEFAULT_PAR = *data.psf.support_par, $
								   GROUP = group_leader)
	for rep = 1, 2 do *data.psf.psf = *data.psf.psf / total(*data.psf.psf)
	return
end

; PSF_SMOOTH_EV: PSF halo smooth event.

PRO psf_smooth_ev, data, group_leader

	on_error, 2
	if  n_elements(*data.psf.psf) eq 0  then begin
	   msg = dialog_message('Please define PSF', /ERROR)
	   return
	endif
	*data.psf.psf = xpsf_smooth(*data.psf.psf, *data.draw, $
								*data.psf.psf_display_par, $
								DEFAULT_PAR = *data.psf.smooth_par, $
								GROUP = group_leader)
	for rep = 1, 2 do *data.psf.psf = *data.psf.psf / total(*data.psf.psf)
	return
end

; NORMALIZE_PSF_EV: PSF normalization event.

PRO normalize_psf_ev, data

	on_error, 2
	if  n_elements(*data.psf.psf) eq 0  then begin
	   msg = dialog_message('Please define PSF', /ERROR)
	   return
	endif
	widget_control, /HOURGLASS
	for  rep = 1, 2  do  *data.psf.psf = *data.psf.psf / total(*data.psf.psf)
	return
end

; SELECT_REF_EV: select and save positions of reference stars in a given image.

PRO select_ref_ev, data

	on_error, 2
	if  n_elements(*data.image.image) eq 0  then begin
	   msg = dialog_message('Please select data', /ERROR)
	   return
	endif
	display_image, *data.image.image, *data.draw, $
				   OPTIONS = *data.image.image_display_par
	msg = dialog_message(['Select the reference stars.', '', $
   	   					  'Use the left button of your mouse; ' + $
   	   					  'push the right button to exit.'], /INFO)
   	if  n_elements(*data.psf.psf_fwhm) ne 0  then $
   	   click_box = round(*data.psf.psf_fwhm)  else  click_box = 3
	click_on_max, *data.image.image, /MARK, /SILENT, x, y, $
				  BOXSIZE = click_box, SYMSIZE = click_box
	if  n_elements(x) eq 0 or n_elements(y) eq 0  then  return
	file = dialog_pickfile(/WRITE, FILTER = '*.txt', PATH = *data.path, $
		   GET_PATH = path, TITLE = 'Select file to save reference stars')
	if  file ne '' then begin
	   widget_control, /HOURGLASS
	   if  strpos(file, '.txt') lt 0  then  file = file + '.txt'
	   *data.path = path
	   out = [transpose(x),transpose(y)]
	   openw, lun, file, /GET_LUN
	   printf, lun, out  &  free_lun, lun
	endif
	return
end

; ASTROM_PHOTOM_EV: astrometry and photometry event.

PRO astrom_photom_event, data, group_leader

	on_error, 2
	if  n_elements(*data.image.image) eq 0  then begin
	   msg = dialog_message('Load Image', /ERROR)
	   return
	endif
	if  n_elements(*data.psf.psf) eq 0  then begin
	   msg = dialog_message('Load or estimate PSF', /ERROR)
	   return
	endif
	if  n_elements(*data.psf.psf_fwhm) eq 0  then $
	   *data.psf.psf_fwhm = fwhm(*data.psf.psf, /CUBIC, MAG = 3)
	if  n_elements(*data.image.backbox) eq 0  then $
	   *data.image.backbox = 9
	xstarfinder_run, *data.image.image, *data.psf.psf, *data.psf.psf_fwhm, $
			*data.image.background, *data.image.backbox, $
			NOISE_STD = *data.image.noise_std, $
			X_BAD = *data.image.x_badpix, Y_BAD = *data.image.y_badpix, $
			STARS = *data.image.stars, $
			*data.image.x_stars, *data.image.y_stars, *data.image.f_stars, $
			*data.image.sx_stars, *data.image.sy_stars, *data.image.sf_stars, $
			*data.image.c_stars, DEFAULT_PAR = *data.image.run_par, $
			GROUP = group_leader, PATH = *data.path
	if  n_elements(*data.image.f_stars) ne 0  then begin
	   *data.image.syn_field = *data.image.stars
	   if  n_elements(*data.image.background) ne 0  then $
	      *data.image.syn_field = *data.image.syn_field + *data.image.background
	endif
	return
end

; SAVE_LIST_EV: save list of detected stars.

PRO save_list_ev, data

	if  n_elements(*data.image.f_stars) eq 0  then begin
	   msg = dialog_message('Analyze the image', /ERROR)
	   return
	endif
	file = dialog_pickfile(/WRITE, FILTER = '*.txt', PATH = *data.path, $
		   GET_PATH = path, TITLE = 'Select file to save list of stars')
	if  file ne '' then begin
	   widget_control, /HOURGLASS
	   if  strpos(file, '.txt') lt 0  then  file = file + '.txt'
	   *data.path = path
	   out = [transpose(*data.image.x_stars), $
	          transpose(*data.image.y_stars), $
	          transpose(*data.image.f_stars), $
	          transpose(*data.image.sx_stars), $
	          transpose(*data.image.sy_stars), $
	          transpose(*data.image.sf_stars), $
	          transpose(*data.image.c_stars)]
	   openw, lun, file, /GET_LUN
	   printf, lun, out  &  free_lun, lun
	endif
	return
end


; INIT_DATA: initialize pointers to image or PSF data.

PRO init_data, data, field

	on_error, 2
	widget_control, /HOURGLASS
	case  field  of
	   'image': begin
	      ptr_free, $
	         data.image.background, $
	         data.image.back_hdr, data.image.noise_std, $
	         data.image.x_badpix, data.image.y_badpix, $
	         data.image.x_stars, data.image.y_stars, data.image.f_stars, $
	         data.image.sx_stars, data.image.sy_stars, data.image.sf_stars, $
	         data.image.c_stars, data.image.stars, data.image.syn_field, $
	         data.psf.psf, data.psf.psf_hdr, data.psf.psf_fwhm, data.psf.x_psf, data.psf.y_psf
	      data.image.background = ptr_new(/ALLOCATE)
	      data.image.back_hdr = ptr_new(/ALLOCATE)
	      data.image.noise_std = ptr_new(/ALLOCATE)
	      data.image.x_badpix = ptr_new(/ALLOCATE)
	      data.image.y_badpix = ptr_new(/ALLOCATE)
	      data.image.x_stars = ptr_new(/ALLOCATE)
	      data.image.y_stars = ptr_new(/ALLOCATE)
	      data.image.f_stars = ptr_new(/ALLOCATE)
	      data.image.sx_stars = ptr_new(/ALLOCATE)
	      data.image.sy_stars = ptr_new(/ALLOCATE)
	      data.image.sf_stars = ptr_new(/ALLOCATE)
	      data.image.c_stars = ptr_new(/ALLOCATE)
	      data.image.stars = ptr_new(/ALLOCATE)
	      data.image.syn_field = ptr_new(/ALLOCATE)
	      data.psf.psf = ptr_new(/ALLOCATE)
	      data.psf.psf_hdr = ptr_new(/ALLOCATE)
	      data.psf.psf_fwhm = ptr_new(/ALLOCATE)
	      data.psf.x_psf = ptr_new(/ALLOCATE)
	      data.psf.y_psf = ptr_new(/ALLOCATE)
	      end
	   'psf': begin
	      ptr_free, data.psf.psf_fwhm, data.psf.x_psf, data.psf.y_psf
	      data.psf.psf_fwhm = ptr_new(/ALLOCATE)
	      data.psf.x_psf = ptr_new(/ALLOCATE)
	      data.psf.y_psf = ptr_new(/ALLOCATE)
	      end
	   endcase
	return
end

; CHECK_PTR: check undefined heap variables in data structure.

PRO check_ptr, data, action

	on_error, 2
	widget_control, /HOURGLASS
	case  action  of
	   'in': begin
	      if  not ptr_valid(data.draw)  then  data.draw = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.path)  then  data.path = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.file)  then  data.file = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.last_display)  then  data.last_display = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.last_display_par)  then  data.last_display_par = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.other_display_par)  then  data.other_display_par = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.image.image)  then  data.image.image = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.image.image_display_par)  then  data.image.image_display_par = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.image.image_hdr)  then  data.image.image_hdr = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.image.background)  then  data.image.background = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.image.back_display_par)  then  data.image.back_display_par = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.image.back_hdr)  then  data.image.back_hdr = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.image.backbox)  then  data.image.backbox = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.image.noise_par)  then  data.image.noise_par = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.image.g_noise_par)  then  data.image.g_noise_par = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.image.plot_par)  then  data.image.plot_par = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.image.noise_std)  then  data.image.noise_std = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.image.x_badpix)  then  data.image.x_badpix = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.image.y_badpix)  then  data.image.y_badpix = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.image.run_par)  then  data.image.run_par = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.image.x_stars)  then  data.image.x_stars = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.image.y_stars)  then  data.image.y_stars = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.image.f_stars)  then  data.image.f_stars = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.image.sx_stars)  then  data.image.sx_stars = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.image.sy_stars)  then  data.image.sy_stars = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.image.sf_stars)  then  data.image.sf_stars = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.image.c_stars)  then  data.image.c_stars = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.image.stars)  then  data.image.stars = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.image.stars_display_par)  then  data.image.stars_display_par = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.image.syn_field)  then  data.image.syn_field = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.image.syn_display_par)  then  data.image.syn_display_par = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.psf.psf)  then  data.psf.psf = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.psf.extract_par)  then  data.psf.extract_par = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.psf.support_par)  then  data.psf.support_par = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.psf.smooth_par)  then  data.psf.smooth_par = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.psf.psf_display_par)  then  data.psf.psf_display_par = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.psf.psf_hdr)  then  data.psf.psf_hdr = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.psf.psf_fwhm)  then  data.psf.psf_fwhm = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.psf.x_psf)  then  data.psf.x_psf = ptr_new(/ALLOCATE)
	      if  not ptr_valid(data.psf.y_psf)  then  data.psf.y_psf = ptr_new(/ALLOCATE)
	      end
	   'out': begin
	      if  n_elements(*data.draw) eq 0  then  data.draw = ptr_new()
	      if  n_elements(*data.path) eq 0  then  data.path = ptr_new()
	      if  n_elements(*data.file) eq 0  then  data.file = ptr_new()
	      if  n_elements(*data.last_display) eq 0  then  data.last_display = ptr_new()
	      if  n_elements(*data.last_display_par) eq 0  then  data.last_display_par = ptr_new()
	      if  n_elements(*data.other_display_par) eq 0  then  data.other_display_par = ptr_new()
	      if  n_elements(*data.image.image) eq 0  then  data.image.image = ptr_new()
	      if  n_elements(*data.image.image_display_par) eq 0  then  data.image.image_display_par = ptr_new()
	      if  n_elements(*data.image.image_hdr) eq 0  then  data.image.image_hdr = ptr_new()
	      if  n_elements(*data.image.background) eq 0  then  data.image.background = ptr_new()
	      if  n_elements(*data.image.back_display_par) eq 0  then  data.image.back_display_par = ptr_new()
	      if  n_elements(*data.image.back_hdr) eq 0  then  data.image.back_hdr = ptr_new()
	      if  n_elements(*data.image.backbox) eq 0  then  data.image.backbox = ptr_new()
	      if  n_elements(*data.image.noise_par) eq 0  then  data.image.noise_par = ptr_new()
	      if  n_elements(*data.image.g_noise_par) eq 0  then  data.image.g_noise_par = ptr_new()
	      if  n_elements(*data.image.plot_par) eq 0  then  data.image.plot_par = ptr_new()
	      if  n_elements(*data.image.noise_std) eq 0  then  data.image.noise_std = ptr_new()
	      if  n_elements(*data.image.x_badpix) eq 0  then  data.image.x_badpix = ptr_new()
	      if  n_elements(*data.image.y_badpix) eq 0  then  data.image.y_badpix = ptr_new()
	      if  n_elements(*data.image.run_par) eq 0  then  data.image.run_par = ptr_new()
	      if  n_elements(*data.image.x_stars) eq 0  then  data.image.x_stars = ptr_new()
	      if  n_elements(*data.image.y_stars) eq 0  then  data.image.y_stars = ptr_new()
	      if  n_elements(*data.image.f_stars) eq 0  then  data.image.f_stars = ptr_new()
	      if  n_elements(*data.image.sx_stars) eq 0  then  data.image.sx_stars = ptr_new()
	      if  n_elements(*data.image.sy_stars) eq 0  then  data.image.sy_stars = ptr_new()
	      if  n_elements(*data.image.sf_stars) eq 0  then  data.image.sf_stars = ptr_new()
	      if  n_elements(*data.image.c_stars) eq 0  then  data.image.c_stars = ptr_new()
	      if  n_elements(*data.image.stars) eq 0  then  data.image.stars = ptr_new()
	      if  n_elements(*data.image.stars_display_par) eq 0  then  data.image.stars_display_par = ptr_new()
	      if  n_elements(*data.image.syn_field) eq 0  then  data.image.syn_field = ptr_new()
	      if  n_elements(*data.image.syn_display_par) eq 0  then  data.image.syn_display_par = ptr_new()
	      if  n_elements(*data.psf.psf) eq 0  then  data.psf.psf = ptr_new()
	      if  n_elements(*data.psf.extract_par) eq 0  then  data.psf.extract_par = ptr_new()
	      if  n_elements(*data.psf.support_par) eq 0  then  data.psf.support_par = ptr_new()
	      if  n_elements(*data.psf.smooth_par) eq 0  then  data.psf.smooth_par = ptr_new()
	      if  n_elements(*data.psf.psf_display_par) eq 0  then  data.psf.psf_display_par = ptr_new()
	      if  n_elements(*data.psf.psf_hdr) eq 0  then  data.psf.psf_hdr = ptr_new()
	      if  n_elements(*data.psf.psf_fwhm) eq 0  then  data.psf.psf_fwhm = ptr_new()
	      if  n_elements(*data.psf.x_psf) eq 0  then  data.psf.x_psf = ptr_new()
	      if  n_elements(*data.psf.y_psf) eq 0  then  data.psf.y_psf = ptr_new()
	      end
	   endcase
	return
end

; SESSION_EV: save or restore XStarFinder session.

PRO session_ev, data, action

	on_error, 2
	case  action  of
	   'save': begin
	      file = dialog_pickfile(/WRITE, FILTER = '*.sav', $
	      						 PATH = *data.path, GET_PATH = path)
	      if  file ne '' then begin
	         widget_control, /HOURGLASS
	         if  strpos(file, '.sav') lt 0  then  file = file + '.sav'
	         *data.path = path
	         check_ptr, data, 'out'
	         save, FILENAME = file, data
	         check_ptr, data, 'in'
	      endif
	      end
	   'restore': begin
	      file = dialog_pickfile(/READ, FILTER = '*.sav', $
	      						 PATH = *data.path, GET_PATH = path)
	      if  file ne '' then begin
	         widget_control, /HOURGLASS
	         restore, file
	         check_ptr, data, 'in'
	         *data.path = path
	      endif
	      end
	endcase
	return
end

; XSTARFINDER_HELP_EV: display help file.

PRO xstarfinder_help_ev, name, group

	on_error, 2
	file = '_help.txt'  &  title = ' help'
	case strlowcase(name) of
	   'xstarfinder': begin
	      file = 'xstarfinder' + file
	      title = 'XStarFinder' + title
	      end
	   'image': begin
	      file = 'image' + file
	      title = 'Image' + title
	      end
	   'psf': begin
	      file = 'psf' + file
	      title = 'PSF' + title
	      end
	   'display': begin
	      file = 'display' + file
	      title = 'Display' + title
	      end
	endcase
	xdispfile, file_name('starfinder', file), TITLE = title, GROUP = group
	return
end

; XSTARFINDER_EVENT: XStarFinder event handler.

PRO xstarfinder_event, event

	catch, error
	if  error ne 0  then begin
	   msg = dialog_message(/ERROR, !err_string)
	   child = widget_info(event.top, /CHILD)
	   widget_control, child, SET_UVALUE = data, /NO_COPY
	   return
	endif
	child = widget_info(event.top, /CHILD)
	widget_control, child, GET_UVALUE = data, /NO_COPY
	quit_confirm = 0B

	case event.value of

	   'Image.Load....Image': begin
	      load_image_ev, data.image.image, data.path, $
	      				 HDR = data.image.image_hdr, /DISPLAY, $
	      				 *data.draw, data.image.image_display_par, $
	      				 LOADED = loaded
	      if  loaded  then begin
	         init_data, data, 'image'
	         data.last_display = data.image.image
	         data.last_display_par = data.image.image_display_par
	      endif
	      end
	   'Image.Load....Background': begin
	      load_image_ev, data.image.background, data.path, $
	      				 HDR = data.image.back_hdr, /DISPLAY, $
	      				 *data.draw, data.image.back_display_par
	      data.last_display = data.image.background
	      data.last_display_par = data.image.back_display_par
	      end
	   'Image.Noise.Load': load_image_ev, data.image.noise_std, data.path
	   'Image.Noise.Compute': noise_std_ev, data, event.top
	   'Image.Bad Pixels': replace_badpix_ev, data, event.top
	   'Image.Reference sources': select_ref_ev, data
	   'Image.Help': xstarfinder_help_ev, 'image', event.top
	   'Image.Save.Image': $
	      save_image_ev, data.image.image, data.path, $
	   					 HDR = data.image.image_hdr
	   'Image.Save.Background': $
	      save_image_ev, data.image.background, data.path, $
	      				 HDR = data.image.back_hdr
	   'Image.Save.Detected stars': $
	      save_image_ev, data.image.stars, data.path
	   'Image.Save.Synthetic field': $
	      save_image_ev, data.image.syn_field, data.path
	   'Image.Save.List of stars': save_list_ev, data

	   'PSF.Load...': begin
	      load_image_ev, data.psf.psf, data.path, $
	      				 HDR = data.psf.psf_hdr, /DISPLAY, $
	      				 *data.draw, data.psf.psf_display_par, $
	      				 LOADED = loaded
	      if  loaded  then begin
	         init_data, data, 'psf'
	         data.last_display = data.psf.psf
	         data.last_display_par = data.psf.psf_display_par
	         for rep = 1, 2 do *data.psf.psf = *data.psf.psf / total(*data.psf.psf)
	      endif
	      end
	   'PSF.Extract from image': psf_extract_ev, data, event.top
	   'PSF.Post process.Support': psf_support_ev, data, event.top
	   'PSF.Post process.Halo smoothing': psf_smooth_ev, data, event.top
;	   'PSF.Normalize': normalize_psf_ev, data
	   'PSF.Help': xstarfinder_help_ev, 'psf', event.top
	   'PSF.Save': $
	       save_image_ev, data.psf.psf, data.path, HDR = data.psf.psf_hdr

	   'Astrometry and Photometry': astrom_photom_event, data, event.top

	   'Compare Lists': $
	      xcompare_lists, *data.draw, PATH = *data.path, GROUP = event.top

	   'Display.Select data.Image': begin
	      display_image_ev, *data.draw, data.path, $
	      					data.image.image_display_par, $
	      					data.image.image, HDR = data.image.image_hdr
	      data.last_display = data.image.image
	      data.last_display_par = data.image.image_display_par
	      end
	   'Display.Select data.PSF': begin
	      display_image_ev, *data.draw, data.path, $
	      					data.psf.psf_display_par, $
	      					data.psf.psf, HDR = data.psf.psf_hdr
	      data.last_display = data.psf.psf
	      data.last_display_par = data.psf.psf_display_par
	      end
	   'Display.Select data.Background': begin
	      display_image_ev, *data.draw, data.path, $
	      					data.image.back_display_par, $
	      					data.image.background, HDR = data.image.back_hdr
	      data.last_display = data.image.background
	      data.last_display_par = data.image.back_display_par
	      end
	   'Display.Select data.Detected stars': begin
	      display_image_ev, *data.draw, data.path, $
	      					data.image.stars_display_par, $
	      					data.image.stars
	      data.last_display = data.image.stars
	      data.last_display_par = data.image.stars_display_par
	      end
	   'Display.Select data.Synthetic field': begin
	      display_image_ev, *data.draw, data.path, $
	      					data.image.syn_display_par, $
	      					data.image.syn_field
	      data.last_display = data.image.syn_field
	      data.last_display_par = data.image.syn_display_par
	      end
	   'Display.Select data.Other...': begin
	      ptr_free, data.other_display_par
	      data.other_display_par = ptr_new(/ALLOCATE)
	      display_image_ev, *data.draw, data.path, $
	                        data.other_display_par, FILE = data.file
	      data.last_display = data.file
	      data.last_display_par = data.other_display_par
	      end
	   'Display.Options': $
	      display_options_ev, *data.draw, data.last_display, $
	      			  data.last_display_par, event.top
	   'Display.Help': xstarfinder_help_ev, 'display', event.top

	   'Session.Save': session_ev, data, 'save'
	   'Session.Restore': session_ev, data, 'restore'

	   'Help': xstarfinder_help_ev, 'xstarfinder', event.top

	   'Quit': begin
	      msg = dialog_message(/QUESTION, 'Really quit XStarFinder?')
	      quit_confirm = strlowcase(msg) eq 'yes'
	      if  quit_confirm  then begin
	         xstarfinder_del, data		; de-allocates heap variables
	         widget_control, event.top, /DESTROY
	      endif
	      end

	   else:

	endcase
	if  event.value ne 'Quit' or $
		event.value eq 'Quit' and not quit_confirm  then $
	   widget_control, child, SET_UVALUE = data, /NO_COPY
	return
end



;;; WIDGET DEFINITION PROCEDURES/FUNCTIONS

; XSTARFINDER_PROCESSING_MENU: define menu 1.

FUNCTION xstarfinder_processing_menu

	return, ['1\Image', $
			 '1\Load...', '0\Image', '2\Background', $
			 '1\Noise', '0\Compute', '2\Load', $
			 '0\Bad Pixels', $
			 '0\Reference sources', $
			 '1\Save', '0\Image', '0\Background', '0\Detected stars', $
			    '0\Synthetic field', '2\List of stars', $
			 '2\Help', $   ; end of Image sub-menu
			 '1\PSF', $
			 '0\Load...', '0\Extract from image', $
			 '1\Post process', '0\Support', '2\Halo smoothing', $
;			 '0\Save', '2\Help',	$   ; end of PSF sub-menu
			 '0\Normalize', '0\Save', '2\Help',	$   ; end of PSF sub-menu
			 '0\Astrometry and Photometry', $   ; Astrometry - Photometry button
			 '0\Compare Lists']   ; Compare Lists button
end

; XSTARFINDER_UTILITIES_MENU: define menu 2.

FUNCTION xstarfinder_utilities_menu

	return, ['1\Display', $
			 '1\Select data', '0\Image', '0\PSF', '0\Background', $
			 '0\Detected stars', '0\Synthetic field', '2\Other...', $
			 '0\Options', '2\Help', $
			 '1\Session', '0\Save', '2\Restore', '0\Help', '0\Quit']
end

; XSTARFINDER_DEF: define structure of pointers to the data
; required by the program.

FUNCTION xstarfinder_def

	return, $
	{ image: $		; image data
		{image: ptr_new(/ALLOCATE), $
		 image_display_par: ptr_new(/ALLOCATE), $
		 image_hdr: ptr_new(/ALLOCATE), $
		 background: ptr_new(/ALLOCATE), $
		 back_display_par: ptr_new(/ALLOCATE), $
		 back_hdr: ptr_new(/ALLOCATE), $
		 backbox: ptr_new(/ALLOCATE), $
		 noise_par: ptr_new(/ALLOCATE), $
		 g_noise_par: ptr_new(/ALLOCATE), $
		 plot_par: ptr_new(/ALLOCATE), $
		 noise_std: ptr_new(/ALLOCATE), $
		 x_badpix: ptr_new(/ALLOCATE), y_badpix: ptr_new(/ALLOCATE), $
		 run_par: ptr_new(/ALLOCATE), $
		 x_stars: ptr_new(/ALLOCATE), $
		 y_stars: ptr_new(/ALLOCATE), $
		 f_stars: ptr_new(/ALLOCATE), $
		 sx_stars: ptr_new(/ALLOCATE), $
		 sy_stars: ptr_new(/ALLOCATE), $
		 sf_stars: ptr_new(/ALLOCATE), $
		 c_stars: ptr_new(/ALLOCATE), $
		 stars: ptr_new(/ALLOCATE), stars_display_par: ptr_new(/ALLOCATE), $
		 syn_field: ptr_new(/ALLOCATE), syn_display_par: ptr_new(/ALLOCATE)}, $
	  psf: $		; PSF data
	  	{psf: ptr_new(/ALLOCATE), $
		 extract_par: ptr_new(/ALLOCATE), $
		 support_par: ptr_new(/ALLOCATE), $
		 smooth_par: ptr_new(/ALLOCATE), $
	  	 psf_display_par: ptr_new(/ALLOCATE), $
	  	 psf_hdr: ptr_new(/ALLOCATE), $
	  	 psf_fwhm: ptr_new(/ALLOCATE), $
	  	 x_psf: ptr_new(/ALLOCATE), y_psf: ptr_new(/ALLOCATE)}, $
	  draw: ptr_new(/ALLOCATE),				 $
	  path: ptr_new(/ALLOCATE),				 $
	  file: ptr_new(/ALLOCATE),				 $
	  last_display: ptr_new(/ALLOCATE),		 $
	  other_display_par: ptr_new(/ALLOCATE), $
	  last_display_par: ptr_new(/ALLOCATE) }
end

; XSTARFINDER_DEL: delete heap variables.

PRO xstarfinder_del, data

	on_error, 2
	; de-allocate memory pointed by data structure fields
	ptr_free, data.image.image, data.image.image_display_par, $
		  data.image.image_hdr, data.image.background, $
		  data.image.back_display_par, data.image.back_hdr, $
		  data.image.backbox, data.image.noise_par, $
		  data.image.g_noise_par, data.image.plot_par, $
		  data.image.noise_std, data.image.x_badpix, data.image.y_badpix, $
		  data.image.run_par, data.image.x_stars, data.image.y_stars, $
		  data.image.f_stars, data.image.sx_stars, data.image.sy_stars, $
		  data.image.sf_stars, data.image.c_stars, data.image.stars, $
		  data.image.stars_display_par, data.image.syn_field, $
		  data.image.syn_display_par, $
		  data.psf.psf, data.psf.extract_par, data.psf.support_par, $
		  data.psf.smooth_par, data.psf.psf_display_par, $
		  data.psf.psf_hdr, data.psf.psf_fwhm, $
		  data.psf.x_psf, data.psf.y_psf, $
		  data.draw, data.path, data.file, data.last_display, $
		  data.other_display_par, data.last_display_par
	; de-allocate any inaccessible heap variables
	heap_gc
	return
end

; XSTARFINDER: XStarFinder widget definition module.

PRO xstarfinder, GROUP = group, BLOCK = block, UVALUE = uvalue

	on_error, 2
	; define top level base, partitioned into two parts: left and right
	if  n_elements(uvalue) eq 0  then  uvalue = 0B
	s = round(0.7 * min(get_screen_size()))
	base = widget_base(TITLE = 'XStarFinder', COLUMN = 2, UVALUE  = uvalue)
	left_base = widget_base(base, ROW = 2)
	right_base = widget_base(base)
	; define commands (in left part of base)
	menu = xstarfinder_processing_menu()
	processing = cw_pdmenu(left_base, menu, /COLUMN, /RETURN_FULL_NAME)
	menu = xstarfinder_utilities_menu()
	util = cw_pdmenu(left_base, menu, /COLUMN, /RETURN_FULL_NAME)
	; define draw window (in right part of base)
	draw = widget_draw(right_base, SCR_XSIZE = s, SCR_YSIZE = s, $
					   /ALIGN_CENTER, RETAIN = 2)
	; realize widgets
	widget_control, base, /REALIZE
	widget_control, draw, GET_VALUE = wnum
	; allocate memory for global data and store it in user value
	; of the left base
	data = xstarfinder_def()  &  *data.draw = wnum
	widget_control, left_base, SET_UVALUE = data, /NO_COPY
	; register with XManager
	xmanager, 'xstarfinder', base, EVENT_HANDLER = 'xstarfinder_event', $
			  GROUP_LEADER = group, NO_BLOCK = not keyword_set(block) and 1B
	return
end
