; $Id: click_on_max.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	CLICK_ON_MAX
;
; PURPOSE:
;	Interactive selection of local maxima in an image by mouse click.
;
; CATEGORY:
;	Data visualization.
;
; CALLING SEQUENCE:
;	CLICK_ON_MAX, Image, X, Y
;
; INPUTS:
;	Image:	2D data array containing local maxima to be selected
;
; KEYWORD PARAMETERS:
;	MARK:	Set this keyword to a nonzero value to mark selected objects
;		as soon as they are clicked on
;
;	N_SELECT:	Set this keyword to an integer (positive) number to specify
;		the number of maxima to select. If this keyword is not used, the
;		selection is interrupted by a right-button click.
;
;	BOXSIZE:	Specify the tolerance on the maximum position in pixel
;		units: after each click, the maximum within a box having this
;		size and centered at the click location is selected.
;		The default is BOXSIZE = 11.
;
;	UPPER:	Set this keyword to a scalar value or a 2D array with the same
;		size as the Image to specify the threshold above which the position
;		of a click must be retained as it is, without searching for the
;		nearest local maximum. This odd requirement is related to the
;		selection of stars in a stellar field.
;
;	DARK:	Set this keyword to a nonzero value to have dark marks for the
;		selected points. This options is available only if the keyword MARK
;		is set (as obvious). For more details, see the procedure CROSSES
;		in the file 'crosses.pro'.
;
;	SYMSIZE:	Specify the size of the symbol used by the routine CROSSES
;			to mark the selected points.
;
;	SILENT:	Set this keyword to avoid printing a brief instruction message
;
; OUTPUTS:
;	X, Y: x- and y- coordinates (in pixel units, data coordinates) of
;		the selected local maxima
;
; OPTIONAL OUTPUTS:
;	Mark each selected maximum if the keyword MARK is set.
;
; RESTRICTIONS:
;	This procedure assumes that the image containing the objects of
;	interest is already displayed in the currently active window.
;	It is also assumed that the window is completely filled by the image.
;
; PROCEDURE:
;	Select peaks by clicking with the left button of the mouse.
;	The selection ends when the number of maxima specified by the input
;	keyword N_SELECT is reached or when the right button of the mouse
;	is pushed.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;	2) Removed call to obsolete routine APPEND_ELEMENTS
;	   (Emiliano Diolaiti, June 2001).
;-

PRO click_on_max, image, MARK = mark, N_SELECT = n_sel, $
   		  BOXSIZE = boxsize, UPPER = threshold, SILENT = silent, $
   		  _EXTRA = extra, x, y

	on_error, 2
	s = float(size52(image, /DIM))
	if  n_elements(boxsize) eq 0  then  boxsize = 11
	if  n_elements(threshold) eq 0  then  threshold = max(image) + 1
	fixed = n_elements(n_sel) ne 0
	if  fixed  then  n_select = round(n_sel > 0)
	if  not keyword_set(silent)  then begin
	   print, 'Select by clicking with the left button of your mouse...'
	   if  not fixed  then  print, 'Push right button to exit'
	endif
	!MOUSE.button = 1  &  n_click = 0
	cursor, x_click, y_click, /NORMAL, /DOWN
	while  !MOUSE.button eq 1  do begin
		x_click = round(x_click * s[0])  &  y_click = round(y_click * s[1])
		if  boxsize ge 2 and image[x_click, y_click] lt threshold  then $
		   m = get_max(sub_array(image, boxsize, REF = [x_click, y_click], $
		   						 LX = lx, LY = ly)) $
		else begin
		   m = [0, 0]  &  lx = x_click  &  ly = y_click
		endelse
		x_click = x_click + m[0] - (x_click - lx)
		y_click = y_click + m[1] - (y_click - ly)
		if  n_elements(x_saved) eq 0  then begin
		   x_saved = x_click
		   y_saved = y_click
		endif else begin
		   x_saved = [x_saved, x_click]
		   y_saved = [y_saved, y_click]
		endelse
		if  keyword_set(mark)  then $
		   crosses, tvrd(), x_click/(s[0] - 1)*!D.x_size, y_click/(s[1] - 1)*!D.y_size, $
		   	    /EXISTING, _EXTRA = extra, /DEVICE
		if  fixed  then begin
		   n_click = n_click + 1
		   if  n_click eq n_select  then  !MOUSE.button = 4
		endif
		if  !MOUSE.button eq 1  then  cursor, x_click, y_click, /NORMAL, /DOWN
	endwhile
	if  n_elements(x_saved) ne 0  then begin
		x = x_saved  &  y = y_saved
	endif
	remove_coincident, x, y, x, y
	return
end
