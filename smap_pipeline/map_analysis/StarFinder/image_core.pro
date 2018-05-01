; $Id: image_core.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	IMAGE_CORE
;
; PURPOSE:
;	Given an image, identify the component connected to a specified
;	starting position and above a pre-fixed threshold.
;
; CATEGORY:
;	Data subsetting.
;
; CALLING SEQUENCE:
;	Result = IMAGE_CORE(Image, Threshold, Flag)
;
; INPUTS:
;	Image:	2D array to be searched
;
;	Threshold:	Lower threshold for connected component identification
;
; KEYWORD PARAMETERS:
;	X, Y:	Use this keywords to specify the starting position.
;		The default starting point is the maximum intensity pixel
;
;	SUBTRACT:	Set this keyword to a nonzero value to subtract the
;		value set by Threshold after extraction of the connected component
;
; OUTPUTS:
;	Result:	2D array, with the same size as the input Image, containing
;		the extracted connected component. Return the input Image if an
;		error occurs in SEARCH2D.
;
; OPTIONAL OUTPUTS:
;	Flag:	Logical value. It is set to true if the input Threshold is
;		greater than the intensity of the starting pixel specified by
;		the keywords X and Y.
;
; PROCEDURE:
;	Use the library routine SEARCH2D to identify the component connected
;	to the starting position and above the fixed Threshold.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

FUNCTION image_core, image, threshold, X = x, Y = y, SUBTRACT = subtract, flag

	on_error, 2
	flag = 0B
	if  n_elements(x) eq 0 or n_elements(y) eq 0  then begin
	   m = get_max(image)  &  x = m[0]  &  y = m[1]
	endif
	s = size52(image, /DIM)
	ccc = make_array(s[0], s[1], TYPE = size52(image, /TYPE))
	flag = threshold gt image[x,y]
	if  flag  then  return, image
	w = search2d(image, x, y, threshold, max(image))
	ccc[w] = image[w]
	if  keyword_set(subtract)  then  ccc[w] = ccc[w] - threshold
	return, ccc
end
