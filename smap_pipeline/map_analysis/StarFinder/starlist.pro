; $Id: starlist.pro, v 1.0 Jun 2001 e.d. $
;
;+
; NAME:
;	STAR
;
; PURPOSE:
;	Create named structure, called "starlet", representing a star.
;	This structure is the basic element of a list of stars, which
;	might both accepted stars and still presumed ones.
;
; CATEGORY:
;	STARFINDER auxiliary procedures.
;
; CALLING SEQUENCE:
;	Result = STAR()
;
; OUTPUTS:
;	Return "starlet" structure
;
;
;
; NAME:
;	UPDATE_LIST
;
; PURPOSE:
;	Update list of stars.
;
; CATEGORY:
;	STARFINDER auxiliary procedures.
;
; CALLING SEQUENCE:
;	UPDATE_LIST, List, SUBSCRIPTS = S, X, Y, F, C, $
;	             Sigma_X, Sigma_Y, Sigma_F, IS_STAR = Is_Star
;
; INPUTS:
;	List:	list of stars.
;
;	X, Y:	1D vectors with x- and y- position of stars to update.
;
; OPTIONAL INPUTS:
;	F, C:	1D vectors with flux and correlation coefficient of stars to update.
;
;	Sigma_X, Sigma_Y, Sigma_F:	errors on position and flux.
;
; KEYWORD PARAMETERS:
;	SUBSCRIPTS:	1D vector of subscripts of stars to update. If not defined,
;		update all the stars in the list.;
;
;	IS_STAR:	set this keyword to say that the stars to update have already
;		been accepted as true stars.
;
; OUTPUTS:
;	List:	updated list.
;
;
;
; NAME:
;	CREATE_ELEMENT
;
; PURPOSE:
;	Generate new element of star list and initialize it.
;
; CATEGORY:
;	STARFINDER auxiliary procedures.
;
; CALLING SEQUENCE:
;	Result = CREATE_ELEMENT(X, Y, F)
;
; INPUTS:
;	X, Y:	x- and y- position of object.
;
; OPTIONAL INPUTS:
;	F:	object flux.
;
; OUTPUTS:
;	Return initialized element, representing a (possibly)
;	presumed star
;
;
;
; NAME:
;	MERGE_LIST
;
; PURPOSE:
;	Merge two lists of stars.
;
; CATEGORY:
;	STARFINDER auxiliary procedures.
;
; CALLING SEQUENCE:
;	Result = MERGE_LIST(L1, L2)
;
; INPUTS:
;	L1, L2:	lists to merge. If L1 is empty, return L2.
;
; OUTPUTS:
;	Return merged list
;
;
;
; NAME:
;	ADD_SUBSCRIPT
;
; PURPOSE:
;	Add new subscript to subscript vector.
;
; CATEGORY:
;	STARFINDER auxiliary procedures.
;
; CALLING SEQUENCE:
;	Result = ADD_SUBSCRIPT(Subscripts, S)
;
; INPUTS:
;	Subscripts:	1D vector of subscripts.
;
;	S:	new subscripts to append to Subscripts
;
;
; OUTPUTS:
;	Return appended vector of subscripts.
;	If input vector Subscripts is not valid, return S.
;
;
;
; NAME:
;	DELETE_ELEMENT
;
; PURPOSE:
;	Delete last element from list of stars.
;
; CATEGORY:
;	STARFINDER auxiliary procedures.
;
; CALLING SEQUENCE:
;	Result = DELETE_ELEMENT(List)
;
; INPUTS:
;	List:	input list to trim.
;
; OUTPUTS:
;	Return trimmed list.
;
;
;
; NAME:
;	STAR_PARAM
;
; PURPOSE:
;	Extract stars parameters from star list, possibly including
;	presumed stars.
;
; CATEGORY:
;	STARFINDER auxiliary procedures.
;
; CALLING SEQUENCE:
;	STAR_PARAM, List, SUBSCRIPTS = S, $
;	            N, X, Y, F, C, Sigma_X, Sigma_Y, Sigma_F
;
; INPUTS:
;	List:	list of stars
;
; KEYWORD PARAMETERS:
;	SUBSCRIPTS:	1D vector of subscript of stars to be extracted.
;		If undefined, extract parameters of all true stars in the list.
;
; OUTPUTS:
;	N:	number of extracted stars
;
;	X, Y, F:	position and flux of stars
;
;	C:	correlation coefficienf
;
;	Sigma_X, Sigma_Y, Sigma_F:	errors on position and flux
;
;
;
; NAME:
;	WHERE_STARS
;
; PURPOSE:
;	Find subscripts of stars in a given star list, which might also
;	include presumed stars.
;
; CATEGORY:
;	STARFINDER auxiliary procedures.
;
; CALLING SEQUENCE:
;	Result = WHERE_STARS(List, LX = Lx, UX = Ux, LY = Ly, UY = Uy, N)
;
; INPUTS:
;	List:	star list
;
; KEYWORD PARAMETERS:
;	LX, UX, LY, UY:	fix lower and upper x- and y- bounds of image
;		region where the stars in the list have to searched
;
; OUTPUTS:
;	Return subscripts of stars, possibly falling within specified
;	region
;
; OPTIONAL OUTPUTS:
;	N:	number of found stars
;
;
;
; NAME:
;	EXTRACT_STARS
;
; PURPOSE:
;	Return sub-list of stars, extracted from a list which might include
;	presumed stars.
;
; CATEGORY:
;	STARFINDER auxiliary procedures.
;
; CALLING SEQUENCE:
;	Result = EXTRACT_STARS(List, N)
;
; INPUTS:
;	List:	star list
;
; OUTPUTS:
;	Return sublist of true stars
;
; OPTIONAL OUTPUTS:
;	N:	number of extracted stars
;
;
;
; NAME:
;	SORT_LIST
;
; PURPOSE:
;	Sort stars by decreasing flux.
;
; CATEGORY:
;	STARFINDER auxiliary procedures.
;
; CALLING SEQUENCE:
;	Result = SORT_LIST(List, SUBSCRIPTS = S)
;
; INPUTS:
;	List:	list of stars
;
; KEYWORD PARAMETERS:
;	SUBSCRIPTS:	subscripts of stars to be sorted. If undefined, sort
;		all stars in the list
;
; OUTPUTS:
;	Return sorted list of sublist, if SUBSCRIPTS is set.
;
;
;
; NAME:
;	REVERSE_CLASS
;
; PURPOSE:
;	Reverse classification (star/not star) of an element in a list.
;
; CATEGORY:
;	STARFINDER auxiliary procedures.
;
; CALLING SEQUENCE:
;	Result = REVERSE_CLASS(List, SUBSCRIPTS = S)
;
; INPUTS:
;	List:	list of stars and presumed stars
;
; KEYWORD PARAMETERS:
;	SUBSCRIPTS:	subscripts of elements whose classification
;		must be reversed. If undefined, reverse classification
;		of all elements in the list
;
; OUTPUTS:
;	Return list where the classification of the subscripted elements;
;	is reversed
;
;
;
; NAME:
;	STARLIST
;
; PURPOSE:
;	Create a new list of elements representing either
;	true or presumed stars.
;
; CATEGORY:
;	STARFINDER auxiliary procedures.
;
; CALLING SEQUENCE:
;	Result = STARLIST(N, X, Y, F)
;
; INPUTS:
;	N:	number of elements in the list
;
; OPTIONAL INPUTS:
;	X, Y, F:	1D vectors with position and flux of new elements
;
; OUTPUTS:
;	Return possibly initialized list of N elements
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, June 2001.
;-



FUNCTION star

	return, {starlet, $
	         x: 0., sigma_x: 0., $   ; x- coordinate and error
	         y: 0., sigma_y: 0., $   ; y- coordinate and error
	         f: 0., sigma_f: 0., $   ; flux and error
	         c: -1., $			      ; correlation
	         is_a_star: 0B}			   ; flag
end



PRO update_list, list, SUBSCRIPTS = s, x, y, f, c, $
	              sigma_x, sigma_y, sigma_f, IS_STAR = is_star

	on_error, 2
	n = n_elements(s)
	if  n eq 0  then  s = lindgen(n_elements(x))
	if  s[0] ge 0  then begin
	   list[s].x = x  &  list[s].y = y
	   if  n_elements(f) ne 0  then  list[s].f = f
	   if  n_elements(c) ne 0  then  list[s].c = c
	   if  n_elements(sigma_x) ne 0  then  list[s].sigma_x = sigma_x
	   if  n_elements(sigma_y) ne 0  then  list[s].sigma_y = sigma_y
	   if  n_elements(sigma_f) ne 0  then  list[s].sigma_f = sigma_f
	   list[s].is_a_star = keyword_set(is_star) and 1B
	endif
	return
end



FUNCTION create_element, x, y, f

	on_error, 2
	element = star()
	update_list, element, x, y, f
	return, element
end



FUNCTION merge_list, l1, l2

	on_error, 2
	n1 = n_elements(l1)
	if  n1 eq 0  then  l = l2  else $
	begin
	   n2 = n_elements(l2)
	   l = starlist(n1 + n2)
	   l[0] = l1  &  l[n1] = l2
	endelse
	return, l
end



FUNCTION add_subscript, subscripts, s

	on_error, 2
	if  subscripts[0] lt 0  then $
	   w = s  else  w = [subscripts, s]
	return, w
end



FUNCTION delete_element, list

	on_error, 2
	l = list  &  n = n_elements(l)
	return, l[0:n-2]
end



PRO star_param, list, SUBSCRIPTS = s, $
	             n, x, y, f, c, sigma_x, sigma_y, sigma_f

	on_error, 2
	if  n_tags(list) eq 0  then begin
	   n = 0L  &  s = -1L  &  return
	endif
	n = n_elements(s)
	if  n eq 0  then begin
	   n = n_elements(list)  &  s = lindgen(n)	; extract all elements
	endif
	if  s[0] lt 0  then  n = 0  else begin $
	   x = list[s].x  &  sigma_x = list[s].sigma_x
	   y = list[s].y  &  sigma_y = list[s].sigma_y
	   f = list[s].f  &  sigma_f = list[s].sigma_f
	   c = list[s].c
	endelse
	return
end



FUNCTION where_stars, list, LX = lx, UX = ux, LY = ly, UY = uy, n

	on_error, 2
	if  n_tags(list) eq 0  then begin
	   n = 0L  &  return, -1L
	endif
	flag = list.is_a_star
	if  n_elements(lx) ne 0  then $
	   flag = flag and list.x ge lx and list.x le ux $
	   			   and list.y ge ly and list.y le uy
	return, where(flag and 1B, n)
end



FUNCTION extract_stars, list, n

	on_error, 2
	s = where_stars(list, n)
	if  n ne 0  then  return, list[s]  else  return, -1
end



FUNCTION sort_list, list, SUBSCRIPTS = s

	on_error, 2
	if  n_tags(list) eq 0  then  return, list
	s = reverse(sort(list.f))
	return, list[s]
end



FUNCTION reverse_class, list, SUBSCRIPTS = s

	on_error, 2
	if  n_elements(s) eq 0  then  s = lindgen(n_elements(list))
	l = list
	l[s].is_a_star = (not l[s].is_a_star) and 1B
	return, l
end



FUNCTION starlist, n, x, y, f

	on_error, 2
	element = star()
	list = replicate(element, n)
	if  n_elements(x) ne 0 and n_elements(y) ne 0  then $
	   update_list, list, x, y, f
	return, list
end

