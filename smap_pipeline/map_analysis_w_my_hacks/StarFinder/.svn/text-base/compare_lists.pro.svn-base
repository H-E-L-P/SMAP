; $Id: compare_lists, v 1.1 Feb 2000 e.d. $
;
;+
; NAME:
;	COMPARE_LISTS
;
; PURPOSE:
;	Given two sets of points on a plane, compare and match their
;	coordinates to find coincidences.
;
; CATEGORY:
;	Miscellaneous.
;
; CALLING SEQUENCE:
;	COMPARE_LISTS, X1, Y1, X2, Y2, X1c, Y1c, X2c, Y2c
;
; INPUTS:
;	X1, Y1:	Coordinates of first set of points
;
;	X2, Y2:	Coordinates of second set of points
;
; KEYWORD PARAMETERS:
;	MAX_DISTANCE:	Two points in set 1 and 2 are considered coincident if
;		their reciprocal distance is smaller than a pre-fixed threshold.
;		Use the MAX_DISTANCE threshold to fix a threshold.
;		If MAX_DISTANCE is undefined, any distance is considered acceptable.
;		In this case, the COMPARE_LISTS procedure just sorts the second
;		list according to the ordering of the first one.
;
; OUTPUTS:
;	X1c, Y1c:	Coordinates of common points in set 1
;
;	X2c, Y2c:	Coordinates of common points in set 2
;
; OPTIONAL OUTPUTS:
;	X_in1, Y_in1:	Coordinates of points in set 1 with no
;		counterpart in set 2.
;
;	X_in2, Y_in2:	Coordinates of points in set 2 with no
;		counterpart in set 1.
;
;	SUBSCRIPTS_1:	Use this output keyword to retrieve the subscripts of
;		the common points in set 1. In other words, the outputs X1c is
;		equal to the quantity X1[Subscripts1], where Subscripts1 is the
;		output value of the keyword SUBSCRIPTS_1.
;
;	SUBSCRIPTS_2:	Use this output keyword to retrieve the subscripts of
;		the common points in set 2. In other words, the outputs X2c is
;		equal to the quantity X2[Subscripts2], where Subscripts2 is the
;		output value of the keyword SUBSCRIPTS_2.
;
;	SUB1, SUB2:	Use these output keywords to retrieve the subscripts of
;		the elements belonging only to set 1 and 2 respectively.
;
; RESTRICTIONS:
;	The COMPARE_LISTS procedure assumes the two sets of coordinates are
;	referred to the same reference frame. If there is an offset or a rotation
;	between the two sets, use MATCH_COORD before (see 'match_coord.pro').
;
; PROCEDURE:
;	Find coincidences between points in sets 1 and 2 by comparing their
;	positions. For each point in the first set, the second set is searched
;	for the still unexamined point nearest to the point under examination.
;	If the distance between the two points is smaller than the pre-fixed
;	threshold, a new coincidence is found and recorded.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;   Updates:
;	1) Fixed bug on output keywords SUBSCRIPTS_1 and SUBSCRIPTS_2
;	   (Emiliano Diolaiti, January 2000).
;	2) Modified output keywords (Emiliano Diolaiti, February 2000).
;-

PRO compare_lists, x1, y1, x2, y2, x1c, y1c, x2c, y2c, $
				   MAX_DISTANCE = max_distance, $
				   SUBSCRIPTS_1 = subc1, SUBSCRIPTS_2 = subc2, $
				   SUB1 = sub1, SUB2 = sub2

	on_error, 2
	any_dist = n_elements(max_distance) eq 0 and 1B
	n1 = n_elements(x1)  &  n2 = n_elements(x2)  &  nc = min([n1, n2])
	x1c = make_array(nc, TYPE = size52(x1, /TYPE))
	y1c = make_array(nc, TYPE = size52(y1, /TYPE))
	x2c = make_array(nc, TYPE = size52(x2, /TYPE))
	y2c = make_array(nc, TYPE = size52(y2, /TYPE))
	marked1 = bytarr(n1)  &  marked2 = bytarr(n2)
	k = 0L
	for  n = 0L, n1 - 1  do begin
	   u = where(marked2 eq 0, n_unmarked)
	   if  n_unmarked ne 0  then begin
	      d = min(distance(x1[n], y1[n], x2[u], y2[u]), w)
	      accept_it = any_dist
	      if  not accept_it  then  accept_it = d le max_distance
	      if  accept_it  then begin
	         x1c[k] = x1[n]  &  y1c[k] = y1[n]
	         x2c[k] = x2[u[w]]  &  y2c[k] = y2[u[w]]
	         if  k eq 0  then begin
	            subc1 = n  &  subc2 = u[w]
	         endif else begin
	            subc1 = [subc1, n]  &  subc2 = [subc2, u[w]]
	         endelse
	         marked1[n] = 1B  &  marked2[u[w]] = 1B
	         k = k + 1
	      endif
	   endif
	endfor
	if  k ne 0  then begin
	   x1c = x1c[0:k-1]  &  y1c = y1c[0:k-1]
	   x2c = x2c[0:k-1]  &  y2c = y2c[0:k-1]
	endif
	sub1 = where(marked1 eq 0B)  &  sub2 = where(marked2 eq 0B)
	return
end
