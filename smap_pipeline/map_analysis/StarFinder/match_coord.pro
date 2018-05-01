; $Id: match_coord.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	MATCH_COORD
;
; PURPOSE:
;	Given two sets of coordinates on a plane, representing the same points
;	in two different reference frames supposed to be reciprocally translated
;	and rotated, find the optimal transformation between the two sets and
;	map one of them onto the other.
;	The transformation between the two reference frames is defined by the
;	position of the origin of frame 2 in frame 1 and the angle between the
;	x- axes of frames 1 and 2, measured counter-clockwise from 1 to 2.
;
; CATEGORY:
;	Mathematics. Spatial transformations.
;
; CALLING SEQUENCE:
;	MATCH_COORD, X1, Y1, X2, Y2, X_ref, Y_ref, X2rt, Y2rt, Origin, Angle
;
; INPUTS:
;	X1, Y1:	Coordinates of points in reference frame no. 1
;
;	X2, Y2:	Coordinates of points in reference frame no. 2
;
;	X_ref, Y_ref:	Coordinates of reference points, used to find the
;		optimal transformation. X_ref, Y_ref may be a subset of X1, Y1
;
; KEYWORD PARAMETERS:
;	ORIGIN_0:	2-components vector, representing a guess of the position
;		of the origin of reference frame 2 in reference frame 1. The
;		default is  ORIGIN_0 = [0., 0.]
;
;	ANGLE_0:	Scalar, guess of the angle (in radians) between the x- axis
;		of reference frame 1 and the x- axis of reference frame 2, measured
;		counter-clockwise from 1 to 2. The default is  ANGLE_0 = 0. rad
;
;	_EXTRA:	Optional input keywords of FIND_ROT_TRANS (see the file
;		'find_rot_trans.pro')
;
; OUTPUTS:
;	X2rt, Y2rt:	Coordinates (X2, Y2) mapped onto reference frame 1.
;		These coordinates may be directly compared to (X1, Y1).
;		A negative scalar value indicates an error condition
;
; OPTIONAL OUTPUTS:
;	Origin:	2-components vector, representing the estimated position of
;		the origin of reference frame 2 in reference frame 1
;
;	Angle:	Scalar, estimated angle (in radians) between the x- axis of
;		reference frame 1 and the x- axis of reference frame 2, measured
;		counter-clockwise from 1 to 2
;
; RESTRICTIONS:
;	1) The procedure assumes the two reference frames are reciprocally rotated
;	and translated. Other kinds of spatial transformation, e.g. those due to
;	geometric distorsions, are not considered and should be negligible.
;	2) The set (X_ref, Y_ref) of reference positions may be a subset of
;	(X1, Y1), even though this is not necessary. What's essential is that,
;	after applying an approximate inverse transformation to (X2, Y2) using
;	the initial guesses ORIGIN_0 and ANGLE_0, it is possible to match each
;	point in the reference set (X_ref, Y_ref) with one and only one point in
;	both the sets (X1, Y1) and (X2, Y2). This occurs if
;		a) the initial guesses ORIGIN_0 and ANGLE_0 are accurate enough
;		b) the reference points are sufficiently isolated
;
; PROCEDURE:
;	Apply an inverse transformation to (X2, Y2) using the initial guesses.
;	Then match the reference points (X_ref, Y_ref) with their counterparts
;	in the sets (X1, Y1) and (X2, Y2) by a minimum distance criterion.
;	Use these subsets of (X1, Y1) and (X2, Y2) to find the transformation
;	between the reference frames 1 and 2 and then apply it to (X2, Y2), in
;	order to map this set of coordinates onto the reference frame of (X1, Y1).
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

PRO match_coord, x1, y1, x2, y2, x_ref, y_ref, x2rt, y2rt, origin, angle, $
				 ORIGIN_0 = origin_0, ANGLE_0 = angle_0, _EXTRA = extra

	on_error, 2
	; default values of reciprocal shift and rotation
	if  n_elements(origin_0) eq 0  then  origin_0 = [0., 0.]
	if  n_elements(angle_0) eq 0  then  angle_0 = 0.
	; approximate inverse transformation of (x2,y2) using origin_0 and angle_0
	rot_trans, x2 + origin_0[0], y2 + origin_0[1], [0., 0.], -angle_0, $
			   x2_temp, y2_temp
	; match reference positions with both the sets of coordinates
	n_ref = n_elements(x_ref)
	x1_ref = fltarr(n_ref)  &  y1_ref = x1_ref
	x2_ref = fltarr(n_ref)  &  y2_ref = x2_ref
	for  n = 0L, n_ref - 1  do begin
	   m = min(distance(x_ref[n], y_ref[n], x1, y1), w)
	   x1_ref[n] = x1[w]  &  y1_ref[n] = y1[w]
	   m = min(distance(x_ref[n], y_ref[n], x2_temp, y2_temp), w)
	   x2_ref[n] = x2_temp[w]  &  y2_ref[n] = y2_temp[w]
	endfor
	; check if the couples of reference coordinates are all distinct
	x2rt = -1  &  y2rt = -1	; set error condition
	remove_coincident, x1_ref, y1_ref, x1_ref, y1_ref
	remove_coincident, x2_ref, y2_ref, x2_ref, y2_ref
	if  n_elements(x1_ref) eq n_ref and n_elements(x2_ref) eq n_ref  then begin
	   ; direct transformation of (x2_ref, y2_ref) with origin_0 and angle_0
	   rot_trans, x2_ref, y2_ref, origin_0, angle_0, x2_temp, y2_temp
	   ; estimate optimal translation and rotation
	   find_rot_trans, x1_ref, y1_ref, x2_temp, y2_temp, origin_0, angle_0, $
					   origin, angle, found, _EXTRA = extra
	   ; apply optimal inverse transformation to original set (x2, y2)
	   if  found  then $
	      rot_trans, x2 + origin[0], y2 + origin[1], [0, 0], -angle, x2rt, y2rt
	endif
	return
end
