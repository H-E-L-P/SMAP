; $Id: rot_trans.pro, v 1.0 Aug 1999 e.d. $
;
;+
; NAME:
;	ROT_TRANS
;
; PURPOSE:
;	Rotate and translate a set of points on a plane.
;
; CATEGORY:
;	Mathematics.
;
; CALLING SEQUENCE:
;	ROT_TRANS, X, Y, Origin, Angle, Rtx, Rty
;
; INPUTS:
;	X:	X- coordinates of points
;
;	Y:	Y- coordinates of points
;
;	Origin:	2-components vector, containing the x- and y-
;		coordinates of the origin of the new set of coordinates
;		with respect to the old one
;	Angle:	Angle (in radians) between the x-axes of the old and the
;		new reference frames
;
; OUTPUTS:
;	Rtx, Rty:	X- and y- coordinates of points in the translated
;		rotated reference frame
;
; PROCEDURE:
;	Transform the input coordinates according to the following rules:
;	Rtx = -Origin[0] + X * cos(angle) + Y * sin(angle)
;	Rty = -Origin[1] - X * sin(angle) + Y * cos(angle)
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, August 1999.
;-

PRO rot_trans, x, y, origin, angle, rtx, rty

	on_error, 2
	rtx = -origin[0] + x * cos(angle[0]) + y * sin(angle[0])
	rty = -origin[1] - x * sin(angle[0]) + y * cos(angle[0])
	return
end
