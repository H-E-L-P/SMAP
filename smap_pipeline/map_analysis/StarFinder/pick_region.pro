; $Id: pick_region.pro, v 1.1 Aug 2005 e.d. $
;
;+
; NAME:
;   PICK_REGION
;
; PURPOSE:
;   Given the bounds of a partition of a rectangular domain on a plane
;   and the coordinates of a point on the same plane, compute the
;   subscript of the sub-region containing the specified point.
;
; CATEGORY:
;   Array processing.
;
; CALLING SEQUENCE:
;   Result = PICK_REGION(Lx, Ux, Ly, Uy, X, Y)
;
; INPUTS:
;   Lx, Ux:   Vectors of bounds of the partition along the x-axis.
;
;   Ly, Uy:   Vectors of bounds of the partition along the y-axis.
;
;   X, Y: Coordinates of the point falling on some sub-region.
;
; OUTPUTS:
;   Result:   Scalar subscript of the sub-region containing the point
;     of coordinates (X, Y).
;
; MODIFICATION HISTORY:
;   Written by:  Emiliano Diolaiti, January 2000.
;   1) Fixed bug on output Result, that was NOT scalar (E.D. August 2005)
;-

FUNCTION pick_region, lx, ux, ly, uy, x, y

    on_error, 2
    nx = n_elements(lx)  &  x_rep = replicate(round(x), nx)
    ny = n_elements(ly)  &  y_rep = replicate(round(y), ny)
    wx = where(x_rep ge lx and x_rep le ux)
    wy = where(y_rep ge ly and y_rep le uy)
    return, wy[0] * nx + wx[0]
end

