Pro Arc, r1, r2, center = cent, radius = rad, angle = ang, degrees = deg, $
    bigarc = big, symmetric= sym, no_show = nsh, shapeout = parc, _extra = _e

;+
; NAME:
;	ARC
; VERSION:
;	3.3
; PURPOSE:
;	Draws a circular arc in the currently defined plot area.  
;	Alternatively, a *SHAPE* representation (see SHAPE_VER for definition) 
;	of the arc may be returned through the SHAPEOUT keyword.  DATA 
;	coordinates are used.  The method of drawing depends on the number of
;	input parameters provided (see details in CALLING SEQUENCE below).
; CATEGORY:
;	General Graphics.
; CALLING SEQUENCE:
;	There are two possible modes of operation, and accordingly two 
;	different calling sequences:
;	Mode 1:
;	    ARC, R1, CENTER = CENT, ANGLE = ANG [, optional keywords]
;	In this mode an arc with angular measure ANG is drawn around the point
;	CENT, in the mathematical-positive direction, starting from point R1.
;	Mode 2:
;	    ARC, R1, R2, {RADIUS = RAD, ANGLE = ANG} [, optional keywords]
;	In this mode the arc is drawn in the mathematical-positive direction,
;	from point R1 to point R2.  The arc is either drawn with radius RAD, or
;	corresponding to an angle ANG (both RAD and ANG cannot be specified).
; INPUTS:
;    R1, R2
;	2-dimensional vectors in the [x,y] format, representing points in the 
;	plotting area.  R1 is mandatory.  The presence of R2 indicates that 
;	Mode 2 is being used.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    CENTER
;	2 dimensional vector, arc center location, format [x,y].
;	Mandatory in Mode 1, forbidden in Mode 2.
;    RADIUS
;	Scalar, the radius of the arc.		|
;	Forbidden in Mode 1, allowed in Mode 2.	|  In Mode 2
;    ANGLE					|  one and only one
;	Scalar, the angle measure of the arc.	|  needs to be provided.
;	Mandatory in Mode 1, allowed in Mode 2. |
;    /DEGREES
;	Switch.  Indicates that the angle ANG is given in degrees (default is 
;	radians).
;    /BIGARC
;	Switch.  When drawing in Mode 2, with the radius provided, there are two
;	possible arcs that can be drawn,  corresponding to angles smaller and
;	bigger than 180 degrees (the sum of both angles is 360 degrees).  By
;	default the smaller arc is drawn. Setting BIGARC selects the bigger arc.
;    /SYMMETRIC
;	Switch.  In Mode 1 causes the arc to be drawn symmetrically on both 
;	sides of R1.  Forbidden in Mode 2.
;    /NO_SHOW
;	Switch.  If set, no plotting is done, but the shape is generated and 
;	may be returned through SHAPEOUT.
;    SHAPEOUT
;	Optional output, see below.
;    _EXTRA
;       A formal keyword used to pass all plotting keywords.  Not to be used
;       directly.
; OUTPUTS:
;	None.
; OPTIONAL OUTPUT PARAMETERS:
;    SHAPEOUT
;	When provided with the name of a variable, on return the variable 
;	contains the *SHAPE* representation of the arc.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	The arc will appear as a true circular arc only if the aspect ratio of
;	the X to Y axes is 1:1.
;	The keywords passed through _EXTRA are transferred to the PLOTS 
;	routine.  No keyword verification is performed by ARC.
; PROCEDURE:
;	Uses calls to ONE_OF and SHAPE_COCON from MIDL.
;	Generates a (2,N) array containing a sufficient number of the arc
;	points to yield a smooth curve.  N is variable, depending both on the 
;	arc size and on the pixel size of the current output device.
; MODIFICATION HISTORY:
;	Created 15-DEC-1991 by Mati Meron.
;       Modified 15-DEC-1993 by Mati Meron.  Now ARC takes advantage of the
;       keyword inheritance property and accepts all PLOTS keywords.
;	Modified 15-JUN-1995 by Mati Meron.  Removed the ALT_EXT keyword which
;	is no longer needed (_EXTRA has been fixed).
;	Modified 25-JUL-1999 by Mati Meron.  Added keywords NO_SHOW and 
;	SHAPEOUT.
;-

    on_error, 1
    nop = One_of(rad,ang)
    if nop eq -1 then message, 'Either radius or angle should be given!'
    if nop eq 1 then if keyword_set(deg) then dang= !dtor*ang else dang= 1.*ang
    tpi = 2*!pi

    errm = ['Underdefined!', 'Overdefined!', 'Invalid option!']
    case n_params() of
	0:	message, errm(0)
	1:	if n_elements(cent) eq 0 then message, errm(0) $
		else if nop eq 0 then message, errm(1) $
		else rad = sqrt(total((r1 - cent)^2))
	2:	begin
		    if keyword_set(sym) then message, errm(2) else $
		    if n_elements(cent) ne 0 then message, errm(1) else begin
			rt = .5*(r1 + r2)
			rl = .5*reverse(r1 - r2)*[1,-1]
			arl = sqrt(total(rl^2))
			if nop eq 0 then begin
			    if rad lt arl then message, 'impossible!'
			    c = sqrt((rad/arl)^2 - 1)
			    if keyword_set(big) then c = -c
			    dang = 2.*((atan(1./c,1) + !pi) mod !pi)
			endif else begin
			    c = 1./tan(dang/2.)
			    rad = arl*sqrt(c^2 + 1)
			endelse
			cent = rt + c*rl
		    endelse
		end
	else:	message, errm(1)
    endcase

    ray = r1 - cent
    stang = (atan(ray(1),ray(0)) + tpi) mod tpi
    if keyword_set(sym) then stang = stang - dang/2

    rdat = transpose([[[0,rad] + cent(0)], [[0,rad] + cent(1)]])
    rdev = Shape_cocon(rdat, to = 'dev')
    npoints = 1 + round(abs(dang)/2*sqrt(max(abs(rdev(*,1) - rdev(*,0)))))
    tem = dang/npoints*indgen(npoints + 1) + stang
    parc = transpose([[cent(0) + rad*cos(tem)], [cent(1) + rad*sin(tem)]])

    if not keyword_set(nsh) then plots, parc, _extra = _e

    return
end
