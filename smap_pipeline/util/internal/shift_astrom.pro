PRO SHIFT_ASTROM, header, rashift, decshift, ADDKEYS=addkeys

;+
;
; SHIFT_ASTROM, header, rashift, decshift, ADDKEYS=addkeys
;
; Shift map by (rashift, decshift) DEGREES by editing CRVAL* in map
; header.
;
; First, use header to convert (rashift, decshift) to shift in in tangent
; plane, (xshift, yshift).
;
; Then the CRVALs are updated to the values corresponding to the point
; (xprime, yprime) in the tangent plane, 
;
;   xprime = CRPIX1 - xshift
;   yprime = CRPIX2 - yshift
;
; Setting the keyword ADDKEYS will add the fields RASHIFT and DECSHIFT to
; the header. They do not take into account existing *SHIFT values,
; so shifting an already shifted map is not safe.
;
; 20100317 gmarsden@phas.ubc.ca
;
; CHANGELOG:
;   20110711 (GM) Now provide delta (ra,dec) offsets instead of tan plane
;                 (to allow for rotated coordinate systems)
;
;-

; convert rashift, decshift to tangent plane shifts
EXTAST, header, astr
shifts = INVERT(astr.cd) ## [rashift, decshift]

; tangent point
x0pix = SXPAR(header, 'CRPIX1') - 1
y0pix = SXPAR(header, 'CRPIX2') - 1

; determine the CRVALs of the pixel (-shifts[0], -shifts[1]) from the
; central PIXEL. 

XYAD, header, x0pix - shifts[0], y0pix - shifts[1], ra0, dec0

; update CRVAL*
SXADDPAR, header, 'CRVAL1', ra0
SXADDPAR, header, 'CRVAL2', dec0

; optionally add shifts to header
IF KEYWORD_SET(addkeys) THEN BEGIN
    SXADDPAR, header, 'RASHIFT', rashift, $
              " Post facto astrometry shift from MIPS24 (deg)"
    SXADDPAR, header, 'DECSHIFT', decshift, $
              " Post facto astrometry shift from MIPS24 (deg)"
ENDIF

END
