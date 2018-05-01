FUNCTION MAKE_CROSSLINK_MASK, jkhits, fwhm, DIRECTION=direction

;+
;
; mask = MAKE_CROSSLINK_MASK(jkhits, fwhm, DIRECTION=direction)
;
; Make a map mask that is 1 where there is converage from 1 or 0 scan 
; directions. "jkhits" is an array of jk_ang jackknife hits maps (2 or more). 
; Each map is first smoothed by a top-hat with diameter given by "fwhm".
;
; INPUTS:
;   jkhits:    3-d array containing direction (jk_ang*) jackknife hits maps 
;              [Nmaps, Nx, Ny]
;   fwhm:      FWHM of beam (in pixels) for smoothing each map
;
; OPTIONAL INPUTS:
;   direction: array [Nmaps] of 0s and 1s indicating which "group" 
;              (eg, horizontal or vertical) each hits map belongs to. 
;              Defaults to alternating 0/1.
;
; OUTPUT:
;   mask:      an image that is 1 where there is no cross-linking and 0 where
;              there is cross-linking
;
; CREATED BY: gmarsden 2011-07-18
;
; HISTORY:
;   2012-01-26 gm: update for hits-to-exposure time conversion
;-

s = SIZE(jkhits, /DIM)

IF N_ELEMENTS(s) NE 3 THEN $
   MESSAGE, "Input 'jkhits' must be 3-d array [Nmaps, Nx, Ny]"

nmaps = s[0]
nx = s[1]
ny = s[2]

; number of different directions (eg horizontal, vertical)
; in principle, can have more than 2, but keep it simple for now
ndir = 2

IF KEYWORD_SET(direction) THEN BEGIN
    ; check that direction array is formatted correctly
    IF N_ELEMENTS(direction) NE nmaps THEN $
       MESSAGE, "'direction' keyword has wrong number of elements"
    IF MAX(direction) GE ndir THEN $
       MESSAGE, "max direction value is " + STRSTRIM(STRING(ndir - 1, '(I)'), 2)
ENDIF ELSE BEGIN
    ; direction is alternating 0/1 if not set
    direction = LONARR(nmaps)
    ind = WHERE(INDGEN(nmaps) MOD 2 EQ 1, nind)
    IF nind GT 0 THEN direction[ind] = 1
ENDELSE

; output array
mask = LONARR(nx, ny)

npixsm = CEIL(fwhm)
kernel = LONARR(npixsm, npixsm)
kxx = REBIN(INDGEN(npixsm) - (npixsm - 1) / 2.0, npixsm, npixsm)
kyy = REBIN(REFORM(INDGEN(npixsm) - (npixsm - 1) / 2.0, 1, npixsm), $
            npixsm, npixsm)
kiii = WHERE( kxx^2 + kyy^2 LE (fwhm/2.0)^2 , nkiii)
IF nkiii GT 0 THEN kernel[kiii] = 1

hitsdirs = BYTARR(ndir, nx, ny)

FOR i=0,nmaps-1 DO BEGIN
    jkhits_sm = CONVOL(REFORM(jkhits[i,*,*]), kernel, /EDGE_TRUNC) GT 0
    hitsdirs[direction[i],*,*] = hitsdirs[direction[i],*,*] OR jkhits_sm
ENDFOR

miii = WHERE(TOTAL(hitsdirs, 1) LE 0.0, nmiii)
IF nmiii GT 0 THEN mask[miii] = 1

RETURN, mask

END
