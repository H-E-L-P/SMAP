FUNCTION MAKE_FILTER_MASK, noisemap, fwhm

;+
;
; mask = MAKE_FILTER_MASK(noisemap, fwhm)
;
; Make a map mask that is 1 where the filter map is "well-behaved", to 
; indicate what region of the map should be considere. "noisemap" is the 
; noise map extention. We find all pixels that have noise < 2 * median of 
; central map region and smooth smoothed by a top-hat with diameter given 
; by "fwhm". These are the "good" pixels.
;
; INPUTS:
;   noisemap:  noise map (duh)
;   fwhm:      FWHM of beam (in pixels) for smoothing each map
;
; OUTPUT:
;   mask:      an image that is 1 in regions with large noise
;
; CREATED BY: gmarsden 2011-07-19
;
; CHANGELOG:
;   20110721/GM: use find_instnoise.pro to get noise params
;
;-

; mask out pixels that are > cutval * median
cutval = 1.2

; find all pix < nsigmakeep * sigma of peak
noiseval = FIND_INSTNOISE(noisemap, NOISEWID=noisewid)

goodind = WHERE(FINITE(noisemap) AND noisemap LE noiseval*cutval, ngood)

; mark good pixels in mask
s = SIZE(noisemap, /DIM)
nx = s[0]
ny = s[1]

mask = LONARR(nx, ny)
mask[goodind] = 1

; smoothing kernel
npixsm = CEIL(fwhm)
kernel = LONARR(npixsm, npixsm)
kxx = REBIN(INDGEN(npixsm) - (npixsm - 1) / 2.0, npixsm, npixsm)
kyy = REBIN(REFORM(INDGEN(npixsm) - (npixsm - 1) / 2.0, 1, npixsm), $
            npixsm, npixsm)
kiii = WHERE( kxx^2 + kyy^2 LE (fwhm/2.0)^2 , nkiii)
IF nkiii GT 0 THEN kernel[kiii] = 1

; smooth
mask_sm = CONVOL(mask, kernel, /EDGE_TRUNC) < 1

; invert masking
mask_ret = mask_sm XOR 1

RETURN, mask_ret

END
