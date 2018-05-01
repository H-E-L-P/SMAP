PRO SMAP_SHIFT_ASTROM, smapfile, xshift, yshift

;+
;
; SMAP_SHIFT_ASTROM, smapfile, xshift, yshift
;
; Apply astrometery shift (xshift, yshift) to SMAP map "smapfile".
;
; 20100318 gmarsden@phas.ubc.ca
;
;-

; read file
RDFITS_STRUCT, smapfile, mapstruct

h1 = mapstruct.hdr1
h2 = mapstruct.hdr2
h3 = mapstruct.hdr3
h4 = mapstruct.hdr4

SHIFT_ASTROM, h1, xshift, yshift, /ADDKEYS
SHIFT_ASTROM, h2, xshift, yshift, /ADDKEYS
SHIFT_ASTROM, h3, xshift, yshift, /ADDKEYS
SHIFT_ASTROM, h4, xshift, yshift, /ADDKEYS

; write data back to file
MWRFITS, mapstruct.im0, smapfile, mapstruct.hdr0, /CREATE
MWRFITS, mapstruct.im1, smapfile, h1
MWRFITS, mapstruct.im2, smapfile, h2
MWRFITS, mapstruct.im3, smapfile, h3
MWRFITS, mapstruct.im4, smapfile, h4

END
