PRO SMAP_FOCALPLANE_JK, bololist, jkindex

;+
;
; SMAP_FOCALPLANE_JK, bololist, jkindex
;
; Divide up focal plane into equal sets for making focal
; plane jackknife maps. Removes bad bolos (in list below)
; and alternates bolos between groupings. 
;
; "bololist" is simply a list of all good bolos at all bands
; "jkindex" is a grouping index
;
; e.g.:
;
;   SMAP_FOCALPLANE_JK, bololist, jkindex
;   ; reject all bolos not in group 0
;   fpbadbolos = bololist[WHERE(jkindex NE 0)]
;   SMAP_MAKE_MAPS, ..., BADBOLOS=fpbadbolos, ...
;
;   gmarsden@phas.ubc.ca (20100304)
;
;-

; for now, just two groupings
njk = 2

pswcols = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'J']
pswnrow = [ 15,  16,  15,  16,  15,  16,  15,  16,  15]

pmwcols = ['A', 'B', 'C', 'D', 'E', 'F', 'G']
pmwnrow = [ 15,  16,  15,  16,  15,  16,  15]

plwcols = ['A', 'B', 'C', 'D', 'E']
plwnrow = [ 15,  16,  15,  16,  15]

badbolos = ['PSWD15', 'PSWC12', 'PSWG8', 'PSWG11','PSWA10','PSWA11', 'PSWA13']

; remove "central" bolo for PLW
badbolos = [badbolos, "PLWC5"]

pswnames = STRARR(TOTAL(pswnrow))
pmwnames = STRARR(TOTAL(pmwnrow))
plwnames = STRARR(TOTAL(plwnrow))

pswindex = INTARR(TOTAL(pswnrow))
pmwindex = INTARR(TOTAL(pmwnrow))
plwindex = INTARR(TOTAL(plwnrow))

; baseline plan: simply put all bolos in alternating groups

group = 0
ind = 0
FOR ic=0,N_ELEMENTS(pswcols)-1 DO BEGIN
    FOR ir=0,pswnrow[ic]-1 DO BEGIN
        name = 'PSW' + pswcols[ic] + STRTRIM(STRING(ir+1), 2)
        IF TOTAL(name EQ badbolos) GT 0 THEN CONTINUE
        pswnames[ind+ir] = name
        pswindex[ind+ir] = group
        group = (group + 1) MOD njk
    ENDFOR
    ind += pswnrow[ic]
ENDFOR

group = 0
ind = 0
FOR ic=0,N_ELEMENTS(pmwcols)-1 DO BEGIN
    FOR ir=0,pmwnrow[ic]-1 DO BEGIN
        name = 'PMW' + pmwcols[ic] + STRTRIM(STRING(ir+1), 2)
        IF TOTAL(name EQ badbolos) GT 0 THEN CONTINUE
        pmwnames[ind+ir] = name
        pmwindex[ind+ir] = group
        group = (group + 1) MOD njk
    ENDFOR
    ind += pmwnrow[ic]
ENDFOR

group = 0
ind = 0
FOR ic=0,N_ELEMENTS(plwcols)-1 DO BEGIN
    FOR ir=0,plwnrow[ic]-1 DO BEGIN
        name = 'PLW' + plwcols[ic] + STRTRIM(STRING(ir+1), 2)
        IF TOTAL(name EQ badbolos) GT 0 THEN CONTINUE
        plwnames[ind+ir] = name
        plwindex[ind+ir] = group
        group = (group + 1) MOD njk
    ENDFOR
    ind += plwnrow[ic]
ENDFOR

bololist = [pswnames, pmwnames, plwnames]
jkindex  = [pswindex, pmwindex, plwindex]






END
