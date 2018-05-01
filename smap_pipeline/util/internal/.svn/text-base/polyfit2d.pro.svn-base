FUNCTION POLYFIT2D, x, y, z, npow, ZERROR=zerror, ZFIT=zfit

;+
; pars = POLYFIT2D(x, y, z, npow, ZERROR=zerror, ZFIT=zfit)
;
; fit a 2-D polynomial to z(x,y)
;
; fits (npow+1)*(npow+2) / 2 terms 
;
; optional input zerror gives 1-sigma errors on z for proper weighting
;
; eg npow = 2: 
;
;   f = a00 + a01*y + a02*y^2 + a10*x + a11*x*y + a20*x^2
;
; that is, x = [a00, a01, a02, a10, a11, a20]
;
; use matrix algebra language for fit: 
;
;   A x = b
;
; where x is array of parameters to be fit, b is data and A is matrix
; nterms X ndata matrix. 
;
; Has weighted solution 
;
;   x = (At N^(-1) A)^(-1) At N^-1 b
;
; where t signifies transpose, (-1) inverse, and N is the noise
; covariance matrix (here, diagonal).
;
; Returns array p:
;
;   p[k] = a_ij, 
;
;-


IF N_PARAMS() LT 4 THEN npow = 0
npow = FIX(npow)

; number of terms to fit
nterms = (npow+1) * (npow+2) / 2

; length of data
n = N_ELEMENTS(x)

; column vectors
xx = DOUBLE(REFORM(x, 1, n))
yy = DOUBLE(REFORM(y, 1, n))
zz = DOUBLE(REFORM(z, 1, n))

; noise
IF KEYWORD_SET(zerror) THEN $
   invcov = DIAG_MATRIX(DOUBLE(REFORM(1.0/zerror^2, 1, n))) $
ELSE $
   invcov = DIAG_MATRIX(REPLICATE(1.0D, n))

a = DBLARR(nterms, n)

; fill in A matrix

ind = 0
FOR i=0,npow DO BEGIN
    FOR j=0,npow-i DO BEGIN
        a[ind,*] = xx^i * yy^j
        ind = ind + 1
    ENDFOR
ENDFOR

; solve
atn = TRANSPOSE(a) ## invcov
p = INVERT( atn ## a ) ## atn ## zz

; build fit
zfit = DBLARR(n)

ind = 0
FOR i=0,npow DO BEGIN
    FOR j=0,npow-i DO BEGIN
        zfit = zfit + p[ind] * xx^FLOAT(i) * yy^FLOAT(j)
        ind = ind + 1
    ENDFOR
ENDFOR

; return parameters
RETURN, REFORM(p)

END
