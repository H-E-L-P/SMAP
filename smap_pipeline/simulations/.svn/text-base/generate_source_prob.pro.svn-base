;+
;NAME
; GENERATE_SOURCE_PROB
;PURPOSE
; Generates a probability of position image from an input power
; spectrum.  Sources placed based on the probability image will
; have a power spectrum matching (to within realization noise) the
; input power spectrum
;USAGE
; im = GENERATE_SOURCE_PROB(ells, pl, numpix1, [ NUMPIX2=numpix2,
;                           PIXSIZE=, SEED= ])
;INPUTS
; ells         ell vector of l's matching the power spectrum pl
; pl           P(l) -- the power spectrum
; numpix1       number of pixel of the side of the output maps (the map
;               will be numpix1 by numpix1 large, but see numpix2)
;OUTPUTS
; im           The probability image, with the maximum set to unity
;OPTIONAL INPUTS
; numpix2      Second dimension; if set, the image is numpix1 by numpix2
; pixsize      Size of pixels in arcsec (def: 2")
; seed         Seed for random number generator
;KEYWORDS
; usingk       User provided k (inverse arcmin) rather than ell
;ADDITIONAL REMARKS
; The original power spectrum is interpolated in log space
;AUTHOR
; Alex Conley, mostly copied from smap_makesimsky_w_sources by Alex Amblard
;-

FUNCTION GENERATE_SOURCE_PROB, ells, pl, numpix1, NUMPIX2=numpix2, $
                               PIXSIZE=pixsize, SEED=seed, USINGK=usingk
  COMPILE_OPT IDL2, STRICTARRSUBS

  IF N_ELEMENTS(pixsize) EQ 0 THEN pixsize = 2.0
  IF pixsize LE 0.0 THEN MESSAGE,"Invalid pixel size"
  nell = N_ELEMENTS(ells)
  IF nell EQ 0 THEN MESSAGE,"No ell values present"
  IF N_ELEMENTS(pl) NE nell THEN MESSAGE,"ells and pl don't match in size"
  IF numpix1 LE 0 THEN MESSAGE,"Invalid (non-positive) numpix1"
  IF N_ELEMENTS(numpix2) EQ 0 THEN i_numpix2 = numpix1 ELSE $
     i_numpix2 = numpix2
  IF i_numpix2 LE 0 THEN MESSAGE,"Invalid (non-positive) numpix2"

  taillei = pixsize/60. * !PI/10800.0d0

  kxx=DINDGEN(numpix1/2+1)/(taillei*numpix1)*2*!PI
  kyy=DINDGEN(i_numpix2/2+1)/(taillei*i_numpix2)*2*!PI

  IF (numpix1 MOD 2 EQ 0) THEN $
     kxx=[kxx,-1.*REVERSE(kxx[1:numpix1/2-1])] $
  ELSE $
     kxx=[kxx,-1.*REVERSE(kxx[1:numpix1/2])]
  IF (i_numpix2 MOD 2 EQ 0) THEN $
     kyy=[kyy,-1.*REVERSE(kyy[1:i_numpix2/2-1])] $
  ELSE $
     kyy=[kyy,-1.*REVERSE(kyy[1:i_numpix2/2])]
  
  kx = kxx#REPLICATE(1.0,N_ELEMENTS(kyy))
  ky = REPLICATE(1.0,N_ELEMENTS(kxx))#kyy

  k = SQRT(TEMPORARY(kx)^2+TEMPORARY(ky)^2)

  ;;Interpolate power spectrum (log/log space)
  ;; the 0.5 allows us to take the square root of the spectrum
  
  IF KEYWORD_SET(usingk) THEN BEGIN
     ;;convert ks to ells, which is (for arcsec) ell = 21600*k
     cfac = ALOG(21600.0d0)
     scl=EXP(0.5*INTERPOL(ALOG(pl),ALOG(ells)+cfac,$
                          ALOG(TEMPORARY(k))))
  ENDIF ELSE BEGIN
     scl=EXP(0.5*INTERPOL(ALOG(pl),ALOG(ells),ALOG(TEMPORARY(k))))
  ENDELSE

  ;;Make the average be zero
  scl[0]=0.

  m=RANDOMN(seed,numpix1,i_numpix2,/DOUBLE)
  m=FFT(temporary(m))
  m=REAL_PART(FFT(temporary(m)*scl,/INVERSE)/(pixsize*!pi/(3600.*180.)))

  ;;Normalize to positive, max unity
  m -= MIN(m)
  m >= 0.0
  m /= MAX(m)

  RETURN,m

END
