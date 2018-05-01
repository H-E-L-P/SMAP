
;+
;NAME
; SMAP_MAKESIMSKY
;PURPOSE
; Create a simple random realization of the sky using a power spectrum P(k)
; 
;USAGE
; SMAP_MAKESIMSKY,specs,ells,res,numpix,outdir,nstart,nend,prefix
;
;INPUTS
; spec         power spectrum of the simulated maps
; ell          ell vector corresponding to the spec
; res          resolution in arcseconds of the output map (2" seems a 
;               good value for SPIRE) 
; numpix       number of pixel of the side of the output maps (the map 
;               will be numpix by numpix large)
; outdir       name of the directory where to output the simulations
; nstart       index of the first simulation to be done (useful to run 
;               several instance of the code simultaneously
; nend         index of the last simulation to be done 
; prefix       prefix to the name of the file containing the simulated map
;              the file will be named : prefix+index+'.dat', it is probably
;              wise to at least include the wavelength    
;
;ADDITIONAL REMARKS
; 
;          The original power spectrum is interpolated in log scale
;          assuming it varies a lot with respect to k 
;
;AUTHOR
; Alex Amblard
;-

FUNCTION MAKE_KX,nombre,taille=taille

  COMPILE_OPT HIDDEN
; nombre is the number of pixel on the side
; taille is the resolution in arcminute
; Default resolution is 1 arcminute

  IF NOT(KEYWORD_SET(taille)) THEN taille = 1.

  taillei = taille * !pi/10800.


  kx=FINDGEN(nombre/2+1)/(taillei*nombre)*2*!pi

  IF (nombre mod 2 eq 0) THEN $
     kx=[kx,-1.*REVERSE(kx[1:nombre/2-1])] $
  ELSE $
     kx=[kx,-1.*REVERSE(kx[1:nombre/2])]

  kx=kx#(kx*0+1.)

  RETURN,kx

END


PRO SMAP_MAKESIMSKY,spec,ell,res,numpix,outdir,nstart,nend,prefix
  COMPILE_OPT IDL2

  kx=make_kx(numpix,taille=res/60.)
  k=temporary(sqrt(kx^2+transpose(kx)^2))
; the 0.5 allow to take the square root of the spectrum
  scl=exp(0.5*interpol(alog(spec),alog(ell),alog(k)))
; Put the average to zero
  scl[0]=0.
  k=0b

  for ii=nstart,nend do begin
     m=randomn(seed,numpix,numpix)
     m=fft(temporary(m))
     m=fft(temporary(m)*scl,/inverse)/(res*!pi/3600./180.)
     openw,1,outdir+prefix+strcompress(ii,/remove_all)+'.dat'
     writeu,1,double(m)
     close,1
  endfor

END

