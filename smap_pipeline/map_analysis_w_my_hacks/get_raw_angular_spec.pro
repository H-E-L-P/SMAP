FUNCTION GET_RAW_ANGULAR_SPEC,map,deltal,pixsize,secondmap=secondmap,lmin=lmin,kpower=kpower,outfilename=outfilename,log_deltal=log_deltal

;+
;NAME
;  get_raw_angular_spec
;PURPOSE
;  To compute the raw angular power spectra 
; 
;USAGE
;  Ck=get_angular_spec(map,deltal,pixsize)
;INPUTS
;  map : input map is an 2D array
;  deltal : the bin size of the ell mode
;  pixsize : the size of the pixel of the map in arcseconds
;  
;;KEYWORDS
; secondmap : if one wants to cross-correlate with the map (need to be the same size)
;             and same pixel size
; lmin : minimum mode to include in the computation (default is 0)
; kpower : to set if one wants to weight mode ell by ell^kpower (default is 0)
;                     
;MODIFICATION HISTORY
;  Author: Alex Amblard, Dec 9, 2009
;-


IF (NOT(KEYWORD_SET(kpower))) THEN kpower=0
IF (NOT(KEYWORD_SET(lmin))) THEN lmin=0

dims=(SIZE(map))[1:2]

kx=FINDGEN(dims[0]/2+1)/(FLOAT(pixsize*dims[0]*!pi/10800./60.))*2.*!pi
kx=[kx,-1.*REVERSE(kx[1:(1+dims[0])/2-1])]
ky=FINDGEN(dims[1]/2+1)/(float(pixsize*dims[1]*!pi/10800./60.))*2.*!pi
ky=[ky,-1.*REVERSE(ky[1:(1+dims[1])/2-1])]
kx=kx#(ky*0+1)
ky=(kx[*,0]*0+1)#ky
k=sqrt(kx^2+ky^2)
kx=0b
ky=0b


if keyword_set(log_deltal) then begin
   h=HISTOGRAM(alog10(k),binsize=log_deltal,reverse_indices=R,min=alog10(lmin))
   binnedell=10.^(FINDGEN(N_ELEMENTS(h))*log_deltal+log_deltal/2.+alog10(lmin))
endif else begin
   h=HISTOGRAM(k,binsize=deltal,reverse_indices=R,min=lmin)
   binnedell=FINDGEN(N_ELEMENTS(h))*deltal+deltal/2.+lmin
endelse
nk=n_elements(h)
Ck=dblarr(nk)


IF KEYWORD_SET(secondmap) THEN $
   spec=FLOAT(FFT(map,/double)*CONJ(FFT(secondmap,/double)))*(!pi*pixsize/10800./60.)^2*(dims[0]*dims[1]) $
ELSE  spec=ABS(FFT(map,/double)*(!pi*pixsize/10800./60.))^2*(dims[0]*dims[1]) 

IF KEYWORD_SET(outfilename) then fits_write, outfilename, shift(spec,dims[0]/2,dims[1]/2)

FOR i=0l,nk-1 DO $
   IF R[i] NE R[i+1] THEN $
      Ck[i]=MEAN(spec[R[R[I] : R[I+1]-1]]*k[R[R[I] : R[I+1]-1]]^kpower)/(binnedell[i]^kpower)

spec=0b

return,Ck

END
