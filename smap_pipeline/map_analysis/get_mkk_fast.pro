FUNCTION GET_MKK_FAST,mask1,mask2=mask2,deltal,pixsize,lmin=lmin,kpower=kpower,callc=callc,mpi=mpi,n_samp=n_samp,log_deltal=log_deltal

;+
;NAME
;  get_mkk_fast
;PURPOSE
;  To compute the angular power spectra
; 
;USAGE
;  Mkk=get_mastrix_mkk(mask,deltal,pixsize)
;INPUTS
;  mask : input mask is an 2D array
;  deltal : the bin size of the ell mode
;  pixsize : the size of the pixel of the map in arcseconds
;  
;;KEYWORDS
; lmin : minimum mode to include in the computation (default is 0)
; kpower : to set if one wants to weight mode ell by ell^kpower (default is 0)
; callc: use the C program to compute the Mkk matrix (faster), if set to 1 use 
;        serial code, if set to > 1 use openMP version with callc processors
; mpi : now you can also request to run a mpi/openmp version, it will
;       run on mpi number of nodes, callc number of threads 
;       change the machinefile to use a different configuration
;               
;MODIFICATION HISTORY
;  Author: Alex Amblard, Dec 9, 2009
;           AA, Apr 14, 2010 : add the mpi option
;           Joseph Smidt, Jan 13, 2011, Randomly sample bin instead of
;              using every point to spped up.
;           
;-

; maybe need to add something in k^kpower to take that into account
; in the mask

if NOT(KEYWORD_SET(N_SAMP)) THEN $
	n_samp=200.

IF NOT(KEYWORD_SET(lmin)) THEN lmin=0
  
dims=(SIZE(mask1))[1:2]

kx=FINDGEN(dims[0]/2+1)/(FLOAT(pixsize*dims[0]*!pi/10800./60.))*2.*!pi
kx=[kx,-1.*REVERSE(kx[1:(1+dims[0])/2-1])]
ky=FINDGEN(dims[1]/2+1)/(FLOAT(pixsize*dims[1]*!pi/10800./60.))*2.*!pi
ky=[ky,-1.*REVERSE(ky[1:(1+dims[1])/2-1])]
kx=kx#(ky*0+1)
ky=(kx[*,0]*0+1)#ky
k=sqrt(kx^2+ky^2)
kx=0.
ky=0.

h=HISTOGRAM(k,binsize=deltal,reverse_indices=R,min=lmin)
binnedell=FINDGEN(N_ELEMENTS(h))*deltal+deltal/2.
nk=n_elements(h)
Ck=dblarr(nk)
Mkk=dblarr(nk,nk)
k2=dblarr(nk)

IF NOT(KEYWORD_SET(mask2)) THEN $
	spec=ABS(FFT(double(mask1),/double))^2 $
	else $
	spec=FLOAT(FFT(mask1,/double)*CONJ(FFT(mask2,/double)))

FOR i=0l,nk-1 DO IF R[i] NE R[i+1] THEN k2[i]=N_ELEMENTS([R[R[I] : R[I+1]-1]])

xtgtall=R MOD dims[0]
w=WHERE(xtgtall GT dims[0]/2)
IF (w[0] NE -1) THEN xtgtall[w]-=dims[0]
ytgtall=R/dims[0]
w=WHERE(ytgtall GT dims[1]/2)
IF (w[0] NE -1) THEN ytgtall[w]-=dims[1]


;   print, strcompress('nk is '+string(nk))
   FOR i=0l,nk-1 DO IF R[i] NE R[i+1] THEN BEGIN
;      print, i
      xtgt=xtgtall[R[i]:R[i+1]-1]
      ytgt=ytgtall[R[i]:R[i+1]-1]
      FOR j=i,nk-1 DO IF R[j] NE R[j+1] THEN BEGIN
         xtgt2=xtgtall[R[J]:R[J+1]-1]
         ytgt2=ytgtall[R[J]:R[J+1]-1]
         ntgt2=N_ELEMENTS(xtgt2)
	 sel=long(randomu(seed,n_samp)*ntgt2)
	 FOR k=0,199 do $
	     Mkk[i,j]+=TOTAL(spec[ABS(xtgt2[sel[k]]-xtgt),ABS(ytgt2[sel[k]]-ytgt)],/double)*ntgt2/n_samp
      ENDIF
   ENDIF




FOR i=0l,nk-1 DO $
   FOR j=i,nk-1 DO Mkk[j,i]=Mkk[i,j]

FOR i=0l,nk-1 DO $
   Mkk[i,*]/=k2[i]


RETURN,Mkk

END
