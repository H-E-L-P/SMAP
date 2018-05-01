FUNCTION GET_ANGULAR_SPEC,map,deltal,pixsize,$
   secondmap=secondmap,lmin=lmin,kpower=kpower,$
   mask1=mask1,mask2=mask2,beam=beam,fsky=fsky,$
   mkk=mkk,ell=ell,nk=nk,callc=callc,mpi=mpi,$
   dl_filt=dl_filt,rotate_angle1=rotate_angle1,rotate_angle2=rotate_angle2,$
   rotate_angle3=rotate_angle3, rotate_angle4=rotate_angle4, $
   retain_ls=retain_ls,$
   horizontal_stripe=horizontal_stripe, $
   vertical_stripe=vertical_stripe, $
   mkl=mkl,$
   ind_by_hand=ind_by_hand,$
   tf=tf,twodfft=twodfft,$
   outfilename=outfilename,log_deltal=log_deltal,$
   Ck_raw=Ck_raw,$
   cmb=cmb,$
   nbin=nbin
;+
;NAME
;  get_angular_spec
;PURPOSE
;  To compute the angular power spectra
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
; mask : if not all the pixels of the map are observed, this can compute the effect
;        due to lack of coverage (can be quite slow !)
; beam : size of the beam in arcseconds or vector in ell describing the beam transfert
;        function
; fsky : instead of using mask one can just correct for lack of coverage by dividing
;        the spectrum by fsky (fraction of the map observed)
; mkk  : optional input of the coupling matrix, if mask is set, the matrix is recomputed
;        and ouputed in this keyword
; ell  : output the ell for which Ck is computed
; nk   : output the number of ell for which Ck is computed
; callc: use the C program to compute the Mkk matrix (faster), if set to 1 use 
;        serial code, if set to > 1 use openMP version with callc processors
;
;                     
;MODIFICATION HISTORY
;  Author: Alex Amblard, Dec 9, 2009
;-

;  Author: Marco Viero, Dec 10, 2010
;	Allow for two masks in cross-correlations

if NOT(keyword_set(log_deltal))then log_deltal=0.
if NOT(KEYWORD_SET(lmin))then lmin=0
if NOT(KEYWORD_SET(nbin))then nbin=0

IF KEYWORD_SET(dl_filt) THEN $
   Ck=GET_RAW_ANGULAR_SPEC_W_FILTERS(map,deltal,dl_filt,pixsize,secondmap=secondmap,lmin=lmin,kpower=kpower, $
   twodfft=twodfft,rotate_angle1=rotate_angle1, rotate_angle2=rotate_angle2, retain_ls=retain_ls,ind_by_hand=ind_by_hand,$
   rotate_angle3=rotate_angle3, rotate_angle4=rotate_angle4, $
   horizontal_stripe=horizontal_stripe, $
   vertical_stripe=vertical_stripe, $
   outfilename=outfilename,log_deltal=log_deltal,tf=tf,nbin=nbin,cmb=cmb) $
   else $
   Ck=GET_RAW_ANGULAR_SPEC(map,deltal,pixsize,secondmap=secondmap,lmin=lmin,kpower=kpower, $
   twodfft=twodfft,outfilename=outfilename,log_deltal=log_deltal,tf=tf,nbin=nbin,cmb=cmb)

nk=N_ELEMENTS(Ck)
ell=DINDGEN(nk)*deltal+deltal/2.+lmin

if keyword_set(log_deltal) then $
   ell=10.^(findgen(nk)*log_deltal+log_deltal/2.+alog10(lmin))

IF KEYWORD_SET(mask1) AND NOT(KEYWORD_SET(mask2)) THEN mask2=mask1

IF KEYWORD_SET(mask1) AND NOT(KEYWORD_SET(Mkk)) THEN BEGIN
   PRINT,"Computing mask effect"
   Mkk=GET_MATRIX_MKK(mask1,mask2=mask2,deltal,pixsize,lmin=lmin,kpower=kpower,callc=callc,mpi=mpi, log_deltal=log_deltal)
   ;Mkk=GET_MKK_FAST(mask1,mask2=mask2,deltal,pixsize,lmin=lmin,kpower=kpower,callc=callc,mpi=mpi, log_deltal=log_deltal)
ENDIF


IF KEYWORD_SET(beam) THEN $
   IF (size(beam))[0] THEN beaml=beam ELSE beaml=exp(-1*ell^2*(beam/3600.*!pi/180.)^2/(8*alog(2.)))


IF KEYWORD_SET(beam) THEN $
   IF (KEYWORD_SET(mask1) OR KEYWORD_SET(mkk)) THEN FOR i=0,nk-1 DO Mkk[*,i]*=beaml[i]

IF (KEYWORD_SET(mask1) OR KEYWORD_SET(mkk)) THEN BEGIN
      Mkki=invert(Mkk)
   IF KEYWORD_SET(mkl) THEN BEGIN
      ;indbox=mkk*0.
      ;nmkk=n_elements(mkk[0,*])
      ;for i=0,nmkk-1 do indbox[i,*]=(shift(dist(1,nmkk)+1.,1.*i))
      ;ind_lim=closest(ell, k_to_ell(mkl))
      ;indbox[where(indbox ge ind_lim)]=0.
      ;indbox[0:ind_lim,nmkk-ind_lim:nmkk-1]=0.
      ;indbox[nmkk-ind_lim:nmkk-1,0:ind_lim]=0.
      ;Mkki[where(indbox eq 0)]=0.
      ;Ck=Mkki#Ck

      indz=where(ell ge k_to_ell(mkl))
      Ckt=Ck
      Cki=Ck
      Ckt[indz]=0.
      Ck=Mkki#Ckt
      Ck[indz]=Cki[indz]
      ;stop
   ENDIF ELSE begin
      ;Mkki=invert(Mkk)
      Ck=Mkki#Ck
   ENDELSE
ENDIF

IF KEYWORD_SET(fsky) THEN Ck=Ck/fsky
   
IF (KEYWORD_SET(beam) AND NOT(KEYWORD_SET(mask1) OR KEYWORD_SET(mkk))) THEN Ck/=beaml

return,Ck

END
