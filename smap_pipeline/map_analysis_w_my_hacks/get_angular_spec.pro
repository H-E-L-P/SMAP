FUNCTION GET_ANGULAR_SPEC,map,deltal,pixsize,secondmap=secondmap,lmin=lmin,kpower=kpower,mask1=mask1,mask2=mask2,beam=beam,fsky=fsky,mkk=mkk,ell=ell,nk=nk,callc=callc,mpi=mpi,dl_filt=dl_filt,outfilename=outfilename,log_deltal=log_deltal
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

IF KEYWORD_SET(dl_filt) THEN $
   Ck=GET_RAW_ANGULAR_SPEC_W_FILTERS(map,deltal,dl_filt,pixsize,secondmap=secondmap,lmin=lmin,kpower=kpower, $
   outfilename=outfilename,log_deltal=log_deltal) $
   else $
   Ck=GET_RAW_ANGULAR_SPEC(map,deltal,pixsize,secondmap=secondmap,lmin=lmin,kpower=kpower, $
   outfilename=outfilename,log_deltal=log_deltal)

nk=N_ELEMENTS(Ck)
ell=FINDGEN(nk)*deltal+deltal/2.+lmin

if keyword_set(log_deltal) then $
   ell=10.^(findgen(nk)*log_deltal+log_deltal/2.+alog10(lmin))

IF KEYWORD_SET(mask1) AND NOT(KEYWORD_SET(mask2)) THEN mask2=mask1

IF KEYWORD_SET(mask1) AND NOT(KEYWORD_SET(Mkk)) THEN BEGIN
   PRINT,"Computing mask effect"
   Mkk=GET_MATRIX_MKK(mask1,mask2=mask2,deltal,pixsize,lmin=lmin,kpower=kpower,callc=callc,mpi=mpi, log_deltal=log_deltal)
ENDIF


IF KEYWORD_SET(beam) THEN $
   IF (size(beam))[0] THEN beaml=beam ELSE beaml=exp(-1*ell^2*(beam/3600.*!pi/180.)^2/(8*alog(2.)))


IF KEYWORD_SET(beam) THEN $
   IF (KEYWORD_SET(mask1) OR KEYWORD_SET(mkk)) THEN FOR i=0,nk-1 DO Mkk[*,i]*=beaml[i]

IF (KEYWORD_SET(mask1) OR KEYWORD_SET(mkk)) THEN BEGIN
   Mkki=invert(Mkk)
   Ck=Mkki#Ck
ENDIF

IF KEYWORD_SET(fsky) THEN Ck=Ck/fsky
   
IF (KEYWORD_SET(beam) AND NOT(KEYWORD_SET(mask1) OR KEYWORD_SET(mkk))) THEN Ck/=beaml

return,Ck

END
