FUNCTION SMAP_GET_ANGULAR_SPEC,mapin,deltal,secondmap=secondmap,lmin=lmin,kpower=kpower,mask=mask,beam=beam,fsky=fsky,mkk=mkk,ell=ell,nk=nk,callc=callc,nomask=nomask,mpi=mpi
;+
;NAME
;  smap_get_angular_spec
;PURPOSE
;  To compute the angular power spectra
; 
;USAGE
;  Ck=smap_get_angular_spec(map,deltal)
;INPUTS
;  map : smap formatted map
;  deltal : the bin size of the ell mode
;  
;;KEYWORDS
; secondmap : if one wants to cross-correlate with the map (need to be the same size)
;             and same pixel size
; lmin : minimum mode to include in the computation (default is 0)
; kpower : to set if one wants to weight mode ell by ell^kpower (default is 0)
; mask : if not all the pixels of the map are observed, this can compute the effect
;        due to lack of coverage (can be quite slow !), if not define it use NaN
;        value as the mask (unless nomask is set)
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
; nomask: specify no mask need to be corrected for
;                     
;MODIFICATION HISTORY
;  Author: Alex Amblard, Dec 9, 2009
;-

  maxprime=11
  map=double(mapin.image)
  pixsize=mapin.pixscale
    

  IF (NOT(KEYWORD_SET(mask)) AND NOT(KEYWORD_SET(nomask))) THEN $
     mask=double(finite(map)) 
  
  IF (KEYWORD_SET(fsky))THEN mask=0

  dims=(size(map))[1:2]
  IF (dims[0] GT dims[1]) THEN FACTOR,dims[0],n,p,/QUIET ELSE $
     FACTOR,dims[1],n,p,/QUIET

  newdim=max(dims)
  IF ((max(n) GT maxprime) OR (n[0] NE 2)) THEN BEGIN
     newdim=max(dims)+1
     WHILE ((newdim LT (max(dims)*2)) AND ((max(n) GT maxprime) OR (n[0] NE 2))) DO BEGIN 
        newdim++
        FACTOR,newdim,n,p,/QUIET
     ENDWHILE
  ENDIF

  maptouse=dblarr(newdim,newdim)
  posx_b=(newdim-dims[0])/2
  posy_b=(newdim-dims[1])/2
  maptouse[posx_b:posx_b+dims[0]-1,posy_b:posy_b+dims[1]-1]=map
  maptouse[where(finite(maptouse) eq 0)]=0.
  

  IF KEYWORD_SET(mask) THEN BEGIN
     masktouse=dblarr(newdim,newdim)
     masktouse[posx_b:posx_b+dims[0]-1,posy_b:posy_b+dims[1]-1]=mask
  ENDIF
  
  IF (KEYWORD_SET(secondmap)) THEN BEGIN
     scndmap=double(secondmap.image)
     secondmaptouse=dblarr(newdim,newdim)
     secondmaptouse[posx_b:posx_b+dims[0]-1,posy_b:posy_b+dims[1]-1]=scndmap
     secondmaptouse[where(finite(secondmaptouse) eq 0)]=0.
  ENDIF

  IF KEYWORD_SET(fsky) THEN $
     fsky=double(N_ELEMENTS(WHERE(maptouse NE 0)))/double(N_ELEMENTS(maptouse))
  
  IF KEYWORD_SET(mask) THEN BEGIN
     maptouse*=masktouse
     IF KEYWORD_SET(secondmap) THEN secondmaptouse*=masktouse
  ENDIF


  Ck=GET_ANGULAR_SPEC(maptouse,deltal,pixsize,secondmap=secondmaptouse,lmin=lmin,kpower=kpower,mask=masktouse,beam=beam,fsky=fsky,mkk=mkk,ell=ell,nk=nk,callc=callc,mpi=mpi)

  
  RETURN,Ck

END
