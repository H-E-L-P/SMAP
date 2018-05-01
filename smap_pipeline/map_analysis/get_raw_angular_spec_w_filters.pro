FUNCTION GET_RAW_ANGULAR_SPEC_W_FILTERS,map,deltal,dl_filt,pixsize,$
   kpower=kpower, $
   secondmap=secondmap,lmin=lmin, $
   rotate_angle1=rotate_angle1, rotate_angle2=rotate_angle2, $
   rotate_angle3=rotate_angle3, rotate_angle4=rotate_angle4, $
   retain_ls=retain_ls, $ 
   horizontal_stripe=horizontal_stripe, $
   vertical_stripe=vertical_stripe, $
   ind_by_hand=ind_by_hand,$
   tf=tf,twodfft=twodfft, $
   outfilename=outfilename, log_deltal=log_deltal, $
   Ck_raw=Ck_raw,$
   cmb=cmb,$
   nbin=nbin
	
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
;  Author: Marco Viero, 2010+ Made it compatible with masking
;-


IF (NOT(KEYWORD_SET(kpower))) THEN kpower=0
IF (NOT(KEYWORD_SET(lmin))) THEN lmin=0
IF (NOT(KEYWORD_SET(rotate_angle1))) THEN rotate_angle1=0
IF (NOT(KEYWORD_SET(rotate_angle2))) THEN rotate_angle2=0
IF (NOT(KEYWORD_SET(rotate_angle3))) THEN rotate_angle3=0
IF (NOT(KEYWORD_SET(rotate_angle4))) THEN rotate_angle4=0

dims=(SIZE(map))[1:2]

kx=DINDGEN(dims[0]/2+1)/(FLOAT(pixsize*dims[0]*!pi/10800./60.))*2.*!pi
kx=[kx,-1.*REVERSE(kx[1:(1+dims[0])/2-1])]
ky=DINDGEN(dims[1]/2+1)/(float(pixsize*dims[1]*!pi/10800./60.))*2.*!pi
ky=[ky,-1.*REVERSE(ky[1:(1+dims[1])/2-1])]
kx=kx#(ky*0+1)
ky=(kx[*,0]*0+1)#ky
k=sqrt(kx^2+ky^2)
kx=0b
ky=0b

dkx=k[1,0]
dky=k[0,1]
stripe_mask=map*0.+1.
if dl_filt ge dkx then begin
   if rotate_angle1 eq 0 and rotate_angle2 eq 0 then begin 
      ind_vsm=rebin(dist(dims[0],1), dims[0], dims[1])*dkx
      stripe_mask[where(ind_vsm le 1.*dl_filt)]=0.
   endif else begin
      if rotate_angle1 ne 0 then begin
	 ind_stripe1=shift(rebin(dist(dims[0]*2.,1), dims[0]*2., dims[1]*2.)*sqrt(dkx*dky),-1.*dims[0],-1.*dims[1])
	 ind_stripe1=ROT(ind_stripe1, rotate_angle1+45.) 
	 ind_stripe1=shift(ind_stripe1,1.*dims[0]+1,1.*dims[1]+1)
	 ind_stripe1=rebin(ind_stripe1, dims[0],dims[1])
	 stripe_mask[where(ind_stripe1 le 2.*dl_filt)]=0.
      endif
      if rotate_angle2 ne 0 then begin
	 ind_stripe2=shift(rebin(dist(dims[0]*2.,1), dims[0]*2., dims[1]*2.)*sqrt(dkx*dky),-1.*dims[0],-1.*dims[1])
	 ind_stripe2=ROT(ind_stripe2, rotate_angle2+45.) 
	 ind_stripe2=shift(ind_stripe2,1.*dims[0]+1,1.*dims[1]+1)
	 ind_stripe2=rebin(ind_stripe2, dims[0],dims[1])
	 stripe_mask[where(ind_stripe2 le 2.*dl_filt)]=0.
      endif
      if rotate_angle3 ne 0 then begin
	 ind_stripe3=shift(rebin(dist(dims[0]*2.,1), dims[0]*2., dims[1]*2.)*sqrt(dkx*dky),-1.*dims[0],-1.*dims[1])
	 ind_stripe3=ROT(ind_stripe3, rotate_angle3+45.) 
	 ind_stripe3=shift(ind_stripe3,1.*dims[0]+1,1.*dims[1]+1)
	 ind_stripe3=rebin(ind_stripe3, dims[0],dims[1])
	 stripe_mask[where(ind_stripe3 le 2.*dl_filt)]=0.
      endif
      if rotate_angle4 ne 0 then begin
	 ind_stripe4=shift(rebin(dist(dims[0]*2.,1), dims[0]*2., dims[1]*2.)*sqrt(dkx*dky),-1.*dims[0],-1.*dims[1])
	 ind_stripe4=ROT(ind_stripe4, rotate_angle4+45.) 
	 ind_stripe4=shift(ind_stripe4,1.*dims[0]+1,1.*dims[1]+1)
	 ind_stripe4=rebin(ind_stripe4, dims[0],dims[1])
	 stripe_mask[where(ind_stripe4 le 2.*dl_filt)]=0.
      endif
   endelse
endif
;stop

if keyword_set(log_deltal) then begin
   h=HISTOGRAM(alog10(k),binsize=log_deltal,reverse_indices=R,min=alog10(lmin)) 
   binnedell=10.^(DINDGEN(N_ELEMENTS(h))*log_deltal+log_deltal/2.+alog10(lmin))
endif else begin
   h=HISTOGRAM(k,binsize=deltal,reverse_indices=R,min=lmin)
   binnedell=DINDGEN(N_ELEMENTS(h))*deltal+deltal/2.+lmin
endelse
nbin=h

nk=n_elements(h)
Ck=dblarr(nk)
Ck_raw=dblarr(nk)
;BooArr=dblarr(nk)

IF KEYWORD_SET(secondmap) THEN $
   spec=FLOAT(FFT(map,/double)*CONJ(FFT(secondmap,/double)))*(!pi*pixsize/10800./60.)^2*(dims[0]*dims[1]) $
ELSE  spec=ABS(FFT(map,/double)*(!pi*pixsize/10800./60.))^2*(dims[0]*dims[1]) 

if keyword_set(retain_ls) then begin
   ind_rls=where(k le retain_ls)
   stripe_mask[ind_rls]=1.
endif
if keyword_set(horizontal_stripe) then begin
   ind_horz_stripe=shift(rebin(dist(dims[0]*2.,1), dims[0]*2., dims[1]*2.)*sqrt(dkx*dky),-1.*dims[0],-1.*dims[1])
   ind_horz_stripe=shift(ind_horz_stripe,1.*dims[0]+1,1.*dims[1]+1)
   ind_horz_stripe=rebin(ind_horz_stripe, dims[0],dims[1])
   stripe_mask[where(ind_horz_stripe le 2.*horizontal_stripe)]=0.
endif
if keyword_set(vertical_stripe) then begin
   ind_vert_stripe=shift(rebin(dist(dims[0]*2.,1), dims[0]*2., dims[1]*2.)*sqrt(dkx*dky),-1.*dims[0],-1.*dims[1])
   ind_vert_stripe=ROT(ind_vert_stripe, 90.) 
   ind_vert_stripe=shift(ind_vert_stripe,1.*dims[0]+1,1.*dims[1]+1)
   ind_vert_stripe=rebin(ind_vert_stripe, dims[0],dims[1])
   stripe_mask[where(ind_vert_stripe le 2.*vertical_stripe)]=0.
endif
mask=stripe_mask
if keyword_set(ind_by_hand) then begin
  dummy=mask*0.+1. 
  dummy[ind_by_hand]=0.
  dummy=shift(dummy,-1.*dims[0]/2,-1.*dims[1]/2)
  mask*=dummy
endif

mss=size(spec,/dim)
msv=size(mask,/dim)
if msv[0] eq mss[0] and msv[1] eq mss[1] then $
   spec*=mask $
else begin
   print, 'fucked--> mask and fft not same size'
   stop
endelse
if keyword_set(tf) then begin
   mstf=size(tf,/dim)
   if mstf[0] eq mss[0] and mstf[1] eq mss[1] then begin
      spec/=tf
      spec=clean_nans(spec)
   endif else begin
      print, 'fucked--> transfer function and fft not same size'
      stop
   endelse
endif
;
mkhdr, ohd, spec
cdout=dblarr(2,2)
cdout[0,0]=dkx;*3600.*!pi
cdout[1,1]=dky;*3600.*!pi
;make_astr, astr, CD=cdout, DELT=[1,1], CRPIX=[dims[0]/2+1,dims[1]/2+1], CRVAL=[0,0]
make_astr, astr, DELT=[ell_to_k(dkx),ell_to_k(dky)], CRPIX=[dims[0]/2+1,dims[1]/2+1], CRVAL=[0,0]
putast, ohd, astr
IF KEYWORD_SET(outfilename) then fits_write, outfilename, shift(spec,dims[0]/2,dims[1]/2),ohd
;IF KEYWORD_SET(outfilename) then fits_write, outfilename, spec
twodfft=spec

;spec*=mask
;stop
FOR i=0l,nk-1 DO $
   IF R[i] NE R[i+1] THEN BEGIN 
     tmp=mask[R[R[I] : R[I+1]-1]]
     indr=where(tmp ne 0,nr)
     if nr gt 0 then begin
	ktmp=k[R[R[I] : R[I+1]-1]]
	tspec=spec[R[R[I] : R[I+1]-1]]
;	if keyword_set(cmb) then $
;	Ck[i]=MEAN(tspec[indr]*ktmp[indr]^kpower)/mean(ktmp[indr]^kpower) $
;	else $
	if keyword_set(cmb) then $ 
	Ck[i]=MEAN(tspec[indr]*ktmp[indr]^2.)/2./!PI/mean(ktmp[indr]^kpower) $
	 else $	
	Ck[i]=MEAN(tspec[indr]*ktmp[indr]^kpower)/mean(ktmp[indr]^kpower) 
	;Ck[i]=MEAN(tspec[indr]*ktmp[indr]^kpower)/(binnedell[i]^kpower)
	;print, nr, binnedell[i]
     endif ;else stop
     Ck_raw[i]=MEAN(spec[R[R[I] : R[I+1]-1]]*k[R[R[I] : R[I+1]-1]]^kpower)/(binnedell[i]^kpower)
     ;Ck[i]=MEAN(spec[R[R[I] : R[I+1]-1]]*k[R[R[I] : R[I+1]-1]]^kpower)/(binnedell[i]^kpower)
     ;BooArr[i]=MEAN(mask[R[R[I] : R[I+1]-1]]*k[R[R[I] : R[I+1]-1]]^kpower)/(binnedell[i]^kpower)
  ENDIF

;   stop
spec=0b
stripe_mask=0b
mask=0b

;ind_zero=where(BooArr eq 0,nz)
;if nz gt 0 then BooArr[ind_zero]=1. 

;stop
;return,Ck/BooArr
return,Ck

END
