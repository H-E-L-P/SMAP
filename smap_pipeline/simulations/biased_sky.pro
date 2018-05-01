function biased_sky, spec, ell, res, sidex, sidey=sidey, hd=hd, writefileto=writefileto

IF NOT(KEYWORD_SET(sidey)) THEN sidey = sidex

k=make_k(sidex, sidey=sidey,res=res)

; the 0.5 allow to take the square root of the spectrum
scl=exp(0.5*interpol(alog(spec),alog(ell),alog(k)))
; Put the average to zero
scl[0]=0.

m=randomn(seed,sidex,sidey)
m=fft(temporary(m))
m=fft(temporary(m)*scl,/inverse)/(res*!pi/3600./180.)

;EXPAND TO WRITE FITS FILES
if not(keyword_set(hd)) then begin
   dims=(SIZE(scl))[1:2]
   mkhdr, ohd, scl
   cdout=dblarr(2,2)
   cdout[0,0]=-1.*res/3600.
   cdout[1,1]=+1.*res/3600.
   make_astr, astr, CD=cdout, DELT=[1,1], CRPIX=[dims[0]/2+1,dims[1]/2+1], CRVAL=[0,0]
   putast, ohd, astr
   hd=ohd
endif

if keyword_set(writefileto) then $
   fits_write, writefileto, float(m), hd

return, float(m)

end

