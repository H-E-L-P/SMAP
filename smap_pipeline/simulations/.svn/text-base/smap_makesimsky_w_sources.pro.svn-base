PRO SMAP_MAKESIMSKY_W_SOURCES,spec,ell,res,numpix,outdir,nstart,nend,$
                              prefix,m,convolve=convolve, $
                              bands=bands,hi_cut=hi_cut,lo_cut=lo_cut, $
                              fits=fits
;+
;NAME
; SMAP_MAKESIMSKY
;PURPOSE
; Create a simple random realization of the sky using a power spectrum P(k)
; 
;USAGE
; SMAP_MAKESIMSKY,specs,ells,res,numpix,outdir,nstart,nend,prefix
;INPUTS
; spec         power spectrum of the simulated maps
; ell          ell vector corresponding to the spec
; res          resolution in arcseconds of the output map (2"
;               seems a good value for SPIRE) 
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
;OPTIONAL INPUTS
;
;
;ADDITIONAL REMARKS
; 
;          The original power spectrum is interpolated in log scale
;          assuming it varies a lot with respect to k 
;
;AUTHOR
; Alex Amblard
;-


taillei = res/60. * !pi/10800.

kxx=FINDGEN(numpix/2+1)/(taillei*numpix)*2*!pi

IF (numpix mod 2 eq 0) THEN $
  kxx=[kxx,-1.*REVERSE(kxx[1:numpix/2-1])] $
ELSE $
  kxx=[kxx,-1.*REVERSE(kxx[1:numpix/2])]

kx=kxx#(kxx*0+1.)

;kx=make_kx(numpix,taille=res/60.)
k=temporary(sqrt(kx^2+transpose(kx)^2))
; the 0.5 allow to take the square root of the spectrum
scl=exp(0.5*interpol(alog(spec),alog(ell),alog(k)))
; Put the average to zero
scl[0]=0.
k=0b

cs_dir='/data/viero/sim_transfunc/clean_skies/'
size_prefix=strcompress('_'+string(res,format='(i10)')+'x'+ $
   string(numpix,format='(i10)')+'x'+string(numpix,format='(i10)'),/remove_all)
if KEYWORD_SET(HI_CUT) THEN $
   cut_suffix=strcompress('_'+string(hi_cut*1000.,format='(i101)')+'mJy',/remove_all) $
   ELSE BEGIN
   HI_CUT=100 
   cut_suffix=''
ENDELSE
for ii=nstart,nend do begin
   mfile=strcompress(cs_dir+prefix+size_prefix+'_'+strcompress(ii,/remove_all)+'.dat',/remove_all)
   ffile=strcompress(cs_dir+prefix+size_prefix+'_'+strcompress(ii,/remove_all)+'.fits',/remove_all)
   if FILE_TEST(mfile) then begin
      print, 'reading existing ',mfile
      m=dblarr(numpix,numpix)
      openr,1,mfile
      readu,1,m
      close,1
   endif else begin
      m=randomn(seed,numpix,numpix)
      m=fft(temporary(m))
      m=fft(temporary(m)*scl,/inverse)/(res*!pi/3600./180.)
      if not(keyword_set(fits)) then begin
	 openw,1,mfile
	 writeu,1,double(m)
	 close,1
      endif else begin
	 mkhdr,mhd, float(m)
	 sxaddpar,mhd,'CD2_2',res/3600.
	 sxaddpar,mhd,'CD1_1',-1.*res/3600.
	 fits_write, ffile, float(m),mhd
      endelse
   endelse

   if KEYWORD_SET(BANDS) THEN BEGIN
      if KEYWORD_SET(CONVOLVE) THEN cc=1 else cc =0
      nwv=n_elements(BANDS)
      m2=BIASED_SKYMODEL2(m,res,BANDS,convolve=cc,hi_cut=hi_cut)
      FOR wv=0,nwv-1 do begin
	 ;m2=BIASED_SKYMODEL(m,res,BANDS[wv],convolve=cc,hi_cut=hi_cut)
	 tempm=dblarr(numpix,numpix)
	 tempm[*,*]=m2[wv,*,*]
	 band_suffix=strcompress('_'+string(bands[wv],format='(i10)'),/remove_all)
	 m2file=strcompress(outdir+prefix+size_prefix+band_suffix+cut_suffix+'_'+strcompress(ii,/remove_all)+'.dat',/remove_all)
	 f2file=strcompress(outdir+prefix+size_prefix+band_suffix+cut_suffix+'_'+strcompress(ii,/remove_all)+'.fits',/remove_all)
	 if not(keyword_set(fits)) then begin
	    openw,2,m2file
	    writeu,2,double(tempm)
	    close,2
	 endif else begin
	    mkhdr,thd, float(tempm)
	    sxaddpar,thd,'CD2_2',res/3600.
	    sxaddpar,thd,'CD1_1',-1.*res/3600.
	    fits_write, f2file,tempm,thd 
	 endelse
      ENDFOR
   ENDIF
endfor

END
