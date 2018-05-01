PRO MAKE_SIMSKIES_W_SOURCES_FROM_SIMCAT2, res,numpix, $
   numpixy=numpixy,$
   spec=spec,ell=ell, $
;   convolve=convolve, $
   fwhm=fwhm, $
   beam_profile=beam_profile, $
   beam_cl=beam_cl, $
   beam_maps=beam_maps,$
   hi_cut=hi_cut,lo_cut=lo_cut, $
   bias=bias,wv=wv, $
   area=area, allcat=allcat, cube=cube,$
   writepixmap=writepixmap, $
   writecleansky=writecleansky, $
   writesourcesky=writesourcesky, $
   writecat=writecat, $
   catfluxcut=catfluxcut, $
   jy_beam=jy_beam,$
   crval=crval,$
   crpix=crpix,$
   z=z 
;+
;NAME
; MAKE_SIMSKY_W_SOURCES
;PURPOSE
; Create a simple random realization of the sky using a power spectrum P(k)
; 
;USAGE
; MAKE_SIMSKIES_W_SOURCES,specs,ells,res,numpix,+EXTRAS
;INPUTS
; spec         power spectrum of the simulated maps
; ell          ell vector corresponding to the spec
; res          resolution in arcseconds 
; numpix       number of pixel of the side of the output maps (the map will be numpix by numpix large)
;
;OPTIONAL INPUTS
; convolve  will convolve with the SPIRE beam, (or with the fwhm supplied if wv=wv not set)
; wv        are 250, 350, 500.  Default is three correlated maps, i.e., wv=[250, 350,500] 
; fwhm      use this if you want to convolve in some other, non-SPIRE, wavelength  
; hi_cut    The max flux, defaults to 500mJy (i.e., hi_cut=0.5)
; lo_cut    The min flux of sources in a map, defaults to 0, but will take a long ass time and will likely 
;           make shit maps.  Suggest lo_cut=.00001
; bias      this is a knob that cranks up the bias, but it's not calibrated, so you'll have to find your fave.  
; writepixmap if you want to keep the 1-0 map of where sources were dropped.
; writecleansky if you want to write the density field to a map 
;               OR use one named writecleansky (i.e., don't make a new one)
; writesourcesky the filename of the final product
; writecat  writes a catalog with all sources above value catfluxcut Jy (defaults to 0.00005 Jy)
; catfluxcut is the lower limit of flux to use
; z         if you have a z-range, z=[z1,z2].  Very cool :)
;ADDITIONAL REMARKS
; 
;AUTHOR
; Marco Viero
;-
if NOT(KEYWORD_SET(writecleansky)) then writecleansky=0
if NOT(KEYWORD_SET(writesourcesky)) then writesourcesky=0
if NOT(KEYWORD_SET(writepixmap)) then writepixmap=0
if NOT(KEYWORD_SET(writecat)) then writecat=0
if NOT(KEYWORD_SET(catfluxcut)) then catfluxcut=0.00005
if NOT(KEYWORD_SET(hi_cut)) then hi_cut=0.75
;if NOT(KEYWORD_SET(lo_cut)) then lo_cut=0.000001
if NOT(KEYWORD_SET(bias)) then bias=0.
if NOT(KEYWORD_SET(z)) then z=[0.001,20.001]
if NOT(KEYWORD_SET(beam_maps)) then beam_maps=0.
if NOT(KEYWORD_SET(wv)) then wv=[250,350,500]
if KEYWORD_SET(beam_maps) or KEYWORD_SET(FWHM) or $
   KEYWORD_SET(beam_cl) or KEYWORD_SET(beam_profile) then convolve=1
nwv=n_elements(wv)
if keyword_set(numpixy) then begin 
   sidey=numpixy
endif else begin
   sidey=0
   numpixy=numpix
endelse
k=make_k(numpix,sidey=sidey,res=res)

IF (KEYWORD_SET(bias)) THEN BEGIN
   if keyword_set(crval) or keyword_set(crpix) then begin
      tmp=dblarr(numpix,numpixy)+1d
      mkhdr, hd, tmp 
      cdout=dblarr(2,2)
      cdout[0,0]=-1.*res/3600.
      cdout[1,1]=+1.*res/3600.
      if not(keyword_set(crval)) then crval=[0,0]
      if not(keyword_set(crpix)) then crpix=[dims[0]/2+1,dims[1]/2+1]
      make_astr, astr, CD=cdout, DELT=[1,1], CRPIX=crpix, CRVAL=CRVAL;[0,0]
      putast, hd, astr
   endif
   ind_p=where(spec gt 0)
   if KEYWORD_SET(writecleansky) eq 1 then $
      if FILE_TEST(writecleansky) eq 1 then $
      probmap=readfits(writecleansky, hd) $
      else $
      probmap=float(biased_sky(spec[ind_p], ell[ind_p], res, numpix, sidey=sidey, hd=hd,writefileto=writecleansky)) $
      else $
      probmap=float(biased_sky(spec[ind_p], ell[ind_p], res, numpix, sidey=sidey, hd=hd))
ENDIF ELSE BEGIN
   probmap=dblarr(numpix,numpixy)+1d
   dims=(SIZE(probmap))[1:2]
   mkhdr, hd, probmap
   cdout=dblarr(2,2)
   cdout[0,0]=-1.*res/3600.
   cdout[1,1]=+1.*res/3600.
   if not(keyword_set(crval)) then crval=[0,0]
   if not(keyword_set(crpix)) then crpix=[dims[0]/2+1,dims[1]/2+1]
   make_astr, astr, CD=cdout, DELT=[1,1], CRPIX=crpix, CRVAL=CRVAL;[0,0]
   putast, hd, astr
ENDELSE

IF KEYWORD_SET(writesourcesky) THEN BEGIN 

size_prefix=strcompress('_'+string(res,format='(i10)')+'x'+ $
   string(numpix,format='(i10)')+'x'+string(numpixy,format='(i10)'),/remove_all)
if KEYWORD_SET(HI_CUT) THEN $
   cut_suffix=strcompress('_'+string(hi_cut*1000.,format='(i101)')+'mJy',/remove_all) $
   ELSE BEGIN
   HI_CUT=100 
   cut_suffix=''
ENDELSE

imagsize= size(probmap,/dim)
imangx = imagsize[0]
imangy = imagsize[1]
outmap=dblarr(nwv,imangx,imangy)
pixmap=dblarr(imangx,imangy)

;; NORMALIZE NSOURCE
nmapsrcs=n_elements(allcat.z)/area*imangx * imangy * (res/3600.)^2.
print, string(nmapsrcs,format='(e10.2)')
n_total =double( floor(round(nmapsrcs + RANDOMN(seed) * SQRT(nmapsrcs))))
print, strcompress('placing '+string(n_total,format='(i20)')+' sources on map in z range'+$
   string(z[0],format='(d20.2)')+' to '+string(z[1],format='(d20.2)'))

IF KEYWORD_SET(writecat) THEN x_y_flux=dblarr(n_total,2+nwv)
   
dir_templates='/home/viero/smaproot/smap_pipeline/simulations/data/'
cold_file=dir_templates+'ias_cold.fits'
starburst_file=dir_templates+'ias_starburst.fits'

if not(keyword_set(allcat)) then begin
   cold = mrdfits(cold_file,1,head,/silent)
   starburst = mrdfits(starburst_file,1,head,/silent)
   print, 'computing bethermin catalog (slow!)'
   z0=z[0]
   z1=z[1]
   l0=9.0
   l1=13.5;12.5
   nn=2000;80;200
   zvals=loggen(z0,z1,nn)
   lumvals=loggen(l0,l1,nn)
   dndlogldzdomega=bethermin_dndlogldzdomega(zvals, lumvals)
   allcat=bethermin_gencat(n_total, dndlogldzdomega, cold, starburst,$
      WAVE=wv);, $
      ;SIGMAPOP=sigmapop, LPOP=lpop, $
      ;SEED=seed
   allcat.obsflux/=1000.
   save, filename='/home/viero/lss_14/data/bethermin_dndlogldzdomega_and_allcat.save', $
      zvals, lumvals, dndlogldzdomega, allcat
endif else print, 'loading bethermin catalog'
nall=n_elements(allcat.obsflux[0])
ALL_FLUX=dblarr(nwv,nall)
for j=0,nwv-1 do ALL_FLUX[j,*]=allcat.obsflux[j]
redshift=allcat.z
ind_wv=dblarr(nwv)
for n=0,nwv-1 do ind_wv[n]=fix(where(fix(wv) eq fix(wv[n])))

for iwv=0,nwv-1 do begin
   testh = HISTOGRAM(1000.*ALL_FLUX[iwv,*],BIN=0.5,LOCATIONS=testflux)
   testflux += 0.25
   testh *= 2000.0/((area)*(!pi/180.)^2.)
   testflux *= 1d-3
   print,iwv,"Shot noise: ",TSUM(testflux,testflux^2.*testh)
endfor

probmap-=min(probmap)
probmap/=max(probmap)
if keyword_set(bias) then begin
   probmap=probmap^bias
   probmap/=max(probmap)
endif
numsrcs_block = 5e7
ndraw_left = n_total
total_placed = 0l
WHILE ndraw_left GT 10 DO BEGIN
   ndraw_iter = ndraw_left < numsrcs_block
   ;print, ndraw_left
   nia=0
   while nia eq 0 do begin
      rand_x = (randomu(seed, ndraw_iter)* numpix)
      rand_y = (randomu(seed, ndraw_iter)* numpixy)
      if keyword_set(bias) then begin
	 rand_z = (randomu(seed, ndraw_iter))*(max(probmap) - min(probmap))
	 ind_above=where(probmap[rand_x,rand_y] gt rand_z,nia)
	 rand_x=rand_x[ind_above]
	 rand_y=rand_y[ind_above]
	 rand_z=rand_z[ind_above]
	 ndraw_above= nia
      endif else begin
	 nia=ndraw_iter
	 ndraw_above=ndraw_iter 
      endelse
      ;print, ndraw_iter, nia
   endwhile
   flag_s=dblarr(ndraw_above)+1.0
   ind_s = floor(RANDOMU(seed,ndraw_above)*n_elements(ALL_FLUX[0,*]))
   if keyword_set(z) then begin
      ind_zcut=where(REDSHIFT[ind_s] le z[0] and REDSHIFT[ind_s] gt z[1], nzcut)
      if nzcut ne 0 then flag_s[ind_zcut]=0.0
   ENDIF
   if keyword_set(hi_cut) then begin
      for n=0,nwv-1 do begin
	 ;KEEP SOURCES LESS THAN HI_CUT 
	 ind_cut=where(ALL_FLUX[ind_wv[n],ind_s] ge hi_cut, nhi_cut)
	 if nhi_cut ne 0 then flag_s[ind_cut]=0.0
      endfor
   endif
   if keyword_set(lo_cut) then begin
      for n=0,nwv-1 do begin
	 ;KEEP SOURCES LESS THAN HI_CUT 
	 ind_cut=where(ALL_FLUX[ind_wv[n],ind_s] lt lo_cut, nlo_cut)
	 if nlo_cut ne 0 then flag_s[ind_cut]=0.0
      endfor
   endif
   ;print, total(flag_s)

   temppix=dblarr(numpix,numpixy)
   temppix[rand_x[0:ndraw_above-1],rand_y[0:ndraw_above-1]]+=1.
   pixmap[*,*]=pixmap[*,*]+temppix
   FOR w=0,nwv-1 do begin 
      tempmap=dblarr(numpix,numpixy)
      tempmap[rand_x[0:ndraw_above-1],rand_y[0:ndraw_above-1]]+=ALL_FLUX[w,ind_s[0:ndraw_above-1]]
      outmap[w,*,*]=outmap[w,*,*]+tempmap
      IF KEYWORD_SET(writecat) THEN $
	 for k=0l, ndraw_above-1 do begin 
	     x_y_flux[total_placed+k,*]=$
	       [float(rand_x[k]), float(rand_y[k]), float(ALL_FLUX[*,ind_s[k]])]
	 endfor
   ENDFOR
   total_placed+=nia
   ndraw_left = ndraw_left - ndraw_above  
   ;print, n_total, ndraw_left
   ;stop
endwhile

print, strcompress(string(total_placed,format='(i20)')+' sources placed')
if KEYWORD_SET(writepixmap) THEN $
   fits_write, writepixmap,float(pixmap), hd
cube=dblarr(nwv,imangx,imangy)
FOR w=0,nwv-1 do begin
   tempmap=dblarr(imangx,imangy)
   tempmap[*,*]=outmap[w,*,*]
   ;tempmap-=mean(tempmap)
   IF KEYWORD_SET(convolve) THEN BEGIN
      ;4 options
      ;Gaussian with fwhm (fwhm)
      ;2D beam files (beam_maps)
      ;1D beam files (beamfile)
      ;default SMAP beams (just wv)
      IF KEYWORD_SET(fwhm) THEN BEGIN
	 ;psf=kernal(fwhm[w],imangx<imangy,pixsize=res)
	 npix = 35. * fwhm[w] / res
	 psf=GET_SPIRE_BEAM(0,res,npix,npix,fwhm=fwhm[w],/SILENT,/NORM)
	 print, 'convolving by '+string(fwhm[w])+'FWHM  beam'
      ENDIF ELSE $
      IF KEYWORD_SET(beam_maps) THEN BEGIN
	 if not(file_test(beam_maps[w])) then begin
	    print, "can't find "+beam_maps[w]
	 endif
	 psf=readfits(beam_maps[w],bhd)
	 msp=size(psf,/dim)
	 psf=frebin(psf,msp[0]*(abs(sxpar(bhd,'CD2_2'))*3600.)/res,$
	    msp[1]*(abs(sxpar(bhd,'CD2_2'))*3600.)/res)
      ENDIF ELSE $
      IF KEYWORD_SET(beam_profile) THEN BEGIN
	 if not(file_test(beam_profile[w])) then begin
	    print, "can't find "+beam_profile[w]
	 endif
	 readcol, beam_profile[w], ell_in, psf_in
	 sde=imangx<imangy
	 psf=shift(ONE_D_TO_TWO_D_PS(psf_in, ell_in, res, sde,sde),sde/2,sde/2) 
	 print, 'convolving by '+beamfile[w]+' beam'
      ENDIF ELSE $
      IF KEYWORD_SET(beam_cl) THEN BEGIN
	 if not(file_test(beam_cl[w])) then begin
	    print, "can't find "+beam_cl[w]
	 endif
	 readcol, beam_cl[w], ell_in, cl_psf_in
	 ps_psf=ONE_D_TO_TWO_D_PS(cl_psf_in, ell_in, res, imangx,imangy)
	 ;psf=SHIFT(+1d*REAL_PART(fft(ps_psf,/INV,DOUBLE=double)),imangx/2,imangy/2)
	 ;psf=-1d*REAL_PART(fft(ps_psf,/INV,DOUBLE=double))
	 psf=SHIFT(+1d*REAL_PART(fft(ps_psf,/INV,DOUBLE=double)),imangx/2,imangy/2)
	 psf-=min(psf)
	 print, 'convolving by '+beam_cl[w]+' beam'
      ENDIF ELSE BEGIN
	 if wv[w] eq 250 then band = 'PSW'
	 if wv[w] eq 350 then band = 'PMW'
	 if wv[w] eq 500 then band = 'PLW'
	 npix = 35. * GET_SPIRE_BEAM_FWHM(band) / res
	 psf=GET_SPIRE_BEAM(band,res,npix,npix,/SILENT,/NORM)
	 print, 'convolving by '+band+' SMAP beam'
      ENDELSE
      ;ZEROPAD AND CONVOLVE WITH BEAM

      ;CROP TO ORIGINAL SIZE

;      if keyword_set(ps_psf) then begin
;	 if keyword_set(jy_beam) then begin
;	    ;ps_psf=ps_psf/max(ps_psf)
;	    psf=psf/max(psf)
;	 endif else begin 
;	    ;ps_psf=ps_psf/TOTAL(ps_psf)/(res/3600.*!PI/180.0)^2.
;	    psf=psf/TOTAL(psf)/(res/3600.*!PI/180.0)^2.
;	 endelse
;	 ;sim_im  = REAL_PART( FFT(FFT(tempmap, DOUBLE=double) * $
;	 ;   FFT(psf, DOUBLE=double), $
;	 ;   ;ps_psf, $
;	 ;   /INV, DOUBLE=double) ) * imangx * imangy
;	 SMOOTHMAP,tempmap,psf,SMMAP=sim_im
;      endif else begin
	 if keyword_set(jy_beam) then begin
	    psf=psf/max(psf)
	 endif else begin 
	    psf=psf/TOTAL(psf)/(res/3600.*!PI/180.0)^2.
	 endelse
	 SMOOTHMAP,tempmap,psf,SMMAP=sim_im
;      endelse
      cube[w,*,*]=sim_im
      if KEYWORD_SET(writesourcesky) THEN $
	 fits_write, writesourcesky[w],float(sim_im), hd
   ENDIF ELSE BEGIN
      cube[w,*,*]=tempmap
      if KEYWORD_SET(writesourcesky) THEN $
	 fits_write, writesourcesky[w],float(tempmap), hd
   ENDELSE
;   stop
   tempmap=0d
ENDFOR

IF KEYWORD_SET(writecat) THEN BEGIN 
   IF KEYWORD_SET(catfluxcut) THEN BEGIN
      ;ind_above=where(x_y_flux[0,2,*] gt catfluxcut,niafc)
      ind_above=where(x_y_flux[*,2] gt catfluxcut,niafc)
      print, string(niafc,format='(i20)')+' sources above '+$
	 string(catfluxcut*1000.,format='(d10.1)')+' mJy'
      ;x_y_flux=x_y_flux[*,*,ind_above]
      x_y_flux=x_y_flux[ind_above,*]
   ENDIF
   save, filename=writecat, x_y_flux
ENDIF

ENDIF


END
