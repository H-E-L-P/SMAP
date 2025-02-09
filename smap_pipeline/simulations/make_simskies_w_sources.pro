PRO MAKE_SIMSKIES_W_SOURCES,spec,ell,res,numpix, convolve=convolve, $
   fwhm=fwhm, hi_cut=hi_cut,lo_cut=lo_cut, $
   bias=bias,wv=wv, noise=noise, $
   allcat=allcat, cube=cube,$
   writepixmap=writepixmap, $
   writecleansky=writecleansky, $
   writesourcesky=writesourcesky, $
   writecat=writecat, $
   catfluxcut=catfluxcut, $
   jy_beam=jy_beam,$
   beams=beams,$
   neptune=neptune,$
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
if NOT(KEYWORD_SET(lo_cut)) then lo_cut=0.0001
if NOT(KEYWORD_SET(bias)) then bias=0.
if NOT(KEYWORD_SET(z)) then z=[0.001,20.001]
if NOT(KEYWORD_SET(beams)) then beams=0.
if NOT(KEYWORD_SET(wv)) then wv=[250,350,500]
if KEYWORD_SET(neptune) then begin
   if KEYWORD_SET(wv) then begin
      neptfile='/data/viero/models/Neptune_esa2.txt'
      readcol, neptfile, format='d,d,d,d',skipline=4,freq,bt,fl,rjt, /silent
      neptflux=dblarr(n_elements(wv))
      for ini=0,n_elements(wv)-1 do $
	 neptflux[ini]=closest(freq,lambda_to_ghz(wv[ini]))
   endif else $
      neptflux=[157.9,100.6,60.1]
endif 
if KEYWORD_SET(beams) or KEYWORD_SET(FWHM) then convolve=1
nwv=n_elements(wv)
k=make_k(numpix,res=res)

ind_p=where(spec gt 0)
IF (KEYWORD_SET(bias)) THEN BEGIN
   if KEYWORD_SET(writecleansky) eq 1 then $
      if FILE_TEST(writecleansky) eq 1 then $
      probmap=readfits(writecleansky, hd) $
      else $
      probmap=float(biased_sky(spec[ind_p], ell[ind_p], res, numpix, hd=hd,writefileto=writecleansky)) $
      else $
      probmap=float(biased_sky(spec[ind_p], ell[ind_p], res, numpix, hd=hd))
ENDIF ELSE BEGIN
   probmap=dblarr(numpix,numpix)+1d
   dims=(SIZE(probmap))[1:2]
   mkhdr, hd, probmap
   cdout=dblarr(2,2)
   cdout[0,0]=-1.*res/3600.
   cdout[1,1]=+1.*res/3600.
   make_astr, astr, CD=cdout, DELT=[1,1], CRPIX=[dims[0]/2+1,dims[1]/2+1], CRVAL=[0,0]
   putast, hd, astr
ENDELSE

size_prefix=strcompress('_'+string(res,format='(i10)')+'x'+ $
   string(numpix,format='(i10)')+'x'+string(numpix,format='(i10)'),/remove_all)
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
NgS0=cumulative_counts_given_flux(lo_cut)
print, string(NgS0,format='(e10.2)')
nmapsrcs = NgS0 * (!PI / 180.0)^2 * imangx * imangy * (res/3600.)^2. 
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
   ;stop
endif else print, 'loading bethermin catalog'
nall=n_elements(allcat.obsflux[0])
ALL_FLUX=dblarr(nwv,nall)
for j=0,nwv-1 do ALL_FLUX[j,*]=allcat.obsflux[j]
redshift=allcat.z
;SOME LOW/HI CUTS 
;Lowest 5e-4  --> 50 mJy
;Highest 1e1  --> 1 Jy ?
ind_wv=dblarr(nwv)
for n=0,nwv-1 do ind_wv[n]=fix(where(fix(wv) eq fix(wv[n])))
if keyword_set(z) then begin
   ind_zcut=where(REDSHIFT ge z[0] and REDSHIFT lt z[1], nzcut)
   REDSHIFT=REDSHIFT[ind_zcut]
   ALL_FLUX=ALL_FLUX[*,ind_zcut]
ENDIF
if keyword_set(hi_cut) then begin
   for n=0,nwv-1 do begin
      ;KEEP SOURCES LESS THAN HI_CUT 
      ind_cut=where(ALL_FLUX[ind_wv[n],*] le hi_cut, nhi_cut)
      if nhi_cut lt n_elements(ALL_FLUX[0,*]) then begin
	 ALL_FLUX=ALL_FLUX[*,ind_cut]
	 print, strcompress('CUT SOURCES WITH FLUX ABOVE '+ string(hi_cut*1000., format='(i10)')+'mJy')
      endif
   endfor
endif
if keyword_set(lo_cut) then begin
   ;for n=0,nwv-1 do begin
   ;KEEP SOURCES MORE THAN LO_CUT 
   ind_cut=where(ALL_FLUX[ind_wv[0],*] gt lo_cut, nlo_cut)
   if nlo_cut lt n_elements(ALL_FLUX[0,*]) then begin
      ALL_FLUX=ALL_FLUX[*,ind_cut]
      print, strcompress('CUT SOURCES WITH FLUX BELOW '+ string(lo_cut*1000., format='(e100.2)')+'mJy')
   endif
endif

cat_area=1.

n_test=n_elements(ALL_FLUX[0,*])*((numpix*res/3600.)^2.)/cat_area
n_total=double( floor(round(n_test + RANDOMN(seed) * SQRT(n_test))))
print, strcompress('REALLY placing '+string(n_total,format='(i20)')+' sources on map in z range'+$
   string(z[0],format='(d20.2)')+' to '+string(z[1],format='(d20.2)'))

testh = HISTOGRAM(1000.*ALL_FLUX[0,*],BIN=0.5,LOCATIONS=testflux)
testflux += 0.25
testh *= 2000.0/((cat_area)*(!pi/180.)^2.)
testflux *= 1d-3
print,"Shot noise: ",TSUM(testflux,testflux^2.*testh)
;testmap=dblarr(numpix,numpix)
;testmap[floor(randomu(seed, n_total)*numpix),$
;   floor(randomu(seed, n_total)*numpix)]+=$
;   ALL_FLUX[0,floor(randomu(seed, n_total)*n_elements(ALL_FLUX[0,*]))]
;mkhdr, h, testmap
;sxaddpar,h,'CD1_1',-1.*res/3600.
;sxaddpar,h,'CD2_2',res/3600.
;testmap/=((res/3600.*!PI/180.0)^2.) 
;ps=alex_power_spec(testmap,h)
;plot,ps[0,*], ps[1,*], /xl,/yl,/xs,xr=[.003,2],yr=[1e2,1e5]

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
      rand_y = (randomu(seed, ndraw_iter)* numpix)
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
   ind_s = floor(RANDOMU(seed,ndraw_above)*n_elements(ALL_FLUX[0,*]))

   temppix=dblarr(numpix,numpix)
   temppix[rand_x[0:ndraw_above-1],rand_y[0:ndraw_above-1]]+=1.
   pixmap[*,*]=pixmap[*,*]+temppix
   FOR w=0,nwv-1 do begin 
      tempmap=dblarr(numpix,numpix)
      tempmap[rand_x[0:ndraw_above-1],rand_y[0:ndraw_above-1]]+=ALL_FLUX[w,ind_s[0:ndraw_above-1]]
      outmap[w,*,*]=outmap[w,*,*]+tempmap
       ;     ps=alex_power_spec(tempmap/((res/3600.*!PI/180.0)^2.),h)
       ;     oplot,ps[0,*], ps[1,*],linestyle=2
       ;     print, mean(ps[1,10:100])
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
if KEYWORD_SET(neptune) then begin
   FOR w=0,nwv-1 do begin
      outmap[w,numpix/2,numpix/2]=outmap[w,numpix/2,numpix/2]+neptflux[w]
   ENDFOR	    
endif

;plot,[0], [0], /xl,/yl,/xs,xr=[.003,2],yr=[1e2,1e5]
;for w=0,nwv-1 do begin
;   testmap=total(outmap[w,*,*],1)/((res/3600.*!PI/180.0)^2.)
;   mkhdr, h, testmap
;   sxaddpar,h,'CD1_1',-1.*res/3600.
;   sxaddpar,h,'CD2_2',res/3600.
;   ps=alex_power_spec(testmap,h)
;   oplot,ps[0,*], ps[1,*]
;   halo_file=strcompress('/home/viero/lss_14/data/mattia_models/pk'+$
;      string(wv[w],format='(i10)')+$
;      'mu_nfw_2Rvir_v230709.out',/remove_all)
;   readcol,halo_file, k_theta_m, cl_m_junk, $
;      cl_m_1h,cl_m_2h,cl_m_tot, /silent
;   oplot, k_theta_m,cl_m_2h+cl_m_1h,linestyle=1
;   ;stop
;endfor

print, strcompress(string(total_placed,format='(i20)')+' sources placed')
if KEYWORD_SET(writepixmap) THEN $
   fits_write, writepixmap,float(pixmap), hd
cube=dblarr(nwv,imangx,imangy)
FOR w=0,nwv-1 do begin
   tempmap=dblarr(imangx,imangy)
   tempmap[*,*]=outmap[w,*,*]
   ;tempmap-=mean(tempmap)

   IF KEYWORD_SET(convolve) THEN BEGIN
      IF NOT(KEYWORD_SET(beams)) THEN BEGIN
	 if wv[w] eq 250 then band = 'PSW'
	 if wv[w] eq 350 then band = 'PMW'
	 if wv[w] eq 500 then band = 'PLW'
	 if convolve eq 2 then begin
	    ;MODEL PSF
	    dirmodelpsf='/data/viero/maps/beams/SPIRE/'
	    if band eq 'PSW' then psffilename='psw-model-beam.fits'
	    if band eq 'PMW' then psffilename='pmw-model-beam.fits'
	    if band eq 'PLW' then psffilename='plw-model-beam.fits'
	    psf=clean_nans(readfits(dirmodelpsf+psffilename,hdpsf))
	 endif else begin
	 if wv[w] ne 250 and wv[w] ne 350 and wv[w] ne 500 then begin
	    if fwhm[w] eq 0 then stop
	    psf=kernal(fwhm[w],imangx<imangy,pixsize=res)
	 endif else begin
	    npix = 35. * GET_SPIRE_BEAM_FWHM(band) / res
	    psf=GET_SPIRE_BEAM(band,res,npix,npix,/SILENT,/NORM)
	    ;npix=600.
	    ;fpsf=GET_SPIRE_BEAM(band,2.,npix,npix,/SILENT,/NORM)
	    ;psf=frebin(fpsf, npix*2./res,npix*2./res)
	 endelse
	 endelse
      ENDIF ELSE BEGIN
	 IF FILE_TEST(beams[w]) eq 1 THEN BEGIN
	    psf=readfits(beams[w],bhd)
	    msp=size(psf,/dim)
	    psf=frebin(psf,msp[0]*(abs(sxpar(bhd,'CD2_2'))*3600.)/res,$
	       msp[1]*(abs(sxpar(bhd,'CD2_2'))*3600.)/res)
	 ENDIF ELSE BEGIN
	    if fwhm[w] eq 0 then stop
	    psf=kernal(fwhm[w],imangx<imangy,pixsize=res)
	 ENDELSE
      ENDELSE
      ;if keyword_set(jy_beam) then ccc=1. else ccc=(res/3600.*!PI/180.0)^2.
      ;psf=psf/TOTAL(psf)/ccc;(res/3600.*!PI/180.0)^2.
      if keyword_set(jy_beam) then begin
	 psf=psf/max(psf)
      endif else begin 
	 psf=psf/TOTAL(psf)/(res/3600.*!PI/180.0)^2.
      endelse
      ;sim_im = CONVOL(pixmap,psf,/EDGE_WRAP)
      SMOOTHMAP,tempmap,psf,SMMAP=sim_im
      cube[w,*,*]=sim_im
      if KEYWORD_SET(writesourcesky) THEN $
	 fits_write, writesourcesky[w],float(sim_im), hd
   ENDIF ELSE BEGIN
      cube[w,*,*]=tempmap
      if KEYWORD_SET(writesourcesky) THEN $
	 fits_write, writesourcesky[w],float(tempmap), hd
   ENDELSE
   tempmap=0d

   ;; A WHITE NOISE GENERATOR
   IF N_ELEMENTS(noise) NE 0 THEN BEGIN
      noise_im=randomn(SEED,imangx,imangy)*noise
      sim_im=sim_im+noise_im
   ENDIF
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


END
