PRO MAKE_SIMSKIES_W_SOURCES,spec,ell,res,numpix, convolve=convolve, $
   fwhm=fwhm, hi_cut=hi_cut,lo_cut=lo_cut, $
   bias=bias,wv=wv, noise=noise, $
   cube=cube,$
   writepixmap=writepixmap,writecleansky=writecleansky, writesourcesky=writesourcesky, z=z 
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
; bias      this does not work yet
; writepixmap if you want to keep the 1-0 map of where sources were dropped.
; writecleansky if you want to write the density field to a map
; writesourcesky the filename of the final product
; z         if you have a z-range, z=[z1,z2].  Very cool :)
;ADDITIONAL REMARKS
; 
;          The original power spectrum is interpolated in log scale
;          assuming it varies a lot with respect to k 
;
;AUTHOR
; Alex Amblard
; Modified by Marco Viero
;-
if NOT(KEYWORD_SET(writecleansky)) then writecleansky=0
if NOT(KEYWORD_SET(writesourcesky)) then writesourcesky=0
if NOT(KEYWORD_SET(writepixmap)) then writepixmap=0
if NOT(KEYWORD_SET(wv)) then wv=[250,350,500]
if NOT(KEYWORD_SET(hi_cut)) then hi_cut=0.5
if NOT(KEYWORD_SET(bias)) then bias=1.
if NOT(KEYWORD_SET(z)) then z=[0,100]

k=make_k(numpix,res=res)

if KEYWORD_SET(writecleansky) eq 1 then $
    if FILE_TEST(writecleansky) eq 1 then $
      probmap=readfits(writecleansky, hd) $
      else $
      probmap=float(biased_sky(spec, ell, res, numpix, bias=bias,hd=hd,writefileto=writecleansky)) $
      else $
      probmap=float(biased_sky(spec, ell, res, numpix, bias=bias,hd=hd))

size_prefix=strcompress('_'+string(res,format='(i10)')+'x'+ $
   string(numpix,format='(i10)')+'x'+string(numpix,format='(i10)'),/remove_all)
if KEYWORD_SET(HI_CUT) THEN $
   cut_suffix=strcompress('_'+string(hi_cut*1000.,format='(i101)')+'mJy',/remove_all) $
   ELSE BEGIN
   HI_CUT=100 
   cut_suffix=''
ENDELSE

lo_cut=0.00020417379

nwv=n_elements(wv)

;SOME LOW/HI CUTS 
;Lowest 5e-4  --> 50 mJy
;Highest 1e1  --> 1 Jy ?
dir_counts  = '/data/viero/models/gaelen_model/'
counts_file = strcompress(dir_counts + 'viero_cat_w_spitzer_evol-1_20100910.sav' , /remove_all)
restore, counts_file
ind_wv=dblarr(nwv)
for n=0,nwv-1 do ind_wv[n]=fix(where(fix(LAMBDA_TARG) eq fix(wv[n])))
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

imagsize= size(probmap,/dim)
imangx = imagsize[0]
imangy = imagsize[1]
outmap=dblarr(nwv,imangx,imangy)
pixmap=dblarr(imangx,imangy)

;; NORMALIZE NSOURCE
dir_counts  = '/home/viero/lss_14/data/srcmodels/'
counts_file = strcompress(dir_counts + 'gp_may01_1_250.dat' , /remove_all)
readcol, counts_file, FORMAT='D,D,D',log10flux, NgS, dn_ds_new,/silent
NgS0=NgS[closest(10.0^log10flux, min(ALL_FLUX[ind_wv[0],*]))]
file_in='/data/viero/counts/dNdz_250um.dat'
readcol, file_in, format='d,d',zz, dndz
totaldndz=0
fracdndz=0
for zi=0,n_elements(dndz)-2 do totaldndz+=dndz[zi]*(zz[zi+1]-zz[zi])
indzz=where(zz ge z[0] and zz lt z[1], nzz)
for zi=0,nzz-2 do fracdndz+=dndz[indzz[zi]]*(zz[indzz[zi]+1]-zz[indzz[zi]])
nmapsrcs = NgS0 * (!PI / 180.0)^2 * imangx * imangy * (res/3600.)^2. * (fracdndz/totaldndz)
n_total =double( floor(round(nmapsrcs + RANDOMN(seed) * SQRT(nmapsrcs))))
print, strcompress('placing '+string(n_total,format='(i20)')+' sources on map in z range'+$
   string(z[0],format='(d20.1)')+' to '+string(z[1],format='(d20.1)'))

numsrcs_block = 1e7
ndraw_left = n_total
total_placed = 0l
;probmap*=bias^2.
WHILE ndraw_left GT 0 DO BEGIN
   ndraw_iter = ndraw_left < numsrcs_block
   ;print, ndraw_left
   rand_x = fix(randomu(seed, ndraw_iter)* imangx)
   rand_y = fix(randomu(seed, ndraw_iter)* imangy)
   rand_z = (randomu(seed, ndraw_iter))*(max(probmap) - min(probmap)) + min(probmap)
   ind_s = floor(RANDOMU(seed,ndraw_iter)*n_elements(ALL_FLUX[0,*]))

   n_placed = 0l
   k = 0l
   for k=0l, ndraw_iter-1 do $
      if probmap[rand_x[k], rand_y[k]] gt rand_z[k] then begin
	 FOR w=0,nwv-1 do $ 
	    outmap[w,rand_x[k], rand_y[k]]+=ALL_FLUX[ind_wv[w],ind_s[k]]
	 pixmap[rand_x[k], rand_y[k]]+=1.
	 n_placed = n_placed + 1.
	 total_placed=total_placed+1.
	 if total_placed ge n_total then break ; IS THIS A MISTAKE?
      endif
      if total_placed ge n_total then break ; IS THIS A MISTAKE?
      ndraw_left = ndraw_left - n_placed ; ndraw_iter
endwhile
print, strcompress(string(n_total,format='(i20)')+' sources placed')
if KEYWORD_SET(writepixmap) THEN $
   fits_write, writepixmap,float(pixmap), hd
convolve=1
cube=dblarr(nwv,imangx,imangy)
FOR w=0,nwv-1 do begin

   IF KEYWORD_SET(convolve) THEN BEGIN
      if wv[w] eq 250 then band = 'PSW'
      if wv[w] eq 350 then band = 'PMW'
      if wv[w] eq 500 then band = 'PLW'
      if wv[w] ne 250 and wv[w] ne 350 and wv[w] ne 500 then begin
	 psf=kernal(fwhm[w],imangx<imangy,pixsize=res)
      endif else begin
	 npix = 5 * GET_SPIRE_BEAM_FWHM(band) / res
	 psf=GET_SPIRE_BEAM(band,res,npix,npix,/SILENT,/NORM)
      endelse
      psf=psf/TOTAL(psf)/(res/3600.*!PI/180.0)^2.
      ;     sim_im = CONVOL(pixmap,psf,/EDGE_WRAP)
      tempmap=dblarr(imangx,imangy)
      tempmap[*,*]=outmap[w,*,*]
      SMOOTHMAP,tempmap-mean(tempmap),psf,SMMAP=sim_im
      cube[w,*,*]=sim_im
      if KEYWORD_SET(writesourcesky) THEN $
	 fits_write, writesourcesky[w],float(sim_im), hd
   ENDIF ELSE cube[w,*,*]=tempmap
   tempmap=0d

   ;; A WHITE NOISE GENERATOR
   IF N_ELEMENTS(noise) NE 0 THEN BEGIN
      noise_im=randomn(SEED,imangx,imangy)*noise
      sim_im=sim_im+noise_im
   ENDIF
ENDFOR

END
