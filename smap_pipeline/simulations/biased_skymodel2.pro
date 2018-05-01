;+
; NAME:
;      biased_skymodel
; PURPOSE:
;      Draw source counts from Marsden 2011 Model Counts and put them in a biased way on background map 
; EXPLANATION:
;     Observed p(D) = histogram of observed map (careful to compute bins in the same way always!)  
;     Model p(D) = broken power law -> counts -> randomly distribute -> convolve with psf -> add noise (white only?) -> compute histogram --> chisq --> new params
;
; CALLING SEQUENCE:
;     sim_im = simple_skymodel(nsourceb, nsourcef, alpha, beta, fmin, fmax,IN_IMAGE=in_image,NPIX=npix,CONVOLVE=convolve)
;
; INPUTS:
;	IN_IMAGE - background map.  if it has large scale structure then sources will trace that.
;	PIXSIZE - Pixel size in arcsec 
;	wv - band, either 250, 350 or 500 
;
; OPTIONAL KEYWORD INPUTS:
;	HI_CUT - Maximum source flux [Jy]
;	LO_CUT - Minimum source flux [Jy]
; 	CONVOLVE - FWHM of gaussian convolution kernel
; 	NOISE - FWHM white noise [Jy]
;
; RETURNS:
;	sim_im - an image with dimensions matching IN_IMAGE, or alternatively, [npix,npix] with the 
;		given source distribution, poisson distributed on the sky 
;
; EXAMPLE:
;        testsim=biased_skymodel(biasmap,pixel_size,spire_band,convolve=1,hi_cut=0.5)
;
; MODIFICATION HISTORY
;  Author: 
;          Marco Viero 2/2011 - made a version with sources as biased tracers of given background, this  
;				 code looks nothing like simple_skymodel! 
;-

FUNCTION BIASED_SKYMODEL2,$
   in_image,$
   pixsize,$
   wv,$
   CONVOLVE=convolve,$
   FWHM=FWHM, $
   NOISE=noise,$  
   HI_CUT=hi_cut, LO_CUT=lo_cut,$
   VERBOSE=verbose
  ; DID YOU PROVIDE AN INPUT MAP?
  IF N_ELEMENTS(in_image) EQ 0 THEN BEGIN
     errmsg='NO INPUT IMAGE PROVIDED, ABORTING.'
     GOTO, err_handler
  ENDIF 

  lo_cut=0.00020417379

  ;seed= float(STRMID(SYSTIME(0), 17, 2) )
  nwv=n_elements(wv)

  ;SOME LOW/HI CUTS 
  ;Lowest 5e-4  --> 50 mJy
  ;Highest 1e1  --> 1 Jy ?
  dir_counts  = '/data/viero/models/gaelen_model/'
  counts_file = strcompress(dir_counts + 'viero_cat_w_spitzer_evol-1_20100910.sav' , /remove_all)
  restore, counts_file
  ind_wv=dblarr(nwv)
  for n=0,nwv-1 do ind_wv[n]=fix(where(fix(LAMBDA_TARG) eq fix(wv[n])))
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
;     for n=0,nwv-1 do begin
	;KEEP SOURCES MORE THAN LO_CUT 
	ind_cut=where(ALL_FLUX[ind_wv[0],*] gt lo_cut, nlo_cut)
	if nlo_cut lt n_elements(ALL_FLUX[0,*]) then begin 
	   ALL_FLUX=ALL_FLUX[*,ind_cut]
	   print, strcompress('CUT SOURCES WITH FLUX BELOW '+ string(lo_cut*1000., format='(e100.2)')+'mJy')
	endif 
;     endfor
  endif

  ;pixmap=float(in_image)*0d
  probmap=float(in_image)
  imagsize= size(in_image,/dim)
  imangx = imagsize[0]
  imangy = imagsize[1] 
  outmaps=dblarr(nwv,imangx,imangy)
  pixmap=dblarr(nwv,imangx,imangy)

  ;; NORMALIZE NSOURCE
  dir_counts  = '/home/viero/lss_14/data/srcmodels/'
  counts_file = strcompress(dir_counts + 'gp_may01_1_250.dat' , /remove_all)
  readcol, counts_file, FORMAT='D,D,D',log10flux, NgS, dn_ds_new,/silent
  NgS0=NgS[closest(10.0^log10flux, min(ALL_FLUX[ind_wv[0],*]))]
  ;NgS0=1.9031499e+08; SAME ANSWER AS ABOVE
  nmapsrcs = NgS0 * (!PI / 180.0)^2 * imangx * imangy * (pixsize/3600.)^2.
  n_total =double( floor(round(nmapsrcs + RANDOMN(seed) * SQRT(nmapsrcs))))
  ;print, 'dropping ', n_total, ' sources on  maps'
  print, strcompress('placing '+string(n_total,format='(i20)')+' sources on map')
  ;stop

  ;; ok, finished set up, now we need to populate the map
  
  ; loop over ndraw sources, make numsrcs_block at a time
  numsrcs_block = 1e7
  ndraw_left = n_total
  total_placed = 0l
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
	  pixmap[w,rand_x[k], rand_y[k]]+=ALL_FLUX[ind_wv[w],ind_s[k]]
	  n_placed = n_placed + 1.
	  total_placed=total_placed+1.
	  if total_placed ge n_total then break ; IS THIS A MISTAKE?
        endif
	if total_placed ge n_total then break ; IS THIS A MISTAKE?
	ndraw_left = ndraw_left - n_placed ; ndraw_iter
  endwhile
  print, strcompress(string(n_total,format='(i20)')+' sources placed')

  FOR w=0,nwv-1 do begin
     IF KEYWORD_SET(convolve) THEN BEGIN
	if wv[w] eq 250 then band = 'PSW'
	if wv[w] eq 350 then band = 'PMW'
	if wv[w] eq 500 then band = 'PLW'
	if wv[w] ne 250 and wv[w] ne 350 and wv[w] ne 500 then begin
	   psf=kernal(fwhm[w],imangx<imangy,pixsize=pixsize)
	endif else begin
	   npix = 5 * GET_SPIRE_BEAM_FWHM(band) / pixsize
	   psf=GET_SPIRE_BEAM(band,pixsize,npix,npix,/SILENT,/NORM)
	endelse
	psf=psf/TOTAL(psf)/(pixsize/3600.*!PI/180.0)^2.
	;     sim_im = CONVOL(pixmap,psf,/EDGE_WRAP)
	tempmap=dblarr(imangx,imangy)
	tempmap[*,*]=pixmap[w,*,*]
	SMOOTHMAP,tempmap,psf,SMMAP=sim_im
     ENDIF ELSE sim_im=tempmap
     tempmap=0d

     ;; A WHITE NOISE GENERATOR
     IF N_ELEMENTS(noise) NE 0 THEN BEGIN
	noise_im=randomn(SEED,imangx,imangy)*noise 
	sim_im=sim_im+noise_im
     ENDIF
     outmaps[w,*,*]=sim_im
  ENDFOR

  RETURN,outmaps

  err_handler:
  IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF

  END
