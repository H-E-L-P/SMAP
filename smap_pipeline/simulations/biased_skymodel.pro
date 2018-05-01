;+
; NAME:
;      biased_skymodel
; PURPOSE:
;      Draw source counts from Patanchon Counts and put them in a biased way on background map 
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

FUNCTION BIASED_SKYMODEL,$
   in_image,$
   pixsize,$
   wv,$
   CONVOLVE=convolve,$
   NOISE=noise,$  
   HI_CUT=hi_cut, LO_CUT=lo_cut,$
   VERBOSE=verbose

  ; DID YOU PROVIDE AN INPUT MAP?
  IF N_ELEMENTS(in_image) EQ 0 THEN BEGIN
     errmsg='NO INPUT IMAGE PROVIDED, ABORTING.'
     GOTO, err_handler
  ENDIF 

  ;SOME LOW/HI CUTS 
  ;Lowest 5e-4  --> 50 mJy
  ;Highest 1e1  --> 1 Jy ?
  dir_counts  = '/home/viero/lss_14/data/srcmodels/'
  counts_file = strcompress(dir_counts + 'gp_may01_1_'+ string(wv) + ".dat" , /remove_all)
  readcol, counts_file, FORMAT='D,D,D',log10flux, NgS, dn_ds_new,/silent
  if keyword_set(hi_cut) then begin
     ;KEEP SOURCES LESS THAN HI_CUT 
     ind_cut=where(10.0^log10flux le hi_cut, nhi_cut)
     if nhi_cut gt 0 then begin 
	log10flux=log10flux[ind_cut]
	NgS=NgS[ind_cut]
	hi_cut_suffix=strcompress('_cut_above_'+string(hi_cut*1000., format='(i10)')+'mJy',/remove_all)
	print, strcompress('CUT SOURCES WITH FLUX ABOVE '+ string(hi_cut*1000., format='(i10)')+'mJy')
     endif else hi_cut_suffix=''
  endif
  if keyword_set(lo_cut) then begin
     ;KEEP SOURCES MORE THAN LO_CUT 
     ind_cut=where(10.0^log10flux gt lo_cut, nlo_cut)
     if nlo_cut gt 0 then begin 
	log10flux=log10flux[ind_cut]
	NgS=NgS[ind_cut]
	lo_cut_suffix=strcompress('_cut_below_'+string(lo_cut*1000., format='(i10)')+'mJy',/remove_all)
	print, strcompress('CUT SOURCES WITH FLUX BELOW '+ string(lo_cut*1000., format='(i10)')+'mJy')
     endif else lo_cut_suffix=''
  endif

  pixmap=float(in_image)*0d
  probmap=float(in_image)
  imagsize= size(in_image,/dim)
  imangx = imagsize[0]
  imangy = imagsize[1] 

  ;; NORMALIZE NSOURCE
  nmapsrcs = NgS[0] * (!PI / 180.0)^2 * imangx * imangy * (pixsize/3600.)^2.
  n_total =double( floor(round(nmapsrcs + RANDOMN(seed) * SQRT(nmapsrcs))))
  print, 'dropping ', n_total, ' sources on  maps'

  ;; ok, finished set up, now we need to populate the map
  
  ; loop over ndraw sources, make numsrcs_block at a time
  numsrcs_block = 1e7
  ndraw_left = n_total
  total_placed = 0l
  idraw = 0
  WHILE ndraw_left GT 0 DO BEGIN
     ndraw_iter = ndraw_left < numsrcs_block
     ;print, ndraw_left
     rand_x = fix(randomu(seed, ndraw_iter)* imangx)
     rand_y = fix(randomu(seed, ndraw_iter)* imangy)
     rand_z = (randomu(seed, ndraw_iter))*(max(probmap) - min(probmap)) + min(probmap)
     s = INTERPOL(10.0^log10flux,NgS/NgS[0], $
	        RANDOMU(seed,ndraw_iter))

     n_placed = 0l
     k = 0l
     for k=0l, ndraw_iter-1 do $
	if probmap[rand_x[k], rand_y[k]] gt rand_z[k]  then begin
	  pixmap[rand_x[k], rand_y[k]]+=s[k]
	  n_placed = n_placed + 1.
	  total_placed=total_placed+1.
	  if total_placed ge n_total then break ; IS THIS A MISTAKE?
        endif
	if total_placed ge n_total then break ; IS THIS A MISTAKE?
	ndraw_left = ndraw_left - n_placed ; ndraw_iter
	idraw = idraw + 1
  endwhile

  IF KEYWORD_SET(convolve) THEN BEGIN
     if wv eq 250 then band = 'PSW'
     if wv eq 350 then band = 'PMW'
     if wv eq 500 then band = 'PLW'
     npix = 5 * GET_SPIRE_BEAM_FWHM(band) / pixsize
     psf=GET_SPIRE_BEAM(band,pixsize,npix,npix,/SILENT,/NORM)
     psf=psf/TOTAL(psf)/(pixsize/3600.*!PI/180.0)^2.
;     sim_im = CONVOL(pixmap,psf,/EDGE_WRAP)
     SMOOTHMAP,pixmap,psf,SMMAP=sim_im
  ENDIF ELSE sim_im=pixmap

  ;; A WHITE NOISE GENERATOR
  IF N_ELEMENTS(noise) NE 0 THEN BEGIN
     noise_im=randomn(SEED,imangx,imangy)*noise 
     sim_im=sim_im+noise_im
  ENDIF

  RETURN,sim_im

  err_handler:
  IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF

END
