;+
; NAME:
;   MAPSPACE_HIFILTER
; CATEGORY:
;   Herschel SMAP map making
; PURPOSE
;   To apply a high-pass filter to SMAP maps to remove cirrus
;   and/or clustering signal
; INPUTS
;   map        - SMAP style fitsmap
; OPTIONAL INPUTS
;   filtscale  - Filter high-pass scale, in arcmin (def: 6)
;   qfactor    - FWHM of smoothing Gaussian compared to the
;                wavenumber of the SHARP HPF.  (def: 0.3)
; KEYWORDS
;   NaN        - If set, set's NaN and infinite values to the map average,
;                 then re-sets them at the end
; RETURNS
;   The smoothed map
; MODIFICATION HISTORY:
;   Initial Author: Timothy Ellsworth Bowers, March 2010
;-

FUNCTION MAPSPACE_HIFILTER, map, FILTSCALE=filtscale, SUCCESS=success, $
                            ERRMSG=errmsg, VERBOSE=verbose, $
                            PLOT=plot, QFACTOR=qfactor, NAN=nan
  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg = ''
  IF N_ELEMENTS( filtscale ) EQ 0 THEN filtscale = 6
  IF N_ELEMENTS( qfactor ) EQ 0 THEN qfactor = 0.3
  verbose = KEYWORD_SET(verbose)

  IF filtscale LE 0 THEN BEGIN
     errmsg = "Filter scale should be positive"
     GOTO, err_handler
  ENDIF
  IF qfactor LT 0 THEN BEGIN
     errmsg = "qfactor must be non-negative"
     GOTO, err_handler
  ENDIF

  ;;======================================================================
  ;; Convert pixelscale into arcmin
  ;;======================================================================
  pixscale = map.pixscale / 60.d
  IF verbose THEN MESSAGE,$
     STRING(pixscale,format="('Pixel scale is ',F0.3,' arcmin')"),/INF
  
  ;;Deal with NaN, etc.
  t_image = map.image
  IF KEYWORD_SET( nan ) THEN BEGIN
     w_nonfin = WHERE( ~ FINITE(t_image), n_nonfin, NCOMPLEMENT=n_finite )
     IF n_finite EQ 0 THEN BEGIN
        errmsg = "All values in image non-finite"
        GOTO, err_handler
     ENDIF
     IF n_nonfin NE 0 THEN BEGIN
        badvals = t_image[w_nonfin]
        t_image[w_nonfin] = MEAN( t_image, /NAN )
     ENDIF
  ENDIF ELSE t_image = map.image

  ;;======================================================================
  ;; FT{ map } 
  ;;======================================================================
  ft_map = FFT( TEMPORARY(t_image), -1, /DOUBLE )
  
  psd_map = REAL_PART(ft_map * CONJ(ft_map))
  
  size_psd = size(psd_map,/DIM)
  N_psd = size_psd - 1
  
  ;;======================================================================
  ;; Build a filter mask 
  ;; Multiply the FT{ map } by the mask and transform back.
  ;;======================================================================
  ;;The tricky bit here has to do with the way that array sizes wrap
  ;; with FFTs
  mask = dcomplexarr(size_psd) + COMPLEX(1., /DOUBLE)
  cplx0 = COMPLEX(0.0,/DOUBLE)
  ;;This could presumably be done more efficiently, but it's
  ;; reasonably fast as is for practical maps
  FOR i=0L, long(N_psd[0]/2) DO BEGIN
     FOR j=0L, long(N_psd[1]/2) DO BEGIN
        kr = SQRT( i^2 + j^2 )
        r  = size_psd[0] * pixscale / kr
        
        IF (r GE filtscale) THEN BEGIN             
           mask[i,          j]                 = cplx0
           IF (j NE 0) THEN $
              mask[i,          N_psd[1]-j+1]   = cplx0
           IF (i NE 0) THEN $
              mask[N_psd[0]-i+1, j]            = cplx0
           IF (i NE 0) AND (j NE 0) THEN $
              mask[N_psd[0]-i+1, N_psd[1]-j+1] = cplx0
        ENDIF
     ENDFOR
  ENDFOR

  ;;=============================================================
  ;; Now, take this SHARP HPF and convolve it with a Gaussian to
  ;; smooth it -- reduces ringing and other effects.
  ;;=============================================================
  IF qfactor GT 0 THEN BEGIN
     ;; Calculate FWHM of Gaussian smoothing kernel
     FWHM = size_psd[0] * pixscale / filtscale * qfactor
     
     IF KEYWORD_SET( plot ) THEN $
        shade_surf,mask,xrange=[0,FWHM / qfactor * 2.],$
                   yrange=[0,FWHM / qfactor * 2.]
  
     IF KEYWORD_SET( verbose ) THEN BEGIN
        message,string(FWHM,format="('Smoothing FWHM = ',F0.5)"),/inf
        message,string(qfactor,format="('Smoothing Quality Factor = ',F0.2)"),$
                /inf
     ENDIF
  
     ;; Make the smoothed mask
     smooth_mask = filter_image( mask, /ALL_PIXELS, FWHM=FWHM,/NO_FT_CONVOL)
  
     IF KEYWORD_SET( plot ) THEN $
        shade_surf,smooth_mask,$
                   xrange=[0,FWHM / qfactor * 2.],yrange=[0,FWHM / qfactor * 2.]
  ENDIF

  ;;=============================================================
  ;; Transform back
  ;;=============================================================
  filt_map = real_part(fft(ft_map * smooth_mask, +1, /DOUBLE))
  
  ;;Reset bad values
  IF KEYWORD_SET( nan ) && n_nonfin NE 0 THEN $
     filt_map[w_nonfin] = TEMPORARY( badvals )

  success = 1b
  RETURN,filt_map
  
  err_handler:
  IF verbose THEN MESSAGE,errmsg,/INF
  success = 0b
  RETURN,filt_map * 0.d
  
  
END
