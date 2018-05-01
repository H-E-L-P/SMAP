;+
;NAME
; red_optfilt_dofilt
;PURPOSE
; Apply optimum filtering to a set of red base maps that can
; then be combined. The output maps are clipped down.
; Inner function for red_optfilt that doesn't do reads and writes.
;USAGE
; red_optfilt, map250, map350, map500, [COEFFS=, /VERBOSE, /NOCLIP, FWHM=]
;INPUTS
; map???      Input maps.  Already masked.  Smoothed on output
;OPTIONAL INPUTS
; coeffs        Combination coefficients.  Used to estimate instrument
;                noise, which affects filtering.
;KEYWORDS
; verbose       Print status messages as it runs
; noclip        Don't clip out the edges (where the mask is set)
;OPTIONAL OUTPUTS
; fwhm          Estimated FWHM of output maps
; beam          Actual beam
; inst          Estimated instrument noise in combined map
;MODIFICATION HISTORY
; Author: Alex Conley, January 2015
;-

;; Clip maps down to specified range
FUNCTION red_optfilt_clip_doclip, map, minx, maxx, miny, maxy
  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN

  xsize = maxx - minx + 1
  ysize = maxy - miny + 1
  
  castr = map.astrometry
  castr.crpix[0] -= minx
  castr.crpix[1] -= miny

  IF map.has_exposure THEN BEGIN
     exptype = SIZE(map.exposure, /TNAME)
     IF exptype EQ 'DOUBLE' or exptype EQ 'FLOAT' THEN exp_dbl = 1b ELSE $
        exp_dbl = 0b
  ENDIF

  getmap_succ = 0b
  mapstruct = get_smap_mapstruct(NPIXX=xsize, NPIXY=ysize, $
                                 BAND=map.names, NOMASK=~map.has_mask,$
                                 ASTROMETRY=TEMPORARY(castr),$
                                 NOEXP=~map.has_exposure, $
                                 NOERR=~map.has_error, /SILENT,$
                                 ERRMSG=errmsg, SUCCESS=getmap_succ,$
                                 /NO_ABORT, EXP_DBL=exp_dbl,$
                                 LAMBDA=lambda)
  IF ~ getmap_succ THEN MESSAGE, errmsg

  mapstruct.image = map.image[minx:maxx, miny:maxy]
  IF map.has_mask THEN mapstruct.mask = map.mask[minx:maxx, miny:maxy]
  IF map.has_exposure THEN $
     mapstruct.exposure = map.exposure[minx:maxx, miny:maxy]
  IF map.has_error THEN $
     mapstruct.error = map.error[minx:maxx, miny:maxy]
  mapstruct.pixscale = map.pixscale

  RETURN, mapstruct

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clip down images to mask
PRO red_optfilt_clip, map250, map350, map500
  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN

  ;; Figure out how much we will clip
  ;; We basically want a list of pixels that are not masked in any
  ;; component map.
  any_mask = map250.has_mask OR map350.has_mask OR map500.has_mask
  IF any_mask EQ 0b THEN RETURN  ;; No information to clip with
  IF map250.has_mask THEN mask = map250.mask ELSE $
     mask = BYTARR(map250.xsize, map250.ysize)
  IF map350.has_mask THEN mask OR= map350.mask
  IF map500.has_mask THEN mask OR= map500.mask
  wnomask = WHERE(TEMPORARY(mask) EQ 0b, nnomask)
  IF nnomask EQ 0 THEN MESSAGE, "All pixels masked"
  aidx = array_indices(map250.image, wnomask)
  minx = MIN(aidx[0, *], MAX=maxx)
  miny = MIN(aidx[1, *], MAX=maxy)
  DELVARX, aidx, wnomask

  ;; Add a small buffer if possible
  bufsize = 3
  IF minx GE bufsize THEN minx -= bufsize
  IF maxx LT map250.xsize - bufsize THEN maxx += bufsize
  IF miny GE bufsize THEN miny -= bufsize
  IF maxy LT map250.ysize - bufsize THEN maxy += bufsize

  ;; If no clipping, return
  IF minx EQ 0 AND miny EQ 0 AND $
     maxx EQ map250.xsize-1 AND maxy EQ map250.ysize-1 THEN RETURN ;; No clipping

  ;; Do clipping
  map250 = red_optfilt_clip_doclip(map250, minx, maxx, miny, maxy)
  map350 = red_optfilt_clip_doclip(map350, minx, maxx, miny, maxy)
  map500 = red_optfilt_clip_doclip(map500, minx, maxx, miny, maxy)
  
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

PRO red_optfilt_dofilt, map250, map350, map500, COEFFS=coeffs, CONF=conf,$
                        NPAD=npad, INST=inst, FWHM=fwhm, BEAM=beam,$
                        NOCLIP=noclip, VERBOSE=verbose
  COMPILE_OPT IDL2, STRICTARRSUBS

  IF N_ELEMENTS(coeffs) EQ 0 THEN coeffs = [-SQRT(1-0.92^2), 0.0, 0.92]
  IF N_ELEMENTS(conf) EQ 0 THEN conf = 4.23e-3 ;; sig conf, Jy
  IF N_ELEMENTS(npad) EQ 0 THEN npad = 100

  ;; Smoothing
  IF KEYWORD_SET(verbose) THEN MESSAGE, "Optimally filtering maps", /INF
  matched_filter_red, map250, map350, map500, conf, $
                      WHITE_VAR=white_var, BEAM=beam, NPAD=npad, $
                      K1=coeffs[0], K2=coeffs[1]
  inst = SQRT(white_var)
  
  ;; Measure
  IF ARG_PRESENT(fwhm) THEN BEGIN
     IF KEYWORD_SET(verbose) THEN MESSAGE, "Measuring FWHM", /INF
     init_fwhm = 35.2 / map500.pixscale ;;in pix, standard 500um PSF assumed
     maxbm = MAX(beam, wmax)
     szbm = SIZE(beam)
     init_pos_x = wmax MOD szbm[1]
     init_pos_y = wmax / szbm[1]
     censize = 5 * CEIL(init_fwhm)
     minx = (init_pos_x - censize) > 0
     maxx = (init_pos_x + censize) < (szbm[1] - 1)
     miny = (init_pos_y - censize) > 0
     maxy = (init_pos_y + censize) < (szbm[2] - 1)
     beam = beam[minx:maxx, miny:maxy]
     maxbm = MAX(beam, wmax)
     szbm = SIZE(beam)
     init_pos_x = wmax MOD szbm[1]
     init_pos_y = wmax / szbm[1]
     init_params = [0.0, 1.0, 0.5 * init_fwhm, 0.5 * init_fwhm, $
                    init_pos_x, init_pos_y, 1.0]
     yfit = MPFIT2DPEAK(beam, params, /CIRCULAR,$
                        ESTIMATES=init_params, /GAUSSIAN)
     fwhm = 2.355 * params[2] * map500.pixscale  ;; param[2] is sigma
  ENDIF

  ;; Potential clipping
  IF ~ KEYWORD_SET(noclip) THEN BEGIN
     IF KEYWORD_SET(verbose) THEN MESSAGE, "Clipping edges", /INF
     red_optfilt_clip, map250, map350, map500
  ENDIF  
END
