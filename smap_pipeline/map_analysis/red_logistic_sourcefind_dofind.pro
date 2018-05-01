;+
;NAME
; red_logistic_sourcefind_dofind
;PURPOSE
; Make a linear combination of input maps and find sources using a
; classifier logistic.  Inner function for red_logistic_sourcefind.
; The maps should be read, masked, and error checked on input.
; 
;USAGE
; cat = red_logistic_sourcefind_dofind(map250, map350, map500, [/VERBOSE,
;                                      FWHM=, COMBMAPFILENAME=, OUTDIR=])
;
;INPUTS
;  map[250,350,500]    Input maps.  Should be masked and already
;                       checked for compatability.
;OPTIONAL INPUTS
;  fwhm                FWHM of smoothed maps in arcsec. Needed for the
;                       simplephot box.  Only needed if /NOSMOOTH is
;                       set, otherwise estimated by smoother
;KEYWORDS
;  verbose             Print informational messages during execution
;  nosmooth            Don't apply smoothing
;OUTPUTS
;  cat                 Resulting catalog as a STRUCT
;OPTIONAL OUTPUTS
; cathdr        Header information for catalog (useful for writing it
;               as a FITS table)
; success       0 on failure, 1 on success, 2 if no sources found
;-

FUNCTION red_logistic_sourcefind_dofind, map250, map350, map500, FWHM=fwhm, $
                                         NOSMOOTH=nosmooth, CATHDR=cathdr, $
                                         VERBOSE=verbose, OUTDIR=outdir, $
                                         COMBMAPFILENAME=combmapfilename,$
                                         SUCCESS=success, ERRMSG=errmsg

  COMPILE_OPT IDL2, STRICTARRSUBS

  success = 0b
  errmsg = ""

  ;; Coeff order is 250, 350, 500
  ;; The first set are for the smoothing only (just for the noise est)
  coeffs_filt = [-0.65, 0.25, SQRT(1.0 - 0.65^2 - 0.25^2)]
  ;; These are for the actual combination; they must match those
  ;;  in red_logistic_model, which is why they aren't user-settable
  coeffs_sub = [-0.650, 0.350, SQRT(1.0 - 0.65^2 - 0.350^2)]
  bgsubsize = 192.0 ;; arcsec
  sn = 3.0          ;; signal to noise ratio
  min_corr = 0.8    ;; Minimum correlation
  fluxrat = [0.3, 0.6]
  mins500 = 20.0
  
  ;; Optimum filtering; note we use the sub coeffs here rather than
  ;; the actual comb ones.  This is to match the training process.
  IF ~ KEYWORD_SET(nosmooth) THEN BEGIN
     red_optfilt_dofilt, map250, map350, map500, COEFFS=coeffs_filt,$
                         FWHM=fwhm, VERBOSE=verbose
  ENDIF ELSE IF N_ELEMENTS(fwhm) EQ 0 THEN BEGIN
     errmsg = "If nosmooth is set, user must provide FWHM"
     GOTO, err_handler
  ENDIF

  ;; Combine and do the source finding
  IF KEYWORD_SET(verbose) THEN $
     MESSAGE," Finding red sources in map", /INF
  redcat = red_sourcefind_dofind(map250, map350, map500, BASENAME=mapbase,$
                                 COEFFS=coeffs_sub, /NOSMOOTH, $
                                 SN=sn, BGSUBSIZE=bgsubsize, MINS500=mins500,$
                                 MIN_CORR=min_corr, FLUXRAT=fluxrat, $
                                 /NOWRITE, SUCCESS=redcat_success, $
                                 ERRMSG=redcat_errmsg, FWHM=fwhm,$
                                 PEAKSEARCHRAD=12.0, ALG=2, MAPCOMB=mapcomb)
  IF redcat_success EQ 0 THEN BEGIN
     errmsg = redcat_errmsg
     GOTO, err_handler
  ENDIF ELSE IF redcat_success EQ 2 THEN BEGIN
     MESSAGE, "No potential sources found in map", /INF
     success = 2b
     RETURN, !VALUES.F_NAN
  ENDIF
  IF KEYWORD_SET(verbose) THEN BEGIN
     fmt = '("   Found ",I0," sources in linearly combined map")'
     MESSAGE, STRING(N_ELEMENTS(redcat),FORMAT=fmt), /INF
  ENDIF

  ;; Now we need to apply the logistic regression bit to select
  ;; actual red sources
  wred = red_logistic_model(redcat.f_sub, redcat.df_sub,$
                            redcat.map_f250, redcat.map_f350,$
                            redcat.map_f500, redcat.corr, N=nred)

  IF nred EQ 0 THEN BEGIN
     MESSAGE, "No red sources identified", /INF
     RETURN, !VALUES.F_NAN
     success = 2b
  ENDIF

  IF KEYWORD_SET(verbose) THEN BEGIN
     fmt = '("   Selected ",I0," sources as red")'
     MESSAGE, STRING(nred, FORMAT=fmt), /INF
  ENDIF
  
  ;; Write combined map if requested
  IF N_ELEMENTS(combmapfilename) NE 0 THEN BEGIN
     ;; Additional header info for combined map
     hdr = ['']
     SXADDPAR, hdr, 'HISTORY', 'Linearly combined map'
     SXADDPAR, hdr, 'K1', coeffs_sub[0], 'map250 coeff'
     SXADDPAR, hdr, 'K2', coeffs_sub[1], 'map350 coeff'
     SXADDPAR, hdr, 'K3', coeffs_sub[2], 'map500 coeff'
     SXADDPAR, hdr, 'K1FILT', coeffs_filt[0], 'map250 filt coeff'
     SXADDPAR, hdr, 'K2FILT', coeffs_filt[1], 'map350 filt coeff'
     SXADDPAR, hdr, 'K3FILT', coeffs_filt[2], 'map500 filt coeff'
     SXADDPAR, hdr, "BGSUB", "F", "Background subtracted"
     IF ~ KEYWORD_SET(nosmooth) THEN BEGIN
        SXADDPAR, hdr, 'SMOOTHED', 'T', "Additonal smoothing in combination"
        IF KEYWORD_SET(optfilt) THEN BEGIN
           SXADDPAR, hdr, 'OPTFILT', 'T', "Optimal filtering used"
        ENDIF ELSE $
           SXADDPAR, hdr, 'OPTFILT', 'F', "Optimal filtering used"
     ENDIF ELSE $
        SXADDPAR, hdr, 'SMOOTHED', 'F', "Additonal smoothing in sourcefind"
     SXADDPAR, hdr, "FWHM", fwhm, "Estimated FWHM in combined map"
     opthdr = hdr[0:N_ELEMENTS(hdr)-2] ;;clip off END

     ;; Write
     st = write_smap_fitsmap(mapcomb, combmapfilename, DIR=outdir,$
                             OPTHDR=opthdr, /NO_ABORT, /SILENT)
     IF st EQ 0 THEN BEGIN
        errmsg =  "Error writing combined output map"
        GOTO, err_handler
     ENDIF
  ENDIF

  IF nred NE N_ELEMENTS(redcat) THEN redcat = redcat[TEMPORARY(wred)]

  IF ARG_PRESENT(cathdr) THEN BEGIN
     cathdr = ['']
     SXADDPAR, cathdr, 'K1', coeffs_sub[0], 'map250 coeff'
     SXADDPAR, cathdr, 'K2', coeffs_sub[1], 'map350 coeff'
     SXADDPAR, cathdr, 'K3', coeffs_sub[2], 'map500 coeff'
     SXADDPAR, hdr, 'K1FILT', coeffs_filt[0], 'map250 filt coeff'
     SXADDPAR, hdr, 'K2FILT', coeffs_filt[1], 'map350 filt coeff'
     SXADDPAR, hdr, 'K3FILT', coeffs_filt[2], 'map500 filt coeff'
     SXADDPAR, cathdr, 'SN', sn, 'Required S/N'
     SXADDPAR, cathdr, 'BGSUBSZ', bgsubsize, 'Background grid size [arcsec]'
     IF N_ELEMENTS(mins500) NE 0 THEN $
        SXADDPAR, cathdr, 'MINS500', mins500, 'Min allowed S_500 [mJy]'
     SXADDPAR, cathdr, 'MINCORR', min_corr, 'Minimum correlation coeff'
     IF ~ KEYWORD_SET(nosmooth) THEN BEGIN
        SXADDPAR, cathdr, 'SMOOTHED', 'T', "Additonal smoothing in sourcefind"
        IF KEYWORD_SET(optimal) THEN BEGIN
           SXADDPAR, cathdr, 'OPTFILT', 'T', "Optimal filtering used"
        ENDIF ELSE $
           SXADDPAR, cathdr, 'OPTFILT', 'F', "Optimal filtering used"
     ENDIF ELSE $
        SXADDPAR, cathdr, 'SMOOTHED', 'F', "Additonal smoothing in sourcefind"
     SXADDPAR, cathdr, "FWHM", fwhm, "Estimated FWHM in combined map"
     SXADDPAR, cathdr, "PHOTALG", "STARFINDER", "Phot algorithm"
  ENDIF  

  success = 1b
  RETURN, redcat

  err_handler:
  IF KEYWORD_SET(verbose) THEN MESSAGE, errmsg, /INF
  success = 0b
  RETURN, !VALUES.F_NAN
END
