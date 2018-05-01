;+
;NAME
; red_optfilt
;PURPOSE
; Apply optimum filtering to a set of red base maps that can
; then be combined.  Any masks are also applied, and the output
; maps clipped down
;USAGE
; red_optfilt, basename, outname, [COEFFS=, MASKFILES=, MAPDIR=, MASKDIR=,
;              OUTDIR=, /VERBOSE, /NOCLIP]
;INPUTS
; basename      Base filename of input maps
; outname       Base filename of output maps
;OPTIONAL INPUTS
; coeffs        Combination coefficients.  Used to estimate instrument
;                noise, which affects filtering.
;KEYWORDS
; verbose       Print status messages as it runs
; noclip        Don't clip out the edges (where the mask is set)
;SIDE EFFECTS
; Smoothed versions of the maps are writting to outname
;MODIFICATION HISTORY
; Author: Alex Conley, October 2014
;-

PRO red_optfilt, basename, outname, COEFFS=coeffs, CONF=conf,$
                 NPAD=npad, MAPDIR=mapdir, MASKFILES=maskfiles,$
                 MASKDIR=maskdir, OUTDIR=outdir, NOCLIP=noclip, $
                 VERBOSE=verbose
  COMPILE_OPT IDL2, STRICTARRSUBS

  IF N_ELEMENTS(coeffs) EQ 0 THEN coeffs = [-SQRT(1-0.92^2), 0.0, 0.92]
  IF N_ELEMENTS(conf) EQ 0 THEN conf = 4.23e-3 ;; sig conf, Jy
  IF N_ELEMENTS(npad) EQ 0 THEN npad = 100

  ;; Read the maps
  IF KEYWORD_SET(verbose) THEN MESSAGE, "Reading maps", /INF
  map250 = READ_SMAP_FITSMAP(basename, 'PSW', DIR=mapdir, SUCCESS=rd_succ,$
                               ERRMSG=rd_errmsg,/SILENT)
  IF ~ rd_succ THEN $
     MESSAGE, "Error reading in PSW micron map: " + rd_errmsg
  map350 = READ_SMAP_FITSMAP(basename, 'PMW', DIR=mapdir, SUCCESS=rd_succ,$
                             ERRMSG=rd_errmsg, /SILENT)
  IF ~ rd_succ THEN $
     MESSAGE, "Error reading in PMW micron map: " + rd_errmsg
  map500 = READ_SMAP_FITSMAP(basename, 'PLW', DIR=mapdir, SUCCESS=rd_succ,$
                             ERRMSG=rd_errmsg, /SILENT)
  IF ~ rd_succ THEN $
     MESSAGE, "Error reading in PLW micron map: " + rd_errmsg

  ;; Deal with masks
  IF N_ELEMENTS(maskfiles) NE 0 THEN BEGIN
     IF KEYWORD_SET(verbose) THEN MESSAGE, "Adding masks", /INF
     IF ~ map250.has_mask THEN $
        MESSAGE,"Can't add user mask to 250um map if none present in map"
     add_user_mapmask, map250, maskfiles, /ANDPLUS, MASKDIR=maskdir
     IF ~ map350.has_mask THEN $
        MESSAGE,"Can't add user mask to 350um map if none present in map"
     add_user_mapmask, map350, maskfiles, /ANDPLUS, MASKDIR=maskdir
     IF ~ map500.has_mask THEN $
        MESSAGE,"Can't add user mask to 500um map if none present in map"
     add_user_mapmask, map500, maskfiles, /ANDPLUS, MASKDIR=maskdir
  ENDIF

  ;; Do actual filtering
  red_optfilt_dofilt, map250, map350, map500, COEFFS=coeffs, CONF=conf,$
                      NPAD=napd, INST=inst, FWHM=fwhm, NOCLIP=noclip,$
                      VERBOSE=verbose

  ;; Additional header info for output
  hdr = ['']
  SXADDPAR, hdr, 'K1', coeffs[0], 'map250 coeff'
  SXADDPAR, hdr, 'K2', coeffs[1], 'map350 coeff'
  SXADDPAR, hdr, 'K3', coeffs[2], 'map500 coeff'
  SXADDPAR, hdr, 'CONF', conf, 'Filtering confusion noise'
  SXADDPAR, hdr, 'INST', inst, 'Filtering instrument noise'
  SXADDPAR, hdr, 'NPAD', npad, 'Filter padding'
  SXADDPAR, hdr, 'SMOOTHED', 'T', "Additonal smoothing in combination"
  SXADDPAR, hdr, 'OPTFILT', 'T', "Optimal filtering used"
  SXADDPAR, hdr, "FWHM", fwhm, "Estimated FWHM in combined map"
  opthdr = hdr[0:N_ELEMENTS(hdr)-2] ;;clip off END

  ;; Write
  IF KEYWORD_SET(verbose) THEN MESSAGE, "Writing results", /INF
  st = write_smap_fitsmap(map250, outname, DIR=outdir, ERRMSG=errmsg, $
                          OPTHDR=opthdr, /NO_ABORT, /SILENT)
  IF st EQ 0 THEN $
     MESSAGE, "Error writing 250um output map: " + errmsg
  st = write_smap_fitsmap(map350, outname, DIR=outdir, ERRMSG=errmsg, $
                          OPTHDR=opthdr, /NO_ABORT, /SILENT)
  IF st EQ 0 THEN $
     MESSAGE, "Error writing 350um output map: " + errmsg
  st = write_smap_fitsmap(map500, outname, DIR=outdir, ERRMSG=errmsg, $
                          OPTHDR=opthdr, /NO_ABORT, /SILENT)
  IF st EQ 0 THEN $
     MESSAGE, "Error writing 500um output map: " + errmsg
END
