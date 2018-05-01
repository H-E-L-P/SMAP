;+
;NAME
; red_sourcefind
;PURPOSE
; Make a linear combination of resolution matched input maps and find sources
;USAGE
; red_sourcefind, basename [, COEFFS=, MASKFILES=, MAPDIR=, MASKDIR=, 
;                           /NOSMOOTH, /OPTIMAL, SN=, BGSUBSIZE=,
;                           MIN_CORR=, FLUXRAT=, VERBOSE=, SCATFILE=, 
;                           RAREFILE=, OUTDIR=, CATRAD=, CATNAME=, CATEXT= ]
;INPUTS
; basename         Basename of maps to read.
;KEYWORDS
; nosmooth         Do not apply any smoothing; setting this is
;                   assuming that the maps are already beam matched in
;                   some fashion.
; usepeakpos       Give the position based on the peak rather than
;                   the finder algorithm.
; optimal          Use Chapin-style optimal smoothing to match.
; nowritemap       Don't write combined map
;OPTIONAL INPUTS
; coeffs           Map combination coefficients (250, 350, 500)
; fwhm             FWHM of maps, in arcsec.  You need to provide
;                   this if nosmooth is set, otherwise the code
;                   attempts to figure this out for you assuming the
;                   standard beam sizes per band.  You can still set
;                   this by hand, in which case it is interpreted as
;                   the FWHM after smoothing is applied internally --
;                   so you had better know what that means.
; alg              Specify source finding algorithm.  Default is ALG=2 
;                  (uses Starfinder). ALG=1 uses find2.pro, modified from 
;                  find.pro in astrolib.  ALG=3 uses find.pro, IDL
;                  version of DAOPHOT finding algorithm.
; conf             Estimated confusion noise in combined map.  
;                   Used to construct the optimal filter (if /OPTIMAL is set).
;OPTIONAL OUTPUTS
; success          0 on failure, 1 on success, 2 if successful but no
;                   sources found
;NOTES
;MODIFICATION HISTORY
;-

PRO red_sourcefind, basename, COEFFS=coeffs, FWHM=fwhm, MASKFILES=maskfiles, $
                    MAPDIR=mapdir, MASKDIR=maskdir, SN=sn, $
                    NOSMOOTH=nosmooth, OPTIMAL=optimal, $
                    BGSUBSIZE=bgsubsize, MIN_CORR=min_corr, OUTDIR=outdir,$
                    FLUXRAT=fluxrat, SCATFILE=scatfile, RAREFILE=rarefile,$
                    CATRAD=catrad, CATNAME=catname, CATEXT=catext,$
                    CONF=conf, USEMAPMAKERERROR=usemapmakererror,$
                    SUCCESS=success, ERRMSG=errmsg, FINDPEAKPIX=findpeakpix,$
                    PEAKSEARCHRAD=peaksearchrad, USEPEAKPOS=usepeakpos,$
                    OUTMAPBASE=outmapbase, ALG=alg, VERBOSE=verbose,$
                    NOWRITEMAP=nowritemap

  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg = ""

  IF N_ELEMENTS(outmapbase) EQ 0 THEN outmapbase = basename
  IF N_ELEMENTS(catext) EQ 0 THEN catext=""
  IF N_ELEMENTS(alg) EQ 0 THEN alg = 2
  IF KEYWORD_SET(verbose) THEN BEGIN
     CASE alg OF
        1: MESSAGE," Using modified DAOPHOT algorithm", /INF
        2: MESSAGE," Using starfinder algorithm", /INF
        3: MESSAGE," Using IDL version of DAOPHOT", /INF
        ELSE: BEGIN
           errmsg = "Invalid, unknown finding algorithm"
           GOTO, err_handler
        END
     ENDCASE
  ENDIF

  ;;Do reads
  map250 = READ_SMAP_FITSMAP(basename,'PSW',DIR=mapdir,SUCCESS=rd_succ,$
                             ERRMSG=rd_errmsg,/SILENT)
  IF ~ rd_succ THEN BEGIN
     errmsg = "Error reading in 250 micron map: "+rd_errmsg
     GOTO, err_handler
  ENDIF
  map350 = READ_SMAP_FITSMAP(basename,'PMW',DIR=mapdir,SUCCESS=rd_succ,$
                             ERRMSG=rd_errmsg,/SILENT)
  IF ~ rd_succ THEN BEGIN
     errmsg = "Error reading in 350 micron map: "+rd_errmsg
     GOTO, err_handler
  ENDIF
  map500 = READ_SMAP_FITSMAP(basename,'PLW',DIR=mapdir,SUCCESS=rd_succ,$
                             ERRMSG=rd_errmsg,/SILENT)
  IF ~ rd_succ THEN BEGIN
     errmsg = "Error reading in 500 micron map: "+rd_errmsg
     GOTO, err_handler
  ENDIF

  ;;Red source find
  cat = red_sourcefind_dofind(map250, map350, map500, BASENAME=outmapbase,$
                              COEFFS=coeffs, MASKFILES=maskfiles, $
                              MASKDIR=maskdir, SN=sn, BGSUBSIZE=bgsubsize, $
                              MIN_CORR=min_corr, FLUXRAT=fluxrat, $
                              SCATFILE=scatfile, RAREFILE=rarefile, $
                              OUTDIR=outdir, CATRAD=catrad, CATHDR=cathdr, $
                              CONF=conf, USEMAPMAKERERROR=usemapmakererror,$
                              SUCCESS=cat_success, ERRMSG=cat_errmsg,$
                              FINDPEAKPIX=findpeakpix, FWHM=fwhm,$
                              PEAKSEARCHRAD=peaksearchrad, $
                              NOWRITE=nowritemap, $
                              NOSMOOTH=nosmooth, OPTFILT=optimal,$
                              USEPEAKPOS=usepeakpos, ALG=alg, VERBOSE=verbose)
  IF cat_success EQ 0 THEN BEGIN
     IF N_ELEMENTS(catname) NE 0 THEN BEGIN
        IF N_ELEMENTS(outdir) NE 0 THEN FILE_DELETE,outdir+catname,/ALLOW ELSE $
           FILE_DELETE,catname,/ALLOW
     ENDIF
     errmsg = "Unable to produce catalog: "+cat_errmsg
     GOTO, err_handler
  ENDIF
  IF cat_success EQ 2 THEN BEGIN
     ;; The code ran, but no sources were found
     IF N_ELEMENTS(catname) NE 0 THEN BEGIN
        IF N_ELEMENTS(outdir) NE 0 THEN FILE_DELETE,outdir+catname,/ALLOW ELSE $
           FILE_DELETE,catname,/ALLOW
     ENDIF
     success = 2b
     RETURN
  ENDIF

  ;; Found some sources -- write out catalog
  IF N_ELEMENTS(catname) EQ 0 THEN $
     catname = outmapbase + catext + '_redcat.fits'
  MWRFITS, cat, outdir + catname, cathdr, /SILENT, /CREATE,$
           STATUS=status
  IF status NE 0 THEN BEGIN
     errmsg = "Failed to write output catalog"
     GOTO, err_handler
  ENDIF
  
  success=1b
  RETURN

  err_handler:
  IF KEYWORD_SET(verbose) THEN MESSAGE, errmsg, /INF
  success = 0b
  RETURN

END
