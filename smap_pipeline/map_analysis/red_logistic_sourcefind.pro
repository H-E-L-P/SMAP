;+
;NAME
; red_logistic_sourcefind
;PURPOSE
; Find red sources in filter-matched maps using a logistic model.
; 
;USAGE
; cat = red_logistic_sourcefind(basename, MAPDIR=, MASKDIR=, 
;                               MASKFILES=, COMBMAPFILENAME=, OUTDIR=,
;                               CATHDR=, SUCCESS=, ERRMSG=])
;
;INPUTS
;  basename            Base name of input files.  Must have the same
;                      pixel scale, shouldn't be filtered.
;OPTIONAL INPUTS
;  mapdir              Directory to look for maps in.
;  maskdir             Directory to look for masks in
;  maskfiles           Name of mask files
;  combmapfilename     Name to write combined map out to (in outdir)
;  outdir              Directory to write combmap to if so specified
;KEYWORDS
;  verbose             Print information messages during execution
;OUTPUTS
;  cat                 Resulting catalog as a STRUCT
;OPTIONAL OUTPUTS
; cathdr        Header information for catalog (useful for writing it
;               as a FITS table)
; success       0 on failure, 1 on success, 2 if no red sources found
; errmsg        Error message if failure
;-

PRO red_logistic_sourcefind_read, basename, map250, map350, map500,$
                                  MAPDIR=mapdir,  VERBOSE=verbose,$
                                  MASKFILES=maskfiles, MASKDIR=maskdir,$
                                  SUCCESS=success, ERRMSG=errmsg
  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN

  success = 0b
  errmsg = ""

  IF KEYWORD_SET(verbose) THEN MESSAGE, "Reading maps", /INF
  
  map250 = READ_SMAP_FITSMAP(basename, 'PSW', DIR=mapdir, $
                             /NO_ABORT, SUCCESS=msuccess, ERRMSG=merrmsg,$
                             /SILENT)
  IF msuccess EQ 0 THEN BEGIN
     errmsg = "Error opening 250um map: " + merrmsg
     RETURN
  ENDIF
  map350 = READ_SMAP_FITSMAP(basename, 'PMW', DIR=mapdir, $
                             /NO_ABORT, SUCCESS=msuccess, ERRMSG=merrmsg,$
                             /SILENT)
  IF msuccess EQ 0 THEN BEGIN
     errmsg = "Error opening 350um map: " + merrmsg
     RETURN
  ENDIF
  map500 = READ_SMAP_FITSMAP(basename, 'PLW', DIR=mapdir, $
                             /NO_ABORT, SUCCESS=msuccess, ERRMSG=merrmsg,$
                             /SILENT)
  IF msuccess EQ 0 THEN BEGIN
     errmsg = "Error opening 500um map: " + merrmsg
     RETURN
  ENDIF

  ;; Check extents and pixel sizes
  IF map250.xsize NE map350.xsize OR $
     map250.ysize NE map350.ysize THEN BEGIN
     errmsg = "Input 250, 350 maps not same size"
     RETURN
  ENDIF
  IF ABS((map250.pixscale - map350.pixscale)/map250.pixscale) $
     GT 1d-3 THEN BEGIN
     errmsg = "Input 250, 350 micron maps don't have same pixel scale"
     RETURN
  ENDIF
  IF map250.xsize NE map500.xsize OR $
     map250.ysize NE map500.ysize THEN BEGIN
     errmsg = "Input maps not same size"
     RETURN
  ENDIF
  IF ABS((map250.pixscale - map500.pixscale)/map250.pixscale) $
     GT 1d-3 THEN BEGIN
     errmsg = "Input 250, 500 micron maps don't have same pixel scale"
     RETURN
  ENDIF

  ;; Add user specified masks if present
  IF N_ELEMENTS(maskfiles) NE 0 THEN BEGIN
     IF KEYWORD_SET(verbose) THEN MESSAGE," Adding user specified masks", /INF
     IF ~ map250.has_mask THEN BEGIN
        errmsg = "Can't add user mask to 250um map if none present in map"
        RETURN
     ENDIF
     add_user_mapmask, map250, maskfiles, /ANDPLUS, MASKDIR=maskdir
     IF ~ map350.has_mask THEN BEGIN
        errmsg = "Can't add user mask to 350um map if none present in map"
        RETURN
     ENDIF
     add_user_mapmask, map350, maskfiles, /ANDPLUS, MASKDIR=maskdir
     IF ~ map500.has_mask THEN BEGIN
        errmsg = "Can't add user mask to 500um map if none present in map"
        RETURN
     ENDIF
     add_user_mapmask, map500, maskfiles, /ANDPLUS, MASKDIR=maskdir
  ENDIF

  success = 1b
  
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION red_logistic_sourcefind, basename, MAPDIR=mapdir, VERBOSE=verbose,$
                                  MASKDIR=maskdir, MASKFILES=maskfiles, $
                                  COMBMAPFILENAME=combmapfilename, $
                                  OUTDIR=outdir, CATHDR=cathdr,$
                                  SUCCESS=success, ERRMSG=errmsg
  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg = ""

  ;; Read in maps, add masks
  red_logistic_sourcefind_read, basename, map250, map350, map500,$
                                MAPDIR=mapdir,  VERBOSE=verbose,$
                                MASKFILES=maskfiles, MASKDIR=maskdir,$
                                SUCCESS=rdsuccess, ERRMSG=rderrmsg
  IF rdsuccess EQ 0 THEN BEGIN
     errmsg = "Error reading files: " + rderrmsg
     GOTO, err_handler
  ENDIF

  ;; Do combination, seach, etc.
  ;; Use errmsg, success from _dofind
  cat = red_logistic_sourcefind_dofind(map250, map350, map500, $
                                       VERBOSE=verbose,$
                                       CATHDR=cathdr, OUTDIR=outdir,$
                                       COMBMAPFILENAME=combmapfilename,$
                                       SUCCESS=success, ERRMSG=errmsg)
  RETURN, cat

  ;; Abnormal termination
  err_handler:
  IF KEYWORD_SET(verbose) THEN MESSAGE, errmsg, /INF
  success = 0b
  RETURN, !VALUES.F_NAN

END
