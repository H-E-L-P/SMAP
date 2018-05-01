;+
;NAME
; read_powerspec
;PURPOSE
; Reads in auto or cross power spectra from Herschel data, wrapping
;  them in a fits structure.  This automatically determines which type
;  of spectrum (auto or cross) from the file.
;USAGE
; spec = read_powerspec(filename)
;INPUTS
; filename       Name of the FITS files to read.  Follows the input conventions
;                 of get_[auto|cross]_psd_from_l1
;RETURNS
;  An anonymous structure containing the information about the PSDs.  The format
;  of the structure should be very similar for both auto- and
;  cross-PSDs.  Returns NaN if not successful.
;OPTIONAL OUTPUTS
;  success       1 on success, 0 on failure
;  errmsg        An error message in case of failure
;  type          Either 'cross' or 'auto' depending what type is read in
;KEYWORDS
;  verbose       Run in verbose mode
;MODIFICATION HISTORY
; Author: Alex Conley, May 2009
;-

FUNCTION read_cross_powerspec,infile,primary_hdr,SUCCESS=success,$
   VERBOSE=verbose, ERRMSG=errmsg

  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN
  success = 0b
  errmsg = ''

  readstatus=0b
  desc = MRDFITS( infile, 1, /SILENT, STATUS=readstatus )
  IF readstatus NE 0 THEN BEGIN
     errmsg = "Error reading descriptive extension of "+infile
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF

  psds = MRDFITS( infile, 2, /SILENT, STATUS=readstatus )
  IF readstatus NE 0 THEN BEGIN
     errmsg = "Error reading PSD image extension of "+infile
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF
  
  files = MRDFITS( infile, 3, /SILENT, STATUS=readstatus )
  IF readstatus NE 0 THEN BEGIN
     errmsg = "Error reading file name extension of "+infile
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF

  file_mask = MRDFITS( infile, 4, mask_hdr, /SILENT, STATUS=readstatus, $
                       /UNSIGNED )
  IF readstatus NE 0 THEN BEGIN
     errmsg = "Error reading file mask extension of "+infile
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF
  
  ndet = N_ELEMENTS( desc ) - 1 ;;-1 for the FREQ entry
  nfreq = ( SIZE(psds) )[2]
  nfiles = N_ELEMENTS(files)

  IF ndet LE 0 THEN BEGIN
     errmsg = "No detectors found"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF
  IF nfreq LE 0 THEN BEGIN
     errmsg = "No frequency range found"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF
  IF nfiles LE 0 THEN BEGIN
     errmsg = "No files found"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF
  IF (SIZE(file_mask))[1] NE ndet+1 THEN BEGIN ;;+1 for freq
     errmsg = "Wrong number of detectors in file mask"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF
  IF (SIZE(file_mask))[2] NE nfiles THEN BEGIN
     errmsg = "Wrong number of files in file mask"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF

  mask_info = get_mask_bits( mask_hdr )
  

  ;;Begin actually constructing what we want to return
  retstr = { type: 'cross', ndet: ndet, nfreq: nfreq,$
             chunksize: 2*(nfreq-1),ndatapoints: -1L,$
             mindatalength: 0, windowing: 0b, windowtype: '',$
             overlapping: 0b, polyremove: 0b, polyorder: 0,$
             badmask: 'FFFFFFFF'XuL,$
             detnames: STRARR(ndet), detidx: INTARR(ndet),$
             det1name: STRARR(ndet), det2name: STRARR(ndet),$
             freq: DBLARR(nfreq), frequnits: '',$
             psds: DBLARR(ndet,nfreq), psd_units: STRARR(ndet),$
             nfiles: nfiles, ngoodfiles: 0, nbadfiles: 0, $
             files: STRARR(nfiles),$
             goodfiles: STRARR(nfiles), badfiles: STRARR(nfiles),$
             nfiles_per_psd: INTARR(ndet), $
             bad: BYTARR(ndet), filemask: ULONARR(ndet,nfiles),$
             mask_info: mask_info }

  windowing = SXPAR( primary_hdr, 'WINDOW', COUNT=count )
  IF count NE 0 AND windowing THEN BEGIN
     retstr.windowing = 1b
     windfunc = SXPAR( primary_hdr, 'WINTYP', COUNT=count )
     IF count NE 1 THEN retstr.windowtype='Unknown' ELSE $
        retstr.windowtype=STRTRIM(windfunc,2)
     overlap = SXPAR( primary_hdr, 'OVERLAP', COUNT=count )
     IF count EQ 1 THEN retstr.overlapping = overlap
  ENDIF
  ndata = SXPAR( primary_hdr, 'NDATAPTS', COUNT=count )
  IF count EQ 1 THEN retstr.ndatapoints = ndata
  mindata = SXPAR( primary_hdr, 'MNDATLEN', COUNT=count )
  IF count EQ 1 THEN retstr.mindatalength = mindata
  polyremove = SXPAR( primary_hdr, 'POLYRMV', COUNT=count )
  IF count EQ 1 AND polyremove THEN BEGIN
     retstr.polyremove = 1b
     polyorder = SXPAR( primary_hdr, 'POLYORDR', COUNT=count )
     IF count EQ 1 THEN retstr.polyorder = polyorder
  ENDIF 
  badmask = SXPAR( primary_hdr, 'BADMASK', COUNT=count )
  IF count EQ 1 THEN BEGIN
     ;;Need to convert from Hexidecimal to unsigned 32bit int
     value=0uL
     READS,badmask,value,FORMAT='(Z)'
     retstr.badmask = value
  ENDIF
  ngoodfiles = SXPAR( primary_hdr, 'NGOODFL', COUNT=count )
  IF count EQ 1 THEN retstr.ngoodfiles = ngoodfiles
  nbadfiles = SXPAR( primary_hdr, 'NBADFL', COUNT=count )
  IF count EQ 1 THEN retstr.nbadfiles = nbadfiles
  IF retstr.ngoodfiles NE 0 THEN BEGIN
     goodfiles = SXPAR( primary_hdr, 'GDFL*', COUNT=count )
     IF count NE 0 && count NE retstr.ngoodfiles THEN BEGIN
        ;;count=0 is a (rare) but acceptable outcome which occurs
        ;; if we input more than 9999 files
        errmsg = "Wrong number of goodfiles found in primary header "+infile
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDIF
     retstr.goodfiles[0:N_ELEMENTS(goodfiles)-1]=goodfiles
  ENDIF
  IF retstr.nbadfiles NE 0 THEN BEGIN
     badfiles = SXPAR( primary_hdr, 'BDFL*', COUNT=count )
     IF count NE 0 && count NE retstr.nbadfiles THEN BEGIN
        errmsg = "Wrong number of badfiles found in primary header "+infile
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDIF
     retstr.badfiles[0:N_ELEMENTS(badfiles)-1]=badfiles
  ENDIF
  
  retstr.detnames = STRTRIM(desc[1:*].name)
  retstr.det1name = STRTRIM(desc[1:*].det1)
  retstr.det2name = STRTRIM(desc[1:*].det2)
  retstr.detidx = SORT( retstr.detnames )
  retstr.freq = psds[0,*]
  retstr.frequnits = STRTRIM(desc[0].units,2)
  retstr.psds = psds[1:*,*]
  retstr.psd_units = STRTRIM(desc[1:*].units,2)
  retstr.files = STRTRIM(files.file,2)
  retstr.nfiles_per_psd = desc[1:*].nfiles
  wbad = WHERE(retstr.nfiles_per_psd EQ 0,nbad)
  IF nbad NE 0 THEN retstr.bad[wbad]=1b
  retstr.filemask = file_mask[1:*,*]

  success=1b
  RETURN,retstr

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION read_auto_powerspec,infile,primary_hdr,SUCCESS=success,$
   VERBOSE=verbose, ERRMSG=errmsg

  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN
  success = 0b
  errmsg = ''
  
  readstatus=0b
  psds = MRDFITS( infile, 1, /SILENT, psd_hdr, STATUS=readstatus )
  IF readstatus NE 0 THEN BEGIN
     errmsg = "Error reading PSD extension of "+infile
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF

  basic_mask = MRDFITS( infile, 2, /SILENT, STATUS=readstatus )
  IF readstatus NE 0 THEN BEGIN
     errmsg = "Error reading basic mask extension of "+infile
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF
  
  file_mask = MRDFITS( infile, 3, mask_hdr, /SILENT, STATUS=readstatus, $
                       /UNSIGNED )
  IF readstatus NE 0 THEN BEGIN
     errmsg = "Error reading file mask extension of "+infile
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF
  
  psdtags = TAG_NAMES(psds)

  ndet = N_ELEMENTS( psdtags ) - 1 ;;-1 for the FREQ entry
  nfreq = N_ELEMENTS( psds )
  nfiles = N_ELEMENTS(file_mask)

  IF ndet LE 0 THEN BEGIN
     errmsg = "No detectors found"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF
  IF nfreq LE 0 THEN BEGIN
     errmsg = "No frequency range found"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF
  IF nfiles LE 0 THEN BEGIN
     errmsg = "No files found"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF
  IF N_ELEMENTS( TAG_NAMES(basic_mask) ) NE 2*ndet THEN BEGIN
     errmsg = "Wrong number of structure tags in basic_mask"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF
  IF N_ELEMENTS( TAG_NAMES(file_mask) ) NE ndet+1 THEN BEGIN ;;+1 for FILE
     errmsg = "Wrong number of structure tags in basic_mask"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF

  mask_info = get_mask_bits( mask_hdr )

  ;;Begin actually constructing what we want to return
  retstr = { type: 'auto', ndet: ndet, nfreq: nfreq,$
             chunksize: 2*(nfreq-1),ndatapoints: -1L,$
             mindatalength: 0,windowing: 0b, windowtype: '',$
             overlapping: 0b, polyremove: 0b, polyorder: 0,$
             badmask: 'FFFFFFFF'XuL,$
             detnames: STRARR(ndet), detidx: INTARR(ndet),$
             freq: DBLARR(nfreq), frequnits: 'Hz',$
             psds: DBLARR(ndet,nfreq), $
             psd_units: STRARR(ndet), nfiles: nfiles,$
             ngoodfiles: 0, nbadfiles: 0, files: STRARR(nfiles),$
             goodfiles: STRARR(nfiles), badfiles: STRARR(nfiles),$
             nfiles_per_psd: INTARR(ndet), $
             bad: BYTARR(ndet), filemask: ULONARR(ndet,nfiles),$
             mask_info: mask_info }

  windowing = SXPAR( primary_hdr, 'WINDOW', COUNT=count )
  IF count NE 0 AND windowing THEN BEGIN
     retstr.windowing = 1b
     windfunc = SXPAR( primary_hdr, 'WINTYP', COUNT=count )
     IF count NE 1 THEN retstr.windowtype='Unknown' ELSE $
        retstr.windowtype=STRTRIM(windfunc,2)
     overlap = SXPAR( primary_hdr, 'OVERLAP', COUNT=count )
     IF count EQ 1 THEN retstr.overlapping = overlap
  ENDIF
  ndata = SXPAR( primary_hdr, 'NDATAPTS', COUNT=count )
  IF count EQ 1 THEN retstr.ndatapoints = ndata
  mindata = SXPAR( primary_hdr, 'MNDATLEN', COUNT=count )
  IF count EQ 1 THEN retstr.mindatalength = mindata
  polyremove = SXPAR( primary_hdr, 'POLYRMV', COUNT=count )
  IF count EQ 1 AND polyremove THEN BEGIN
     retstr.polyremove = 1b
     polyorder = SXPAR( primary_hdr, 'POLYORDR', COUNT=count )
     IF count EQ 1 THEN retstr.polyorder = polyorder
  ENDIF
  badmask = SXPAR( primary_hdr, 'BADMASK', COUNT=count )
  IF count EQ 1 THEN BEGIN
     ;;Need to convert from Hexidecimal to unsigned 32bit int
     value=0uL
     READS,badmask,value,FORMAT='(Z)'
     retstr.badmask = value
  ENDIF
  ngoodfiles = SXPAR( primary_hdr, 'NGOODFL', COUNT=count )
  IF count EQ 1 THEN retstr.ngoodfiles = ngoodfiles
  nbadfiles = SXPAR( primary_hdr, 'NBADFL', COUNT=count )
  IF count EQ 1 THEN retstr.nbadfiles = nbadfiles
  IF retstr.ngoodfiles NE 0 THEN BEGIN
     goodfiles = SXPAR( primary_hdr, 'GDFL*', COUNT=count )
     IF count NE 0 && count NE retstr.ngoodfiles THEN BEGIN
        ;;count=0 is a (rare) but acceptable outcome which occurs
        ;; if we input more than 9999 files
        errmsg = "Wrong number of goodfiles found in primary header "+infile
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDIF
     retstr.goodfiles[0:N_ELEMENTS(goodfiles)-1]=STRTRIM(goodfiles,2)
  ENDIF
  IF retstr.nbadfiles NE 0 THEN BEGIN
     badfiles = SXPAR( primary_hdr, 'BDFL*', COUNT=count )
     IF count NE 0 && count NE retstr.nbadfiles THEN BEGIN
        errmsg = "Wrong number of badfiles found in primary header "+infile
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     ENDIF
     retstr.badfiles[0:N_ELEMENTS(badfiles)-1]=STRTRIM(badfiles,2)
  ENDIF

  vals = SXPAR(psd_hdr,'TUNIT*',COUNT=count)
  IF count NE 0 AND count NE ndet+1 THEN BEGIN
     ;;+1 for the Freq
     errmsg = "Number of units tags doesn't match number of dets"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF
  retstr.psd_units = STRTRIM(vals[1:*],2)

  ;;Add PSDs
  retstr.detnames = STRTRIM(psdtags[1:*],2)
  retstr.freq = psds.freq
  FOR i=0, ndet-1 DO $
     retstr.psds[i,*] = psds.(i+1) ;;+1 for freq
  retstr.detidx = SORT( retstr.detnames ) ;;Lets us binary search to find dets


  ;;Add basic mask info
  FOR i=0, ndet-1 DO $
     retstr.bad[i] = basic_mask.(i)
  FOR i=0, ndet-1 DO $
     retstr.nfiles_per_psd[i] = basic_mask.(i+ndet)

  ;;Add full mask info
  retstr.files = STRTRIM(file_mask.file,2)
  w=WHERE( TAG_NAMES(file_mask) NE 'FILE',n)
  IF n NE ndet THEN BEGIN
     errmsg = "Missing some detector names in file mask"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF
  FOR i=0,ndet-1 DO $
     retstr.filemask[i,*] = file_mask.(w[i])
  
  success = 1b
  RETURN,retstr
 
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION read_powerspec, infile, SUCCESS=success, ERRMSG=errmsg,$
                         VERBOSE=verbose, TYPE=type

  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg = ''
  type = ''

  IF ~ FILE_TEST( infile ) THEN BEGIN
     errmsg = "File "+infile+" not found"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF
  IF ~ FILE_TEST( infile, /READ ) THEN BEGIN
     errmsg = "File "+infile+" not readable"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF

  ;;Get the primary header to determine the type
  primary_hdr = HEADFITS( infile, ERRMSG=errmsg, /SILENT )
  IF errmsg NE '' THEN BEGIN
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF

  desc = SXPAR(primary_hdr,'DESC',/SILENT)
  IF !ERR EQ -1 THEN BEGIN
     errmsg = "Unable to find DESC in primary_header; can't determine PSD type"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN,!VALUES.F_NAN
  ENDIF

  CASE STRUPCASE(STRTRIM(desc,2)) OF
     'CROSSPSD' : BEGIN
        type = 'cross'
        RETURN,read_cross_powerspec(infile,primary_hdr,SUCCESS=success,$
                                    VERBOSE=verbose, ERRMSG=errmsg )
     END
     'AUTOPSD' : BEGIN
        type = 'auto'
        RETURN,read_auto_powerspec(infile,primary_hdr,SUCCESS=success,$
                                   VERBOSE=verbose, ERRMSG=errmsg )
     END
     ELSE : BEGIN
        errmsg = "Unknown powerspec type: "+desc
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN,!VALUES.F_NAN
     END
  ENDCASE
END
