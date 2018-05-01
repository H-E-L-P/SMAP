;+
;NAME
; choose_l1_chunksize
;PURPOSE
; To determine what chunksize to use when estimating
; the power spectra.  This tries to find the smallest
; size possible to give the desired minimum frequency,
; and makes sure all of the files can support it.  It
; also tries to get a power of 2 length if that can be
; easily accommidated
;USAGE
; chunksize = choose_l1_chunksize( files, minfreq )
;INPUTS
; files         List of files to examine in L1/HIPE format
; minfreq       Desired minimum frequency
;RETURNS
; An chunk size, -1 on failure.
;OPTIONAL OUTPUTS
; success       1 on success, 0 on failure
; errmsg        The reason for failure if unsuccessful
; minlength     The smallest file length
;KEYWORDS
; fullsize      Ignores minfreq and simply returns the longest
;                allowable length.
; nopow2        Don't try to round up to a power of 2
;NOTES
; This assumes all files have the same sampling timestep, so
; only explicitly reads the first one.  It also assumes the
; data in each file is monotonic in the time step, with no gaps.
;MODIFICATION HISTORY
; Author: Alex Conley, June 2009
;-

FUNCTION choose_l1_chunksize, files, minfreq, SUCCESS=success,$
                              ERRMSG=errmsg, FULLSIZE=fullsize,$
                              NOPOW2=nopow2, MINLENGTH=minlength
  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg = ''

  ;;Input checks
  nfiles = N_ELEMENTS(files)
  IF nfiles EQ 0 THEN BEGIN
     errmsg="Input file list is empty"
     RETURN,-1
  ENDIF
  IF SIZE(files,/TNAME) NE 'STRING' THEN BEGIN
     errmsg = "Input file list is not a string but a: "+$
              SIZE(files,/TNAME)
     RETURN,-1
  ENDIF
  IF ~ KEYWORD_SET( fullsize ) AND minfreq LE 0.0 THEN BEGIN
     errmsg=STRING(minfreq,FORMAT='("Invalid min freq (non-positive) ",F)')
     RETURN,-1
  ENDIF
  FOR i=0,nfiles-1 DO BEGIN
     IF ~ FILE_TEST( files[i] ) THEN $
        errmsg = "File "+files[i]+" doesn't exist"
     IF ~ FILE_TEST( files[i], /READ ) THEN $
        errmsg = "File "+files[i]+" exists but is non-readable"
     IF FILE_TEST(files[i], /DIRECTORY) THEN $
        errmsg = "File "+files[i]+" is a directory, not a file"
     IF STRLEN(errmsg) NE 0 THEN RETURN,-1
  ENDFOR

  ;;Find signal size of each file
  lengths = INTARR(nfiles)
  FOR i=0,nfiles-1 DO BEGIN
     FITS_OPEN,files[i],fcb,MESSAGE=errmsg,/NO_ABORT
     FITS_CLOSE,fcb
     IF STRLEN(errmsg) NE 0 THEN RETURN,-1
     
     ;;Find signal extension
     wsignal = WHERE( fcb.extname EQ 'signal', nsignal )
     IF nsignal EQ 0 THEN BEGIN
        errmsg = "No signal extension in "+files[i]
        RETURN,-1
     ENDIF
     IF nsignal GT 1 THEN BEGIN
        errmsg = "Too many signal extensions in "+files[i]
        RETURN,-1
     ENDIF

     ;;Get dimens
     IF fcb.naxis[wsignal] NE 2 THEN BEGIN
        errmsg = "Signal table is of unexpected dimensionality"
        RETURN,-1
     ENDIF
     lengths[i] = fcb.axis[1,wsignal[0]]
  ENDFOR

  minlength = MIN(lengths)
  IF minlength EQ 0 THEN BEGIN
     errmsg = "All files of zero length!"
     RETURN,-1
  ENDIF
  ;;Simple return
  IF KEYWORD_SET( fulllength ) THEN BEGIN
     success = 1b
     RETURN,minlength
  ENDIF
        
  ;;Figure out the timestep
  dat = MRDFITS( files[0], wsignal[0], /SILENT, STATUS=read_status )
  IF read_status LT 0 THEN BEGIN
     errmsg = "Error reading first file signal to get timestep"
     RETURN,-1
  ENDIF
  ndat = N_ELEMENTS(dat)
  IF ~ TAG_EXIST( dat, 'SAMPLETIME', /TOP_LEVEL ) THEN BEGIN
     errmsg = "First file "+files[0]+" doesn't have sampletime info"
     RETURN,-1
  ENDIF
  delta = dat[1:*].sampletime-dat[0:ndat-2].sampletime
  timestep = MEAN( delta, /NAN )
  IF STDEV(delta) GT timestep/50.0 THEN BEGIN
     errmsg = "Time sampling not sufficiently monotonic"
     RETURN,-1
  ENDIF

  reqlen = CEIL(1.0/(minfreq * timestep))
  IF reqlen GT minlength THEN BEGIN
     errmsg = "Data files don't contain enough points to allow for "+$
              "specified minimum frequency.  "
     errmsg += STRING(reqlen,minlength,FORMAT='("Needed ",I0," have ",I0)')
     RETURN,-1
  ENDIF
  IF KEYWORD_SET(verbose) THEN $
     MESSAGE,STRING(reqlen,$
                    FORMAT='("Minimum chunk size ",I0," required")'),/INF

  ;;Now try to round up to a power of 2 if easy
  IF ~ KEYWORD_SET(nopow2) THEN BEGIN
     targpow2 = 2^( CEIL(ALOG(reqlen)/ALOG(2.0)) )
     IF targpow2 LT minlength THEN BEGIN
        IF KEYWORD_SET(verbose) THEN $
           MESSAGE,STRING(targpow2,reqlen,FORMAT='("Rounding up to power'+$
                          ' of 2 length ",I0," from ",I0)'),/INF
        reqlen = targpow2
     ENDIF
  ENDIF

  success = 1b
  RETURN,reqlen
END
