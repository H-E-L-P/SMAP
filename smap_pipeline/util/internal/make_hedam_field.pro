;+
; MAKE_HEDAM_FIELD, datadir, prefix, fieldname, outdir, version, $
;                   tempmaskdir
;
; Create HEDAM-formatted fits files for all maps in a field
; directory. 
;
; INPUTS:
;   datadir:   location of image files
;   prefix:    common leading part of filename (eg
;              "goodsn_itermap_20111102")
;   fieldname: name of field for output filename (should match what
;              has been previously used in HEDAM)
;   outdir:    output directory for created files. will be created if
;              necessary.
;   version:   version tag for filename (eg "v1.0")
;
; CREATED BY: G. Marsden, 20110318
;
; HISTORY:
;  20110331 (GM) remove calls to STRIP_HEADER (keep all keywords)
;                change "exposure" to "coverage"
;  20110401 (GM) code to add HIPE headers (temporary)
;  20110718 (GM) crosslink masking (temporary)
;  20110909 (GM) add filter and effpsf extensions for filter maps
;  20120104 (GM) move location of "temporary" mask to input parameter
;-

;; Fix Equinox=Nan if present
PRO MHF_EQUINOX_FIX, head
  COMPILE_OPT IDL2, HIDDEN

  eqval = SXPAR(head, 'EQUINOX', COUNT=neq, COMMENT=com)
  IF neq EQ 0 THEN RETURN ;; Not in header
  IF ~ FINITE(eqval[0]) THEN $
     SXADDPAR, head, 'EQUINOX', 2000.0, com
END

;; return copy of inhead containing only keywords
FUNCTION MHF_STRIP_HEADER, inhead, keywords

  COMPILE_OPT IDL2, HIDDEN


  emptyval = ""
  emptycom = ""

  nkeywords = N_ELEMENTS(keywords)

  FOR i=0,nkeywords-1 DO BEGIN
     key = keywords[i]
     
     val = SXPAR(inhead, key, COMM=com, COUNT=c)

     IF c EQ 0 THEN BEGIN
        val = emptyval
        com = emptycom
     ENDIF

     SXADDPAR, outhead, key, val, com
  ENDFOR

;---------------------------------------------------------------------
; fix previously bad spelling in old version of pipeline
  val = SXPAR(outhead, "CRPIX1", COMM=c)
  SXADDPAR, outhead, "CRPIX1", val, $
            STRJOIN(STRSPLIT(c, 'pixle', /EXT, /REG), 'pixel')

  val = SXPAR(outhead, "CRPIX2", COMM=c)
  SXADDPAR, outhead, "CRPIX2", val, $
            STRJOIN(STRSPLIT(c, 'pixle', /EXT, /REG), 'pixel')
;---------------------------------------------------------------------

  RETURN, outhead

END


PRO MHF_MAKE_FILE, infile, fieldname, outfile, obsstart, obsend, meanmjd, $
                   tempmaskdir, CALFACT=calfact, CALDATE=caldate, OPTHDR=opthdr

  COMPILE_OPT IDL2, HIDDEN
; create individual HEDAM-style fits file

; keywords to copy from smap header
  keywords = ['BITPIX', $
              'NAXIS', $
              'NAXIS1', $
              'NAXIS2', $
              'TIMESYS', $
              'CREATOR', $
              'DATE', $
              'TELESCOP', $
              'INSTRUME', $
              'WAVELN', $
              'EXTNAME', $
              'EQUINOX', $
              'EPOCH', $
              'CRVAL1', $
              'CRVAL2', $
              'CD1_1', $
              'CD1_2', $
              'CD2_1', $
              'CD2_2', $
              'CTYPE1', $
              'CTYPE2', $
              'CRPIX1', $
              'CRPIX2', $
              'LONPOLE', $
              'LATPOLE']

  RDFITS_STRUCT, infile, mapstr, /SILENT
  
; convert wavelen to P?W
  wavelen = SXPAR(mapstr.hdr1, 'WAVELN')
  CASE wavelen OF 
     250: bandname = 'PSW'
     350: bandname = 'PMW'
     500: bandname = 'PLW'
  ENDCASE


;------------------------------------------------------------------------
; SIGNAL EXTENSION
;------------------------------------------------------------------------

  signalmap = mapstr.im1
;signalhdr = MHF_STRIP_HEADER(mapstr.hdr1, keywords)
  signalhdr = mapstr.hdr1
  MHF_EQUINOX_FIX, signalhdr

  nhdr = N_ELEMENTS(opthdr)
  IF nhdr NE 0 THEN BEGIN
     FOR i=0,nhdr-1 DO BEGIN
        ;;Figure out what keyword this is
        ;;Only support standard FITS keywords
        kywrd = STRTRIM(STRMID(opthdr[i],0,8),2)
        IF kywrd EQ 'END' OR kywrd EQ '' THEN CONTINUE
        val = SXPAR( opthdr, STRTRIM(kywrd,2), COM=com )
        IF !ERR GE 0 THEN $
           SXADDPAR,signalhdr,kywrd,val,com
     ENDFOR
  ENDIF

  SXADDPAR, signalhdr, "FIELD", fieldname, " Name of field observed", $
            AFTER="WAVELN"
  SXADDPAR, signalhdr, "RADESYS", "ICRS", $
            " Coordinate reference frame for the RA and DEC", $
            AFTER="FIELD"
  SXADDPAR, signalhdr, "DATE-OBS", obsstart, " Start Date/Time of observation"
  SXADDPAR, signalhdr, "DATE-END", obsend, " End Date/Time of observation"
  SXADDPAR, signalhdr, "MEAN-MJD", meanmjd, " Weighted mean MJD of observation"
  SXADDPAR, signalhdr, "CREATOR", "SMAP", /SAVE

  IF KEYWORD_SET(calfact) THEN BEGIN
     signalmap *= calfact
     caldatestr = ""
     IF KEYWORD_SET(caldate) THEN caldatestr = "("+caldate+") "
     calfactstr = " ICC Cal factor" + caldatestr + " applied to maps"
     SXADDPAR, signalhdr, "CALFACT", calfact, calfactstr
  ENDIF

  mainhdr = signalhdr
  delexten = ['EXTNAME', 'XTENSION', 'NAXIS1', 'NAXIS2', 'EQUINOX', 'EPOCH', $
              'CRVAL1', 'CRVAL2', 'CD1_1', 'CD1_2', 'CD2_1', 'CD2_2', 'CTYPE1', $
              'CTYPE2', 'CRPIX1', 'CRPIX2', 'LONPOLE', 'LATPOLE', 'PV2_1']
  FOR hi=0,N_ELEMENTS(delexten)-1 DO SXDELPAR, mainhdr, delexten[hi]

; HIPE stuff
  SXADDPAR, mainhdr, 'HCSS____', 5, " HCSS Fits Product Version"
  SXADDPAR, mainhdr, 'CLASS___', 'herschel.ia.dataset.image.SimpleImage', $
            ' java representation'
  SXADDPAR, mainhdr, 'INFO____', bandname+' map '
  SXADDPAR, mainhdr, 'DSETS___', 4, ' Number of datasets'
  SXADDPAR, mainhdr, 'DS_0', 1, ' HDU of Child Dataset'
  SXADDPAR, mainhdr, 'DS_1', 2, ' HDU of Child Dataset'
  SXADDPAR, mainhdr, 'DS_2', 3, ' HDU of Child Dataset'
  SXADDPAR, mainhdr, 'DS_3', 4, ' HDU of Child Dataset'


  delexten = ['PV2_1']
  FOR hi=0,N_ELEMENTS(delexten)-1 DO SXDELPAR, signalhdr, delexten[hi]
  SXADDPAR, signalhdr, "BUNIT", "Jy / beam"

; HIPE stuff
  SXADDPAR, signalhdr, 'CLASS___', 'herschel.ia.dataset.ArrayDataset', $
            ' java representation'
  SXADDPAR, signalhdr, 'INFO____', 'Image   ' 
  SXADDPAR, signalhdr, 'DATA____', 'herschel.ia.numeric.Double2d', ' java Data'
  SXADDPAR, signalhdr, 'QTTY____', 'Jy/beam ', ' Unit of the data'

;------------------------------------------------------------------------
; ERROR EXTENSION
;------------------------------------------------------------------------

  errormap  = mapstr.im2
;errorhdr  = MHF_STRIP_HEADER(mapstr.hdr3, keywords)
  errorhdr  = mapstr.hdr2
  MHF_EQUINOX_FIX, errorhdr

  IF nhdr NE 0 THEN BEGIN
     FOR i=0,nhdr-1 DO BEGIN
        ;;Figure out what keyword this is
        ;;Only support standard FITS keywords
        kywrd = STRTRIM(STRMID(opthdr[i],0,8),2)
        IF kywrd EQ 'END' OR kywrd EQ '' THEN CONTINUE
        val = SXPAR( opthdr, STRTRIM(kywrd,2), COM=com )
        IF !ERR GE 0 THEN $
           SXADDPAR,errorhdr,kywrd,val,com
     ENDFOR
  ENDIF

  delexten = ['PV2_1']
  FOR hi=0,N_ELEMENTS(delexten)-1 DO SXDELPAR, errorhdr, delexten[hi]
  SXADDPAR, errorhdr, "FIELD", fieldname, " Name of field observed", $
            AFTER="WAVELN"
  SXADDPAR, errorhdr, "RADESYS", "ICRS", $
            " Coordinate reference frame for the RA and DEC", $
            AFTER="FIELD"
  SXADDPAR, errorhdr, "DATE-OBS", obsstart, " Start Date/Time of observation"
  SXADDPAR, errorhdr, "DATE-END", obsend, " End Date/Time of observation"
  SXADDPAR, errorhdr, "MEAN-MJD", meanmjd, " Weighted mean MJD of observation"
  SXADDPAR, errorhdr, "CREATOR", "SMAP", /SAVE

  IF KEYWORD_SET(calfact) THEN BEGIN
     errormap *= calfact
     SXADDPAR, errorhdr, "CALFACT", calfact, calfactstr
  ENDIF

  SXADDPAR, errorhdr, "BUNIT", "Jy / beam"

; HIPE stuff
  SXADDPAR, errorhdr, 'CLASS___', 'herschel.ia.dataset.ArrayDataset', $
            ' java representation'
  SXADDPAR, errorhdr, 'INFO____', 'Error map' 
  SXADDPAR, errorhdr, 'DATA____', 'herschel.ia.numeric.Double2d', ' java Data'
  SXADDPAR, errorhdr, 'QTTY____', 'Jy/beam ', ' Unit of the data'

;------------------------------------------------------------------------
; EXPOSURE EXTENSION
;------------------------------------------------------------------------

  expmap   = DOUBLE(mapstr.im3)
  exphdr   = mapstr.hdr3
  MHF_EQUINOX_FIX, exphdr

  IF nhdr NE 0 THEN BEGIN
     FOR i=0,nhdr-1 DO BEGIN
        ;;Figure out what keyword this is
        ;;Only support standard FITS keywords
        kywrd = STRTRIM(STRMID(opthdr[i],0,8),2)
        IF kywrd EQ 'END' OR kywrd EQ '' THEN CONTINUE
        val = SXPAR( opthdr, STRTRIM(kywrd,2), COM=com )
        IF !ERR GE 0 THEN $
           SXADDPAR,exphdr,kywrd,val,com
     ENDFOR
  ENDIF

  delexten = ['PV2_1']
  FOR hi=0,N_ELEMENTS(delexten)-1 DO SXDELPAR, exphdr, delexten[hi]
  SXADDPAR, exphdr, "BITPIX", 64
  SXDELPAR, exphdr, "BSCALE"
  SXDELPAR, exphdr, "BZERO"
  SXADDPAR, exphdr, "EXTNAME", "exposure", /SAVE
  SXADDPAR, exphdr, "FIELD", fieldname, " Name of field observed", $
            AFTER="WAVELN"
  SXADDPAR, exphdr, "RADESYS", "ICRS", $
            " Coordinate reference frame for the RA and DEC", $
            AFTER="FIELD"
  SXADDPAR, exphdr, "DATE-OBS", obsstart, " Start Date/Time of observation"
  SXADDPAR, exphdr, "DATE-END", obsend, " End Date/Time of observation"
  SXADDPAR, exphdr, "MEAN-MJD", meanmjd, " Weighted mean MJD of observation"
  SXADDPAR, exphdr, "CREATOR", "SMAP", /SAVE

; HIPE stuff
  SXADDPAR, exphdr, 'CLASS___', 'herschel.ia.dataset.ArrayDataset', $
            ' java representation'
  SXADDPAR, exphdr, 'INFO____', 'Coverage'
  SXADDPAR, exphdr, 'DATA____', 'herschel.ia.numeric.Double2d', ' java Data'
  SXADDPAR, exphdr, 'QTTY____', '1', ' Unit of the data'

  maskmap   = LONG(mapstr.im4)

;------------------------------------------------------------------------
; MASK EXTENSION
;------------------------------------------------------------------------

;maskhdr   = MHF_STRIP_HEADER(mapstr.hdr4, keywords)
  maskhdr   = mapstr.hdr4
  MHF_EQUINOX_FIX, maskhdr
  SXADDPAR, maskhdr, 'BITPIX', 32
  SXDELPAR, maskhdr, 'BSCALE'
  SXDELPAR, maskhdr, 'BZERO'

  IF nhdr NE 0 THEN BEGIN
     FOR i=0,nhdr-1 DO BEGIN
        ;;Figure out what keyword this is
        ;;Only support standard FITS keywords
        kywrd = STRTRIM(STRMID(opthdr[i],0,8),2)
        IF kywrd EQ 'END' OR kywrd EQ '' THEN CONTINUE
        val = SXPAR( opthdr, STRTRIM(kywrd,2), COM=com )
        IF !ERR GE 0 THEN $
           SXADDPAR,maskhdr,kywrd,val,com
     ENDFOR
  ENDIF

  delexten = ['PV2_1']
  FOR hi=0,N_ELEMENTS(delexten)-1 DO SXDELPAR, maskhdr, delexten[hi]

  SXADDPAR, maskhdr, "EXTNAME", "flag", /SAVE
  SXADDPAR, maskhdr, "FIELD", fieldname, " Name of field observed", $
            AFTER="WAVELN"
  SXADDPAR, maskhdr, "RADESYS", "ICRS", $
            " Coordinate reference frame for the RA and DEC", $
            AFTER="FIELD"
  SXADDPAR, maskhdr, "DATE-OBS", obsstart, " Start Date/Time of observation"
  SXADDPAR, maskhdr, "DATE-END", obsend, " End Date/Time of observation"
  SXADDPAR, maskhdr, "MEAN-MJD", meanmjd, " Weighted mean MJD of observation"
  SXADDPAR, maskhdr, "CREATOR", "SMAP", /SAVE

; HIPE stuff
  SXADDPAR, maskhdr, 'CLASS___', 'herschel.ia.dataset.ArrayDataset', $
            ' java representation'
  SXADDPAR, maskhdr, 'INFO____', 'Flag map'
  SXADDPAR, maskhdr, 'DATA____', 'herschel.ia.numeric.Int2d', ' java Data'
  SXADDPAR, maskhdr, 'QTTY____', '1', ' Unit of the data'

;---------------------------------------------
; temporary fix to mask (it's currently empty)
  iii = WHERE(expmap EQ 0, niii)
  IF niii GT 0 THEN maskmap[iii] = maskmap[iii] OR 1

; get fname
  temp = FILE_BASENAME(infile, '.fits')
  words = STRSPLIT(temp, '_', /EXTRACT)
  fname = words[0]

; add values to header
  SXADDPAR, maskhdr, "MASK0", "No data"

  IF KEYWORD_SET(tempmaskdir) THEN BEGIN
      tempmaskdir = ADDSLASH(tempmaskdir)

      tempmaskname = fname + "_" + bandname + "_mask.fits"
      IF NOT FILE_TEST(tempmaskdir + tempmaskname) THEN $
         MESSAGE, "Temporary mask file '" + tempmaskname + "' not found."
      tempmask = READFITS(tempmaskdir + tempmaskname, /SILENT)

      maskmap = maskmap OR tempmask

      ; extra mask vals for header
      SXADDPAR, maskhdr, "MASK1", "Data not cross-linked"
      SXADDPAR, maskhdr, "MASK2", $
                "Large noise region (for filtered maps only)"
  ENDIF ELSE $
     MESSAGE, "TEMPMASKDIR not set... not using extended mask", /INF

;---------------------------------------------


;------------------------------------------------------------------------
; FILTER EXTENSION (on filter maps only)
;------------------------------------------------------------------------

IF TAG_EXIST(mapstr, 'im5', /TOP) THEN BEGIN
  filtermap  = mapstr.im5
;filterhdr  = MHF_STRIP_HEADER(mapstr.hdr5, keywords)
  filterhdr  = mapstr.hdr5
  MHF_EQUINOX_FIX, filterhdr

  IF nhdr NE 0 THEN BEGIN
     FOR i=0,nhdr-1 DO BEGIN
        ;;Figure out what keyword this is
        ;;Only support standard FITS keywords
        kywrd = STRTRIM(STRMID(opthdr[i],0,8),2)
        IF kywrd EQ 'END' OR kywrd EQ '' THEN CONTINUE
        val = SXPAR( opthdr, STRTRIM(kywrd,2), COM=com )
        IF !ERR GE 0 THEN $
           SXADDPAR,filterhdr,kywrd,val,com
     ENDFOR
  ENDIF

  delexten = ['PV2_1']
  FOR hi=0,N_ELEMENTS(delexten)-1 DO SXDELPAR, filterhdr, delexten[hi]
  SXADDPAR, filterhdr, "FIELD", fieldname, " Name of field observed", $
            AFTER="WAVELN"
  SXADDPAR, filterhdr, "RADESYS", "ICRS", $
            " Coordinate reference frame for the RA and DEC", $
            AFTER="FIELD"
  SXADDPAR, filterhdr, "DATE-OBS", obsstart, " Start Date/Time of observation"
  SXADDPAR, filterhdr, "DATE-END", obsend, " End Date/Time of observation"
  SXADDPAR, filterhdr, "MEAN-MJD", meanmjd, " Weighted mean MJD of observation"
  SXADDPAR, filterhdr, "CREATOR", "SMAP", /SAVE

  SXADDPAR, filterhdr, "BUNIT", '1'

; HIPE stuff
  SXADDPAR, filterhdr, 'CLASS___', 'herschel.ia.dataset.ArrayDataset', $
            ' java representation'
  SXADDPAR, filterhdr, 'INFO____', 'Filter map' 
  SXADDPAR, filterhdr, 'DATA____', 'herschel.ia.numeric.Double2d', ' java Data'
  SXADDPAR, filterhdr, 'QTTY____', '1', ' Unit of the data'
ENDIF

;------------------------------------------------------------------------
; EFFPSF EXTENSION (on filter maps only)
;------------------------------------------------------------------------


IF TAG_EXIST(mapstr, 'im6', /TOP) THEN BEGIN
  effpsfmap  = mapstr.im6
;effpsfhdr  = MHF_STRIP_HEADER(mapstr.hdr6, keywords)
  effpsfhdr  = mapstr.hdr6
  MHF_EQUINOX_FIX, effpsfhdr

  IF nhdr NE 0 THEN BEGIN
     FOR i=0,nhdr-1 DO BEGIN
        ;;Figure out what keyword this is
        ;;Only support standard FITS keywords
        kywrd = STRTRIM(STRMID(opthdr[i],0,8),2)
        IF kywrd EQ 'END' OR kywrd EQ '' THEN CONTINUE
        val = SXPAR( opthdr, STRTRIM(kywrd,2), COM=com )
        IF !ERR GE 0 THEN $
           SXADDPAR,effpsfhdr,kywrd,val,com
     ENDFOR
  ENDIF

  delexten = ['PV2_1']
  FOR hi=0,N_ELEMENTS(delexten)-1 DO SXDELPAR, effpsfhdr, delexten[hi]
  SXADDPAR, effpsfhdr, "FIELD", fieldname, " Name of field observed", $
            AFTER="WAVELN"
  SXADDPAR, effpsfhdr, "RADESYS", "ICRS", $
            " Coordinate reference frame for the RA and DEC", $
            AFTER="FIELD"
  SXADDPAR, effpsfhdr, "DATE-OBS", obsstart, " Start Date/Time of observation"
  SXADDPAR, effpsfhdr, "DATE-END", obsend, " End Date/Time of observation"
  SXADDPAR, effpsfhdr, "MEAN-MJD", meanmjd, " Weighted mean MJD of observation"
  SXADDPAR, effpsfhdr, "CREATOR", "SMAP", /SAVE

  SXADDPAR, effpsfhdr, "BUNIT", '1'

; HIPE stuff
  SXADDPAR, effpsfhdr, 'CLASS___', 'herschel.ia.dataset.ArrayDataset', $
            ' java representation'
  SXADDPAR, effpsfhdr, 'INFO____', 'Effpsf map' 
  SXADDPAR, effpsfhdr, 'DATA____', 'herschel.ia.numeric.Double2d', ' java Data'
  SXADDPAR, effpsfhdr, 'QTTY____', '1', ' Unit of the data'
ENDIF

;------------------------------------------------------------------------
; END EXTENSION PROCESSING
;------------------------------------------------------------------------



  IF KEYWORD_SET(shifts) THEN BEGIN
     SHIFT_ASTROM, mainhdr,   shifts[0], shifts[1], /ADDKEYS
     SHIFT_ASTROM, signalhdr, shifts[0], shifts[1], /ADDKEYS
     SHIFT_ASTROM, errorhdr,  shifts[0], shifts[1], /ADDKEYS
     SHIFT_ASTROM, exphdr,    shifts[0], shifts[1], /ADDKEYS
     SHIFT_ASTROM, maskhdr,   shifts[0], shifts[1], /ADDKEYS
     IF TAG_EXIST(mapstr, 'im5', /TOP) THEN $
        SHIFT_ASTROM, filterhdr,   shifts[0], shifts[1], /ADDKEYS
     IF TAG_EXIST(mapstr, 'im6', /TOP) THEN $
        SHIFT_ASTROM, effpsfhdr,   shifts[0], shifts[1], /ADDKEYS
  ENDIF

  MWRFITS, dummy, outfile, mainhdr, /CREATE, /NO_COMMENT, /SILENT
  MWRFITS, signalmap, outfile, signalhdr, /NO_COMMENT, /SILENT
  MWRFITS, errormap, outfile, errorhdr, /NO_COMMENT, /SILENT
  MWRFITS, expmap, outfile, exphdr, /NO_COMMENT, /SILENT
  MWRFITS, maskmap, outfile, maskhdr, /NO_COMMENT, /SILENT
  IF TAG_EXIST(mapstr, 'im5', /TOP) THEN $
     MWRFITS, filtermap, outfile, filterhdr, /NO_COMMENT, /SILENT
  IF TAG_EXIST(mapstr, 'im6', /TOP) THEN $
     MWRFITS, effpsfmap, outfile, effpsfhdr, /NO_COMMENT, /SILENT


END


PRO MAKE_HEDAM_FIELD, datadir, prefix, fieldname, outdir, version, $
                      tempmaskdir


  COMPILE_OPT IDL2
; process all images in a field directory (all bands and all image
; types) 

  filesuffix = ".fits"

; tag used to search for obstime file
  obstimetag = "obstime"

; translation table for band name
  bandtrans = [['PSW', '250'], $
               ['PMW', '350'], $
               ['PLW', '500']]

  smaptag = "SMAP"

; find map files
  infiles = FILE_SEARCH(ADDSLASH(datadir)+prefix+"*"+filesuffix, $
                        COUNT=nfiles)

; strip off directory and suffix
  infiles_base = FILE_BASENAME(infiles, filesuffix)

; read obstime info (for header)
  obsfile = FILE_SEARCH(datadir, "*"+obstimetag+"*", COUNT=nobs)
  IF nobs NE 1 THEN $
     MESSAGE, "Found wrong number (" + STRTRIM(STRING(nobs),2) + $
              ") of 'obstime' files"

  READCOL, obsfile[0], FORMAT="(A)", times, /SILENT

  obsstart = times[0]
  obsend   = times[1]
  meanmjd  = DOUBLE(times[2])

; ouput directory
  IF FILE_TEST(outdir, /DIR) EQ 0 THEN FILE_MKDIR, outdir

; grab HIPE headers from level1 files
; this will be fixed in later versions of the maps, so is temporary
  level1dir = '/data/spiredaq/reprocessed/'

; strip off '-nest' or '-filter'
  stripsuff = ['-Nest', '-Filter', '-ACT'] ; add 'ACT' for HELMS-ACT
  fnametest = fieldname
  FOR i=0,N_ELEMENTS(stripsuff)-1 DO BEGIN
     IF STRMATCH(fnametest, "*"+stripsuff[i]) THEN BEGIN 
        fnametest = STRMID(fnametest, 0, STRPOS(fnametest, stripsuff[i], /REVERSE_SEARCH))
        BREAK
     ENDIF
  ENDFOR

  CASE fnametest OF
     "Abell-370":  level1='A370_ntL1g/1342201311/hspire1342201311obs.fits'
     "Abell-1689": level1='A1689_new_ntL1g/1342201246/hspire1342201246obs.fits'
     "Abell-1835": level1='A1835_ntL1g/1342201127/hspire1342201127obs.fits'
     "Abell-2218": level1='Abell2218_ntL1g/1342183679/hspire1342183679obs.fits'
     "Abell-2219": level1='Abell2219_ntL1g/1342193018/hspire1342193018obs.fits'
     "Abell-2390": level1='Abell2390_ntL1g/1342195731/hspire1342195731obs.fits'
     "ADFS": level1='ADFS_ntL1g/1342188096/hspire1342188096obs.fits'
     "BOOTES-HerMES": level1='BooSp_ntL1g/1342187711/hspire1342187711obs.fits'
     "CDFS-SWIRE": level1='CDFS-SWIRE_ntL1g/1342201371/hspire1342201371obs.fits'
     "CDFS-SWIRE3": level1='CDFS-SWIRE3_ntL1g/1342224029/hspire1342224029obs.fits.gz'
     "Cl0024+16": level1='Cl0024+16_ntL1g/1342201208/hspire1342201208obs.fits'
     "COSMOS": level1='COSMOS_ntL1g/1342195856/hspire1342195856obs.fits'
     "COSMOS2": level1='COSMOS2_ntL1g/1342222819/hspire1342222819obs.fits.gz'
     "ECDFS": level1='ECDFS_ntL1g/1342191158/hspire1342191158obs.fits'
     "Groth-Strip": level1='EGroth_ntL1g/1342190294/hspire1342190294obs.fits'
     "EGS-HerMES": level1='EGS-SCUBA_ntL1g/1342190310/hspire1342190310obs.fits'
     "ELAIS-N1-HerMES": level1='ELAIS-N1-SCUBA_ntL1g/1342187646/hspire1342187646obs.fits'
     "ELAIS-N1-NEW": level1='en1_new_ivan_ntL1g/1342228354/hspire1342228354obs.fits.gz'
     "ELAIS-N2-HerMES": level1='ELAIS-N2-SWIRE_ntL1g/1342214712/hspire1342214712obs.fits'
     "ELAIS-S1-SWIRE": level1='ELAIS-S1-NewSWIRE_ntL1g/1342195729/hspire1342195729obs.fits'
     "ELAIS-S1-VIDEO": level1='S1_video-1_ntL1g/1342196656/hspire1342196656obs.fits.gz'
     "FLS": level1='FLS_ntL1g/1342186123/hspire1342186123obs.fits'
     "GLOBAL-EPICENTRE1": level1='Global_Epicentre-1_ntL1g/1342222120/hspire1342222120obs.fits.gz'
     "GOODS-N": level1='GOODS-N_ntL1g/1342185536/hspire1342185536obs.fits.gz'
     "GOODS-N2": level1='GOODS-N2_ntL1g/1342195274/hspire1342195274obs.fits.gz'
     "GOODS-S": level1='GOODS-S_ntL1g/1342201265/hspire1342201265obs.fits'
     "HELMS": level1='HELMS_ntL1g/1342234749/hspire1342234749obs.fits.gz'
     "Lockman-East-ROSAT": level1='Lockman-East_ntL1g/1342195947/hspire1342195947obs.fits'
     "Lockman-North": level1='LockmanNorth_ntL1g/1342186110/hspire1342186110obs.fits'
     "Lockman-SWIRE": level1='LockmanSw_ntL1g/1342186108/hspire1342186108obs.fits'
     "Lockman-SWIRE2": level1='LockmanSw2_ntL1g/1342222591/hspire1342222591obs.fits.gz'
     "Lockman-SWIRE3": level1='Lockman-SWIRE3_ntL1g/1342222588/hspire1342222588obs.fits.gz'
     "MS0451.6-0305": level1='MS0451.6-0305_ntL1g/1342192999/hspire1342192999obs.fits'
     "MS1054.4-0321": level1='MS1054.4-0321_ntL1g/1342198886/hspire1342198886obs.fits'
     "MS1358+62": level1='MS1358+62_ntL1g/1342195971/hspire1342195971obs.fits'
     "RXJ0152.7-1357": level1='RXJ0152.7-1357_ntL1g/1342201303/hspire1342201303obs.fits'
     "RXJ13475-1145": level1='RXJ13475-1145_ntL1g/1342201256/hspire1342201256obs.fits'
     "UDS": level1='UDS_ntL1g/1342201437/hspire1342201437obs.fits'
     "VIDEO-XMM1": level1='video-xmm1_ntL1g/1342223217/hspire1342223217obs.fits.gz'
     "VIDEO-XMM2": level1='video-xmm2_ntL1g/1342223219/hspire1342223219obs.fits.gz'
     "VIDEO-XMM3": level1='video-xmm3_ntL1g/1342223213/hspire1342223213obs.fits.gz'
     "VVDS": level1='VVDS_ntL1g/1342201438/hspire1342201438obs.fits'
     "XMM-LSS-SWIRE": level1='XMM-LSS_ntL1g/1342189003/hspire1342189003obs.fits'
  ENDCASE

  IF FILE_TEST(level1dir+level1, /READ) EQ 0 THEN $
     MESSAGE, "Could not read level1 file for field '"+fieldname+"'"

  level1head = HEADFITS(level1dir+level1)
  val = SXPAR(level1head,'RA_NOM',COMMENT=com,COUNT=nky)
  IF nky NE 0 THEN SXADDPAR,opthdr,'RA_NOM',val,com
  val = SXPAR(level1head,'DEC_NOM',COMMENT=com,COUNT=nky)
  IF nky NE 0 THEN SXADDPAR,opthdr,'DEC_NOM',val,com
  val = SXPAR(level1head,'POSANGLE',COMMENT=com,COUNT=nky)
  IF nky NE 0 THEN SXADDPAR,opthdr,'POSANGLE',val,com
  val = SXPAR(level1head,'HCSS____',COMMENT=com,COUNT=nky)
  IF nky NE 0 THEN SXADDPAR,opthdr,'HCSS____',val,com
  val = SXPAR(level1head,'CUSMODE',COMMENT=com,COUNT=nky)
  IF nky NE 0 THEN SXADDPAR,opthdr,'CUSMODE',val,com
  val = SXPAR(level1head,'AOR',COMMENT=com,COUNT=nky)
  IF nky NE 0 THEN SXADDPAR,opthdr,'AOR',val,com
  val = SXPAR(level1head,'INSTMODE',COMMENT=com,COUNT=nky)
  IF nky NE 0 THEN SXADDPAR,opthdr,'INSTMODE',val,com
  val = SXPAR(level1head,'OBJECT',COMMENT=com,COUNT=nky)
  IF nky NE 0 THEN SXADDPAR,opthdr,'OBJECT',val,com
  val = SXPAR(level1head,'OBSERVER',COMMENT=com,COUNT=nky)
  IF nky NE 0 THEN SXADDPAR,opthdr,'OBSERVER',val,com
  val = SXPAR(level1head,'OBS_MODE',COMMENT=com,COUNT=nky)
  IF nky NE 0 THEN SXADDPAR,opthdr,'OBS_MODE',val,com
  val = SXPAR(level1head,'POINTMOD',COMMENT=com,COUNT=nky)
  IF nky NE 0 THEN SXADDPAR,opthdr,'POINTMOD',val,com
  val = SXPAR(level1head,'PROPOSAL',COMMENT=com,COUNT=nky)
  IF nky NE 0 THEN SXADDPAR,opthdr,'PROPOSAL',val,com
  val = SXPAR(level1head,'SUBSYS',COMMENT=com,COUNT=nky)
  IF nky NE 0 THEN SXADDPAR,opthdr,'SUBSYS',val,com

; loop over files
  FOR ifile=0,nfiles-1 DO BEGIN

                                ; extract tag and band 
     words = STREGEX(infiles[ifile], prefix+'_(.*_?)(P.W)', $
                     /EXTRACT, /SUBEXPR)

     intag = words[1]
     inband = words[2]

     IF intag EQ "" THEN intag = "image_"

                                ; translate band
     bandind = (WHERE(inband EQ bandtrans[0,*]))[0]
     outband = bandtrans[1,bandind]

                                ; output file name
     outfile = fieldname + "_" + intag + outband + "_" + $
               smaptag + "_" + version + filesuffix
     MHF_MAKE_FILE, infiles[ifile], fieldname, ADDSLASH(outdir)+outfile, $
                    obsstart, obsend, meanmjd, tempmaskdir, OPTHDR=opthdr

  ENDFOR


END

