FUNCTION get_smap_header,image,XTENSION=xtension

;+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  function get_smap_header.pro
;;  May 8, 2009
;;  Mike Zemcov
;;  This program sets up a default header for the SMAP pipeline
;;  Inputs: [image] - a map array for which header is being created - 
;;                    can be dummy as only axial lengths are read from 
;;                    this parameter.
;;  Outputs: [header] - the header containing default values for
;;                      required parameters.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-

  ;; make sure we got an image to work with
  IF N_ELEMENTS(image) EQ 0 THEN BEGIN
     ;; if we didn't, bail
     MESSAGE,'No image data present to make header, aborting!'
  ENDIF

  ; make size of image array
  imsize = SIZE(image)

  IF NOT(KEYWORD_SET(xtension)) THEN BEGIN
     ; get very basic header structure
     MKHDR,header,image,/EXTEND
  ENDIF ELSE BEGIN
     ; make more extensive header
     MKHDR,header,image,IMAGE=xtension
  ENDELSE

  ; remove the stuff from the basic header we don't want
  origcomment = SXPAR(header,'COMMENT')
  SXDELPAR,header,'COMMENT'
  SXDELPAR,header,'DATE'

  ; add some space between fundamental header info and SMAP info
  SXADDPAR,header,'COMMENT','',/SAVECOMMENT
  SXADDPAR,header,'COMMENT',$
           ' This is a header for an SMAP pipeline product.',/SAVECOMMENT
  SXADDPAR,header,'COMMENT','',/SAVECOMMENT

  ; go through basic SMAP info
  SXADDPAR,header,'TIMESYS','UTC     ',' All dates are in UTC time'
  SXADDPAR,header,'CREATOR',getenv('USER'),' User who created this header'
  SXADDPAR,header,'DATE',SYSTIME(/UTC),' Creation data of this product'
  SXADDPAR,header,'TELESCOP','Herschel',' Name of the telescope'
  SXADDPAR,header,'INSTRUME','SPIRE',' Name of the instrument'
  SXADDPAR,header,'DESC','XXX map ',' Name of the array'
  SXADDPAR,header,'WAVELN',0,' Wavelength of the array'

  ; add some space before astrometery section
  SXADDPAR,header,'COMMENT','',BEFORE='EQUINOX'
  SXADDPAR,header,'COMMENT',$
           ' Astrometry section.',BEFORE='EQUINOX'
  SXADDPAR,header,'COMMENT','',BEFORE='EQUINOX'

  ; add in astrometry; some of this needs to be modified by user later
  SXADDPAR,header,'EQUINOX',2000.0,' WCS: Equinox of celestial pointing'
  SXADDPAR,header,'EPOCH',2000.0,' WCS: Epoch of celestial pointing'
  SXADDPAR,header,'CRVAL1',0.0,$
           ' [degrees] WCS: First coordinate of reference pixel.'
  SXADDPAR,header,'CRVAL2',0.0,$
           ' [degrees] WCS: Second coordinate of reference pixel.'
  SXADDPAR,header,'CD1_1',0.0,$
           ' [degrees] WCS: Pixel scale axis 1,1'
  SXADDPAR,header,'CD1_2',0.0,$
           ' [degrees] WCS: Pixel scale axis 1,2'
  SXADDPAR,header,'CD2_1',0.0,$
           ' [degrees] WCS: Pixel scale axis 2,1'
  SXADDPAR,header,'CD2_2',0.0,$
           ' [degrees] WCS: Pixel scale axis 2,2'
  SXADDPAR,header,'CTYPE1','RA---TAN',' WCS: Projection type axis 1, default'
  SXADDPAR,header,'CTYPE2','DEC--TAN',' WCS: Projection type axis 1, default'
  ; default the reference pixel to middle of array
  SXADDPAR,header,'CRPIX1',imsize[1] / 2,$
           ' [pixel] WCS: Reference pixel position axis 1.'
  SXADDPAR,header,'CRPIX2',imsize[2] / 2,$
           ' [pixel] WCS: Reference pixel position axis 2.'

  ; add SVN version number
  svnname = "smap_pipeline"
  svnverexec = "svnversion"

  ; parse !PATH to find "smap_pipeline":
  pathdirs = STRSPLIT(!PATH, ":", /EXTRACT)
  svnind = WHERE(STREGEX(pathdirs, svnname, /BOOL), nsvn)

  ; skip if not found
  IF nsvn EQ 0 THEN GOTO, NOSVNNUM

  ; grab first match, parse out path up to end of svnname
  svnpath = STREGEX(pathdirs[svnind[0]], "(.*"+svnname+")", /EXT)

  SPAWN, svnverexec+" "+svnpath, vernum, vernumerrmsg, EXIT_STAT=vernumerr
  IF vernumerr NE 0 THEN GOTO, NOSVNNUM

  ; add to header
  SXADDPAR, header, "SMAPSVN", vernum[0], " SMAP SVN version number"

  NOSVNNUM:

  ; return the variable
  RETURN,header

  ; and we're out
END


