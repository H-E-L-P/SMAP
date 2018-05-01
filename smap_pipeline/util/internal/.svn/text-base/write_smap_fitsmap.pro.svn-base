;+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  function write_smap_fitsmap.pro
;;  May 12, 2009
;;  Mike Zemcov
;;  This function takes an smap structure and writes the relevant bits
;;  to file.
;;  Options:
;;     /NO_ABORT   On error, return
;;     errmsg      Error message if problem encountered
;;     dir         Write to this directory instead of !SMAP_MAPS
;;     opthdr      Additional info to add to header, in the form
;;                  of a string array fits header.  Note that
;;                  everything in this header will be added, so you
;;                  don't want it to have stuff like NAXIS.
;;                  Only standard length FITS keywords are supported
;;     silent      Don't print informational messages
;;  For example:
;;   st = write_smap_fitsmap(mapstruct,'sample_map')
;;  will, if map_struct is a PMW map, write a file 
;;   !SMAP_MAPS + sample_map_PMW.fits
;;  returning 1 if it succeeds, 0 if it doesn't.
;;
;; CHANGELOG:
;;  2011-09-06/gm add filter extensions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-
FUNCTION WRITE_SMAP_FITSMAP,mapstruct,fileroot,SILENT=silent,$
                            DIR=dir, NO_ABORT=no_abort, $
                            ERRMSG=errmsg, OPTHDR=opthdr

  COMPILE_OPT IDL2
  errmsg = ''

  silent = KEYWORD_SET(silent)
  abort  = ~ KEYWORD_SET(no_abort)

  ; check to see what the state of passing parameters is - if they're
  ; not defined, bail
  IF (N_ELEMENTS(mapstruct) EQ 0) THEN BEGIN
     errmsg = 'No map structure passed to me, aborting!'
     IF abort THEN MESSAGE,errmsg ELSE BEGIN
        IF ~ silent THEN MESSAGE,errmsg,/INF
        RETURN,0
     ENDELSE
  ENDIF
  IF SIZE(mapstruct,/TNAME) NE 'STRUCT' THEN BEGIN
     errmsg = 'Map structure is not a structure, aborting!'
     IF abort THEN MESSAGE,errmsg ELSE BEGIN
        IF ~ silent THEN MESSAGE,errmsg,/INF
        RETURN,0
     ENDELSE
  ENDIF
  IF (N_ELEMENTS(fileroot) EQ 0) THEN BEGIN
     errmsg = 'No root file name passed to me, aborting!'
     IF abort THEN MESSAGE,errmsg ELSE BEGIN
        IF ~ silent THEN MESSAGE,errmsg,/INF
        RETURN,0
     ENDELSE
  ENDIF

  IF N_ELEMENTS(dir) EQ 0 THEN dir = addslash(!SMAP_MAPS) ELSE $
     dir = addslash(dir)
  IF ~ FILE_TEST(dir,/DIRECTORY,/WRITE) THEN BEGIN
     errmsg = "Unable to write to output directory: "+dir
     IF abort THEN MESSAGE,errmsg ELSE BEGIN
        IF ~ silent THEN MESSAGE,errmsg,/INF
        RETURN,0
     ENDELSE
  ENDIF

  ; tell me what you're up to
  IF ~ silent THEN MESSAGE,'Writing FITS files.',/INFORMATIONAL

  ; make the file name from known info
  filename = dir + fileroot + '_' + mapstruct.names + '.fits'

  IF FILE_TEST(filename) AND ~ silent THEN $
     MESSAGE,"Will overwrite "+filename,/INF

  ; make a dummy primary fits HDU
  MWRFITS,dummy,filename,/CREATE,/SILENT

  ; get the header and append some known stuff to it
  header = get_smap_header(mapstruct.image,XTENSION='IMAGE')
  SXADDPAR,header,'DESC',mapstruct.bands
  SXADDPAR,header,'WAVELN',mapstruct.lambda
  ; update the naxis info
  SXADDPAR,header,'NAXIS1',mapstruct.xsize
  SXADDPAR,header,'NAXIS2',mapstruct.ysize

  nhdr = N_ELEMENTS(opthdr)
  IF nhdr NE 0 THEN BEGIN
     FOR i=0,nhdr-1 DO BEGIN
        ;;Figure out what keyword this is
        ;;Only support standard FITS keywords
        kywrd = STRTRIM(STRMID(opthdr[i],0,8),2)
        IF kywrd EQ 'END' OR kywrd EQ '' THEN CONTINUE
        val = SXPAR( opthdr, STRTRIM(kywrd,2), COM=com, /SILENT )
        IF !ERR GE 0 THEN BEGIN
           IF SIZE(val,/TNAME) EQ 'BYTE' THEN BEGIN
              ;;Bytes are treated specially by SXPAR/SXADDPAR,
              ;; so just SXADDPARing what you got back will
              ;; 'break' the header
              IF val THEN SXADDPAR,header,kywrd,'T',com ELSE $
                 SXADDPAR,header,kywrd,'F',com
           ENDIF ELSE SXADDPAR,header,kywrd,val,com
        ENDIF
     ENDFOR
  ENDIF

  ;;Add the astrometric info
  PUTAST, header, mapstruct.astrometry

  ;;Add maskbits if present
  IF TAG_EXIST( mapstruct,'tod_excludemask',/TOP_LEVEL ) THEN $
     SXADDPAR,header,'TODMASK',mapstruct.tod_excludemask

  ; write the image extension
  IF ~ TAG_EXIST( mapstruct, 'image', /TOP_LEVEL ) THEN BEGIN
     errmsg = "Map structure doesn't have image"
     IF abort THEN MESSAGE,errmsg ELSE BEGIN
        IF ~ silent THEN MESSAGE,errmsg,/INF
        RETURN,0
     ENDELSE
  ENDIF

  SXADDPAR,header,'EXTNAME','image   ',' name of this HDU'
  MWRFITS, mapstruct.image, filename, header, /SILENT

  ; write the error extension
  IF TAG_EXIST( mapstruct, 'error', /TOP_LEVEL ) THEN BEGIN
     SXADDPAR,header,'EXTNAME','error   ',' name of this HDU'
     MWRFITS, mapstruct.error, filename, header, /SILENT
  ENDIF

  ; write the exposure extension
  IF TAG_EXIST( mapstruct, 'exposure', /TOP_LEVEL ) THEN BEGIN
     SXADDPAR,header,'EXTNAME','exposure',' name of this HDU'
     MWRFITS, mapstruct.exposure, filename, header, /SILENT
  ENDIF

  ; Note that writefits is smart enough to update the
  ; header to reflect the size and type of the mask
  IF TAG_EXIST( mapstruct, 'mask', /TOP_LEVEL ) THEN BEGIN
     SXADDPAR,header,'EXTNAME','mask    ',' name of this HDU'

     ; add mapmask info to header
;     ADD_MAPMASK_HEADER, mapstruct, header

     MWRFITS, mapstruct.mask, filename, header, /SILENT
     SXDELPAR, header, 'BZERO' ; otherwise, following exts screwed up
  ENDIF
  
  ; Add filter extensions
  IF TAG_EXIST( mapstruct, 'filter', /TOP_LEVEL ) THEN BEGIN
     SXADDPAR,header,'EXTNAME','filter  ',' name of this HDU'
     MWRFITS, mapstruct.filter, filename, header, /SILENT
 ENDIF

  IF TAG_EXIST( mapstruct, 'effpsf', /TOP_LEVEL ) THEN BEGIN
     SXADDPAR,header,'EXTNAME','effpsf  ',' name of this HDU'
     MWRFITS, mapstruct.effpsf, filename, header, /SILENT
  ENDIF

  ; return 1 to show we're successful
  RETURN,1

  ; and we're out
END
