;+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  pro smap_remove_mapsrc.pro
;;  May 13, 2009
;;  Mike Zemcov
;;  This procedure reads in a map and a catalog and then subtracts the
;;  sources listed in the catalog from the map, saving a new map
;;  source subtracted map as output.
;;  Inputs: fileroot = root file name to read from
;;  Outputs:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-

PRO SMAP_REMOVE_SRC,mapstruct,x,y,flux,maskwidth,maskonly

  maskwidth = FLOAT(maskwidth)

  testimage = FLTARR(mapstruct.xsize,mapstruct.ysize)

  nsrc = N_ELEMENTS(flux)
  FOR ind=0,nsrc-1 DO BEGIN
     beamkern = get_spire_beam(mapstruct.names,mapstruct.pixscale,$
                               mapstruct.xsize,mapstruct.ysize,$
                               x[ind],y[ind],/SILENT,oversamp=1)

     IF ~maskonly THEN BEGIN
        mapstruct.image = mapstruct.image - beamkern * flux[ind] 
        testimage = testimage + beamkern * flux[ind] 
     ENDIF
     beamkern = beamkern / MAX(beamkern)
     whpl = WHERE(beamkern GT EXP(-maskwidth^2 * ALOG(2.)))
     mapstruct.mask[whpl] = 1

  ENDFOR
  
  RETURN
  
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO SMAP_REMOVE_MAPSRC,$
   fileroot,catfile,$
   PSW=psw,$
   PMW=pmw,$
   PLW=plw,$
   MASKWIDTH=maskwidth,$
   MASKONLY=maskonly,$
   SILENT=silent
  

  ; make sure we got a file name, if we didn't, bail
  IF N_ELEMENTS(fileroot) EQ 0 THEN MESSAGE,'No file name given, aborting!'
  IF N_ELEMENTS(catfile) EQ 0 THEN $
     MESSAGE,'No catalog file name given, aborting!'
  IF ~KEYWORD_SET(silent) THEN silent = 0

  IF ~KEYWORD_SET(maskwidth) THEN maskwidth = 2. ; fwhm
  IF ~KEYWORD_SET(maskonly) THEN maskonly = 0

  ; an array telling me which colours we want to process - 
  ;  in this case, do them all
  lambda_on = [1,1,1] 

  IF KEYWORD_SET(psw) OR KEYWORD_SET(pmw) OR KEYWORD_SET(plw) THEN BEGIN
     IF ~KEYWORD_SET(psw) THEN lambda_on[0] = 0
     IF ~KEYWORD_SET(pmw) THEN lambda_on[1] = 0
     IF ~KEYWORD_SET(plw) THEN lambda_on[2] = 0
  ENDIF
     
  catstruct=mrdfits(addslash(!SMAP_CATS) + catfile,1,h)

  whpl = WHERE(lambda_on EQ 1, ncolors)
  nsrc = N_ELEMENTS(catstruct.id)
  
  IF (lambda_on[0]) THEN BEGIN
     mapstruct_psw = read_smap_fitsmap(fileroot,'PSW')

     whpsw = WHERE(catstruct.det[0,*] EQ 1)

     smap_remove_src,mapstruct_psw,$
                     catstruct[whpsw].x_cen_250,$
                     catstruct[whpsw].y_cen_250,$
                     catstruct[whpsw].f_250,$
                     maskwidth,maskonly

     outfile = fileroot + '_srcsub'
     one = write_smap_fitsmap(mapstruct_psw,outfile)
  ENDIF

  IF (lambda_on[1]) THEN BEGIN
     mapstruct_pmw = READ_SMAP_FITSMAP(fileroot,'PMW')

     whpmw = WHERE(catstruct.det[1,*] EQ 1)

     smap_remove_src,mapstruct_pmw,$
                     catstruct[whpmw].x_cen_350,$
                     catstruct[whpmw].y_cen_350,$
                     catstruct[whpmw].f_350,$
                     maskwidth,maskonly

     outfile = fileroot + '_srcsub'
     one = write_smap_fitsmap(mapstruct_pmw,outfile)
  ENDIF

  IF (lambda_on[2]) THEN BEGIN
     mapstruct_plw = READ_SMAP_FITSMAP(fileroot,'PLW')

     whplw = WHERE(catstruct.det[2,*] EQ 1)

     smap_remove_src,mapstruct_plw,$
                     catstruct[whplw].x_cen_500,$
                     catstruct[whplw].y_cen_500,$
                     catstruct[whplw].f_500,$
                     maskwidth,maskonly

     outfile = fileroot + '_srcsub'
     one = write_smap_fitsmap(mapstruct_plw,outfile)
  ENDIF

  RETURN

END
