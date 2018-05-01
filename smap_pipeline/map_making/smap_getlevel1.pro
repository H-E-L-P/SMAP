;+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  function smap_getlevel1.pro
;;  Aug 20, 2009
;;  Mike Zemcov
;;  This function reads in level 1 data from hipe format and makes it
;;   into smap struct format.
;;  Usage: tod = smap_getlevel1(file)
;;  Inputs: file = string file name (w/ full path) of file to read.
;;  Outputs: dataout = struct of data to pass around smap.
;;  Options: 
;;           verbose = verbose error messages
;;           sps = Input data is from SPS, and hence is missing
;;                   quality and temperature extensions
;;           noastrom = Unless set, will adjust ra/dec fields based on
;;                      astrometry correction based on stacking
;;                      measurements (see map_making_data/astromOffsets*.dat)
;;           oneband = 1/2/3 to read only 250/350/500 micron detectors
;;           speedcut = set to a float arcsec/s 1 < speedcut < 29 for which 
;;                       can spped at which to mask points
;;           badobsid = contains badobsid structure for manual masking
;;           novelmask= don't mask slow or fast scan regions
;;  Optional inputs
;;           fileinfo = passed output from parse_timestream_detinfo
;;  Optional outputs
;;           success = succes flag, 0b if error, 1b if success
;;           errmsg = passed out error message
;;
;; CHANGELOG:
;;   2011-02-01 (GM): add noastrom keyword. 
;;   2011-04-15 (MZ): fixed up speed threshold cut
;;   2011-04-18 (MZ): added badobsid manual masking
;;   2012-07-24 (GM): add todmask
;; NOTES:
;;   By default, this will also add mask bits and flag parts of the
;;   TOD with too fast (slew) or too slow (turnaround) motions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-
FUNCTION SMAP_GETLEVEL1, file, FILEINFO=fileinfo, ASTROMOFFSETS=astromoffsets, $
                         ONEBAND=oneband, SPS=sps, SPEEDCUT=speedcut, $
                         BADOBSID=badobsid, TODMASK=todmask, $
                         VERBOSE=verbose, SUCCESS=success, $
                         ERRMSG=errmsg, NOVELMASK=novelmask

  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg = ''

  ; make this not verbose by default
  IF ~(KEYWORD_SET(verbose)) THEN verbose = 0b

  ; do oneband?
  do_oneband = 0B
  IF KEYWORD_SET(oneband) THEN BEGIN
      IF oneband LT 1 OR oneband GT 3 THEN BEGIN
          errmsg = "Optional input ONEBAND must be 1,2 or 3"
          GOTO, err_handler
      ENDIF
      do_oneband = 1B
  ENDIF

  ;;Figure out what file is and test it
  IF N_ELEMENTS(file) EQ 0 THEN BEGIN
     errmsg = "No input files/directory provided"
     GOTO, err_handler
  ENDIF
  IF SIZE(file,/TNAME) NE 'STRING' THEN BEGIN
     errmsg = "Input file list is not of string type"
     GOTO, err_handler
  ENDIF

  ; check some other things about file
  IF N_ELEMENTS( file ) EQ 1 THEN BEGIN
     ;;See if it's a directory
     IF ~ FILE_TEST(file) THEN BEGIN
        errmsg = "Can't find: " + file[0] + " for reading"
        GOTO, err_handler
     ENDIF
     IF ~ FILE_TEST( file, /READ ) THEN BEGIN
        errmsg = "Can't read: " + file[0]
        GOTO, err_handler
     ENDIF
     IF FILE_TEST( file, /DIRECTORY ) THEN BEGIN
        errmsg = file[0] + " is a directory!  Please provide a file."
        GOTO, err_handler
     ENDIF 
  ENDIF 

  ; figure out how many extensions we have to look through
  IF N_ELEMENTS( fileinfo ) NE 0 THEN BEGIN
     IF SIZE( fileinfo, /TNAME ) NE 'STRUCT' THEN BEGIN
        errmsg = "User provided fileinfo not a structure but a "+$
                 STRLOWCASE(SIZE(fileinfo,/TNAME))
        GOTO, err_handler
     ENDIF
     required_tags = ['SIGNAL_EXTN','RA_EXTN','DEC_EXTN','MASK_EXTN',$
                      'BOLOMETER_NAMES','LIGHT_BOLOS','SIGNAL_INDEX',$
                      'RA_INDEX','DEC_INDEX','MASK_INDEX','FILE',$
                      'SHORTFILE','BBID','OBSID','OBJECT']
     IF ~ KEYWORD_SET( sps ) THEN $
        required_tags = [required_tags,$
                         ['TEMPERATURE_EXTN','TEMPERATURE_NAMES',$
                          'TEMPERATURE_INDEX']]
     tgfile = TAG_NAMES( fileinfo )
     wpresent = WHERE_ARRAY( tgfile, required_tags, npresent )
     IF npresent NE N_ELEMENTS(required_tags) THEN BEGIN
        wmissing = MISSING( tgfile, required_tags )
        errmsg = "Missing some required tags from user provided fileinfo: "+$
                 STRJOIN(required_tags[wmissing],',')
        GOTO, err_handler
     ENDIF
  ENDIF ELSE BEGIN
     fileinfo = PARSE_TIMESTREAM_DETINFO(file,SUCCESS=ptd_success,$
                                         ERRMSG=ptd_errmsg,$
                                         NOTEMPERATURE=sps,SIGNALONLY=sps)
     ;; if we ran into trouble, panic
     IF ~ptd_success THEN BEGIN
        errmsg = "Error message from PARSE_TIMESTREAM_DETINFO: " + ptd_errmsg
        GOTO, err_handler
     ENDIF
  ENDELSE

  ;;Get the primary header
  head = HEADFITS(file, ERRMSG=hd_errmsg, /SILENT) ;;primary head
  IF hd_errmsg NE '' THEN BEGIN
     errmsg = "Unable to get primary header: "+hd_errmsg
     GOTO, err_handler
  ENDIF

  ;;See if we do have quality and temperature
  IF TAG_EXIST( fileinfo, 'quality_extn', /TOP_LEVEL ) AND $
     ~ KEYWORD_SET( sps ) THEN $
     have_quality = 1b ELSE have_quality = 0b
  IF TAG_EXIST( fileinfo, 'temperature_extn', /TOP_LEVEL ) AND $
     ~ KEYWORD_SET( sps ) THEN $
     have_temperature = 1b ELSE have_temperature = 0b

  ; read in the signal extension
  sigin = MRDFITS(file,fileinfo.signal_extn,/SILENT,STATUS=status)
  IF (status LT 0) THEN BEGIN
     errmsg = "MRDFITS had error reading signal extension."
     GOTO, err_handler
  ENDIF

  ; read in right ascension extension
  rain = MRDFITS(file,fileinfo.ra_extn,/SILENT)
  IF (status LT 0) THEN BEGIN
     errmsg = "MRDFITS had error reading RA extension."
     GOTO, err_handler
  ENDIF

  ; read in declination extension
  decin = MRDFITS(file,fileinfo.dec_extn,/SILENT)
  IF (status LT 0) THEN BEGIN
     errmsg = "MRDFITS had error reading declination extension."
     GOTO, err_handler
  ENDIF

  ; read in mask extension
  maskin = MRDFITS(file,fileinfo.mask_extn,/SILENT)
  IF (status LT 0) THEN BEGIN
     errmsg = "MRDFITS had error reading mask extension."
     GOTO, err_handler
  ENDIF

  ; read in quality extension
  IF have_quality THEN BEGIN
     qualin = MRDFITS(file,fileinfo.quality_extn,/SILENT)
     IF (status LT 0) THEN BEGIN
        errmsg = "MRDFITS had error reading quality extension."
        GOTO, err_handler
     ENDIF
  ENDIF

  ; read in thermometry extension
  IF have_temperature THEN BEGIN
     thermin = MRDFITS(file,fileinfo.temperature_extn,/SILENT)
     IF (status LT 0) THEN BEGIN
        errmsg = "MRDFITS had error reading temperature extension."
        GOTO, err_handler
     ENDIF
  ENDIF

  ; figure out how many time samples there are
  nsamps = N_ELEMENTS(sigin)
  IF nsamps EQ 0 THEN BEGIN
     errmsg = "Number of samples seems to be zero!"
     GOTO, err_handler
  ENDIF 

  ; get the correct sized default structure
  IF have_temperature THEN ntherms = fileinfo.ntherms ELSE ntherms=0

  ; if only one band, set up indices
  IF do_oneband THEN BEGIN
      CASE oneband OF
          1: bandstr = 'PSW'
          2: bandstr = 'PMW'
          3: bandstr = 'PLW'
      ENDCASE
      bandind = WHERE(STRCMP(fileinfo.bolometer_names, bandstr, 3, /FOLD), $
                      nbands)
  ENDIF ELSE BEGIN
      nbands = fileinfo.nchannels
      bandind = INDGEN(nbands)
  ENDELSE

  dataout = SMAP_GETDEFTSSTRUCT(nsamps,nbands,ntherms,$
                                SUCCESS=gds_success,ERRMSG=gds_errmsg,$
                                NOQUALITY=(~have_quality),$
                                NOTHERM=(~have_temperature))
  IF ~(gds_success) THEN BEGIN
     errmsg = "Error message from SMAP_GETDEFTSTRUCT: " + gds_errmsg
     GOTO, err_handler
  ENDIF
  
  ; set the file name to the structure
  ; no need to error check this as if we've made it this far
  ; we know these are set to something
  ; the LONG's are explicitly set because mwrfits doesn't 
  ; seem to implicitly know about uL
  dataout.progenitorfile = fileinfo.file
  dataout.shortfile      = fileinfo.shortfile
  dataout.bbid           = ULONG(fileinfo.bbid)
  dataout.obsid          = ULONG(fileinfo.obsid)
  dataout.object         = fileinfo.object

  ;;Add date obs if possible
  dateobs = SXPAR( head, 'DATE-OBS', COUNT=ndpos )
  IF ndpos NE 0 THEN dataout.dateobs = dateobs ELSE $
     dataout.dateobs="Unknown"

  ; set up for receiving TOD
  ; find the length of the time sample array, make sure it isn't 
  ; zero, and as long as that's fine dump it in the output struct
  baselength = N_ELEMENTS(sigin.sampletime)
  IF (baselength LT 1) THEN BEGIN
     errmsg = 'Number of time samples appears to be ' + $
              STRCOMPRESS(STRING(baselength),/REMOVE_ALL)
     GOTO, err_handler
  ENDIF ELSE dataout.samptime = sigin.sampletime
  
  ;;Set sampling frequency as average
  IF baselength GT 1 THEN $
     dataout.sampfreq = 1.0/MEAN( dataout.samptime[1:baselength-1] - $
                                  dataout.samptime[0:baselength-2], /NAN )

  ; check that the mask sample time is right and dump it
  IF TAG_EXIST( maskin, 'sampletime', /TOP_LEVEL ) THEN BEGIN
     IF (N_ELEMENTS(maskin.sampletime) NE baselength) THEN BEGIN
        errmsg = 'Number of mask time samples does not match!'
        GOTO, err_handler
     ENDIF ELSE dataout.masktime = maskin.sampletime
  ENDIF ELSE BEGIN
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,"WARNING: no mask time found; setting to signal"+$
                " sampletime",/INF
     dataout.masktime = sigin.sampletime
  ENDELSE

  ; test to see if astrometry offsets provided
  doastrom = 0B
  IF (N_ELEMENTS(astromoffsets) GT 0 && $
      SIZE(astromoffsets[0], /TNAME) EQ 'STRUCT') THEN BEGIN
      ; find obsids
      astromind = WHERE(astromoffsets.obsid EQ fileinfo.obsid, nastromind)
      IF nastromind GT 0 THEN BEGIN
          thisastoff = astromoffsets[astromind[0]]
          doastrom = 1B
      ENDIF ELSE $
         MESSAGE, "obsid not found astrometry offsets", /INF
  ENDIF
     

  ; loop through all channels dumping out signal etc data -
  ; can't think of a better way to error check this than let
  ; idl tank if the struct sizes are wrong (this is checked in a 
  ; previous subroutine anyway) so just let it work it's magic
  FOR b=0,nbands-1 DO BEGIN
     ibolo = bandind[b]
     dataout.chan[b] = fileinfo.bolometer_names[ibolo]
     dataout.islight[b] = fileinfo.light_bolos[ibolo]
     dataout.signal[b,*] = sigin.(fileinfo.signal_index[ibolo])
     dataout.ra[b,*] = rain.(fileinfo.ra_index[ibolo])
     dataout.dec[b,*] = decin.(fileinfo.dec_index[ibolo])
     dataout.mask[b,*] = maskin.(fileinfo.mask_index[ibolo])
     IF have_quality THEN BEGIN
        dataout.adcerrors[b] = $
           qualin[fileinfo.quality_index[ibolo]].adcerrors
        dataout.truncation[b] = $
           qualin[fileinfo.quality_index[ibolo]].truncation
    ENDIF

     IF doastrom THEN BEGIN
         ; use astr structure with 1 deg pixels for projection
         MAKE_ASTR, thisastr, CRPIX=[1,1], DELT=[1,1], $
                    CRVAL=[thisastoff.RA_0,thisastoff.Dec_0]

         ; perform tangent plane projection
         AD2XY, dataout.ra[b,*], dataout.dec[b,*], thisastr, $
                thisxx, thisyy

         ; apply offsets and inverse transform 
         XY2AD, thisxx+thisastoff.delta_RA, thisyy+thisastoff.delta_Dec, $
                thisastr, thisra, thisdec

         dataout.ra[b,*] = thisra
         dataout.dec[b,*] = thisdec
     ENDIF

  ENDFOR

  ;; ok, now check that the thermometry makes sense
  IF have_temperature THEN BEGIN
     IF (N_ELEMENTS(thermin.sampletime) NE baselength) THEN BEGIN
        errmsg = 'Number of mask time samples does not match!'
        GOTO, err_handler
     ENDIF ELSE dataout.temperaturetime = thermin.sampletime

     ;; dump out the thermometry data under same assumptions as above
     FOR itherm=0,fileinfo.ntherms-1 DO BEGIN
        dataout.therm[itherm] = fileinfo.temperature_names[itherm]
        dataout.temperature[itherm,*] = $
           thermin.(fileinfo.temperature_index[itherm])
     ENDFOR
  ENDIF

  ;;Add mask bit info
  ;;This makes a file structure that can't simply be written
  ;; as a fits file
  mb = get_mask_bits(head, SUCCESS=mb_success)
  IF mb_success THEN $
     dataout = CREATE_STRUCT(dataout,'mask_bits',TEMPORARY(mb))

  ;; now generate user masks
  IF TAG_EXIST( dataout, 'mask_bits', /TOP_LEVEL ) THEN BEGIN
     IF ~ KEYWORD_SET( novelmask ) THEN BEGIN
        construct_turnaround_mask,dataout,SPEEDCUT=speedcut,$
                                  VERBOSE=verbose,SUCCESS=success_cta
        IF success_cta NE 1 THEN BEGIN
           errmsg = "Error in construct_turnaround_mask!"
           GOTO,err_handler
        ENDIF
     ENDIF

     IF N_ELEMENTS(badobsid) GT 0 THEN BEGIN
        construct_manual_mask,dataout,badobsid,$
                              VERBOSE=verbose,SUCCESS=success_cma
        IF success_cma NE 1 THEN BEGIN
           errmsg = "Error in construct_manual_mask!"
           GOTO,err_handler
        ENDIF
     ENDIF
     
     IF N_ELEMENTS(todmask) GT 0 THEN BEGIN
        SMAP_SET_TODMASKS, dataout, todmask, $
                           VERBOSE=verbose,SUCCESS=success_stm


     ENDIF
  ENDIF

  ;; ok, that's it, get me out of here with my data
  success = 1b
  RETURN,dataout

err_handler:
  IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
  RETURN,!VALUES.F_NAN

END
