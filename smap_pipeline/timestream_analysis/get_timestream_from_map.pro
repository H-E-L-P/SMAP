;+
;NAME
; get_timestream_from_map
;PURPOSE
; Takes a real timestream and a map and constructs a fake timestream
; representing what should have been observed according to the map.
; This is a higher level interface to get_scan_fluxes
;CATEGORY
; Herschel SPIRE SMAP
;USAGE
;  get_timestream_from_map, timestream_in, map, timestream_out
; usually you will want to loop over bands:
;  get_timestream_from_map, timestream_in, map_psw, timestream_out, /ZERO
;  get_timestream_from_map, timestream_in, map_pmw, timestream_out
;  get_timestream_from_map, timestream_in, map_plw, timestream_out
;INPUTS
; timestream_in           Input timestream in SMAP level 1 format (see
;                          smap_getlevel1).
; map                     Map to construct timestream from.  Must be
;                          in Jy/beam in the SMAP map format.
;OUTPUTS
; timestream_out          A 'lightweighted' version of the input
;                          timestream with the fluxes estimated from
;                          the map at the same positions as the input
;                          timestream.  If this is initialized on input,
;                          then the map info is added to this structure.
;                          If not, a new one is created.  Only
;                          bolometers that see sky signal are
;                          included, and ra/dec/mask/temp info
;                          is also omitted.  Bands which don't match
;                          the input map are included, but set to zero.
;KEYWORDS
; zero                    If timestream_out is initialized, zero out the
;                          signal before running.
; cubic                   Use sinc interpolation instead of bilinear 
;                          interpolation when getting fluxes; see
;                          the interpolate documentation for details.
;                          This is on by default (set CUBIC=0b to turn off).
; verbose                 Runs in verbose mode
;OPTIONAL INTPUS
;  mapmaskbits            Bit mask for input maps.  Pixels which have a
;                          non-zero or with this are not used in the map
;                          subtraction. If no mask info is present in
;                          the map, this is ignored. The default is 
;                          'FFFFFFFF'xUL -- i.e., all ones.
;OPTIONAL OUTPUTS
; success                 1b on success, 0b on failure
; errmsg                  Error message for failure
;ROUTINES CALLED
; get_scan_fluxes
;MODIFICATION HISTORY
; Author, Alex Conley, Sep 1, 2009
;         AA, Apr 23, 2010 : combined detectors for get_scan_fluxes (speed-up)
;-

PRO get_timestream_from_map, timestream_in, map, timestream_out,$
                             ZERO=zero, MAPMASKBITS=mapmaskbits,$
                             CUBIC=cubic, VERBOSE=verbose, SUCCESS=success,$
                             ERRMSG=errmsg
  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg  = ''

  IF N_ELEMENTS(cubic) EQ 0 THEN cubic=1b

  ;;Input checks.
  IF SIZE( timestream_in, /TNAME ) NE 'STRUCT' THEN BEGIN
     errmsg = "Input timestream not a structure"
     GOTO, err_handler
  ENDIF
  timestream_required_tags = ['PROGENITORFILE','SHORTFILE',$
                              'NSAMPS','CHAN','ISLIGHT',$
                              'RA','DEC']
  wpresent = WHERE_ARRAY( TAG_NAMES(timestream_in), $
                          timestream_required_tags, npresent )
  IF npresent NE N_ELEMENTS(timestream_required_tags) THEN BEGIN
     wmissing = MISSING( TAG_NAMES(timestream_in), $
                         timestream_required_tags, nmissing )
     errmsg = "Missing tags in input timestream structure: "+$
              STRJOIN(timestream_required_tags[wmissing],',')
     GOTO, err_handler
  ENDIF

  IF SIZE( map, /TNAME ) NE 'STRUCT' THEN BEGIN
     errmsg = "Input map struct not a structure"
     GOTO, err_handler
  ENDIF
  map_required_tags = ['BANDS','IMAGE','ASTROMETRY']
  wpresent = WHERE_ARRAY( TAG_NAMES(map), map_required_tags, npresent )
  IF npresent NE N_ELEMENTS(map_required_tags) THEN BEGIN
     wmissing = MISSING( TAG_NAMES(map), map_required_tags, nmissing )
     errmsg = "Missing tags in input map structure: "+$
              STRJOIN(map_required_tags[wmissing],',')
     GOTO, err_handler
  ENDIF

  IF N_ELEMENTS( timestream_out ) NE 0 THEN BEGIN
     ;;Make sure the input one is acceptable
     IF SIZE( timestream_out, /TNAME ) NE 'STRUCT' THEN BEGIN
        errmsg = 'Passed in timestream_out not a struct'
        GOTO, err_handler
     ENDIF
     timestream_required_tags = ['PROGENITORFILE','SHORTFILE','NSAMPS',$
                                 'NCHAN','CHAN','SIGNAL']
     wpresent = WHERE_ARRAY( TAG_NAMES(timestream_out), $
                             timestream_required_tags, npresent )
     IF npresent NE N_ELEMENTS(timestream_required_tags) THEN BEGIN
        wmissing = MISSING( TAG_NAMES(timestream_out), $
                            timestream_required_tags, nmissing )
        errmsg = "Missing tags in passed in timestream output structure: "+$
                 STRJOIN(timestream_required_tags[wmissing],',')
        GOTO, err_handler
     ENDIF

     ;;Reset file names
     timestream_out.progenitorfile = timestream_in.progenitorfile
     timestream_out.shortfile = timestream_in.shortfile

     ;;Make sure .chan is sorted
     wbad = WHERE( timestream_out.chan NE $
                   timestream_out.chan[SORT(timestream_out.chan)], nbad )
     IF nbad NE 0 THEN BEGIN
        errmsg = "User provided output structure has non-sorted channels"
        GOTO, err_handler
     ENDIF

     ;;Zero the signal if requested
     IF KEYWORD_SET( zero ) THEN timestream_out.signal = 0.0d0

     IF KEYWORD_SET( verbose ) AND KEYWORD_SET(zero) THEN $
        MESSAGE,"Zeroing user provided output structure",/INF
  ENDIF ELSE BEGIN
     ;;Build a new one
     wlight = WHERE( timestream_in.islight, nlight )
     IF nlight EQ 0 THEN BEGIN
        errmsg = "No detectors in passed in structure see the sky!"
        GOTO, err_handler
     ENDIF
     chan = timestream_in.chan[wlight]
     chan = chan[ SORT( chan ) ]
     nchans = N_ELEMENTS(chan)

     timestream_out = { progenitorfile: timestream_in.progenitorfile,$
                        shortfile: timestream_in.shortfile,$
                        nsamps: timestream_in.nsamps,$
                        nchan: nlight, chan: TEMPORARY(chan), $
                        signal: dblarr( nchans, timestream_in.nsamps ) }
  ENDELSE

  ;;Make sure all of the channels in the output structure have
  ;; matching input channels, and get their indices
  wpresent = WHERE_ARRAY( STRUPCASE( timestream_in.chan ),$
                          STRUPCASE( timestream_out.chan ), npresent )
  IF npresent NE timestream_out.nchan THEN BEGIN
     wmissing = MISSING( STRUPCASE( timestream_in.chan ),$
                         STRUPCASE( timestream_out.chan ) )
     errmsg = "Couldn't find some bands in input struct needed by "+$
              "output struct: "+STRJOIN(timestream_out.chan[wmissing],',')
     GOTO,err_handler
  ENDIF

  ;;See if input .CHAN is sorted
  wbad = WHERE( timestream_in.chan NE $
                timestream_in.chan[SORT(timestream_in.chan)], nbad )
  IF nbad EQ 0 THEN in_chan_sorted=1b ELSE in_chan_sorted=0b

  ;;See if there are any dets in this band
  bandname = STRUPCASE( STRMID(timestream_out.chan,0,3) )
  wband = WHERE( bandname EQ STRUPCASE(map.names), nband )
  IF nband EQ 0 THEN BEGIN
     IF KEYWORD_SET( verbose ) THEN $
        MESSAGE,"No detectors in band: "+map.names+"; doing nothing",/INF
     success = 1b
     RETURN
  ENDIF

  ;;Deal with mask
  IF TAG_EXIST( map, 'has_mask', /TOP_LEVEL ) && map.has_mask THEN BEGIN
     IF N_ELEMENTS( mapmaskbits ) EQ 0 THEN $
        mapbits = 'FFFFFFFF'xUL ELSE mapbits=  mapmaskbits
  ENDIF

  ;;Now build
  ipos=intarr(nband)
  FOR i=0, nband-1 DO BEGIN
     idx = wband[i]
     ;;Get the RA/DEC for this channel
     IF in_chan_sorted THEN BEGIN
        ipos[i] = VALUE_LOCATE( timestream_in.chan, timestream_out.chan[idx] )
     ENDIF ELSE BEGIN
        ipos[i] = ( WHERE(timestream_in.chan EQ timestream_out.chan[idx]) )[0]
     ENDELSE
     IF timestream_in.chan[ipos[i]] NE timestream_out.chan[idx] THEN BEGIN
        errmsg = "Couldn't find required channel: "+$
                 timestream_out.chan[idx]
        GOTO, err_handler
     ENDIF
  ENDFOR

     ;;Build the flux from the ra/dec and map
  flux = get_scan_fluxes( map, timestream_in.ra[ipos,*],$
                          timestream_in.dec[ipos,*], CUBIC=cubic,$
                          MASKBITS=mapbits, /NOCHECK, SUCCESS=flux_success,$
                          ERRMSG=errmsg )
  IF flux_success EQ 0 THEN BEGIN
     errmsg = "While building flux map for a(some) channel(s)"+$
              " for file "+timestream_out.shortfile+": "+errmsg
     GOTO, err_handler
  ENDIF

  timestream_out.signal[wband,*] += TEMPORARY(flux)
 

  success = 1b
  RETURN
  
  err_handler:
  IF KEYWORD_SET( verbose ) THEN MESSAGE,errmsg,/INF
  RETURN

END
  
