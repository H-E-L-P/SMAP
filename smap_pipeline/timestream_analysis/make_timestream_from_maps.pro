;+
;NAME
; make_timestream_from_maps
;PURPOSE
; Given a set of input maps, build fake timestreams based on
; the pointing information from real timestreams.
;USAGE
; timestream = make_timestram_from_maps( tod, map_array )
;INPUTS
; tod           Time ordered data from, i.e., smap_getlevel1
; map_array     Map data, either an array of map structures, or, more
;                likely, a pointer array of maps
;OPTIONAL INPUTS
; mapmaskbits   Bit array of things you don't want to include in the 
;                timestream as found in the input maps .mask
;                extension.
;KEYWORDS
; cubic         See get_timestream_from_map.  On by default.
;OPTIONAL OUTPUTS
; success       The success flag, =1 if success, =0 if not
; errmsg        If an error occurs, this holds a string explaining the
;                    error
;NOTES
; This is basically a wrapper for get_timestream_from_map
;MODIFICATION HISTORY
; Author: Alex Conley, Nov 2009
;-

FUNCTION make_timestream_from_maps, tod, map_array, SUCCESS=success,$
                                    ERRMSG=errmsg, MAPMASKBITS=mapmaskbits,$
                                    CUBIC=cubic
  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg  = ''

  IF SIZE(tod,/TNAME) NE 'STRUCT' THEN BEGIN
     errmsg = "TOD is not a structure, as expected"
     GOTO, err_handler
  ENDIF
  nmaps = N_ELEMENTS(map_array)
  IF nmaps EQ 0 THEN BEGIN
     errmsg = "No map array present"
     GOTO, err_handler
  ENDIF
  IF SIZE( map_array, /TNAME ) EQ 'POINTER' THEN BEGIN
     maps_are_pointers = 1b
     IF SIZE( *map_array[0],/TNAME ) NE 'STRUCT' THEN BEGIN
        errmsg = "Map array doesn't contain structures!"
        GOTO, err_handler
     ENDIF
  ENDIF ELSE BEGIN
     maps_are_pointers = 0b
     IF SIZE(map_array, /TNAME) NE 'STRUCT' THEN BEGIN
        errmsg = "Map array is of unexpected type: "+SIZE(map_array,/TNAME)
        GOTO, err_handler
     ENDIF
  ENDELSE

  ;;Processing loop
  FOR i=0, nmaps-1 DO BEGIN
     IF maps_are_pointers THEN BEGIN
        get_timestream_from_map, tod, *map_array[ i ],$
                                 subtrahend, SUCCESS=ts_success, ERRMSG=errmsg,$
                                 MAPMASKBITS=mapmaskbits, CUBIC=cubic
        IF ts_success EQ 0 THEN BEGIN
           errmsg = "While making timestream from map in band: "+$
                    (*map_array[i]).name + ": "+errmsg
           GOTO, err_handler
        ENDIF
     ENDIF ELSE BEGIN
        get_timestream_from_map, tod, map_array[ i ],$
                                 subtrahend, SUCCESS=ts_success, ERRMSG=errmsg,$
                                 MAPMASKBITS=mapmaskbits, CUBIC=cubic
        IF ts_success EQ 0 THEN BEGIN
           errmsg = "While making timestream from map in band: "+$
                    map_array[i].name + ": "+errmsg
           GOTO, err_handler
        ENDIF
     ENDELSE
  ENDFOR

  success = 1b
  RETURN, subtrahend
  
err_handler:
  IF KEYWORD_SET( verbose ) THEN MESSAGE,errmsg,/INF
  RETURN,!VALUES.F_NAN

END
