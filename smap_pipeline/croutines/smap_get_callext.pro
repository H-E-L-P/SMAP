FUNCTION SMAP_GET_CALLEXT, VERBOSE=verbose

;+
; function smap_get_callext.pro
; Nov. 17, 2009
; Gaelen Marsden (gmarsden@phas.ubc.ca)
;
;   shlib = SMAP_GET_CALLEXT()
;
; Compiles external C library and returns name and location of shared
; object file. To be used in call to CALL_EXTERNAL.
;
; This routine is adapted from get_callext_exlib.pro from the IDL
; examples directory.
;
;
; NOTE: 
;
; This system is set up with generic names, eg "smap_external" for the
; name of the created shared object file, with the thought that all
; routines used by the SMAP pipeline will be included in a single
; library.
;
;
; TODO:
;
;   * probably want to fix up location of .c files
;
;
; HISTORY:
; 
; 2009-11-17 (gm): initial version
;
;-

  common SMAP_GET_CALLEXT_BLK, shlib
  lastmod = 1334338409L ;;Last time contents of library changed

  ;; Build sharable library if this is the first call or lib doesn't exist
  build_lib = n_elements(shlib) eq 0
  IF (~ build_lib) THEN BEGIN
     lib_exists = FILE_TEST(shlib, /READ)
     IF lib_exists THEN BEGIN
        fileinfo = FILE_INFO(shlib)
        IF fileinfo.mtime LT lastmod THEN build_lib = 1b
     ENDIF
  ENDIF
  if (build_lib) then begin
     ;; Location of the CALL_EXTERNAL files 

     ;; assume library files are in same directory as this routine

     temppath = FILE_WHICH("smap_get_callext.pro")

     ;; remove filename from temppath
     FDECOMP, temppath, temp, call_ex_dir
     
     ;; Use MAKE_DLL to build the widget_call_ex sharable library in the
     ;; !MAKE_DLL.COMPILE_DIRECTORY directory.
     ;;
     ;; Normally, you wouldn't use VERBOSE, or SHOW_ALL_OUTPUT once your
     ;; work is debugged, but as a learning exercize it can be useful to
     ;; see all the underlying work that gets done. If the user specified
     ;; VERBOSE, then use those keywords to show what MAKE_DLL is doing.
     source = [ 'smap_accumulatemap_extern', 'smap_polywind_extern',$
                'smap_bgestimator_extern','smap_convolve_factor_extern']
     export_rtns = [ 'smap_accumulatemap_extern', 'smap_polywind_extern',$
                     'smap_bgestimator_extern','smap_convolve_factor_extern']
     
     MAKE_DLL, source, 'smap_external', export_rtns, $
               INPUT_DIR=call_ex_dir, DLL_PATH=shlib, $
               VERBOSE=verbose, SHOW_ALL_OUTPUT=verbose
  ENDIF
  
  RETURN, shlib
END

