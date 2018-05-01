;+
; NAME:
;   construct_mask_bitmask
; CATEGORY
;   Herschel SPIRE SMAP pipeline
; PURPOSE:
;   Given a list of inputs you want to mask, constructs a bitmask
;   from the output from get_mask_bits.
; CALLING SEQUENCE:
;   bitmask = construct_mask_bitmask( names, maskinfo )
; INPUTS:
;   names        A string array of things you want to set the mask for.
;                 If they don't start with 'mask' it will be appended
;   maskinfo     Output from get_mask_bits
; KEYWORDS
;   require_tag  Normally, if a name you specify isn't found, the code
;                 will ignore it.  If this is set, the code will
;                 instead report failure via SUCCESS
; OUTPUTS:
;   An unsigned integer you can AND against your mask bits to see if
;   something fails one of your cuts.
; OPTIONAL OUTPUTS
;   success      1b if it succeeded, 0 if not
;   errmsg       Informative message about what went wrong
; NOTES
;   Case is important
; MODIFICATION HISTORY:
;   Author: Alex Conley, May 1, 2009
;-

FUNCTION construct_mask_bitmask, names, maskinfo, SUCCESS=success,$
                                 ERRMSG=errmsg, REQUIRE_TAG=require_tag, $
                                 VERBOSE=verbose

  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg = ''

  retval = 0uL ;;32 bit unsigned

  nnames = N_ELEMENTS(names)
  IF nnames EQ 0 THEN BEGIN
     ;;Well, this is a success, I guess
     success = 1b
     RETURN,retval
  ENDIF
  
  ;;Make sure names are strings
  IF SIZE( names, /TNAME ) NE 'STRING' THEN BEGIN
     errmsg = "Names were not strings, but "+SIZE(names,/TNAME)
     GOTO, err_handler
  ENDIF

;;Check maskinfo
  IF SIZE( maskinfo, /TNAME ) NE 'STRUCT' THEN BEGIN
     IF KEYWORD_SET( verbose ) THEN $
        errmsg="Maskinfo was not a struct, but a "+SIZE(maskinfo,/TNAME)
     GOTO, err_handler
  ENDIF
  IF ~ TAG_EXIST( maskinfo, 'name', /TOP_LEVEL ) THEN BEGIN
     errmsg = "Maskinfo did not have .name field"
     GOTO, err_handler
  ENDIF
  IF ~ TAG_EXIST( maskinfo, 'bits', /TOP_LEVEL ) THEN BEGIN
     errmsg = "Maskinfo did not have .bits field"
     GOTO, err_handler
  ENDIF

  ;;See if we need to prepend 'mask'
  IF SIZE(names,/N_DIMENSIONS) EQ 0 THEN internal_names = [names] ELSE $
     internal_names = names
  wnotstartmask = WHERE( STRMID(internal_names,0,4) NE 'mask', nnotstartmask )
  IF nnotstartmask NE 0 THEN $
     internal_names[wnotstartmask] = 'mask' + internal_names[wnotstartmask]

  wpresent = WHERE_ARRAY( internal_names, maskinfo.name, npresent )

  FOR i=0,npresent-1 DO $
     retval OR= maskinfo[wpresent[i]].bits
  
  IF KEYWORD_SET( require_tag ) && npresent NE nnames THEN BEGIN
     errmsg = "Couldn't find all tags"
     GOTO, err_handler
  ENDIF

  success = 1b
  RETURN,retval

  err_handler:
  IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
  RETURN,0uL

END
