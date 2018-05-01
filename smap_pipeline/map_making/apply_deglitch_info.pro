;+
;NAME
; apply_deglitch_info
;CATEGORY
; SMAP map making
;PURPOSE
; To apply a mask to a set of timestreams based on pre-existing
; deglitch information from the iterative map maker
;USAGE
; apply_deglitch_info, tods, deglitchinfo
;REQUIRED ARGUMENTS
; tods         List of tods to modify (from smap_read_and_filter).
;               Can be either strings or list of files on disk to modify
; deglitchinfo Information about glitches from SMAP iterative map
;               maker
;OPTIONAL INPUTS
; maskname     Name of mask to set (def: 'maskManual')
;OPTIONAL OUTPUTS
; success      0 if something goes wrong, 1 if it works
; errmsg       Error message if something went wrong
;MODIFICATION HISTORY
; Author: Alex Conley, August 2, 2011
;-

PRO apply_deglitch_info, tods, deglitchinfo, SUCCESS=success, $
                         ERRMSG=errmsg, MASKNAME=maskname
  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg  = ""
  
  IF N_ELEMENTS(maskname) EQ 0 THEN maskname = 'maskManual'
  IF N_ELEMENTS(maskname) NE 1 THEN BEGIN
     errmsg = "Can only provide one maskname"
     RETURN
  ENDIF

  ntods = N_ELEMENTS(tods)
  IF ntods EQ 0 THEN RETURN

  IF N_ELEMENTS(deglitchinfo) EQ 0 THEN BEGIN
     errmsg = "No deglitchinfo provided"
     RETURN
  ENDIF
  IF deglitchinfo.nglitches EQ 0 THEN RETURN

  todtype = SIZE(tods,/TNAME)
  IF todtype EQ 'POINTER' THEN doptr = 1b ELSE IF $
     todtype EQ 'STRING' THEN doptr = 0b ELSE BEGIN
     errmsg = "ERROR: input tods are not an expected type: "+todtype
     RETURN
  ENDELSE

  IF ntods NE deglitchinfo.nfiles THEN BEGIN
     errmsg = "Number of tods doesn't match information in deglitchinfo"
     RETURN
  ENDIF

  IF doptr THEN BEGIN
     ;;Now, for each tod find all the samples and mask away
     FOR i=0, ntods-1 DO BEGIN

        ;;First make sure this tod can be found in the deglitchinfo spec
        wmatch = WHERE( (*tods[i]).bbid EQ deglitchinfo.bbids AND $
                        (*tods[i]).obsid EQ deglitchinfo.obsids, nmatch )
        IF nmatch EQ 0 THEN BEGIN
           errmsg = "Unable to find matching obsid/bbid with "+$
                    STRING((*tods[i]).obsid) + $
                    TO_HEX( (*tods[i]).bbid )
           RETURN
        ENDIF
        IF nmatch GT 1 THEN BEGIN
           errmsg = "Found mutliple matches for obsid/bbid pair with "+$
                    STRING((*tods[i]).obsid) + $
                    TO_HEX( (*tods[i]).bbid )
           RETURN
        ENDIF

        ;;Now find all glitches matching this
        wglitch = WHERE( deglitchinfo.samples.fileidx EQ wmatch[0],$
                         nglitch )
        IF nglitch EQ 0 THEN CONTINUE ;;no glitches for this file

        ;;Find bits for mask we want to set
        IF ~ TAG_EXIST( (*tods[i]), 'mask_bits', /TOP_LEVEL ) THEN BEGIN
           errmsg = "Tod has no mask bit definitions"
           RETURN
        ENDIF
        wmask = WHERE( (*tods[i]).mask_bits.name EQ maskname, nmask )
        IF nmask NE 1 THEN BEGIN
           errmsg = "Couldn't match mask name: "+maskname
           RETURN
        ENDIF
        maskbit = (*tods[i]).mask_bits[wmask].bits
        
        ;;Now, finally, mask those things
        FOR j=0, nglitch-1 DO BEGIN
           curr_glitch = deglitchinfo.samples[wglitch[j]]
           bolname = deglitchinfo.bolometers[ curr_glitch.bolidx ]
           wbol = WHERE( (*tods[i]).chan EQ bolname, nbol )
           IF nbol NE 1 THEN BEGIN
              errmsg = "Unable to find bolometer: "+bolname
              RETURN
           ENDIF
           (*tods[i]).mask[wbol[0],curr_glitch.samp] OR= maskbit
        ENDFOR ;;loop over glitches in this tod
     ENDFOR ;;loop over tods

  ENDIF ELSE BEGIN
     ;;The same thing but with files
     FOR i=0, ntods-1 DO BEGIN

        curr_tod = smap_readtod( tods[i], SUCCESS=rdsuccess, $
                                ERRMSG=rderrmsg )
        IF rdsuccess EQ 0 THEN BEGIN
           errmsg = "Error reading: "+tods[i]+": "+rderrmsg
           RETURN
        ENDIF

        wmatch = WHERE( curr_tod.bbid EQ deglitchinfo.bbids AND $
                        curr_tod.obsid EQ deglitchinfo.obsids, nmatch )
        IF nmatch EQ 0 THEN BEGIN
           errmsg = "Unable to find matching obsid/bbid with "+$
                    STRING(curr_tod.obsid) + $
                    TO_HEX( curr_tod.bbid )
           RETURN
        ENDIF
        IF nmatch GT 1 THEN BEGIN
           errmsg = "Found mutliple matches for obsid/bbid pair with "+$
                    STRING(curr_tod.obsid) + $
                    TO_HEX( curr_tod.bbid )
           RETURN
        ENDIF

        wglitch = WHERE( deglitchinfo.samples.fileidx EQ wmatch[0],$
                         nglitch )
        IF nglitch EQ 0 THEN CONTINUE ;;no glitches for this file

        IF ~ TAG_EXIST( curr_tod, 'mask_bits', /TOP_LEVEL ) THEN BEGIN
           errmsg = "Tod has no mask bit definitions"
           RETURN
        ENDIF
        wmask = WHERE( curr_tod.mask_bits.name EQ maskname, nmask )
        IF nmask NE 1 THEN BEGIN
           errmsg = "Couldn't match mask name: "+maskname
           RETURN
        ENDIF
        maskbit = curr_tod.mask_bits.bits[wmask]
        
        FOR j=0, nglitch-1 DO BEGIN
           curr_glitch = deglitchinfo.samples[wglitch[j]]
           bolname = deglitchinfo.bolometers[ curr_glitch.bolidx ]
           wbol = WHERE( curr_tod.chan EQ bolname, nbol )
           IF nbol NE 1 THEN BEGIN
              errmsg = "Unable to find bolometer: "+bolname
              RETURN
           ENDIF
           curr_tod.mask[wbol[0],curr_glitch.samp] OR= maskbit
        ENDFOR ;;loop over glitches in this tod

        ;;Write back out
        smap_writetod, TEMPORARY(curr_tod), tods[i], /NO_ABORT,$
                       SUCCESS=wrsuccess, ERRMSG=wrerrmsg
        IF wrsuccess NE 0 THEN BEGIN
           errmsg = "Error writing tod to: "+tods[i]+": "+werrmsg
           RETURN
        ENDIF

     ENDFOR ;;loop over tods

  ENDELSE

  success = 1b

END
