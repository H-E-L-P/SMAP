;+
;NAME
; write_level1_from_smap
;PURPOSE
; Writes a level1 HCSS file from a SMAP level1 IDL structure, copying
; over unknown or unmodified extensions from the original file
;USAGE
; To write out an idl structure where the signal and mask were
; modified:
;  modstruct = { signal: 1b, mask: 1b }
;  write_level1_from_smap, level1_struct, outfile, modstruct [,PRIMARYHEAD= ]
;INPUTS
; level1_struct         Level 1 IDL structure to write to disk.  See
;                        smap_getlevel1 (or smap_read_and_filter).
; outfile               File to write to.  This can -not- be the same
;                        as level1_struct.progenitorfile
; modstruct             Structure saying which extensions were
;                        modified.  Understood tags are .SIGNAL,
;                        .MASK, and .TEMPERATURE.  Make a structure with
;                        each tag you want set to 1 to have the output
;                        modified.  Tags not present are ignored.
;OPTIONAL INPUTS
; primaryhead           Header to replace primary header with.  This
;                        does not preserve the contents of the
;                        original header.
;KEYWORDS
; verbose               Print informational messages
;OPTIONAL OUTPUTS
; success               1b if it worked, 0b if it didn't
; errmsg                An error string explaining what went wrong, or
;                        '' if it worked.
;NOTES
; The structure is output in the same order as the input in order to
; preserve the HCSS information, capitalization, etc.  So, the level1_struct
; had better have all the info in the input file!
;MODIFICATION HISTORY
; Author: Alex Conley, Sep 1, 2009
;-

PRO write_level1_from_smap, level1_struct, outfile, modstruct,$
                            PRIMARYHEAD=primaryhead, VERBOSE=verbose,$
                            SUCCESS=success, ERRMSG=errmsg
  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg  = ''

  ;;Input checks
  IF N_ELEMENTS( level1_struct ) EQ 0 THEN BEGIN
     errmsg = "No level 1 structure passed in!"
     GOTO, err_handler
  ENDIF
  IF N_ELEMENTS( outfile ) EQ 0 THEN BEGIN
     errmsg = "No outfile passed in!"
     GOTO, err_handler
  ENDIF
  IF N_ELEMENTS( modstruct ) EQ 0 THEN BEGIN
     errmsg = "No modstruct passed in!"
     GOTO, err_handler
  ENDIF
  
  modtags_understood = ['SIGNAL','MASK','TEMPERATURE']
  modtags = TAG_NAMES(modstruct)
  wpresent = WHERE_ARRAY( modtags_understood, modtags, npresent )
  IF npresent NE N_ELEMENTS( modtags ) THEN BEGIN
     wmissing = MISSING( modtags_understood, modtags )
     errmsg = "Unknown tags in modstruct: "+STRJOIN(modtags[wmissing])
     GOTO, err_handler
  ENDIF
  IF TAG_EXIST( modstruct, 'signal', /TOP_LEVEL ) && modstruct.signal THEN $
     signal_mod = 1b ELSE signal_mod = 0b
  IF TAG_EXIST( modstruct, 'mask', /TOP_LEVEL ) && modstruct.mask THEN $
     mask_mod = 1b ELSE mask_mod = 0b
  IF TAG_EXIST( modstruct, 'temperature', /TOP_LEVEL ) && $
     modstruct.temperature THEN temperature_mod = 1b ELSE temperature_mod = 0b

  ;;Check input struct
  IF SIZE( level1_struct, /TNAME ) NE 'STRUCT' THEN BEGIN
     errmsg = "Level 1 struct is not, well, a struct"
     GOTO, err_handler
  ENDIF
  tags_required = ['PROGENITORFILE','SHORTFILE','NCHANS','SAMPTIME']
  IF signal_mod THEN tags_required = [tags_required,'SIGNAL']
  IF mask_mod THEN tags_required = [tags_required,'MASK']
  IF temperature_mod THEN tags_required = $
     [tags_required,'NTHERMS','TEMPERATURE','THERM']
  l1names = TAG_NAMES( level1_struct )
  wpresent = WHERE_ARRAY( l1names, tags_required, npresent )
  IF npresent NE N_ELEMENTS( tags_required ) THEN BEGIN
     wmissing = MISSING( l1names, tags_required, nmissing )
     errmsg = "Missing tags in level1_struct structure: "+$
              STRJOIN(tags_required[wmissing],',')
     GOTO, err_handler
  ENDIF
  DELVARX, l1names
  
  ;;Check outfile
  IF SIZE( outfile, /TNAME ) NE 'STRING' THEN BEGIN
     errmsg = "outfile is not a string"
     GOTO, err_handler
  ENDIF
  IF STRLEN( outfile ) EQ 0 THEN BEGIN
     errmsg = "outfile is empty string"
     GOTO, err_handler
  ENDIF
  IF KEYWORD_SET( verbose ) AND FILE_TEST( outfile ) THEN $
     MESSAGE,"WARNING: will overwrite "+outfile,/INF
  IF outfile EQ level1_struct.progenitorfile THEN BEGIN
     errmsg = "outfile and level1 original file are the same; this is"+$
              " verboten"
     GOTO, err_handler
  ENDIF

  ;;Make sure we can still read the progenitor file
  IF ~ FILE_TEST( level1_struct.progenitorfile ) THEN BEGIN
     errmsg = "Original input file "+level1_struct.progenitorfile+$
              "no longer exists!"
     GOTO, err_handler
  ENDIF
  IF ~ FILE_TEST( level1_struct.progenitorfile, /READ ) THEN BEGIN
     errmsg = "Original input file "+level1_struct.progenitorfile+$
              "is unreadable"
     GOTO, err_handler
  ENDIF
  FITS_OPEN, level1_struct.progenitorfile, fcb, MESSAGE=errmsg
  IF errmsg NE '' THEN BEGIN
     errmsg = "Error opening input fits file "+level1_struct.progenitorfile+$
              ": "+errmsg
     GOTO, err_handler
  ENDIF
  FITS_CLOSE, fcb

  ;;Figure out if nothing was modified; if not, just copy the input
  ;;file
  IF (~ signal_mod) AND (~ mask_mod) AND (~ temperature_mod) THEN BEGIN
     IF N_ELEMENTS(primary_head) EQ 0 THEN BEGIN
        ;;Woot, nothing to do
        FILE_COPY, level1_struct.progenitorfile, outfile
     ENDIF ELSE BEGIN
        ;;Ok, not quite as easy, but still not too bad
        FILE_COPY, level1_struct.progenitorfile, outfile
        MODFITS, outfile, 0, primaryhead, ERRMSG=errmsg
        IF errmsg NE '' THEN BEGIN
           errmsg = "While modifying primary header of "+$
                    outfile+": "+errmsg
           FILE_DELETE, outfile, /ALLOW
           GOTO, err_handler
        ENDIF
     ENDELSE
     success = 1b
     RETURN
  ENDIF

  ;;So, we weren't that lucky
  ;;First do the primary, which contains no data
  IF N_ELEMENTS( primaryhead ) EQ 0 THEN BEGIN
     dat = MRDFITS( level1_struct.progenitorfile, 0, head, STATUS=status,$
                    /SILENT )
     IF status LT 0 THEN BEGIN
        errmsg = "Error reading primary HDU from "+level1_struct.progenitorfile
        GOTO, err_handler
     ENDIF
     MWRFITS, TEMPORARY(dat), outfile, head, STATUS=write_status, /CREATE,$
              /SILENT
     IF write_status LT 0 THEN BEGIN
        errmsg = "Error writing primary extension of "+outfile
        FILE_DELETE, outfile, /ALLOW
        GOTO, err_handler
     ENDIF
  ENDIF ELSE BEGIN
     MWRFITS, 0, outfile, primaryhead, STATUS=write_status, /CREATE,$
              /SILENT
     IF write_status LT 0 THEN BEGIN
        errmsg = "Error writing primary extension of "+outfile
        FILE_DELETE, outfile, /ALLOW
        GOTO, err_handler
     ENDIF
  ENDELSE

  ;;Now loop extentions, treating each that is modified individually
  FOR idx = 1, fcb.nextend DO BEGIN
     extname = STRLOWCASE( fcb.extname[idx] )

     ;;Read it
     dat = MRDFITS( level1_struct.progenitorfile, idx, head, STATUS=status,$
                    /SILENT )
     IF status LT 0 THEN BEGIN
        errmsg = "Error reading "+extname+" extension from "+$
                 level1_struct.progenitorfile
        FILE_DELETE, outfile, /ALLOW
        GOTO, err_handler
     ENDIF

     ;;Modify if we have to.
     IF extname EQ 'signal' AND signal_mod THEN BEGIN
        ;;Copy the new signal into the input one, changing in place
        ;;First make sure every tag in the level1_struct has a home
        ;; in the output one
        output_tags = TAG_NAMES( dat )
        wpresent = WHERE_ARRAY( output_tags, level1_struct.chan, npresent )
        IF npresent NE level1_struct.nchans THEN BEGIN
           wmissing = MISSING( output_tags, level1_struct.chan )
           errmsg = "Missing some signal channels in original file: "+$
                    STRJOIN( level1_struct.chan[wmissing],',')
           FILE_DELETE, outfile, /ALLOW
           STOP
           GOTO, err_handler
        ENDIF

        ;;Make sure channel length is the same
        IF N_ELEMENTS( dat ) NE level1_struct.nsamps THEN BEGIN
           errmsg = "Signal in original file has different number of samples"+$
                    " than level1_struct"
           FILE_DELETE, outfile, /ALLOW
           GOTO, err_handler
        ENDIF
           
        ;;Now, locate each bolometer channel
        out_sort = SORT( output_tags )
        bol_idx = out_sort[ VALUE_LOCATE( output_tags[out_sort], $
                                          level1_struct.chan ) ]

        ;;Do samptime if present
        wsamp = WHERE( output_tags EQ 'sampletime', ntime )
        IF ntime EQ 1 THEN dat.(wsamp[0]) = level1_struct.samptime

        ;;Now bolometers
        FOR j = 0, level1_struct.nchans-1 DO $
           dat.(bol_idx[j]) = REFORM(level1_struct.signal[idx,*])
     ENDIF ELSE IF extname EQ 'mask' AND mask_mod THEN BEGIN
        ;;And mask, which is pretty much idential to signal
        output_tags = TAG_NAMES( dat )
        wpresent = WHERE_ARRAY( output_tags, level1_struct.chan, npresent )
        IF npresent NE level1_struct.nchans  THEN BEGIN
           wmissing = MISSING( output_tags, level1_struct.chan )
           errmsg = "Missing some mask channels in original file: "+$
                    STRJOIN( level1_struct.chan[wmissing],',')
           FILE_DELETE, outfile, /ALLOW
           GOTO, err_handler
        ENDIF
        IF N_ELEMENTS( dat ) NE level1_struct.nsamps THEN BEGIN
           errmsg = "Mask in original file has different number of samples"+$
                    " than level1_struct"
           FILE_DELETE, outfile, /ALLOW
           GOTO, err_handler
        ENDIF
        out_sort = SORT( output_tags )
        bol_idx = out_sort[ VALUE_LOCATE( output_tags[out_sort], $
                                          level1_struct.chan ) ]
        ;;Yes, I choose to write sampletime not masktime since they
        ;; damned well better be the same anyways
        wsamp = WHERE( output_tags EQ 'sampletime', ntime )
        IF ntime EQ 1 THEN dat.(wsamp[0]) = level1_struct.samptime
        FOR j = 0, level1_struct.nchans-1 DO $
           dat.(bol_idx[j]) = REFORM(level1_struct.mask[idx,*])
     ENDIF ELSE IF extname EQ 'temperature' AND temperature_mod THEN BEGIN
        ;;This one is slightly different
        output_tags = TAG_NAMES( dat )
        wpresent = WHERE_ARRAY( output_tags, level1_struct.therm, npresent )
        IF npresent NE level1_struct.ntherms THEN BEGIN
           wmissing = MISSING( output_tags, level1_struct.therm )
           errmsg = "Missing some temp channels in original file: "+$
                    STRJOIN( level1_struct.therm[wmissing],',')
           FILE_DELETE, outfile, /ALLOW
           GOTO, err_handler
        ENDIF
        IF N_ELEMENTS( dat ) NE level1_struct.nsamps THEN BEGIN
           errmsg = "Therm in original file has different number of samples"+$
                    " than level1_struct"
           FILE_DELETE, outfile, /ALLOW
           GOTO, err_handler
        ENDIF
        out_sort = SORT( output_tags )
        bol_idx = out_sort[ VALUE_LOCATE( output_tags[out_sort], $
                                          level1_struct.therm ) ]
        ;;Yes, I choose to write sampletime not temperaturetime since they
        ;; damned well better be the same anyways
        wsamp = WHERE( output_tags EQ 'sampletime', ntime )
        IF ntime EQ 1 THEN dat.(wsamp[0]) = level1_struct.samptime
        FOR j = 0, level1_struct.ntherm-1 DO $
           dat.(bol_idx[j]) = REFORM(level1_struct.temperature[idx,*])
     ENDIF

     ;;Write whatever it was
     ;;The no_type is critical here because HCSS is case sensitive and
     ;; IDL isn't!
     MWRFITS, TEMPORARY(dat), outfile, head, $
              STATUS=write_status, /SILENT, /NO_TYPE
     IF write_status LT 0 THEN BEGIN
        errmsg = "Error writing out "+outfile+" extension: "+extname
        FILE_DELETE, outfile, /ALLOW
        GOTO, err_handler
     ENDIF
  ENDFOR

  success = 1b
  RETURN

err_handler:
  IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
  RETURN

END



