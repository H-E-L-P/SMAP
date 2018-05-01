;+
;NAME
; write_sanepic_powerspec
;PURPOSE
; Writes PSDs (auto and posibly cross) in SANEPIC format
;USAGE
; write_sanepic_powerspec, autospec [, CROSSSPEC=crossspec,
;                                      OUTDIR=outdir, BASENAME=basename ]
;INPUTS
; autospec        Auto spectrum (see get_auto_psd_from_l1 and read_powerspec)
;OPTIONAL INPUTS 
; crossspec       Cross spectrum (see get_cross_psd_from_l1 and read_powerspec)
; outdir          Output directory (default: the current dir)
; basename        Base name of output files (def: spirepsd)
;SIDE EFFECTS
; Writes a number of files to outdir.  These include:
;  1) A file basename+'_dets.txt', which is a text listing of the
;      detector names
;  2) Ndetector files containing the auto (maybe cross) power spectra
;      basename+detector in binary format
; The format of the latter files depends on whether cross-spectra are
;  present or not.  If no cross spectra are present, the format is:
;     I) Number of frequency bins as double precision
;    II) Array of frequency bins as doubles, nbins+1 values with the
;         spectrum between i and i+1 
;   III) The noise PSD in Jy^2/Hz
; If cross spectra are present, the format is
;     I) Number of frequency bins as double precision
;    II) Array of frequency bins as doubles, nbins+1 values with the
;         spectrum between i and i+1 
;   III) Ndet inverse cross-spectra Noise PSDs in Jy^2/Hz in the same
;          order as in the dets file.
;MODIFICATION HISTORY
; Author: Alex Conley, August 2009
;
; Alex Amblard, 10/19/09 :
;   - remove the 1/ps for the cross-spectrum input, the ps matrix need to be inverse
;     so it's not just a simple 1/ps of each detectors, Guillaume Patanchon gave me
;     a C program to do it, it will be called before SANEPIC is launched and will
;     take care of the ps matrix inversion
;    
;   - in the no-correlation input, SANEPIC wants ASCII input file for the spectrum
;     so I changed the format accordingly 
;
;-

PRO write_sanepic_autospec, autospec, idx, file, SUCCESS=success, ERRMSG=errmsg
  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN
  success = 0b
  errmsg  = ''

  ;;Don't write zero frequency as SANEPIC doesn't want it
  nspec = autospec.nfreq-1
  IF idx LT 0 OR idx GE autospec.ndet THEN BEGIN
     errmsg = "Invalid location in autospec"
     RETURN
  ENDIF

  ;;Sanepic wants freqs between the places they were calculated
  df = 0.5*(autospec.freq[1]-autospec.freq[0])
  freq = DOUBLE([autospec.freq - df, autospec.freq[nspec]+df])
  psd  = DOUBLE(REFORM(autospec.psds[idx,*]))
  
  OPENW, unit, file, /GET_LUN
  PRINTF, unit, DOUBLE( nspec )
  PRINTF, unit, TRANSPOSE([[freq[0:nspec-1],[psd]])
  PRINTF, unit, freq[nspec]
  FREE_LUN,unit

  success = 1b
  RETURN
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO write_sanepic_crossspec, autospec, crossspec, idxs, autoloc, file, $
                             SUCCESS=success, ERRMSG=errmsg
  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN
  success = 0b
  errmsg  = ''

  IF MAX( idxs ) GE crossspec.ndet THEN BEGIN
     errmsg = "Invalid index in crossspec write"
     RETURN
  ENDIF
  wbad = WHERE( idxs LT 0, nbad )
  IF nbad NE 1 THEN BEGIN
     errmsg = "Wrong number of autospec locations found when writing cross file"
     RETURN
  ENDIF
  IF autoloc LT 0 OR autoloc GE autospec.ndet THEN BEGIN
     errmsg = "Invalid index in autospec in crossspec write"
     RETURN
  ENDIF

  ;;Don't write zero frequency as SANEPIC doesn't want it
  nspec = autospec.nfreq-1
  
  ;;Sanepic wants freqs between the places they were calculated
  df = 0.5*(autospec.freq[1]-autospec.freq[0])
  freq = DOUBLE([autospec.freq - df, autospec.freq[nspec]+df])
  
  OPENW, unit, file, /GET_LUN
  WRITEU, unit, DOUBLE( nspec )
  WRITEU, unit, TEMPORARY(freq)

  FOR i=0,N_ELEMENTS(idxs)-1 DO BEGIN
     IF idxs[i] GE 0 THEN BEGIN
        ;;Crossspec
        WRITEU, unit, DOUBLE( REFORM(crossspec.psds[idxs[i],*]))
     ENDIF ELSE BEGIN
        ;;Autospec
        WRITEU, unit, DOUBLE( REFORM(autospec.psds[autoloc,*]))
     ENDELSE
  ENDFOR
  FREE_LUN,unit
  success = 1b
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO write_sanepic_powerspec, autospec, CROSSSPEC=crossspec, OUTDIR=outdir,$
                             BASENAME=basename, VERBOSE=verbose, $
                             SUCCESS=success, ERRMSG=errmsg
  COMPILE_OPT IDL2, STRICTARRSUBS
  success = 0b
  errmsg  = ''

  IF N_ELEMENTS(autospec) EQ 0 THEN BEGIN
     errmsg = "Input autospec not given"
     GOTO, err_handler
  ENDIF
  IF SIZE(autospec,/TNAME) NE 'STRUCT' THEN BEGIN
     errmsg = "Input autospec not structure"
     GOTO, err_handler
  ENDIF

  IF N_ELEMENTS( crossspec ) NE 0 THEN BEGIN
     have_crossspec = 1b
     IF SIZE(crossspec,/TNAME) NE 'STRUCT' THEN BEGIN
        errmsg = "Input crossspec not structure"
        GOTO, err_handler
     ENDIF
  ENDIF ELSE have_crossspec = 0b

  IF N_ELEMENTS( outdir ) EQ 0 THEN outdir = './'
  IF ~ FILE_TEST( outdir, /DIRECTORY ) THEN BEGIN
     errmsg = "Outdir: "+outdir+" not a directory"
     GOTO, err_handler
  ENDIF
  IF ~ FILE_TEST( outdir, /WRITE ) THEN BEGIN
     errmsg = "Can't write to outdir: "+outdir
     GOTO, err_handler
  ENDIF

  IF N_ELEMENTS( basename ) EQ 0 THEN basename = 'spirepsd'
  
  ;;Figure out which bolometers in autospec are good
  ;;This is a bit difficult because if there is a cross-spec
  ;; any problem that involves each bolometer is an issue
  ;;Here is where we discard things that don't start with P[SML]W
  ;; (i.e., PTCP1)
  wbad = WHERE( autospec.bad OR $
                ~ STREGEX( STRMID(autospec.detnames,0,3), /BOOLEAN, /FOLD_CASE,$
                           'P[SML]W'), nbad, COMPLEMENT=wgood, $
                NCOMPLEMENT=ngood )
  IF ngood EQ 0 THEN BEGIN
     errmsg = "No good PSDs!"
     GOTO, err_handler
  ENDIF
  detnames = autospec.detnames[wgood]

  ;;For crossspec we need to know the indices into the psd array
  ;; for anything involving the good detectors
  ;;If we have cross-spec we need to build a double ended
  ;; index so we can find the cross-spectra in either order
  ;;The names are in alphabetic order from cross_spec
  IF have_crossspec THEN BEGIN
     detsort = detnames[ SORT(detnames) ]

     val1 = VALUE_LOCATE( detsort, crossspec.det1name )
     val2 = VALUE_LOCATE( detsort, crossspec.det2name )
     isbad = BYTARR( crossspec.ndet )
     wbad1 = WHERE( crossspec.det1name NE detsort[val1], nbad1 )
     wbad2 = WHERE( crossspec.det2name NE detsort[val2], nbad2 )
     IF nbad1 NE 0 THEN isbad[wbad1] = 1b
     IF nbad2 NE 0 THEN isbad[wbad2] = 1b
     wcross = WHERE( ~isbad, ncross )
     IF ncross EQ 0 THEN BEGIN
        errmsg = "No good dets in cross-spectra list"
        GOTO, err_handler
     ENDIF
     cross_index = REPLICATE( { det1: '', det2: '', idx: -1 },$
                              ncross )
     cross_index.idx = wcross
     cross_index.det1 = detsort[val1[wcross]]
     cross_index.det2 = detsort[val2[wcross]]

     ;;Now make sure these are good.  If any detector has bad any cross
     ;; spectra with another detector that is in detnames, we need
     ;; to flag that whole detector as bad.
     isgood = BYTARR( ngood, /NOZERO ) & isgood[*]=1b
     FOR m=0,ngood-1 DO BEGIN
        wdet = WHERE( cross_index.det1 EQ detnames[m] OR $
                      cross_index.det2 EQ detnames[m], ndet )
        IF ndet EQ 0 THEN BEGIN
           errmsg = "Found zero cross-spectra matching "+$
                    STRJOIN(autospec.detnames[wgood[m]],', ')
           GOTO, err_handler
        ENDIF
        wbad = WHERE( crossspec.bad[cross_index[wdet].idx], nbad )
        IF nbad NE 0 THEN isgood[m] = 0b
     ENDFOR
     wgood2 = WHERE( isgood, ngood )
     IF ngood EQ 0 THEN BEGIN
        errmsg = "No detectors with entirely good spectra!"
        GOTO, err_handler
     ENDIF
     detnames = detnames[wgood2]
     wgood = wgood[wgood2]
  ENDIF

  
  ;;Write bolometer file
  bolfile = ADDSLASH(outdir)+basename+'_dets.txt'
  IF KEYWORD_SET(verbose) AND FILE_TEST( bolfile ) THEN $
     MESSAGE,"Will overwrite: "+bolfile,/INF
  FORPRINT, detnames, TEXTOUT=bolfile, /SILENT, /NOCOMMENT
  
  ;;Write PSD files
  FOR i=0, ngood-1 DO BEGIN
     outfile = ADDSLASH(outdir)+basename+detnames[i]
     IF KEYWORD_SET(verbose) THEN BEGIN
        IF FILE_TEST( outfile ) THEN $
           MESSAGE,"Will overwrite: "+outfile,/INF ELSE $
              MESSAGE,"Writing: "+outfile,/INF
     ENDIF
     IF have_crossspec THEN BEGIN
        ;;Figure out which indexes into crossspec we want
        ;; and where the autospec should be inserted.  We need
        ;; these to come out in the same order as detnames
        shortdet = STRMID(detnames[i],3)
        wdet1 = WHERE( cross_index.det1 EQ detnames[i] AND $
                       cross_index.det2 NE detnames[i], ndet1 )
        wdet2 = WHERE( cross_index.det1 NE detnames[i] AND $
                       cross_index.det2 EQ detnames[i], ndet2 )
        IF ndet1+ndet2 EQ 0 THEN BEGIN
           errmsg = "Couldn't find "+detnames[i]
           GOTO, err_handler
        ENDIF
        bandpair = REPLICATE( { bolometer: '', idx: -1 }, ndet1+ndet2+1 )
        IF ndet1 NE 0 THEN BEGIN
           bandpair[0:ndet1-1].bolometer = cross_index[wdet1].det2
           bandpair[0:ndet1-1].idx = cross_index[wdet1].idx
        ENDIF
        IF ndet2 NE 0 THEN BEGIN
           bandpair[ndet1:ndet1+ndet2-1].bolometer = cross_index[wdet2].det1
           bandpair[ndet1:ndet1+ndet2-1].idx = cross_index[wdet2].idx
        ENDIF
        ;;Last one is the auto spec.  If we use the where's, we get it
        ;; twice
        bandpair[ndet1+ndet2].bolometer = detnames[i]
        bandpair[ndet1+ndet2].idx       = -1
        
        ;;Get into same order as detnames and clip down
        ;; Because we didn't clip cross_index down in the
        ;; cross-spectrum check, there may be extra ones here we
        ;; don't want
        wpresent = WHERE_ARRAY( detnames, bandpair.bolometer, nmatch )
        IF nmatch LT N_ELEMENTS(bandpair) THEN $
           bandpair = bandpair[wpresent]
        wpresent = WHERE_ARRAY( bandpair.bolometer, detnames )
        mord = MATCH_ORDER( detnames[wpresent], bandpair.bolometer )
        bandpair = bandpair[mord]

        ;;Make sure auto survived and get it's location in autospec
        wauto = WHERE( bandpair.bolometer EQ detnames[i], nauto )
        IF nauto NE 1 THEN BEGIN
           errmsg = "Autospec failed to survive clipping, which shouldn't"+$
                    " happen"
           GOTO, err_handler
        ENDIF
        loc = VALUE_LOCATE( autospec.detnames[autospec.detidx],detnames[i] )
        loc = autospec.detidx[loc]
        IF autospec.detnames[loc] NE detnames[i] THEN BEGIN
           errmsg = "Couldn't find "+detnames[i]+" in autospec"
           GOTO,err_handler
        ENDIF
        write_sanepic_crossspec, autospec, crossspec, bandpair.idx, loc, $
                                 outfile, SUCCESS=success_wr,$
                                 ERRMSG=errmsg
     ENDIF ELSE BEGIN
        loc = VALUE_LOCATE( autospec.detnames[autospec.detidx],detnames[i] )
        loc = autospec.detidx[loc]
        IF autospec.detnames[loc] NE detnames[i] THEN BEGIN
           errmsg = "Couldn't find "+detnames[i]+" in autospec"
           GOTO, err_handler
        ENDIF
        write_sanepic_autospec, autospec, loc, outfile, $
                                SUCCESS=success_wr,$
                                ERRMSG=errmsg
     ENDELSE
     IF success_wr EQ 0 THEN GOTO,err_handler
  ENDFOR
     
  success=1b
  RETURN

  err_handler: 
  IF KEYWORD_SET( verbose ) THEN MESSAGE,errmsg,/INF
  RETURN
  

END
