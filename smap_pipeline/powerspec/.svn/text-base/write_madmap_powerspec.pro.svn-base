;+
;NAME
; write_madmap_powerspec
;PURPOSE
; Writes PSDs (auto only) in format for export to MADMap
;USAGE
; write_madmap_powerspec, autospec, outfile
;INPUTS
; autospec        Auto spectrum (see get_auto_psd_from_l1 and read_powerspec)
;SIDE EFFECTS
; Writes an output file which is a fits binary table with the
; following entries:
;   .nfreq    The number of frequencies
;   .npsw     Number of PSW bolometers + 1 (the +1 for freq)
;   .npmw     Number of PMW bolometers + 1
;   .nplw     Number of PLW bolometers + 1
;   .psw_col  A string array of PSW names, starting with 'freq' of
;              length .npsw
;   .pmw_col  Same, but for PMW
;   .plw_col  Same, but for PLW
;   .psw_psd  A nfreq x npsw array of PSW PSDs in Jy/Hz^{1/2}
;   .pmw_psd  Same for PMW
;   .plw_psd  Same for PLW
;MODIFICATION HISTORY
; Author: Alex Conley, Sep 2009
;-

PRO write_madmap_powerspec, autospec, outfile,VERBOSE=verbose, $
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

  IF N_ELEMENTS(outfile) EQ 0 THEN BEGIN
     errmsg = "No outfile provided"
     GOTO, err_handler
  ENDIF
  IF SIZE( outfile, /TNAME ) NE 'STRING' THEN BEGIN
     errmsg = "Expected string for outfile, got: "+SIZE(outfile,/TNAME)
     GOTO, err_handler
  ENDIF
  IF KEYWORD_SET(verbose) && FILE_TEST(outfile) THEN $
     MESSAGE,"Will overwrite "+outfile,/INF

  ;;Figure out the number of each band
  wpsw = WHERE( STREGEX( autospec.detnames, '^PSW', /BOOLEAN, /FOLD_CASE), npsw )
  wpmw = WHERE( STREGEX( autospec.detnames, '^PMW', /BOOLEAN, /FOLD_CASE), npmw )
  wplw = WHERE( STREGEX( autospec.detnames, '^PLW', /BOOLEAN, /FOLD_CASE), nplw )
  IF KEYWORD_SET( verbose ) THEN $
     MESSAGE,STRING(npsw,npmw,nplw,FORMAT='("Have ",I0," PSW ",I0," PMW ",'+$
                    'I0," PLW dets")'),/INF
  
  IF npsw EQ 0 THEN BEGIN
     errmsg = "No PSW detectors found!"
     GOTO, err_handler
  ENDIF
  IF npmw EQ 0 THEN BEGIN
     errmsg = "No PMW detectors found!"
     GOTO, err_handler
  ENDIF
  IF nplw EQ 0 THEN BEGIN
     errmsg = "No PLW detectors found!"
     GOTO, err_handler
  ENDIF

  ;;Make output struct
  outstruct = { nfreq: autospec.nfreq, npsw: npsw+1, npmw: npmw+1, nplw: nplw+1,$
                psw_col: STRARR( npsw+1 ), pmw_col: STRARR( npmw + 1 ),$
                plw_col: STRARR( nplw+1 ), $
                psw_psd: DBLARR( npsw+1, autospec.nfreq ),$
                pmw_psd: DBLARR( npmw+1, autospec.nfreq ),$
                plw_psd: DBLARR( nplw+1, autospec.nfreq ),$
                freq_units: 'Hz', psd_units: 'Jy/Sqrt(Hz)'}

  outstruct.psw_col[0] = 'freq'
  outstruct.psw_col[1:*] = autospec.detnames[wpsw]
  outstruct.pmw_col[0] = 'freq'
  outstruct.pmw_col[1:*] = autospec.detnames[wpmw]
  outstruct.plw_col[0] = 'freq'
  outstruct.plw_col[1:*] = autospec.detnames[wplw]

  outstruct.psw_psd[0,*] = autospec.freq
  outstruct.pmw_psd[0,*] = autospec.freq
  outstruct.plw_psd[0,*] = autospec.freq
  outstruct.psw_psd[1:*,*] = SQRT(autospec.psds[wpsw,*]) ;;Jy^2/Hz->Jy/SQRT(Hz)
  outstruct.pmw_psd[1:*,*] = SQRT(autospec.psds[wpmw,*])
  outstruct.plw_psd[1:*,*] = SQRT(autospec.psds[wplw,*])

  ;;Prepare primary header
  MKHDR, base_hdr, '', /EXTEND
  SXADDPAR,base_hdr,'CHUNKSIZ',autospec.chunksize,'Chunk size used',FORMAT='(I)'
  SXADDPAR,base_hdr,'MNDATLEN',autospec.mindatalength,$
           'Minimum length data sample',FORMAT='(I)'
  IF autospec.windowing THEN BEGIN
     SXADDPAR,base_hdr,'WINDOW','T','Window function used'
     SXADDPAR,base_hdr,'WINTYP',autospec.windowtype,'Window function used'
  ENDIF ELSE SXADDPAR,base_hdr,'WINDOW','F','Window function used'
  IF autospec.polyremove THEN BEGIN
     SXADDPAR,base_hdr,'POLYRMV','T','Polynomial removal'
     SXADDPAR,base_hdr,'POLYORDR',autospec.polyorder,'Polynomial order'
  ENDIF ELSE BEGIN
     SXADDPAR,base_hdr,'POLYRMV','F','Polynomial removal'
  ENDELSE
  ;;Z is hexadecimal
  SXADDPAR,base_hdr,'BADMASK',autospec.badmask,FORMAT='(Z)','Mask fail pattern'
  SXADDPAR,base_hdr,'COMMENT',$
           'This is a header for an SMAP pipeline product.'
  SXADDPAR,base_hdr,'TIMESYS','UTC     ','All dates are in UTC time'
  SXADDPAR,base_hdr,'CREATOR',getenv('USER'),'User who created this file'
  SXADDPAR,base_hdr,'TELESCOP','Herschel','Name of the telescope'
  SXADDPAR,base_hdr,'INSTRUME','SPIRE','Name of the instrument'
  SXADDPAR,base_hdr,'DESC','AutoPSD','Description of file contents'
  SXADDPAR, base_hdr, 'HISTORY', 'Created by write_madmap_powerspec.pro'
  SXADDPAR, base_hdr, 'HISTORY', ' on '+systime()

  ;;Write actual file
  IF FILE_TEST( outfile ) THEN FILE_DELETE,outfile
  ;;Make initial extension
  WRITEFITS, outfile, 0, base_hdr, /CHECKSUM

  ;;PSD header
  FXBHMAKE,psd_hdr, N_ELEMENTS(outstruct), 'PSD', 'Auto-PSDs'
  ;;Append psd
  write_status = 0b
  MWRFITS, outstruct, outfile, psd_hdr, /SILENT, STATUS=write_status
  IF write_status NE 0 THEN BEGIN
     errmsg = "Unable to add PSD to outfile: "+outfile
     GOTO, err_handler
  ENDIF

  success=1b
  RETURN

  err_handler: 
  IF KEYWORD_SET( verbose ) THEN MESSAGE,errmsg,/INF
  RETURN
  

END
