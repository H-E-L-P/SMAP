;+
; SMAP_RUNALL_ASTROM, /ONE, /TWO, /THREE, /FOUR, /FIVE, $
;                     FIELDFILE=
;
; Run SMAP_AOR_ASTROM on all fields.
;
; Options ONE, TWO, THREE, FOUR are for divided up the fields into batches,
; for processing on multiple nodes. If none are set, all segments are run.
;
;
; Optional input FIELDFILE allows specifying alternative input file.
; instead of running on all fields. Fields passed as scalar string or list 
; of strings. If set, /ONE, /TWO, etc are ignored.
;
; CREATED BY G. Marsden 2011-02-25
;
; CHANGELOG
;   20110303 (GM) change location of astromoffsets
;   20110309 (GM) change location of astrometry
;   20110315 (GM) update to L1g
;   20110321 (GM) add one/two/three/four
;   20110323 (GM) add goodrad/goodfrac params to pass through to stack_thumb
;                 change thumbsize from 41 to 35
;   20120507 (GM) add FIELDLIST && AORMAPDATE keywords
;   20150204 (AC) remove FIELDLIST && AORMAPDATE, use input file instead
;-

PRO SMAP_RUNALL_ASTROM, ONE=one, TWO=two, THREE=three, FOUR=four, $
                        FIVE=five, FIELDFILE=fieldfile
  COMPILE_OPT IDL2, STRICTARRSUBS

  aormapbasedir = "/data/spire/aormaps"
  fieldfile_def = ADDSLASH(!SMAP_PIPELINE_PATH) + "astrometry/astrom_input.txt"
  dataversion   = 'ntL1i'
  srccatdir     = "/data/spire/ancillary/cats/formatted"
  offsetsdir    = "/data/spire/astromoffsets"
  thumbsize     = 25            ; size of thumbnail map (in pixels)
  goodrad       = 4
  goodfrac      = 0.9

  ;; The settings are kept in a file giving:
  ;; Field: is the name of the output file from createmap_run_aormaps
  ;; Date: The date the aormap was made (the dir in
  ;;       /data/spire/aormaps the map you want is in for each field)
  ;; Data name: is the data dir name (e.g., the A370 part of A370_ntL1i)
  ;; segment: is which segement to run each map in -- this should be
  ;;                                                  kept as in createmap_run_aormaps

  IF N_ELEMENTS(fieldfile) EQ 0 THEN fieldfile = fieldfile_def
  READCOL, fieldfile, field, date, cat, dname, segment,$
           FORMAT='(A,A,A,A,I)', /SILENT, COMMENT='#', COUNT=nfl
  IF nfl EQ 0 THEN MESSAGE, "No data read from: " + fieldfile

  fieldind = INTARR(nfl)

  ;; turn off all segments to begin
  nseg = 5
  do_segment = BYTARR(nseg)
  IF KEYWORD_SET(one)   THEN do_segment[0] = 1B
  IF KEYWORD_SET(two)   THEN do_segment[1] = 1B
  IF KEYWORD_SET(three) THEN do_segment[2] = 1B
  IF KEYWORD_SET(four)  THEN do_segment[3] = 1B
  IF KEYWORD_SET(five)  THEN do_segment[4] = 1B
  
  ;; if none turned on, do all
  IF TOTAL(do_segment) EQ 0 THEN do_segment = REPLICATE(1B, nseg)

  ;; loop over segments
  FOR iseg=0,nseg-1 DO BEGIN
     IF do_segment[iseg] EQ 0 THEN CONTINUE
     
     find = WHERE(segment EQ iseg, nf)
     IF nf EQ 0 THEN CONTINUE
     
     ;; loop over fields
     FOR i=0,nf-1 DO BEGIN
        curr_idx = find[i]
        curr_field = field[curr_idx]
        curr_date = date[curr_idx]
        mapdir = ADDSLASH(aormapbasedir) + ADDSLASH(curr_date) + curr_field
        srccatfile = ADDSLASH(srccatdir) + cat[curr_idx]
        outfile = ADDSLASH(offsetsdir) + dname[curr_idx] + "_" + dataversion + ".dat"
        
        SMAP_AOR_ASTROM, mapdir, srccatfile, outfile, thumbsize, $
                         GOODRAD=goodrad, GOODFRAC=goodfrac
        
     ENDFOR
  ENDFOR
  
  MESSAGE, "Done processing astrometry", /INF

END
