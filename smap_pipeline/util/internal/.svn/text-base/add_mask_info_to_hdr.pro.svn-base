;+
; NAME
;  add_mask_info_to_hdr
; CATEGORY
;  Herschel SPIRE SMAP pipeline
; PURPOSE
;  Take the structure from get_mask_bits and add it to a new header,
;   following HCSS conventions
; CALLING SEQUENCE
;   add_mask_info_to_hdr, header, maskinfo
; INPUTS
;   header        FITS header to modify.
;   maskinfo      Mask information structure from something like 
;                  get_mask_bits
; OPTIONAL OUTPUTS
;   errmsg        Error message on failure
;   success       1 on success, 0 on failure
; KEYWORDS
;   verbose       Run in verbose mode
; MODIFICATION HISTORY
;   Author: Alex Conley, May 2009
;-

PRO add_mask_info_to_hdr, header, maskinfo, ERRMSG=errmsg,$
                          SUCCESS=success, VERBOSE=verbose

  COMPILE_OPT IDL2
  success = 0b

  int_maskinfo = maskinfo

  ;;Make sure we have a string array
  IF SIZE(header,/TNAME) NE 'STRING' THEN BEGIN
     errmsg = "Input header is not string type"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN
  ENDIF
  IF ( SIZE(header) )[0] EQ 0 THEN BEGIN
     errmsg = "Input header is not array"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN
  ENDIF

  IF SIZE(int_maskinfo,/TNAME) NE 'STRUCT' THEN BEGIN
     errmsg = "Input maskinfo is not structure"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN
  ENDIF
  IF ~ (TAG_EXIST(int_maskinfo,'name',/TOP_LEVEL) AND $
        TAG_EXIST(int_maskinfo,'bits',/TOP_LEVEL) AND $
        TAG_EXIST(int_maskinfo,'comment',/TOP_LEVEL) ) THEN BEGIN
     errmsg = "Input maskinfo does not have required structure tags"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN
  ENDIF
  
  ;;Warn user if any mask bits aren't a power of 2
  bits = int_maskinfo.bits
  wbad = WHERE( bits AND (bits-1), nbad )
  IF nbad NE 0 THEN BEGIN
     mssg = "WARNING: the bits corresponding to "+$
            STRJOIN( int_maskinfo[wbad].name,', ' )+" are not powers of 2"
     MESSAGE,mssg,/INF
  ENDIF

  ;;See if the header already has mask information
  current_maskinfo = get_mask_bits( header, SUCCESS=curr_success )
  IF curr_success THEN BEGIN
     ;;We do have some.  This is messy
     ;;Four possiblities:
     ;; 1) The new bits are a superset of the old.  Then just add the
     ;;     new
     ;; 2) The new bits are a subset of the old.  Do nothing
     ;; 3) They disagree.  Then throw up our hands and give up
     ;; 4) They are disjoint.  This is ok, we can proceed as if there
     ;;    weren't any before

     ;;Make sure they are arrays
     IF N_ELEMENTS(int_maskinfo) EQ 1 THEN $
        mbits = [int_maskinfo.bits] ELSE mbits=int_maskinfo.bits
     IF N_ELEMENTS(current_maskinfo) EQ 1 THEN $
        cbits = [current_maskinfo.bits] ELSE cbits=current_maskinfo.bits

     wbitmatch1 = WHERE_ARRAY( cbits, mbits, count1 )
     wbitmatch2 = WHERE_ARRAY( mbits, cbits, count2 )
     IF wbitmatch1[0] EQ -2 OR wbitmatch2[0] EQ -2 THEN BEGIN
        errmsg = "Input match has wrong type for bits"
        IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
        RETURN
     ENDIF
     IF count1 NE 0 THEN BEGIN
        ;;There are at least some bits in int_maskinfo that already exist
        ;; in current_maskinfo.  Check to see that they have the same
        ;; meaning
        wtmp = WHERE_ARRAY( mbits[wbitmatch1], cbits )
        wbad = WHERE( int_maskinfo[wbitmatch1].name NE $
                      current_maskinfo[wtmp].name,$
                      nbad )
        IF nbad NE 0 THEN BEGIN
           errmsg = "Header already has a mask that disagrees with yours"
           IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
           RETURN
        ENDIF
        IF N_ELEMENTS(wbitmatch1) EQ N_ELEMENTS(int_maskinfo) THEN BEGIN
           ;;So now we know that every element of int_maskinfo is found
           ;; in current_maskinfo and they have the same meaning
           ;;So, this is easy: do nothing
           success=1b
           RETURN
        ENDIF ELSE BEGIN
           ;;There's at least one element in int_maskinfo NOT in 
           ;; current_maskinfo.  We need to clip down
           missing_idx = MISSING( cbits, mbits, nmissing )
           IF nmissing EQ 0 THEN BEGIN
              errmsg = "Inconsistency in mask calculations.  This is a bug."
              errmsg = " Please complain to someone"
              IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
              RETURN
           ENDIF
           int_maskinfo = int_maskinfo[missing_idx]
           IF N_ELEMENTS(int_maskinfo) EQ 1 THEN $
              mbits = [int_maskinfo.bits] ELSE mbits=int_maskinfo.bits
           wbitmatch2 = WHERE_ARRAY( mbits, cbits, count2 )
        ENDELSE
     ENDIF
     IF count2 NE 0 THEN BEGIN
        ;;There are at least some bits in current_maskinfo that already exist
        ;; in maskinfo.  Check to see that they have the same
        ;; meaning
        wtmp = WHERE_ARRAY( cbits[wbitmatch2], mbits )
        wbad = WHERE( current_maskinfo[wbitmatch2].name NE $
                      int_maskinfo[wtmp].name,$
                      nbad )
        IF nbad NE 0 THEN BEGIN
           errmsg = "Header already has a mask that disagrees with yours"
           IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
           RETURN
        ENDIF
        
        IF N_ELEMENTS(wbitmatch2) EQ N_ELEMENTS(int_maskinfo) THEN BEGIN
           ;;So now we know that every element of curr_maskinfo is found
           ;; in int_maskinfo and they have the same meaning
           ;;We need to pare down int_maskinfo to only the new ones
           missing_idx = MISSING( cbits, mbits, nmissing )
           IF nmissing EQ 0 THEN BEGIN
              errmsg = "Inconsistency in mask calculations.  This is a bug."
              errmsg = " Please complain to someone"
              IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
              RETURN
           ENDIF
           int_maskinfo = int_maskinfo[missing_idx]
        ENDIF

     ENDIF
  ENDIF

  ;;See how many Metas we already have
  ;;start will be the first one we insert
  wmeta = WHERE(STRMID(header,0,4) EQ 'META',nmeta)
  IF nmeta NE 0 THEN BEGIN
     ;;Already have some, see if they are META_number
     tags = STRMID(header[wmeta],0,8)
     pos = RSTRPOS( tags,'_' )
     wnum = WHERE( pos NE -1, nnum )
     IF nnum NE 0 THEN BEGIN
        numbers = FIX( STRMID( tags, REFORM(pos[wnum]+1,1,nnum) ) )
        start = MAX(numbers)+1 ;;start one higher
     ENDIF ELSE start=1
  ENDIF ELSE start=1

  ;;Add them to header
  ;;Two things to set: 1) a META_ header giving the mask bits and
  ;;comment 2) a HIERARCH key.META_?? giving the name as a string
  nmaskbits = N_ELEMENTS(int_maskinfo)
  IF start+nmaskbits GE 1000 THEN BEGIN
     errmsg = "Too many mask bits to add to header"
     IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
     RETURN
  ENDIF
  
  metanames = STRING(LINDGEN(nmaskbits)+start,FORMAT='("META_",I0)')
  IF start NE 1 THEN BEGIN
     prevname = STRING(start-1,FORMAT='("META_",I0)')
     HERSCHEL_SXADDPAR,header,metanames[0],int_maskinfo[0].bits,$
                       " [] "+int_maskinfo[0].comment,$
                       AFTER=prevname
  ENDIF ELSE BEGIN
     HERSCHEL_SXADDPAR,header,metanames[0],int_maskinfo[0].bits,$
                       " [] "+int_maskinfo[0].comment
  ENDELSE
  FOR i=1,nmaskbits-1 DO BEGIN
     ;;Dunno why, but HCSS likes the []
     HERSCHEL_SXADDPAR,header,metanames[i],int_maskinfo[i].bits,$
                       " [] "+int_maskinfo[i].comment,$
                       AFTER=metanames[i-1]
  ENDFOR
  FOR i=0,nmaskbits-1 DO BEGIN
     outval = '  key.'+metanames[i]+"='"+int_maskinfo[i].name+"'"
     HERSCHEL_SXADDPAR,header,'HIERARCH',outval
  ENDFOR

  success=1b
  RETURN
END
