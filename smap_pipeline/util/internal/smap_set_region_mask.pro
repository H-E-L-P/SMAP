;+
;NAME
;  smap_set_region_mask
;CATEGORY
;  Herschel SPIRE SMAP pipeline
;PURPOSE
;  Set mask bits for all points within a certain radius of
;  specified location(s).  Good for masking bright objects
;CALLING SEQUENCE
;  smap_set_region_mask, tods, masknames, ra, dec, radii, $
;                        POLYGON=polygon, OBSID=obsid
;REQUIRED ARGUMENTS
; tods            SMAP tods (from smap_getlevel1).  Either an
;                  array of pointers, or a list of filenames.  Will
;                  be modified on output, either by modifying in place
;                  (if pointers or struct) or on disk (if strings)
; masknames       Mask names to set at points satisfying criterion.
; ra/dec          RA/DEC values to mask around.  Can be decimal
;                  degrees or strings in h:m:s/d:m:s format.  Can be
;                  arrays.
; radii           Radii around RA/DEC to mask, in arcseconds.  Must
;                  have same number of elements as ra/dec
;
;KEYWORDS
; polygon         if set, ra/dec treated as coordinates of polygon to mask
;                 radii is ignored
; obsid           if set, only tod with id EQ obsid will be masked (string)
; dotherms        Add the therms to the list of masked channels
;NOTES
; This will set the flag in all light bolometers in all bands
;MODIFICATION HISTORY
; Author: Alex Conley, July 22, 2011
;
;CHANGELOG
; 20120723 gm: add POLYGON option
; 20120724 gm: allow tods to be a struct
;-

PRO smap_set_region_mask, tods, masknames, ra, dec, radii, $
                          POLYGON=polygon, OBSID=obsid, DOTHERMS=dotherms
  COMPILE_OPT IDL2, STRICTARRSUBS

  do_poly = KEYWORD_SET(polygon)

  ntods = N_ELEMENTS(tods)
  IF ntods EQ 0 THEN RETURN

  IF N_ELEMENTS(masknames) EQ 0 THEN BEGIN
     MESSAGE,"WARNING: no masknames provided, doing nothing",/INF
     RETURN
  ENDIF

  npos = N_ELEMENTS(ra)
  IF npos EQ 0 THEN BEGIN
     MESSAGE,"WARNING: No ra/dec values provided, doing nothing",/INF
     RETURN
  ENDIF
  IF npos NE N_ELEMENTS(dec) THEN $
     MESSAGE,"ra and dec must be same length"
  IF do_poly EQ 0 && npos NE N_ELEMENTS(radii) THEN $
     MESSAGE,"ra and dec must be same length"

  ; 
  tod_stype = SIZE(tods,/TNAME)
  string_type  = 1
  pointer_type = 2
  struct_type  = 3
  CASE tod_stype OF
      'STRING':  tod_type = string_type
      'POINTER': tod_type = pointer_type
      'STRUCT':  tod_type = struct_type
      ELSE : MESSAGE,"Input TODs of unexpected type "+tod_stype
  ENDCASE

  ;; convert string coords if necessary
  cra  = ra
  cdec = dec
  IF SIZE(cra,/TNAME) EQ 'STRING' THEN cra = ra_to_radeg(cra)
  IF SIZE(cdec,/TNAME) EQ 'STRING' THEN cdec = dec_to_decdeg(dec)

  IF do_poly EQ 1 THEN BEGIN
     ;; validate inputs
     ;; for now, assume delta_ra range is <30 degrees

     ;; try to unwrap around ra=0
     IF MAX(ra) - MIN(ra) GT 30.0 THEN BEGIN
        cra = ((cra MOD 360) + 360) MOD 360 ; force 0<=ra<360
        cra = ((cra + 180) MOD 360) - 180   ; force -180<=ra<180
     ENDIF

     ;; if still too large, error
     IF MAX(cra) - MIN(cra) GT 30.0 THEN $
        MESSAGE, "ERROR: poly ra points extend more than 30.0"

  ENDIF

  therms = ['PSWT1', 'PSWT2', 'PMWT1', 'PMWT2', 'PLWT1', 'PLWT2']
  ;;Loop over tods
  FOR i=0, ntods-1 DO BEGIN
     CASE tod_type OF
        string_type: BEGIN
           ctod = SMAP_READTOD(tods[i], SUCCESS=rd_succ, ERRMSG=rd_errmsg)
           IF rd_succ EQ 0 THEN $
              MESSAGE,"ERROR reading tod "+tods[i]+" : "+rd_errmsg
        
           tod_obsid = ctod.obsid
           tod_ra  = ctod.ra
           tod_dec = ctod.dec
           mbits = ctod.mask_bits
           nchans = ctod.nchans
           bolmask = BYTARR(nchans) ;; list of bols we bother with
           bolmask[WHERE(ctod.islight)] = 1b
           IF KEYWORD_SET(dotherm) THEN BEGIN
              FOR j = 0, N_ELEMENTS(therms) - 1 DO BEGIN
                 wth = WHERE(ctod.chan EQ therms[i], nth)
                 IF nth GT 0 THEN bolmask[wth] = 1b
              ENDFOR
           ENDIF
        END
        pointer_type: BEGIN
           IF ~ TAG_EXIST((*tods[i]), 'mask_bits', /TOP ) THEN $
              MESSAGE,"TODs must have mask_bits information"
           tod_obsid = (*tods[i]).obsid
           tod_ra  = (*tods[i]).ra
           tod_dec = (*tods[i]).dec
           mbits = (*tods[i]).mask_bits
           nchans = (*tods[i]).nchans
           bolmask = BYTARR(nchans) ;; list of bols we bother with
           bolmask[WHERE((*tods[i]).islight)] = 1b
           IF KEYWORD_SET(dotherm) THEN BEGIN
              FOR j = 0, N_ELEMENTS(therms) - 1 DO BEGIN
                 wth = WHERE((*tods[i]).chan EQ therms[i], nth)
                 IF nth GT 0 THEN bolmask[wth] = 1b
              ENDFOR
           ENDIF
        END
        struct_type: BEGIN
           ; copy pointer_type, but without dereference
           IF ~ TAG_EXIST((tods[i]), 'mask_bits', /TOP) THEN $
              MESSAGE,"TODs must have mask_bits information"
           tod_obsid = tods[i].obsid
           tod_ra  = tods[i].ra
           tod_dec = tods[i].dec
           mbits = tods[i].mask_bits
           nchans = tods[i].nchans
           bolmask = BYTARR(nchans) ;; list of bols we bother with
           bolmask[WHERE(tods[i].islight)] = 1b
           IF KEYWORD_SET(dotherm) THEN BEGIN
              FOR j = 0, N_ELEMENTS(therms) - 1 DO BEGIN
                 wth = WHERE(tods[i].chan EQ therms[i], nth)
                 IF nth GT 0 THEN bolmask[wth] = 1b
              ENDFOR
           ENDIF
        END
     ENDCASE
           

     IF KEYWORD_SET(obsid) && tod_obsid NE obsid THEN CONTINUE

     ;;Get bits to set
     setbits = construct_mask_bitmask(masknames, mbits, SUCCESS=msuccess,$
                                      ERRMSG=merrmsg)
     IF msuccess EQ 0 THEN $
        MESSAGE,"ERROR building mask bits: "+merrmsg
     IF setbits EQ 0 THEN CONTINUE ;;none to set!

     IF do_poly EQ 0 THEN BEGIN
         ;; mask circles
         ;Loop over positions
         FOR j=0, npos-1 DO BEGIN
             crad = radii[j]
             IF ~ FINITE(crad) THEN BEGIN
                 MESSAGE,"WARNING: skipping non finite radius",/INF
                 CONTINUE
             ENDIF
             IF crad LE 0.0 THEN BEGIN
                 MESSAGE,"WARNING: skipping non positive radius",/INF
                 CONTINUE
             ENDIF

             IF ~ FINITE(cra[j]) THEN BEGIN
                 MESSAGE,"WARNING: Skipping non-valid ra location",/INF
                 CONTINUE
             ENDIF
             IF ~ FINITE(cdec[j]) THEN BEGIN
                 MESSAGE,"WARNING: Skipping non-valid dec location",/INF
                 CONTINUE
             ENDIF
        
             ;;Distances
             GCIRC, 2, cra[j], cdec[j], tod_ra, tod_dec, dist
             wmask = WHERE(dist LE crad, nmask)
             IF nmask EQ 0 THEN CONTINUE

             ;;Now mask.  Recall that, if an array
             ;; a is n x m, then a[k] = a[k mod n, k / n ]
             ;;So, in this case, wmask mod nchans is the bolometer
             ;; index of the channels that will have some masking
             bol_masked = wmask MOD nchans ;; note channels can repeat
             ;; wmask2 is an index into bolometers of things we will do
             wmask2 = WHERE(bolmask[TEMPORARY(bol_masked)], nmask2, $
                            NCOMPLEMENT=nnot)
             ;; Clip out unwanted channels (no light, etc.)
             IF nnot NE 0 THEN wmask = wmask[TEMPORARY(wmask2)]
             CASE tod_type OF
                string_type: BEGIN
                   IF nmask2 GT 0 THEN BEGIN
                      ctod.mask[wmask] OR= setbits
                   ENDIF
                END
                pointer_type: BEGIN
                   IF nmask2 GT 0 THEN BEGIN
                      (*tods[i]).mask[wmask] OR= setbits
                   ENDIF
                END
                struct_type: BEGIN
                   IF nmask2 GT 0 THEN BEGIN
                      tods[i].mask[wmask] OR= setbits
                   ENDIF
                END
             ENDCASE
         ENDFOR ;;pos loop
     ENDIF ELSE BEGIN
         ; mask polygon

        ; test if cra has neg. values
        ; if so, wrap tod_ra around
        IF MIN(cra) LT 0 THEN BEGIN
           tod_ra = ((tod_ra MOD 360) + 360) MOD 360 ; force 0<=ra<360
           tod_ra = ((tod_ra + 180) MOD 360) - 180   ; force -180<=ra<180
        ENDIF

        wmask = WHERE(POLYWIND(tod_ra, tod_dec, cra, cdec) NE 0, nmask)
        IF nmask GT 0 THEN BEGIN

           ;; copy this part from above
           bol_masked = wmask MOD nchans ;; note channels can repeat
           wmask2 = WHERE(bolmask[TEMPORARY(bol_masked)], nmask2, $
                          NCOMPLEMENT=nnot)
           IF nnot NE 0 THEN wmask = wmask[TEMPORARY(wmask2)]
           CASE tod_type OF
              string_type: BEGIN
                 IF nmask2 GT 0 THEN BEGIN
                    ctod.mask[wmask] OR= setbits
                 ENDIF
              END
              pointer_type: BEGIN
                 IF nmask2 GT 0 THEN BEGIN
                    (*tods[i]).mask[wmask] OR= setbits
                 ENDIF
              END
              struct_type: BEGIN
                 IF nmask2 GT 0 THEN BEGIN
                    tods[i].mask[wmask] OR= setbits
                 ENDIF
              END
           ENDCASE
         ENDIF
     ENDELSE

     IF tod_type EQ string_type THEN BEGIN
        ;;Have to re-write
        smap_writetod, ctod, tods[i], /NO_ABORT, ERRMSG=werrmsg,$
                       SUCCESS=wsuccess
        IF wsuccess EQ 0 THEN $
           MESSAGE,"ERROR writing out modified "+tods[i]
     ENDIF

  ENDFOR ;; tod loop

END
