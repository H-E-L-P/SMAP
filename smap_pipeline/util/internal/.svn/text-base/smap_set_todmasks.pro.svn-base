;+
;NAME
;  smap_set_todmasks
;CATEGORY
;  Herschel SPIRE SMAP pipeline
;PURPOSE
;  Wrapper function to SMAP_SET_REGION_MASK using todmask data structures
;
;  Format of todmask structure:
;    {obsid:"", type:"", params:PTR_NEW([parameters])}
;    obsid:  obsid to be masked. Can be "*" for all obsids
;    type:   circ or poly. See below.
;    params: ptr to array of parameters. Meaning of params depends on type.
;
;  Allowed types of masks:
;    circ:  mask a circular region
;           parameters are [ra_cen, dec_cen, rad], all in degrees
;
;    poly:  mask a polygon
;           params are [ra1,dec1,...,raN,decN] of N vertices, all in deg
;
;CALLING SEQUENCE
;  smap_set_todmasks, tods, todmask
;REQUIRED ARGUMENTS
; tods            SMAP tods (from smap_getlevel1).  Either an
;                  array of pointers, or a list of filenames.  Will
;                  be modified on output, either by modifying in place
;                  (if pointers) or on disk (if strings)
; todmask         array of structures indicating mask regions. See above.
;
;NOTES
; This will set the flag in all light bolometers in all bands
;
;MODIFICATION HISTORY
; Author: Gaelen Marsden, July 23, 2012
;
;CHANGELOG
; 20120724 gm: combine todmaskcirc and todmaskpoly into todmask
;-

PRO smap_set_todmasks, tods, todmask, VERBOSE=verbose, SUCCESS=success
  COMPILE_OPT IDL2, STRICTARRSUBS

  success = 0B

  maskname = 'maskManual'

  ntodmask = N_ELEMENTS(todmask)
  FOR i=0,ntodmask-1 DO BEGIN

     IF todmask[i].obsid EQ "*" THEN obsid = "" $
     ELSE obsid = todmask[i].obsid
   
     CASE todmask[i].type OF

        "circ": BEGIN
           ;; NOTE: could do this by grouping by obsids and passing arrays of
           ;; coords to SMAP_SET_REGION_MASK, but I don't think that would
           ;; gain us anything
      
           npars = N_ELEMENTS(*todmask[i].params)
           IF npars NE 3 THEN BEGIN
              MESSAGE, "todmask type 'circ' must have 3 parameters"
              RETURN
           ENDIF
           
           ra_c  = (*todmask[i].params)[0]
           dec_c = (*todmask[i].params)[1]
           rad   = (*todmask[i].params)[2] * 3600.0 ; convert to arcsec
           SMAP_SET_REGION_MASK, tods, maskname, ra_c, dec_c, rad, OBSID=obsid
        END

        "poly": BEGIN
           npars = N_ELEMENTS(*todmask[i].params)
           IF npars MOD 2 NE 0 THEN BEGIN
              MESSAGE, "todmask type 'poly' must have even number of  params"
              RETURN
           ENDIF

           vert_ra  = (*todmask[i].params)[2 * INDGEN(npars/2)]
           vert_dec = (*todmask[i].params)[2 * INDGEN(npars/2) + 1]
           SMAP_SET_REGION_MASK, tods, maskname, vert_ra, vert_dec, $
                                OBSID=obsid, /POLYGON
        END

        ELSE: BEGIN
           MESSAGE, "unknown mask type '"+todmask[i].type+"'", /INF
           RETURN
        END

     ENDCASE

  ENDFOR

  success = 1B

END
