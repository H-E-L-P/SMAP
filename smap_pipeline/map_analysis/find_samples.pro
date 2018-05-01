;+
;NAME
; find_samples
;PURPOSE
; For a set of TODs, find which data samples are close to a given
; sky position in a given band
;USAGE
; samples = find_samples(tods,band,ra,dec [, RADIUS=, EXCLUDEMASK=, /VERBOSE])
;INPUTS
; tods        Array of TODs, either pointers or filenames
; band        Band -- either 'PSW', 'PMW', or 'PLW'
; ra          RA of position.  Either decimal degrees or a string in hms
; dec         DEC of position.  Either decimal degrees or a string in dms
;OPTIONAL INPUTS
; radius      Radius of samples considered to match, in arcsec.
;              The default is 15 arcsec.
; excludemask String array of mask bits to exclude (like in
;              smap_itermap).  Masked samples will not be included in
;              output.
;KEYWORDS
; verbose     Print %completed information
;RETURNS
; A structure containing information about which samples match the
; input criterion.  The structure contains:
;   .nsamples        Number of samples
;   .nfiles          Number of files
;   .filenames       Short filename array
;   .bbids           BBID array (same order as filename)
;   .obsids          OBSID array (same order as filename)
;   .nbols           Number of bolometers
;   .bolometers      Bolometer names
;   .samples         A structure array identifying the matching
;                     samples:
;                       .fileidx: Index into file/bbid/obsid arrays
;                       .bolindex: Index into bolometer array
;                       .sample: Which timesample matches
; Note that there is no guarantee that bolometers are in the same
; order in different TODs -- therefore, when trying to find the
; matching samples for a particular file, make sure to use the bolometer
; name to identify where to look.
;MODIFICATION HISTORY
; Author: Alex Conley, April 14, 2011
;-

FUNCTION find_samples,tods,band,ra,dec,RADIUS=radius,VERBOSE=verbose,$
                      EXCLUDEMASK=excludemask
  COMPILE_OPT IDL2, STRICTARRSUBS

  ntods   = N_ELEMENTS(tods)
  IF ntods EQ 0 THEN MESSAGE,"No TODs provided"
  tod_type = SIZE(tods,/TNAME)
  IF SIZE(ra,/TNAME) EQ 'STRING' THEN ra_used = ra_to_radeg(ra) ELSE $
     ra_used = ra
  IF SIZE(dec,/TNAME) EQ 'STRING' THEN dec_used = dec_to_decdeg(dec) ELSE $
     dec_used = dec
  IF N_ELEMENTS(radius) EQ 0 THEN radius = 16.0

  ;;Take first TOD to set up list of bolometers, etc.
  IF tod_type EQ 'STRING' THEN BEGIN
     curr_tod = smap_readtod(tods[0], SUCCESS=rsuccess, ERRMSG=errmsg)
     IF rsuccess EQ 0 THEN $
        MESSAGE,"Error reading tod from "+tods[0]+": "+errmsg
  ENDIF ELSE IF tod_type EQ 'POINTER' THEN BEGIN
     curr_tod = *tods[0]
  ENDIF ELSE IF tod_type EQ 'STRUCT' AND ntods EQ 1 THEN BEGIN
     curr_tod = tods
  ENDIF ELSE MESSAGE,"tods not what was expected"
  
  chans = curr_tod.chan[ SORT(curr_tod.chan) ]
  sampleinfo = { nsamples: 0L, nfiles: ntods, $
                 filenames: STRARR(ntods),$
                 bbids: ULONARR(ntods), obsids: ULONARR(ntods),$
                 nbols: N_ELEMENTS(chans), bolometers: TEMPORARY(chans) }

  ;;Main loop -- read in each TOD, find the samples, store them
  sample = { fileidx: 0U, bolidx: 0U, samp: 0U }
  FOR i=0,ntods-1 DO BEGIN

     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,STRING(i,100.0*i/(ntods-1),$
                       FORMAT='(" On scan ",I4," [",F5.1,"%]")'),/INF

     ;;Get current tod
     IF tod_type EQ 'STRING' THEN BEGIN
        curr_tod = smap_readtod(tods[i], SUCCESS=rsuccess, ERRMSG=errmsg)
        IF rsuccess EQ 0 THEN $
           MESSAGE,"Error reading tod from "+tods[i]+": "+errmsg
     ENDIF ELSE IF tod_type EQ 'POINTER' THEN BEGIN
        curr_tod = *tods[i]
     ENDIF ELSE curr_tod = tods ;;single structure case

     sampleinfo.filenames[i] = curr_tod.shortfile
     sampleinfo.bbids[i]     = curr_tod.bbid
     sampleinfo.obsids[i]    = curr_tod.obsid

     ;;Build mask
     IF TAG_EXIST(curr_tod, 'mask_bits', /TOP_LEVEL) AND $
        N_ELEMENTS(excludemask) GT 0 THEN BEGIN
        mapmaskbits = construct_mask_bitmask(excludemask, curr_tod.mask_bits)
        do_mask = 1b
     ENDIF ELSE do_mask = 0b

     ;;Figure out which bolos to search
     boloind = WHERE( STRMID(curr_tod.chan,0,3) EQ band, nbolo )
     IF nbolo EQ 0 THEN CONTINUE

     FOR j=0,nbolo-1 DO BEGIN
        this_boloind = boloind[j]
        this_bolo    = curr_tod.chan[this_boloind]
        ;;Get distance
        GCIRC,2,ra_used,dec_used,curr_tod.ra[this_boloind,*],$
              curr_tod.dec[this_boloind,*],dist
        wflag = WHERE( dist LE radius, nflag )
        IF nflag EQ 0 THEN CONTINUE

        IF do_mask THEN BEGIN
           wgood = WHERE( (curr_tod.mask[this_boloind,wflag] AND $
                           mapmaskbits) EQ 0, nflag )
           IF nflag EQ 0 THEN CONTINUE
           wflag = wflag[wgood]
        ENDIF

        ;;Store masked sample info
        chanpos = VALUE_LOCATE(sampleinfo.bolometers, this_bolo )
        IF (chanpos LT 0) || (chanpos GE sampleinfo.nbols) || $
           (sampleinfo.bolometers[chanpos] NE this_bolo) THEN $
              MESSAGE,"Could not find current bolometer "+$
                      curr_tod.chan[i]+" in list of bolometers"+$
                      " from first TOD "+sampleinfo.filenames[0]

        newsample = REPLICATE(sample,nflag)
        newsample.fileidx   = i
        newsample.bolidx    = chanpos
        newsample.samp      = TEMPORARY(wflag)

        ;;Inefficient, but better than a two pass algorithm
        ;; since we may be reading files
        IF N_ELEMENTS(samples) EQ 0 THEN samples=TEMPORARY(newsample) ELSE $
           samples = [samples,TEMPORARY(newsample)]

     ENDFOR
  ENDFOR

  ;;Append on samples
  sampleinfo.nsamples = N_ELEMENTS(samples)
  IF sampleinfo.nsamples GT 0 THEN $
     sampleinfo = CREATE_STRUCT(sampleinfo,'samples',TEMPORARY(samples))

  RETURN,sampleinfo
END
