;+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  pro smap_accumulatepoly.pro
;;  Sept 16, 2009
;;  Mike Zemcov
;;  This procedure takes an smap tod structure, a fitted polynomial
;;  structure, a set of 3 color polynomial map 
;;  structures, and a mapparam structure and makes a set of maps of the
;;  polynomials removed from the tod.
;;  Inputs: tod: standard smap tod structure filled with data - only
;;               pointing and mask are important for this application
;;          polyfits = fitted polynomial structure for tod
;;          map250: initialized 250 micron smap map structure; may already
;;           contain data
;;          map350: initialized 350 micron smap map structure; may already
;;           contain data
;;          map500: initialized 500 micron smap map structure; may already
;;           contain data
;;          mapparam: a standard smap map parameter structure
;;          corresponding to these data
;;  Options: verbose = verbosity flag, 0 = silent
;;           success = success flag, 1=successful, 0=not
;;           errmsg = if error, string containing error message 
;;  Outputs = none
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-
PRO SMAP_ACCUMULATEPOLY,tod,polyfits,map250,map350,map500,mapparam,$
                       VERBOSE=verbose,SUCCESS=success,ERRMSG=errmsg

  COMPILE_OPT IDL2

  ; set up the error handling variables
  success = 0b
  errmsg = ''

  nchannels = N_ELEMENTS( tod.chan )
  t0 = MEAN(tod.samptime)
  IF ~ FINITE(t0) THEN BEGIN
     errmsg = "Couldn't find finite sampletimes"
     RETURN
  ENDIF
  timevals = tod.samptime - t0

  ; set up verbosity
  IF ~(KEYWORD_SET(verbose)) THEN verbose = 0b

  ; loop through colors
  FOR icol=0,2 DO BEGIN

     ; tell me what we're up to
     IF KEYWORD_SET(verbose) THEN $
        MESSAGE,'Accumulating polynomials, on band ' + $
                mapparam.bands[icol],/INF   
     
    ; due to the stupid way idl handles structs, have to select 
     ; the one we're after this loop
     CASE icol OF
        0: map=map250
        1: map=map350
        2: map=map500
     ENDCASE

     ; initialize a temp holder for data
     tempmap = 0. * map.image
     temphit = 0. * map.exposure
     
     ; find the detectors belonging to this array
     myboloselect = WHERE(STRPOS(tod.chan,mapparam.bands[icol]) GE 0,$
                          countchans)
     mypolyselect = WHERE(STRPOS(polyfits.chan,mapparam.bands[icol]) GE 0,$
                          countchans)

     ; now do a loop through the list of bolometers I found for this array
     FOR ibolo=0,countchans-1 DO BEGIN
        ; pick off a single bolo to look at
        thisbolo = myboloselect[ibolo]
        thatbolo = mypolyselect[ibolo]
        ; find the finite values in this time stream
        whfinite = WHERE(FINITE(tod.signal[thisbolo,*]) EQ 1,countndat)
        ; if this is a light device and there are non-nan
        ; values, continue
        IF tod.islight[thisbolo] AND countndat GT 0 THEN BEGIN
           ;; now work out the pixel values - the xrange first 
           ;; is to get the right handedness, the 1 subtraction 
           ;; is because idl is zero based
           thisx = mapparam.xrange[icol] - 1L - $
                   fix((tod.ra[thisbolo,whfinite] - $
                        mapparam.minra) / $
                       mapparam.pixsize[icol] * $
                       cos(!DTOR * mapparam.middec))
           thisy = fix((tod.dec[thisbolo,whfinite] - $
                        mapparam.mindec) / $
                       mapparam.pixsize[icol]) 

           thissig = REPLICATE(polyfits.coeffs[thatbolo,0],tod.nsamps)
           FOR ipoly=1,polyfits.order DO BEGIN
              thissig = thissig + $
                        polyfits.coeffs[thatbolo,ipoly] * $
                        timevals^FLOAT(ipoly)
           ENDFOR
           
           ;; and dump data into the temporary map - this is for 
           ;; just super naive dumping
              
           FOR ip=0,N_ELEMENTS(thisx) - 1 DO BEGIN
              tempmap[thisx[ip],thisy[ip]] = $
                 tempmap[thisx[ip],thisy[ip]] + $
                 thissig[whfinite[ip]]
              temphit[thisx[ip],thisy[ip]] = $
                 temphit[thisx[ip],thisy[ip]] + 1.0
              ; and this is for variance weighting
           ENDFOR

              ; ok, that's this scan dumped
        ENDIF ELSE BEGIN
           IF KEYWORD_SET(verbose) THEN $
              MESSAGE,'Removed bolometer ' + $
                      STRCOMPRESS(tod.chan[ibolo],/REMOVE_ALL) + $
                      ' from poly coadd.',/INF
        ENDELSE
        ; this is the end of the 'this bolometer' loop
     ENDFOR

     ; write temp maps to the sum maps properly.  This is just 
     ; repeated 3 times of the same thing
     CASE icol OF
        0: BEGIN
           map250.image = map250.image + tempmap
           map250.exposure = map250.exposure + temphit
        END
        1: BEGIN
           map350.image = map350.image + tempmap
           map350.exposure = map350.exposure + temphit
        END
        2: BEGIN
           map500.image = map500.image + tempmap
           map500.exposure = map500.exposure + temphit
        END
     ENDCASE
     
     DELVARX,map,tempmap,tempwei
     ;; this is the end of the 'this color' loop
   ENDFOR

  success = 1b
  RETURN

END
