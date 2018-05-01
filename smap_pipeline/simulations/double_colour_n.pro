;+
;NAME
; double_colour_n
;PURPOSE
; Compute n(S) for a multi-band spline+Gaussian colour model
;USAGE
; n = double_colour_n( flux1, flux2, params )
;INPUTS
;  flux1         Flux 1 values
;  flux2         Flux 2 values
;  params        A structure holding the model parameters.  This
;                 should have fields:
;                  .nfluxknots        Number of flux knots in band 1
;                  .fluxknotpos       Positions of flux knots in band
;                                      1 (array)
;                  .fluxknotval       Log10 n(f1) -- i.e., values at
;                                                    knot positions in
;                                                    band1 (array)
;                  .nsigmaknots       Number of colour dispersion
;                                      knots
;                  .sigmaknotpos      Position of sigma knots
;                  .sigmaknotval      Values at sigma knots
;                  .noffsetknots      Number of offset knots
;                  .offsetknotpos     Positions of offset knots
;                  .offsetknotval     Values at offset knots
;RETURNS
; The differential number counts model evaluated at fluxes.
;NOTES
;MODIFICATION HISTORY
; Author: Alex Conley, Mar 2011
;-

FUNCTION double_colour_n, flux1, flux2, params

  COMPILE_OPT IDL2

  nflux = N_ELEMENTS(flux1)
  IF N_ELEMENTS(flux2) NE N_ELEMENTS(flux1) THEN $
     MESSAGE,"Flux1 and flux2 must be the same length"

  ;;Test to make sure that input params are valid
  IF SIZE(params,/TNAME) NE 'STRUCT' THEN $
     MESSAGE,"Input parameters not structure"
  required_tags = ["nfluxknots","fluxknotpos","fluxknotval",$
                   "nsigmaknots","sigmaknotpos","sigmaknotval",$
                   "noffsetknots","offsetknotpos","offsetknotval"]
  FOR i=0,N_ELEMENTS(required_tags)-1 DO $
     IF ~ TAG_EXIST(params,required_tags[i],/TOP_LEVEL) THEN $
        MESSAGE,"Required tag: "+required_tags[i]+" not found in params"
  IF params.nfluxknots EQ 0 THEN MESSAGE,"Need some flux knots"
  IF params.nfluxknots NE N_ELEMENTS(params.fluxknotpos) THEN $
     MESSAGE,"Number of flux knot positions doesn't match specificiation"
  IF params.nfluxknots NE N_ELEMENTS(params.fluxknotval) THEN $
     MESSAGE,"Number of flux values doesn't match specificiation"
  IF params.nsigmaknots EQ 0 THEN MESSAGE,"Need some sigma knots"
  IF params.nsigmaknots NE N_ELEMENTS(params.sigmaknotpos) THEN $
     MESSAGE,"Number of sigma knot positions doesn't match specificiation"
  IF params.nsigmaknots NE N_ELEMENTS(params.sigmaknotval) THEN $
     MESSAGE,"Number of sigma values doesn't match specificiation"
  IF params.noffsetknots EQ 0 THEN MESSAGE,"Need some offset knots"
  IF params.noffsetknots NE N_ELEMENTS(params.offsetknotpos) THEN $
     MESSAGE,"Number of offset knot positions doesn't match specificiation"
  IF params.noffsetknots NE N_ELEMENTS(params.offsetknotval) THEN $
     MESSAGE,"Number of offset values doesn't match specificiation"
  IF MIN(params.fluxknotpos) LE 0 THEN $
     MESSAGE,"Flux knot positions must be positive"
  IF MIN(params.sigmaknotpos) LE 0 THEN $
     MESSAGE,"Sigma knot positions must be positive"
  IF MIN(params.sigmaknotval) LE 0 THEN $
     MESSAGE,"Sigma knot values must be positive"
  IF MIN(params.offsetknotpos) LE 0 THEN $
     MESSAGE,"Offset knot positions must be positive"

  retarr = DBLARR(nflux)

  ;;Do band 1 part
  lkpos = ALOG(params.fluxknotpos)
  spl = SPL_INIT( lkpos, params.fluxknotval )
  minflux1 = MIN( params.fluxknotpos, MAX=maxflux1 )
  wnonzero = WHERE( flux1 GE minflux1 AND flux1 LT maxflux1, nnonzero )
  IF nnonzero EQ 0 THEN RETURN,retarr ;;all zero
  nzflux1 = flux1[wnonzero]
  retarr[wnonzero] = 10^SPL_INTERP( lkpos, params.fluxknotval, spl,$
                                    ALOG(nzflux1) )


  ;;Generate sigmas and offset vals
  IF params.nsigmaknots EQ 1 THEN BEGIN
     sminval = params.sigmaknotval[0]
     sigvals = sminval * nzflux1
  ENDIF ELSE BEGIN
     sigvals = DBLARR(nnonzero)
     minsgknot = MIN(params.sigmaknotpos,swmin,MAX=maxsgknot,$
                     SUBSCRIPT_MAX=swmax)
     sminval = params.sigmaknotval[swmin]
     wlow = WHERE(nzflux1 LE minsgknot, nlow)
     IF nlow NE 0 THEN sigvals[wlow] = nzflux1[wlow]*params.sigmaknotval[swmin]
     wmed = WHERE(nzflux1 GT minsgknot AND nzflux1 LE maxsgknot, nmed)
     IF nmed NE 0 THEN BEGIN
        spl = SPL_INIT(params.sigmaknotpos, params.sigmaknotval)
        sigvals[wmed]=nzflux1*SPL_INTERP(params.sigmaknotpos,$
                                       params.sigmaknotval, spl,$
                                         nzflux1[wmed])
     ENDIF
     whi = WHERE(nzflux1 GT maxsgknot,nhi)
     IF nhi NE 0 THEN sigvals[whi] = nzflux1[whi]*params.sigmaknotval[swmax]
  ENDELSE
  IF params.noffsetknots EQ 1 THEN BEGIN
     ominval = params.offsetknotval[0]
     offvals = ominval * nzflux1
  ENDIF ELSE BEGIN
     offvals = DBLARR(nnonzero)
     minoffknot = MIN(params.offsetknotpos,owmin,MAX=maxoffknot,$
                      SUBSCRIPT_MAX=owmax)
     ominval = params.offsetknotval[owmin]
     wlow = WHERE(nzflux1 LE minoffknot, nlow)
     IF nlow NE 0 THEN offvals[wlow] = nzflux1[wlow]*params.offsetknotval[owmin]
     wmed = WHERE(nzflux1 GT minoffknot AND nzflux1 LE maxoffknot, nmed)
     IF nmed NE 0 THEN BEGIN
        spl = SPL_INIT(params.offsetknotpos, params.offsetknotval)
        offvals[wmed]=nzflux1*SPL_INTERP(params.offsetknotpos,$
                                         params.offsetknotval, spl,$
                                         nzflux1[wmed])
     ENDIF
     whi = WHERE(nzflux1 GT maxoffknot,nhi)
     IF nhi NE 0 THEN offvals[whi] = nzflux1[whi]*params.offsetknotval[owmax]
  ENDELSE

  retarr[wnonzero] *= $
     EXP( -0.5*( (flux2[wnonzero]-nzflux1-offvals)/sigvals )^2 ) / $
     ( SQRT(2.0*!PI)*sigvals )

  RETURN,retarr
END
