;+
;  Evaluates simplified version of 2D R colour model formula
;  R = r_double_colour(flux1,flux2,params,beam1,beam2,pixscale)
;-
FUNCTION r_double_colour,flux1,flux2, params,$
                         beam1, beam2, pixscale, $
                         WEIGHTARR=weightarr
  COMPILE_OPT IDL2

  n1 = N_ELEMENTS(flux1)
  n2 = N_ELEMENTS(flux2)

  IF N_ELEMENTS(beam1) NE N_ELEMENTS(beam2) THEN $
     MESSAGE,"Beams must be same size"
  
  nweight = N_ELEMENTS(weightarr)
  IF nweight NE 0 && nweight NE N_ELEMENTS(beam1) THEN $
     MESSAGE,"Weights must be same size as beam"

  R = DBLARR(n1,n2)

  wnotzero = WHERE( (beam1 NE 0.0) AND (beam2 NE 0.0), nnotzero )
  IF nnotzero EQ 0 THEN MESSAGE,"Beams all zero"
  i_wbeam1 = 1.0/beam1[wnotzero]
  i_wbeam2 = 1.0/beam2[wnotzero]

  IF nweight NE 0 THEN weights = weightarr[wnotzero]

  IF MIN(i_wbeam1) LT 0.0 THEN MESSAGE,"Only positive beams supported"
  IF MIN(i_wbeam2) LT 0.0 THEN MESSAGE,"Only positive beams supported"

  FOR i=0,n1-1 DO BEGIN
     arg1 = flux1[i]*i_wbeam1
     FOR j=0,n2-1 DO BEGIN
        arg2 = flux2[j]*i_wbeam2
        rval = i_wbeam1*i_wbeam2*double_colour_n(arg1,arg2,params)
        IF nweight NE 0 THEN rval *= weights
        R[i,j] = TOTAL(rval)
     ENDFOR
  ENDFOR

  R *= (pixscale/3600.0)^2

  RETURN,R
END
        
           
