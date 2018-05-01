;+
;NAME
; SMAP_POPULATE_SOURCES
;USAGE
; smap_populate_sources, prob_im, nsources, xpos, ypos, [NCHUNK=, SEED=]
;INPUTS
; prob_im    Probability image
; nsources   Number of sources to generate
;OUTPUTS
; xpos       X positions
; ypos       Y positions
;OPTIONAL INPUTS
; nchunk     Chunk size for source generation (def: 100000)
; seed       Seed for random number generator
;MODIFICATION HISTORY
; Author: Alex Conley, May 2011
;-

PRO smap_populate_sources, prob_im, nsources, xpos, ypos, SEED=seed,$
                           NCHUNK=nchunk

  COMPILE_OPT IDL2, STRICTARRSUBS

  szim = SIZE(prob_im)
  IF szim[0] NE 2 THEN MESSAGE,"Prob_im is not 2D"
  IF szim[1] EQ 0 THEN MESSAGE,"Dimension 1 of prob_im is zero extent"
  IF szim[2] EQ 0 THEN MESSAGE,"Dimension 2 of prob_im is zero extent"
  szx = szim[1]
  szy = szim[2]

  IF nsources LE 0 THEN MESSAGE,"Not asking for any sources"
  IF N_ELEMENTS(nchunk) EQ 0 THEN nchunk = 100000L
  IF nchunk LE 0 THEN MESSAGE,"Invalid (non-positive) nchunk"

  min_prob = MIN( prob_im, /NAN, MAX=max_prob )
  prob_range = max_prob - min_prob
  IF max_prob EQ 0.0 THEN $
     MESSAGE,"Max probability is zero, invalidating uniform check"
  IF ABS(prob_range/max_prob) LT 1d-5 THEN BEGIN
     ;;short cut -- evenly distributed
     MESSAGE,"Using flat probability distribution in populate",/INF
     xpos = ROUND(szx*RANDOMU( seed, nsources )) > 0 < szx
     ypos = ROUND(szy*RANDOMU( seed, nsources )) > 0 < szy
     RETURN
  ENDIF
     
  xpos = DBLARR(nsources)
  ypos = DBLARR(nsources)

  ngen = 0L
  WHILE ngen LT nsources DO BEGIN
     nrem = nsources - ngen ;;how many more we need
     curr_xvals = FLOOR(szx*RANDOMU( seed, nchunk )) > 0 < (szx-1)
     curr_yvals = FLOOR(szy*RANDOMU( seed, nchunk )) > 0 < (szy-1)
     wkeep = WHERE( prob_range*RANDOMU(seed,nchunk) + min_prob LE $
                    prob_im[curr_xvals,curr_yvals], nkeep )
     IF nkeep EQ 0 THEN CONTINUE
     IF nkeep GT nsources-ngen THEN BEGIN
        ;;Only keep some of them
        xpos[ngen:nsources-1] = curr_xvals[wkeep[0:nrem-1]]
        ypos[ngen:nsources-1] = curr_yvals[wkeep[0:nrem-1]]
        ngen = nsources
     ENDIF ELSE BEGIN
        xpos[ngen:ngen+nkeep-1] = curr_xvals[wkeep]
        ypos[ngen:ngen+nkeep-1] = curr_yvals[wkeep]
        ngen += nkeep
     ENDELSE
  ENDWHILE
  RETURN
END

