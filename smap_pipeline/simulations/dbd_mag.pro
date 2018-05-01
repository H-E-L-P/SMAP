;+
;NAME
; dbd_mag
;PURPOSE
; Given redshifts, randomly generate magnifications from the
; De Bernardis model (the one used in Wardlow et al. 2013).
;USAGE
; mag = dbd_mag(z, SEED=seed)
;INPUTS
; z    Redshifts
;OUTPUTS
; mag  List of magnification factors
;MODIFICATION HISTORY
; Author: Alex Conley, April 2013
;-

FUNCTION dbd_mag, z, SEED=seed
  COMPILE_OPT IDL2

  IF N_ELEMENTS(z) EQ 0 THEN MESSAGE, "No redshifts provided"

  datafile = ADDSLASH(!SMAP_PIPELINE_PATH)+'simulations/data/db_lens_model.fits'
  IF ~ FILE_TEST(datafile, /READ) THEN $
     MESSAGE,"Unable to locate data file: "+datafile
  lensmod = MRDFITS(datafile, 1, STATUS=status, /SILENT)
  IF status NE 0 THEN MESSAGE,"Unable to read in datafile: "+datafile

  mag = FLTARR(N_ELEMENTS(z))
  mag[*] = 1.0

  nlz = N_ELEMENTS(lensmod.z)


  rvals = RANDOMU(seed, N_ELEMENTS(z), 2)

  nolens_prob = INTERPOL(lensmod.nolens_prob, lensmod.z, z)
  minz = MIN(lensmod.z, MAX=maxz)
  wtop = WHERE(z GT maxz, ntop)
  IF ntop NE 0 THEN nolens_prob[wtop] = lensmod.nolens_prob[nlz-1]
  wbot = WHERE(z LT minz, nbot)
  IF nbot NE 0 THEN nolens_prob[wbot] = 1.0

  idx = VALUE_LOCATE(lensmod.z, z)

  ;; Stuff above the maxz treat as being at the max z
  wtop = WHERE(idx EQ nlz-1, ntop)
  IF ntop NE 0 THEN BEGIN
     nv = lensmod.nnonzero[nlz-1]
     wl = WHERE(rvals[wtop,0] GT nolens_prob[wtop], nl)
     IF nl NE 0 THEN BEGIN
        wtopwl = wtop[wl]
        mag[wtopwl] = INTERPOL(lensmod.mu[nlz-1, 0:nv-1], $
                               lensmod.cumprob[nlz-1, 0:nv-1], $
                               rvals[wtopwl, 1])
     ENDIF
  ENDIF

  ;;For others, interpolate linearly in z
  wnorm = WHERE(idx GE 0 AND idx LT nlz-1, nnorm)
  IF nnorm NE 0 THEN BEGIN
     z_norm = z[wnorm]
     idx_norm = idx[wnorm]
     rvals_norm = rvals[wnorm, *]
     nolens_norm = nolens_prob[wnorm]

     ;;Now use histogram magic to sort into interpolants
     h = HISTOGRAM(idx_norm, REV=R)
     FOR i = 0, N_ELEMENTS(h)-1 DO BEGIN
        IF h[i] GT 0 THEN BEGIN
           ;;index into z_norm, idx_norm, r_norm for objects in this bin
           idx1 = R[R[i]:R[i+1]-1] 

           interp_idx = idx_norm[idx1[0]] ;;lower index into lensmodel.z
           interp_idxp1 = idx_norm[idx1[0]]+1 ;;upper index into lensmodel.z
           
           ;;Now, see which ones are lensed
           wl = WHERE(rvals_norm[idx1, 0] GT nolens_norm[idx1], nl)
           IF nl EQ 0 THEN CONTINUE ;; no lensing in this bin

           idx1wl = idx1[wl]

           ;;Interpolate on lower index
           nv = lensmod.nnonzero[interp_idx]
           low_mu = INTERPOL(lensmod.mu[interp_idx, 0:nv-1],$
                             lensmod.cumprob[interp_idx, 0:nv-1],$
                             rvals_norm[idx1wl, 1])
           nv = lensmod.nnonzero[interp_idxp1]
           hi_mu = INTERPOL(lensmod.mu[interp_idxp1, 0:nv-1],$
                            lensmod.cumprob[interp_idxp1, 0:nv-1],$
                            rvals_norm[idx1wl, 1])
           
           wt1 = (lensmod.z[interp_idxp1] - z_norm[idx1wl]) / $
                 (lensmod.z[interp_idxp1] - lensmod.z[interp_idx])
           wt2 = 1.0 - wt1
           mag[wnorm[idx1wl]] = wt1 * low_mu + wt2 * hi_mu

        ENDIF
     ENDFOR
  ENDIF

  RETURN, mag
END
