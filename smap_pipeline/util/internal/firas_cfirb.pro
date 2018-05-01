;+
;NAME
; firas_cfirb
;PURPOSE
; To supply the FIRAS constraint on the cosmic far-infrared
; background (Fixsen et al. 1998)
;USAGE
; flux = firas_cfirb( wave, ERR=err )
;INPUTS
; wave     Wavelength, in microns
;RETURNS
; The flux, in MJy/sr
;OPTIONAL INPUTS
; nmc        Number of Monte-Carlo realizations if asking for err (def: 10000)
;OPTIONAL OUTPUTS
; mnval      Mean value of MC.  Note -- will not be the same as the
;             nominal constraint because the errors are very correlated.
; sdev       Standard deviation of values.
; lowerlimit 1 sigma lower limit on constraint.  This is computed via
;             Monte Carlo so can be slow, and is limited by the accuracy of the
;             correlation coefficients specified by Fixsen.  This
;             should be compared with mnval, not the main returned flux.
; upperlimit 1 sigma upper limit on constraint
;KEYWORDS
; nufnu      Return nu F_nu in nW/m^2/sr instead of MJy/sr
;NOTES
;  The Fixsen errors are highly correlated, and the resulting
; distribution is strongly non-Gaussian. This makes the limits and errors a bit
; complex to interpret.
;MODIFICATION HISTORY
; Author: Alex Conley
;-

FUNCTION firas_cfirb_inner, nu, inten, spec_idx, temp, NUFNU=nufnu
  COMPILE_OPT IDL2, HIDDEN

  h = 6.6260693d-34
  c = 2.99792458d8
  k  = 1.3806505d-23
  hok = 4.7992372d-11
  pval = 2*h*nu^3/c^2/(exp(hok*nu/temp)-1.0)
  nu0 = c/100e-6
  Iv = 1e-5*inten*(nu/nu0)^spec_idx*pval*1e20

  IF KEYWORD_SET( nufnu ) THEN $
     Iv *= nu*1e-11

  RETURN,Iv
END

FUNCTION firas_cfirb, wave, MNVAL=mnval, LOWERLIMIT=lowerlimit, $
                      UPPERLIMIT=upperlimit, SDEV=sdev, NMC=nmc,$
                      NUFNU=nufnu
  COMPILE_OPT IDL2

  c = 2.99792458d8
  nu = c/(wave*1e-6)

  baseT = 18.5
  baseInten = 1.3
  baseIdx = 0.64

  retval = firas_cfirb_inner( nu, baseInten, baseIdx, baseT, NUFNU=nufnu )

  IF ARG_PRESENT(mnval) OR ARG_PRESENT(lowerlimit) OR $
     ARG_PRESENT(upperlimit) OR ARG_PRESENT(sdev) THEN BEGIN
     ;;I'm too lazy to do the error propogation properly, so instead
     ;; I just generate a large number of realizations from the
     ;; Fixsen covariance matrix and use that
     IF N_ELEMENTS(nmc) EQ 0 THEN nmc=10000
     ;;Order: inten, idx, T
     ;;Values from Fixsen
     errs = [ 0.4, 0.12, 1.2 ]
     corrmat = [ [ 1.00,  0.98, -0.99 ],$
                 [ 0.98,  1.00, -0.95 ],$
                 [-0.99, -0.95,  1.00 ] ]
     covmat = corrmat * (errs ## errs)
     LA_CHOLDC, covmat, /DOUBLE
     covmat[1,0] = 0 ;;Because choldc doesn't zero the off pieces
     covmat[2,0] = 0
     covmat[2,1] = 0

     vals = DBLARR(nmc)
     FOR i=0,nmc-1 DO BEGIN
        pars = covmat ## RANDOMN(seed,3) + [ baseInten,baseIdx,baseT ]
        vals[i] = firas_cfirb_inner( nu, pars[0],pars[1],pars[2],$
                                   NUFNU=nufnu)
     ENDFOR
     ;;Compute mean around value returned, not around mean
     wgood = WHERE( vals GT 0.0, ngood )
     IF ngood LT 1000 THEN MESSAGE,"Not enough good values!"
     vals = vals[wgood]
     vals = vals[ SORT( vals ) ]
     idxlow = ROUND( ngood * (1.0 - 0.683 )/2.0 )
     lowerlimit = vals[idxlow]
     idxhigh = ROUND( ngood * (0.5 + (1.0 - 0.683 )/2.0 ) )
     upperlimit = vals[idxhigh]
     sdev = STDEV( vals )
     mnval = MEAN( TEMPORARY( vals ) )
  ENDIF

  RETURN,retval
END
