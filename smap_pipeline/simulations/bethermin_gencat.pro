;+
;NAME
; bethermin_gencat
;PURPOSE
; Generate a catalog drawn from the Bethermin 2010 model
;USAGE
; cat = bethermin_gencat( nsources, dndlogldzdomega, cold, starburst,$
;                         LENSPROB=lensprob)
;INPUTS
; nsources        Number of sources to generate
; dndlogldzdomega Output of bethermin_dndlogldzdomega
; cold            Holds information about IAS cold templates
; starburst       Holds information about IAS starburst templates
;OPTIONAL INPUTS
; wave            Wavelengths to generate observed flux densities for, in
;                  microns (def: [250,350,500])
; lensprob        Structure containing information about lensing 
;                  probabilities.
;KEYWORDS
; verbose         Run verbosely
;RETURNS
; Structure of sources with luminsity, fluxes (in mJy).
;MODIFICATION HISTORY
; Author: Alex Conley, April 12, 2011
;-

FUNCTION bethermin_gencat_lensprob, z, lensprob, seed
  COMPILE_OPT IDL2, HIDDEN
  nlens = N_ELEMENTS(lensprob)
  rval = RANDOMU(seed)
  logrval = ALOG(rval)
  IF z LE lensprob[0].z THEN BEGIN
     n = lensprob[0].nnonzero
     IF n EQ 0 THEN RETURN,1.0
     IF rval LT lensprob[0].cumprob[0] THEN $
        RETURN,lensprob[0].mag[0]
     RETURN,INTERPOL(lensprob[0].logmag[0:n-1],$
                     lensprob[0].logcumprob[0:n-1],$
                     logrval )
  ENDIF ELSE IF z GE lensprob[nlens-1].z THEN BEGIN
     n = lensprob[nlens-1].nnonzero
     IF n EQ 0 THEN RETURN,1.0
     IF rval LT lensprob[nlens-1].cumprob[0] THEN $
        RETURN,lensprob[nlens-1].mag[0]
     RETURN,EXP(INTERPOL(lensprob[nlens-1].logmag[0:n-1],$
                         lensprob[nlens-1].logcumprob[0:n-1],$
                         logrval))
  ENDIF ELSE BEGIN
     idx = value_locate( lensprob.z, z )
     frac = 1.0 - (z - lensprob[idx].z)/(lensprob[idx+1].z-lensprob[idx].z)
     n1 = lensprob[idx].nnonzero
     IF n1 EQ 0 THEN mag1 = 1.0 ELSE BEGIN
        IF rval LT lensprob[idx].cumprob[0] THEN BEGIN
           mag1 = lensprob[idx].mag[0] 
        ENDIF ELSE BEGIN
           mag1 = EXP( INTERPOL(lensprob[idx].logmag[0:n1-1],$
                                lensprob[idx].logcumprob[0:n1-1],$
                                logrval ) )
        ENDELSE
     ENDELSE
     n2 = lensprob[idx+1].nnonzero
     IF n2 EQ 0 THEN mag2 = 1.0 ELSE BEGIN
        IF rval LT lensprob[idx+1].cumprob[0] THEN BEGIN
           mag2 = lensprob[idx+1].mag[0] 
        ENDIF ELSE BEGIN
           mag2 = EXP( INTERPOL(lensprob[idx+1].logmag[0:n2-1],$
                                lensprob[idx+1].logcumprob[0:n2-1],$
                                logrval ) )
        ENDELSE
     ENDELSE
     final_mag = frac*mag1 + (1.0-frac)*mag2
     RETURN,final_mag
  ENDELSE
END

;----------------------------------------------

FUNCTION bethermin_gencat, nsources, dndlogldzdomega, cold, starburst,$
                           WAVE=wave, SIGMAPOP=sigmapop, LPOP=lpop, $
                           SEED=seed, LENSPROB=lensprob, VERBOSE=verbose
  COMPILE_OPT IDL2

  IF N_ELEMENTS(sigmapop) EQ 0 THEN sigmapop = 1.769
  IF N_ELEMENTS(lpop) EQ 0 THEN lpop = 23.677d10
  IF N_ELEMENTS(wave) EQ 0 THEN wave = [250.0,350,500]
  nwave = N_ELEMENTS(wave)

  IF nsources LE 0 THEN MESSAGE,"nsources invalid"
  IF SIZE(dndlogldzdomega,/TNAME) NE 'STRUCT' THEN $
     MESSAGE,"dndlogldzdomega not structure"
  IF SIZE(cold,/TNAME) NE 'STRUCT' THEN $
     MESSAGE,"cold not structure"
  IF SIZE(starburst,/TNAME) NE 'STRUCT' THEN $
     MESSAGE,"starburst not structure"

  nlens = N_ELEMENTS(lensprob)
  IF nlens NE 0 THEN do_lensing = 1b ELSE do_lensing = 0b
  
  ;;Get log lums, zs
  lumz = bethermin_genl(nsources,dndlogldzdomega,SEED=seed)

  ;;Set up output
  retstr = REPLICATE({z: !VALUES.F_NAN, loglum: !VALUES.F_NAN,$
                      type: 0b, mag: 1.0, obsflux: FLTARR(nwave) }, nsources)
  retstr.z = lumz.z
  retstr.loglum = lumz.loglum
  
  ;;For each source, decide if it's Cold [0] or Starburst [1]
  fsb = 0.5*(1+TANH( sigmapop*(retstr.loglum-ALOG10(lpop)) ) )  
  wsb = WHERE( fsb GT RANDOMU(seed,nsources), nsb )
  IF nsb NE 0 THEN retstr[wsb].type = 1b

  ;;Get observed fluxes
  ncold = N_ELEMENTS(cold)
  nstarburst = N_ELEMENTS(starburst)
  wavework = wave*1d-6 ;;m
  idcold = 1.0/(cold[1:*].lum - cold[0:*].lum)
  idstarburst = 1.0/(starburst[1:*].lum - starburst[0:*].lum)

  modcheck = nsources / 10

  FOR i=0,nsources-1 DO BEGIN
     IF KEYWORD_SET( verbose ) THEN BEGIN
        IF (i+1) MOD modcheck EQ 0 THEN BEGIN
           frac = 1.0*(i+1)/nsources
           msg = STRING(frac*100.0,FORMAT='("Completed ",F0.1,"%")')
           MESSAGE,msg,/INF
        ENDIF
     ENDIF
     opz = 1.0 + retstr[i].z
     cwave = wavework / opz
     IF retstr[i].type EQ 0b THEN BEGIN
        idx = value_locate( cold.lum, retstr[i].loglum )
        IF idx LT 0 OR idx GE ncold-1 THEN MESSAGE,"Luminosity out of range"
        frac = (retstr[i].loglum - cold[idx].lum)*idcold[idx]
        fluxes = (1.0-frac)*INTERPOL(cold[idx].flux,cold[idx].wave,$
                                     cwave) + $
                 frac*INTERPOL(cold[idx+1].flux,cold[idx+1].wave,$
                               cwave)
     ENDIF ELSE BEGIN
        idx = value_locate( starburst.lum, retstr[i].loglum )
        IF idx LT 0 OR idx GE nstarburst-1 THEN $
           MESSAGE,"Luminosity out of range"
        frac = (retstr[i].loglum - starburst[idx].lum)*idstarburst[idx]
        fluxes = (1.0-frac)*INTERPOL(starburst[idx].flux,starburst[idx].wave,$
                                     cwave) + $
                 frac*INTERPOL(starburst[idx+1].flux,starburst[idx+1].wave,$
                               cwave)
     ENDELSE

     ;;To mJy
     lumdist = LUMDIST( retstr[i].z, OMEGA_M=dndlogldzdomega.omega_m,$
                        H0=dndlogldzdomega.h0, $
                        LAMBDA0=1.0-dndlogldzdomega.omega_m, /SILENT )
     lumdist *= 3.08568025d22 ;;Mpc to m

     ;;mJy
     retstr[i].obsflux = 1d29*opz*fluxes/(4*!PI*lumdist^2)

     ;;Deal with lensing
     IF do_lensing THEN BEGIN
        nolensprob = INTERPOL( lensprob.nolens_prob, lensprob.z, retstr[i].z )
        IF randomu(seed) GT nolensprob THEN BEGIN
           magval = bethermin_gencat_lensprob( retstr[i].z, lensprob, seed )
           retstr[i].mag = magval
           retstr[i].obsflux *= magval
        ENDIF
     ENDIF

  ENDFOR
  
  RETURN,retstr

END
