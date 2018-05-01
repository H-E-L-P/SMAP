;+
; NAME:
;     smap_compute_transfer_function
; PURPOSE:
;     Compute the transfer function of a smap mapper.
; EXPLANATION:
;     This requires you have run make_noiseless_sim.pro or similar 
;      and have both input and output, noiseless maps run through 
;      your map maker.
; CALLING SEQUENCE:
;     smap_compute_transfer_function
;
; REQUIRES:
;     You have defined !SMAP_MAPS which points to the directory 
;     containing both input and output simulated maps.  If you 
;     don't have a matching set (ie the same number and colors 
;     for both) it will not work.
; INPUTS:
;     NONE
; OPTIONAL INPUTS:
;     NONE
; OPTIONAL KEYWORD INPUTS:
;     VERBOSE: controls verbose output (def=0)
;
; RETURNS:
;     SUCCESS = success flag, =1 if successfully executed
;     ERRMSG = error message if error
;
; MODIFICATION HISTORY:
;     Original, Mike Zemcov, Feb 2010
;-

PRO smap_compute_transfer_function
 
  filelist_sim = FILE_SEARCH(addslash(!SMAP_MAPS) + $
                             '*simmap*_PSW.fits',/FULLY)
  filelist_proc = FILE_SEARCH(addslash(!SMAP_MAPS) + $
                              '*procmap*_PSW.fits',/FULLY)

  nfiles_sim = N_ELEMENTS(filelist_sim)
  nfiles_proc = N_ELEMENTS(filelist_proc)

  IF nfiles_sim NE nfiles_proc THEN BEGIN
     MESSAGE,'error'
  ENDIF ELSE BEGIN
     nfiles = nfiles_sim
  ENDELSE
  
  setup_simmap = 0.0 * MRDFITS(filelist_sim[1],1)
  setup_procmap = 0.0 * MRDFITS(filelist_proc[1],1)

  sizemap = SIZE(setup_simmap)
  nj = sizemap[1]
  nk = sizemap[2]

  IF nj NE nk THEN BEGIN
     IF (sizemap[1] GT sizemap[2]) THEN nu = sizemap[1] ELSE nu = sizemap[2]
  ENDIF

  uber_simmap = FLTARR(nu,nu)
  uber_procmap = FLTARR(nu,nu)

  whgood = WHERE(FINITE(setup_procmap) EQ 1,COMPLEMENT=whnan)

  setup_simmap[whnan] = 0.0
  setup_procmap[whnan] = 0.0

  uber_simfft = ABS(FFT(uber_simmap))
  uber_procfft = ABS(FFT(uber_procmap))
  uber_tf = uber_simfft

  FOR ifile = 0, nfiles-1L DO BEGIN

     insimmap = MRDFITS(filelist_sim[ifile],1,/SILENT)
     insimmap[whnan] = 0.0
     simmap = FLTARR(nu,nu)
     simmap[0:nj-1,0:nk-1] = insimmap
     simfft = FFT(simmap)
;     simfft[0,0] = 0.0
     uber_simfft = uber_simfft + ABS(simfft)

     inprocmap = MRDFITS(filelist_proc[ifile],1,/SILENT)
     inprocmap[whnan] = 0.0
     procmap = FLTARR(nu,nu)
     procmap[0:nj-1,0:nk-1] = inprocmap
     procfft = FFT(procmap)
;     procfft[0,0] = 1.0
     uber_procfft = uber_procfft + ABS(procfft)

     uber_tf = uber_tf + ABS(simfft / procfft)

  ENDFOR

  uber_tf = uber_tf / FLOAT(nfiles); + 1.0

  WRITEFITS,addslash(!SMAP_MAPS) + 'test_tf2_PSW.fits',uber_tf

  WRITEFITS,addslash(!SMAP_MAPS) + 'test_sim_PSW.fits',uber_simfft
  WRITEFITS,addslash(!SMAP_MAPS) + 'test_proc_PSW.fits',uber_procfft

  uber_simfft = FIX_FFT(uber_simfft)
  uber_procfft = FIX_FFT(uber_procfft)
  uber_tf = FIX_FFT(uber_tf)

  mask = INTARR(nu,nu) 
  tf = FLTARR(nu,nu) + 1.0
  radlo = 25.
  radhi = 45
  radtot = FLOAT(nu) * SQRT(2.)

  FOR ij=0,nu-1 DO BEGIN
     FOR ik=0,nu-1 DO BEGIN
        rad = SQRT((FLOAT(ij) - FLOAT(nu) / 2. + 1.)^2 + $
                   (FLOAT(ik) - FLOAT(nu) / 2. + 1.)^2)
        IF rad gt radlo and rad LT radhi THEN $
           tf[ij,ik] = uber_tf[ij,ik] ; / uber_procfft[ij,ik]); * $ 
        IF rad GT radhi - 5 AND rad LT radhi THEN $
           tf[ij,ik] = uber_tf[ij,ik] * (1. - rad / radhi)
        

;           tf[ij,ik] = (uber_simfft[ij,ik] / uber_procfft[ij,ik]); * $ 
;                       (1. - (rad -radlo) / (radtot - radlo))
;* (rad / radtot)
        ;IF rad GT radhi THEN tf[ij,ik] = 0.0 ;$
           ;(uber_simfft[ij,ik] / uber_procfft[ij,ik]) * $

     ENDFOR
  ENDFOR

  tf = FIX_FFT(tf)

  ;tf = SMOOTH(tf,5,/EDGE_TRUNCATE)

  WRITEFITS,addslash(!SMAP_MAPS) + 'test_tf_PSW.fits',tf

  IF 1 THEN BEGIN
  insimmap = MRDFITS(filelist_sim[1],1)
  inprocmap = MRDFITS(filelist_proc[1],1)
  inprocmap[whnan] = 0.0
  insimmap[whnan] = 0.0
  simmap = FLTARR(nu,nu)
  simmap[0:nj-1,0:nk-1] = insimmap
  procmap = FLTARR(nu,nu)
  procmap[0:nj-1,0:nk-1] = inprocmap
  inprocfft = FFT(procmap)
  tfprocfft = inprocfft * tf
;  WRITEFITS,addslash(!SMAP_MAPS) + 'test_fixfft_PSW.fits',tfprocfft
  fixedmap = REAL_PART(FFT(tfprocfft,/INVERSE))
  fixedmap = fixedmap[0:nj-1,0:nk-1]
  fixedmap[whnan] = !VALUES.F_NAN
  inprocmap[whnan] = !VALUES.F_NAN

  WRITEFITS,addslash(!SMAP_MAPS) + 'test_diff1_PSW.fits',insimmap - inprocmap

  WRITEFITS,addslash(!SMAP_MAPS) + 'test_fixed_PSW.fits',fixedmap

  WRITEFITS,addslash(!SMAP_MAPS) + 'test_diff2_PSW.fits',insimmap - fixedmap

  WRITEFITS,addslash(!SMAP_MAPS) + 'test_diff3_PSW.fits',inprocmap - fixedmap

  ENDIF

END
