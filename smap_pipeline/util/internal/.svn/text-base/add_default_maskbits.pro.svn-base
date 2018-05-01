;+
;NAME
; add_default_maskbits
;CATEGORY
; Herschel SPIRE SMAP pipeline
;PURPOSE
; Adds missing default mask bits
;CALLING SEQUENCE
; add_default_maskbits, maskbits
;INPUTS/OUTPUTS
; maskbits           Set of maskbits read from header.  Missing ones
;                     are appended.
;KEYWORDS
; addsmap            Add SMAP defined mask bits
;MODIFICATION HISTORY
; Author: Alex Conley, Aug 18, 2010
;         Mike Zemcov, Apr 13, 2011 - added maskManual to SMAP bits.
;         Mike Zemcov, May 6, 2011 - added maskSlew to SMAP bits.
;         Alex Conley, Jul 22, 2011 - add maskManualNoParamWeight
;-

PRO add_default_maskbits, maskbits, ADDSMAP=addsmap

  ;;Define the default set.  There are a few missing in most headers,
  ;; oddly enough.
  ;;This is taken from the SPIRE Pipeline Mask Policy
  ;; issue 1.10 (20 July 2010)
  ndefmasks = 21
  nsmapmasks = 5
  IF KEYWORD_SET( addsmap ) THEN nmasks = ndefmasks + nsmapmasks ELSE $
     nmasks = ndefmasks
  
  maskstruct = { NAME: '', BITS: 0UL, COMMENT: '' }
  defarr = REPLICATE( maskstruct, nmasks ) 

  defarr[0:ndefmasks-1].name=['maskMaster','maskInvalidTime','maskAdcLatch',$
                              'maskTruncated','maskUncorrectedTruncation',$
                              'maskDead','maskNoisy','maskNotChoppedToSky',$
                              'maskVoltageOol','maskGlitchL1Detected',$
                              'maskGlitchL1NotRemoved','maskGlitchL2Detected',$
                              'maskGlitchL2NotRemoved','maskSlow',$
                              'maskVoltageBelowK3','maskNoRespData',$
                              'maskTSignalHdv','maskBsmChopOol',$
                              'maskBsmJigOol',$
                              'maskJumpThermistorsDarksSignal',$
                              'maskNoThermistorAvailable']
  defarr[0:ndefmasks-1].bits = [1uL, 2uL, 4uL, 8uL, 16uL, 128uL, 256uL,$
                                512uL, 1024uL, 2048uL, 4096uL, 8192uL, 16384uL,$
                                32768uL, 65536uL, 131072uL, 262144uL, $
                                524288uL, 1048576uL, 2097152uL, 4194304uL]
  defarr[0:ndefmasks-1].comment = $
     ['Mask value for master bit',$
      'Mask value for invalid sample time',$
      'Mask value for possible ADS latchup error',$
      'Mask value for ADC conversion truncation',$
      'Mask value for uncorrected ADC conversion truncation',$
      'Mask value for dead channel', $
      'Mask value for noisy channel',$
      'Mask value for channel not chopped to sky',$
      'Mask value for voltage out of range',$
      'Mask value for Level 1 glitch detected',$
      'Mask value for Level 1 glitch detected but not removed',$
      'Mask value for Level 2 glitch detected',$
      'Mask value for Level 2 glitch detected but not removed',$
      'Mask value for slow channel',$
      'Mask value for voltage below K3 calibration factor',$
      'Mask value where flux conversoin not possible',$
      'Mask value for large thermistor/dp signal deviations',$
      'Mask value for BSM outside chop soft limits',$
      'Mask value for BSM outside jiggle soft limits',$
      'Mask value for sudden thermistor and dark channel jumps',$
      'Mask value for all thermistors which have jump or are saturated']
  
  IF KEYWORD_SET( addsmap ) THEN BEGIN
     mbit = MAX(defarr[0:ndefmasks-1].bits)
     lowbit = [2uL,4uL,8uL,16uL,32uL]*mbit

     defarr[ndefmasks:*].name = ['maskZeroVelocity','maskManual',$
                                 'maskSlew','maskManualNoParamWeight',$
                                 'maskNoSMAPThermcorr']
     defarr[ndefmasks:*].bits = lowbit
     defarr[ndefmasks:*].comment = $
        ['Mask value for zero (=small) velocity regions',$
         'Mask value for manually masked regions.',$
         'Mask value for fast slews.',$
         'Mask value for can be used in map, but not in parameter fits',$
         'Unable to compute SMAP thermistor correction']

  ENDIF
     
  ;;quick return if nothing passed in
  IF N_ELEMENTS(maskbits) EQ 0 THEN BEGIN
     maskbits = TEMPORARY(defarr)
     RETURN
  ENDIF

  ;;First, make sure input has unique bits
  nuniq = N_ELEMENTS( UNIQ( maskbits.bits, SORT(maskbits.bits) ) )
  IF nuniq LT N_ELEMENTS(maskbits) THEN $
     MESSAGE,"ERROR -- input mask information has non-unique mask bits"

  ;;Look for missing bits
  wmissing = MISSING( maskbits.bits, defarr.bits, nmissing )
  IF nmissing EQ 0 THEN RETURN
  
  ;;Join them on
  maskbits = struct_concat( maskbits, defarr[wmissing] )
  
END
