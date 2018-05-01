PRO CREATE_ITERMAP_DEFPARAMS, badbolos, excludemask, exname, pixscale

; Default values for itermap map-maper routines
;pix=6.0;8.33333;12.;6.;18.;5.;14.;12.;7.
;pix=8.33333;12.;6.;18.;5.;14.;12.;7.
pix=6.0;8.33333;12.;6.;18.;5.;14.;12.;7.
;pix=6.0;90.;24.;50.0;8.33333;12.;6.;18.;5.;14.;12.;7.
pixscale=pix*[1.,1.,1.]/3600.

badbolos = ['PSWD15','PSWC12','PSWG8','PSWG11','PSWA10','PSWA11','PSWA13']

excludemask=["maskMaster", "maskUncorrectedTruncation", "maskDead",$
             "maskNoisy", "maskSlow", "maskGlitchL1Detected",$
             "maskNoThermistorAvailable", "maskZeroVelocity"]

CALDAT, SYSTIME(/JUL), m, d, y
exname = strcompress(STRING(pix,FORM='(D10.1)')+'_arcsec_pixels',/remove_all);+ $
;   STRING([y,m,d], FORM='(I04,I02,I02)'),/remove_all)

END
