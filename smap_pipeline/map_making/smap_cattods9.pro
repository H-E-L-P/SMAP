PRO SMAP_CATTODS9,filein,fileout=fileout,size_src=size_src,byfreq=byfreq,snrcut=snrcut
;+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  procedure smap_cattods9
;;  Dec 7, 2009
;;  Alexandre Amblard
;;  This function takes a catalog file as the one output by
;;  ps_extractor and converts it into 1 or 3 files (1 per frequency)
;;  which are region files of the ds9 software
;;
;;  Inputs: file = string file name (w/ full path) of file to read.
;;  Outputs: region file(s) for ds9 
;;  Options:
;;           fileout : name of the output file(s) if no name
;;           is given the program will take the filein name
;;           and replace the .fits extension by .reg
;;           if byfreq keyword is set then it will add an 
;;           extra-extension _PSW _PMW _PLW
;;           
;;           size_src : size of the source in ds9, the default
;;           is the FWHM at each frequency 18,25,36 arcsecs
;;           
;;           byfreq : output 3 files instead of 1 (1 per frequency)
;;
;;           snrcut : only convert the sources with flux snr greater
;;           than snrcut 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-

IF NOT(KEYWORD_SET(snrcut)) THEN snrcut=0.
IF NOT(KEYWORD_SET(size_src)) THEN size_src=[18,25,36]
IF NOT(KEYWORD_SET(fileout)) THEN IF KEYWORD_SET(byfreq) THEN BEGIN
   fileout=strarr(3)
   freq=["PSW","PMW","PLW"]
   FOR i=0,2 DO fileout[i]=strmid(filein,0,strlen(filein)-5)+"_"+freq[i]+".reg"
ENDIF ELSE fileout=strmid(filein,0,strlen(filein)-5)+".reg"

cat=MRDFITS(filein,1)
ncat=n_elements(cat)
ncatf=[n_elements(where(finite(cat.f_250))),n_elements(where(finite(cat.f_350))),$
      n_elements(where(finite(cat.f_500)))]

luns=LONARR(3)
IF KEYWORD_SET(byfreq) THEN BEGIN   
   FOR i=0,2 DO BEGIN
      GET_LUN,lun
      luns[i]=lun
   ENDFOR
   FOR i=0,2 DO OPENW,luns[i],fileout[i]   
   FOR i=0,2 DO BEGIN
      printf,luns[i],"# Region file format: DS9 version 4.0"
      printf,luns[i],'global color=green font="helvetica 10 normal" select=1 highlite=1 edit=1 move=1 delete=1 include=1 fixed=0 source'
      printf,luns[i],"fk5"
   ENDFOR
ENDIF ELSE BEGIN
   GET_LUN,lun
   OPENW,lun,fileout
   FOR i=0,2 DO luns[i]=lun
   printf,lun,"# Region file format: DS9 version 4.0"
   printf,lun,'global color=green font="helvetica 10 normal" select=1 highlite=1 edit=1 move=1 delete=1 include=1 fixed=0 source'
   printf,lun,"fk5"  
ENDELSE

FOR i=0,ncatf[0]-1 DO $
   IF (cat[i].f_250/cat[i].df_250 GT snrcut) THEN $
      printf,luns[0],string(cat[i].ra,cat[i].dec,size_src[0],format='("circle(",f9.5,",",f9.5,",",f9.5)')+'")'
FOR i=ncatf[0],ncatf[1]+ncatf[0]-1 DO $
   IF (cat[i].f_350/cat[i].df_350 GT snrcut) THEN $
      printf,luns[1],string(cat[i].ra,cat[i].dec,size_src[1],format='("circle(",f9.5,",",f9.5,",",f9.5)')+'")'
FOR i=ncatf[1]+ncatf[0],ncat-1 DO $
   IF (cat[i].f_500/cat[i].df_500 GT snrcut) THEN $
      printf,luns[2],string(cat[i].ra,cat[i].dec,size_src[2],format='("circle(",f9.5,",",f9.5,",",f9.5)')+'")'


IF KEYWORD_SET(byfreq) THEN BEGIN   
   FOR i=0,2 DO CLOSE,luns[i]
   FOR i=0,2 DO FREE_LUN,luns[i]
ENDIF ELSE BEGIN
   CLOSE,lun
   FREE_LUN,lun
ENDELSE


END
