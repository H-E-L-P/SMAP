FUNCTION GET_MATRIX_MKK_HYDRA,mask1,mask2=mask2,deltal,pixsize,lmin=lmin,kpower=kpower,mpi=mpi,log_deltal=log_deltal

;+
;NAME
;  get_matrix_mkk
;PURPOSE
;  To compute the angular power spectra
; 
;USAGE
;  Mkk=get_mastrix_mkk(mask1,deltal,pixsize)
;INPUTS
;  mask1 : input mask1 is an 2D array
;  deltal : the bin size of the ell mode
;  pixsize : the size of the pixel of the map in arcseconds
;  
;;KEYWORDS
; lmin : minimum mode to include in the computation (default is 0)
; kpower : to set if one wants to weight mode ell by ell^kpower (default is 0)
; callc: use the C program to compute the Mkk matrix (faster), if set to 1 use 
;        serial code, if set to > 1 use openMP version with callc processors
; mpi : now you can also request to run a mpi/openmp version, it will
;       run on mpi number of nodes, callc number of threads 
;       change the machinefile to use a different configuration
;               
;MODIFICATION HISTORY
;  Author: Alex Amblard, Dec 9, 2009
;           AA, Apr 14, 2010 : add the mpi option
;-
; maybe need to add something in k^kpower to take that into account
; in the mask1
; Author: Marco Viero, Dec 10, 2010 (a year later!!)
;	   add mask2 in order to calcualte cross-correlation Mkks.

;IF NOT(KEYWORD_SET(mask2)) then mask2=mask1

IF NOT(KEYWORD_SET(lmin)) THEN lmin=0
  
dims=(SIZE(mask1))[1:2]

kx=FINDGEN(dims[0]/2+1)/(FLOAT(pixsize*dims[0]*!pi/10800./60.))*2.*!pi
kx=[kx,-1.*REVERSE(kx[1:(1+dims[0])/2-1])]
ky=FINDGEN(dims[1]/2+1)/(FLOAT(pixsize*dims[1]*!pi/10800./60.))*2.*!pi
ky=[ky,-1.*REVERSE(ky[1:(1+dims[1])/2-1])]
kx=kx#(ky*0+1)
ky=(kx[*,0]*0+1)#ky
k=sqrt(kx^2+ky^2)
kx=0.
ky=0.

if keyword_set(log_deltal) then $
h=HISTOGRAM(alog10(k),binsize=log_deltal,reverse_indices=R,min=alog10(lmin)) $
else $
h=HISTOGRAM(k,binsize=deltal,reverse_indices=R,min=lmin) 

nk=n_elements(h)
Ck=dblarr(nk)
Mkk=dblarr(nk,nk)
k2=dblarr(nk)

IF NOT(KEYWORD_SET(mask2)) THEN $
   spec=ABS(FFT(double(mask1),/double))^2 $ 
   else $
   ;spec=ABS(FFT(mask1,/double)*CONJ(FFT(mask2,/double)))
   spec=FLOAT(FFT(mask1,/double)*CONJ(FFT(mask2,/double)))

FOR i=0l,nk-1 DO IF R[i] NE R[i+1] THEN k2[i]=N_ELEMENTS([R[R[I] : R[I+1]-1]])

xtgtall=R MOD dims[0]
w=WHERE(xtgtall GT dims[0]/2)
IF (w[0] NE -1) THEN xtgtall[w]-=dims[0]
ytgtall=R/dims[0]
w=WHERE(ytgtall GT dims[1]/2)
IF (w[0] NE -1) THEN ytgtall[w]-=dims[1]

DEFSYSV,'!SMAP_PIPELINE_PATH',EXISTS=pip_path
;IF KEYWORD_SET(callc) THEN $
;   IF (NOT(pip_path)) THEN BEGIN
;   PRINTF,"NO !SMAP_PIPELINE_PATH VARIABLE, WILL USE IDL INSTEAD !"
;   callc=0
;ENDIF ELSE IF (callc EQ 1) THEN IF NOT((FILE_INFO(!SMAP_PIPELINE_PATH+'/map_analysis/mkkmask')).exists) THEN BEGIN 
;   PRINTF,"NO C EXECUTABLE TO COMPUTE Mkk MATRIX, WILL USE IDL INSTEAD !"
;   callc=0
;ENDIF ELSE IF NOT((FILE_INFO(!SMAP_PIPELINE_PATH+'/map_analysis/mkkmask_omp')).exists) THEN BEGIN
;   PRINTF,"NO OMP C EXECUTABLE TO COMPUTE Mkk MATRIX, WILL USE IDL INSTEAD !"
;   callc=0
;ENDIF

IF KEYWORD_SET(mpi)  THEN $
   IF NOT((FILE_INFO(!SMAP_PIPELINE_PATH+'/map_analysis/mkkmask_omp')).exists) OR $
   NOT((FILE_INFO(!SMAP_PIPELINE_PATH+'/map_analysis/machinefile.txt')).exists) THEN BEGIN
   PRINTF,"NO OMP/MPI C EXECUTABLE OR MACHINEFILE TO COMPUTE Mkk MATRIX, WILL USE IDL INSTEAD !"
;   callc=0
ENDIF

   



IF (KEYWORD_SET(mpi)) THEN BEGIN   
   print, 'running on ' +string(mpi)+' nodes'
   nr=n_elements(R)
   randstr=STRING(RANDOMU(seed,1)*1e7,format='(I07)')
   GET_LUN,LUN
   spawn,'whoami',username
   IF KEYWORD_SET(MPI) THEN TMPDIR='/data/'+username+'/' ELSE TMPDIR='/tmp/' 
   OPENW,LUN,TMPDIR+'in_get_matrix_mkk_'+randstr+'_.dat'
   WRITEU,LUN,LONG(nk),LONG(nr),LONG(dims[0]),LONG(R),LONG(xtgtall),LONG(ytgtall),DOUBLE((spec))
   CLOSE,LUN
   FREE_LUN,LUN
;   IF (NOT(KEYWORD_SET(mpi))) THEN $
;      IF (callc EQ 1) THEN $
;         spawn,!SMAP_PIPELINE_PATH+'/map_analysis/mkkmask '+TMPDIR+'in_get_matrix_mkk_'+randstr+'_.dat '+TMPDIR+'out_get_matrix_mkk_'+randstr+'_.dat' $
;      ELSE spawn,'setenv OMP_NUM_THREADS '+string(callc,format='(I2)')+';'+!SMAP_PIPELINE_PATH+'/map_analysis/mkkmask_omp '+TMPDIR+'in_get_matrix_mkk_'+randstr+'_.dat '+TMPDIR+'out_get_matrix_mkk_'+randstr+'_.dat' $
;      ELSE $
         ;spawn,'mpirun -machinefile '+!SMAP_PIPELINE_PATH+'/map_analysis/machinefile.txt -env OMP_NUM_THREADS '+strcompress(callc,/remove_all)+$
         ;      ' -n '+string(mpi,format='(I2)')+' '+!SMAP_PIPELINE_PATH+'/map_analysis/mkkmask_ompi '+$
         ;      TMPDIR+'in_get_matrix_mkk_'+randstr+'_.dat '+TMPDIR+'out_get_matrix_mkk_'+randstr+'_.dat'
         spawn,'mpiexec -f '+!SMAP_PIPELINE_PATH+'/map_analysis/machinefile.txt ' + $ ;-env OMP_NUM_THREADS '+strcompress(callc,/remove_all)+$
               ' -n '+string(mpi,format='(I2)')+' '+!SMAP_PIPELINE_PATH+'/map_analysis/mkkmask_ompi '+$
               TMPDIR+'in_get_matrix_mkk_'+randstr+'_.dat '+TMPDIR+'out_get_matrix_mkk_'+randstr+'_.dat'

   GET_LUN,LUN
   OPENR,LUN,TMPDIR+'out_get_matrix_mkk_'+randstr+'_.dat'
   READU,LUN,Mkk
   CLOSE,LUN
   FREE_LUN,LUN
   ;SPAWN,'rm -f '+TMPDIR+'out_get_matrix_mkk_'+randstr+'_.dat'

ENDIF ELSE BEGIN

   FOR i=0l,nk-1 DO IF R[i] NE R[i+1] THEN BEGIN
      xtgt=xtgtall[R[i]:R[i+1]-1]
      ytgt=ytgtall[R[i]:R[i+1]-1]
      FOR j=i,nk-1 DO IF R[j] NE R[j+1] THEN BEGIN
         xtgt2=xtgtall[R[J]:R[J+1]-1]
         ytgt2=ytgtall[R[J]:R[J+1]-1]
         ntgt2=N_ELEMENTS(xtgt2)
         FOR k=0,ntgt2-1 DO $
            Mkk[i,j]+=TOTAL(spec[ABS(xtgt2[k]-xtgt),ABS(ytgt2[k]-ytgt)],/double)
      ENDIF
   ENDIF

ENDELSE



FOR i=0l,nk-1 DO $
   FOR j=i,nk-1 DO Mkk[j,i]=Mkk[i,j]

FOR i=0l,nk-1 DO $
   Mkk[i,*]/=k2[i]


RETURN,Mkk

END
