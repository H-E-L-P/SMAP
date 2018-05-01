FUNCTION fill_hole,dat,pos_hole,n_hole,lc=lc,linear=linear
;+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  function fill_hole.pro
;;  Nov 19, 2009
;;  Alex Amblard
;;  This function fill a hole with :
;;       1) some constrained noise realization using algorithm described
;;          in Hoffman Y. and Ribak E., ApJ 1991, 380, L5 
;;       2) a combination of some noise realization + a linear
;;       component to match edges of the signal
;;
;;  The timestream must not have any noise before the hole for
;;  more than 2*lc+n_hole samples since they are used to compute the correlation
;;  function
;;
;;  Options: lc : the length of the correlation to be used in method 1)
;;                default is 101 or the number of element available  
;;                before the hole divided by 5 (whichever is smaller)
;;
;;           linear : the keyword linear activate method 2) instead of 1)
;;  Outputs: a new time ordered data with the hole data replaced
;;  Changelog: v1.0, AA, Nov 19 2009, original
;;             
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-

data=dat

IF KEYWORD_SET(linear) THEN BEGIN

pse=(abs(fft(data[0:pos_hole-1]))^2)[0:pos_hole/2]*pos_hole
pse=[pse,reverse(pse[1:pos_hole/2-1])]
epsi=(float(fft(fft(randomn(seed,pos_hole))*sqrt(pse),/inverse)))[0:n_hole-1]
res=poly_fit(findgen(n_hole), epsi, 1)
epsi=epsi+((data[pos_hole+n_hole]-data[pos_hole-1])/n_hole-res[1])*findgen(n_hole)+(data[pos_hole-1]-res[0])
pnew=[data[0:pos_hole-1],epsi,data[n_hole+pos_hole:*]]

ENDIF ELSE BEGIN

   data0=data[0]
   data-=data0

   nele=N_ELEMENTS(data)
   IF NOT(KEYWORD_SET(lc)) THEN lc=MIN([101,pos_hole/5.])

   data1=data[nele-1]
   data-=data1*findgen(nele)/(nele-1)

   
   Ntlc=dblarr(2*lc,2*lc)
   fc=a_correlate(data[0:pos_hole-1],findgen(2*lc+n_hole),/covariance,/double)

   FOR i=0,lc-1 DO FOR j=i,lc-1 DO Ntlc[i,j]=fc[j-i]
   FOR i=0,lc-1 DO FOR j=i,lc-1 DO Ntlc[i+lc,j+lc]=fc[j-i]
   FOR i=0,lc-1 DO FOR j=0,lc-1 DO Ntlc[i,j+lc]=fc[lc-i+n_hole+j]
   FOR i=0,2*lc-1 DO FOR j=0,i DO Ntlc[i,j]=Ntlc[j,i]


   Ntlci=INVERT(Ntlc,/double)

   N=DBLARR(n_hole,2*lc)
   FOR i=0,n_hole-1 DO FOR j=0,lc-1 DO N[i,j]=fc[lc-j+i]
   FOR i=0,n_hole-1 DO FOR j=lc,2*lc-1 DO N[i,j]=fc[j-lc+n_hole-i]


   partforspec=[data[0:pos_hole-1],reverse(data[0:pos_hole-1])]
   pse=((ABS(FFT(partforspec))^2)[0:pos_hole])*pos_hole*2
   pse=[pse,REVERSE(pse[1:pos_hole-1])]
   epsi=(FLOAT(FFT(FFT(RANDOMN(seed,pos_hole*2))*SQRT(pse),/inverse)))[0:2*lc+n_hole-1]



   dgap=epsi[lc:lc+n_hole-1]+N#Ntlci#([data[pos_hole-lc:pos_hole-1],data[pos_hole+n_hole:pos_hole+n_hole+lc-1]]-[epsi[0:lc-1],epsi[lc+n_hole:*]])
   
   pnew=[data[0:pos_hole-1],dgap,data[pos_hole+n_hole:*]]

   pnew+=data0
   pnew+=data1*findgen(nele)/(nele-1)

ENDELSE


RETURN,pnew

END
