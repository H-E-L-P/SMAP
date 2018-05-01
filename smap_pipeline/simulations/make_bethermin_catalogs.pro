;SCRIPT TO PRODUCE CATALOGS
;Marco Viero, Nov 2011

;USAGE:
;.r make_bethermin_catalogs.pro 
;
;;;;;;;;EDIT IN THIS BOX;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

write_cat=1 ; 1 to write catalogs
its=1.;50.;00.     ; Number of Catalogs to make
area=25.;100.     ; Area of sky to fill

z0=0.01;0010   ; Desired z-range
z1=10;20.
l0=9.0      ; Desired Luminosity range
l1=13.5

wavelength=[100,250,350,500]; can add wavelengths if desired, e.g., ,1380,2018]
wavelength=[250,350,500, 1360, 2000, 3330]; can add wavelengths if desired, e.g., ,1380,2018]
;wavelength=[250,350,500]; can add wavelengths if desired, e.g., ,1380,2018]

dir='/data/viero/cats/bethsims/'
dir_templates='/home/viero/smaproot/smap_pipeline/simulations/data/'
exname='_cm'
exname='_w_iras'
exname='_test'
exname='_hermes_counts_sims'
exname='_spt_6band'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

cold_file=dir_templates+'ias_cold.fits'
starburst_file=dir_templates+'ias_starburst.fits'
dndlogldzdomega_file=dir_templates+'bethermin_dNdLogLdzdOmega.fits'
cold = mrdfits(cold_file,1,head,/silent)
starburst = mrdfits(starburst_file,1,head,/silent)

;IF FILE_TEST(dndlogldzdomega_file) THEN BEGIN
   dndlogldzdomega = mrdfits(dndlogldzdomega_file,1);mrdfits(dndlogldzdomega_file,1,head,/silent)
;   stop
;ENDIF ELSE BEGIN
;   nn=2000
;   zvals=loggen(z0,z1,nn)
;  lumvals=loggen(l0,l1,nn)
;   dndlogldzdomega=bethermin_dndlogldzdomega(zvals, lumvals)
   ;NgS0=1.9031499e+08
   ;ALEX HAS 7.0533327e+08
   ;NgS0=2.7351051e+09
   NgS0=7.0533327e+08
   ;NgS0=3.5e+08
   nmapsrcs = NgS0 * (!PI / 180.0)^2. *area;* imangx * imangy * (res/3600.)^2.
;ENDELSE

;stop
for it=0,its-1 do begin

   n_total = nmapsrcs;double( floor(round(nmapsrcs + RANDOMN(seed) * SQRT(nmapsrcs))))
   file_name=strcompress(dir+'bethermin_catalog_'+$
      string(area, format='(d10.1)')+'deg2_'+$
      string(it,format='(i100)')+exname+'.sav',/remove_all)
   allcat=bethermin_gencat(n_total, dndlogldzdomega, cold, starburst,$
      WAVE=wavelength, $
      ;SIGMAPOP=sigmapop, LPOP=lpop, $
      SEED=seed,/VERB)

   ;So, I have a catalog on disk with 1e6 bethermin sources.  That corresponds to 
   ;to 4.65 sq deg, or 1.418e-3 sr. 

   ;So, forming dN/dS in 0.5 mJy bins (recall the fluxes output by get cat are in mJy):
   cat =allcat[ WHERE(allcat.obsflux[0] LT 100.0)]
   h = HISTOGRAM(cat.obsflux[0],BIN=0.5,LOCATIONS=flux)
   ;;Shift locations to bin centers
   flux += 0.25
   ;;convert dN/dS dOmega from N per 0.5 mJy per 1.418e-3 sr
   ;;to N per Jy per sr
   h *= 2000.0/(area*(!pi/180.)^2.)
   ;;convert flux to Jy
   flux *= 1d-3
   ;;now integrate S^2 dN/dS dS
   print,"Shot noise: ",TSUM(flux,flux^2.*h)
   ;stop
   ;I get 9148 Jy^2/sr
   ;print, strcompress('seed is '+string(seed))
   allcat.obsflux/=1000.
   if write_cat eq 1 then $
      save,filename=file_name,allcat
   if write_cat eq 0 then stop
   ;if it eq 10 then stop

endfor
end

