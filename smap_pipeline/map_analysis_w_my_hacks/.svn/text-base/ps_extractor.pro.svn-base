;+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  9/01/09
;;  Louis Levenson
;;  This program extracts point sources from Herschel maps 
;;  To run this in an IDL session or procedure use:
;;  IDL> ps_extractor,inmap250,inmap350,inmap500,DLIMIT=dlimit,$
;;                 CAT_NAME=cat_name,CHECKI_IMS=CHECK_ims,ALGORITHM=algorithm,$
;;                 MAPMASKBITS=mapmaskbits,SOURCES_FOUND=sources_found,$
;;                 OBSID=obsid, EXNAME=exname, PS_SMOOTH=ps_smooth,$
;;                 MIN_CORR=min_corr
;;  where map*** is a smap map structure
;;    map250, etc. can be read in with read_smap_fitsmap
;;    ALGORITHM chooses the finding algorithm.  The options are 
;;      1: find2 2: starfinder.  Either the string or the number
;;      can be passed in. (def: 1/'find2').
;;    DLIMIT is the detection limit in sigmas above the background r.m.s.
;;    CAT_NAME is an option output keyword for the variable containing the 
;;     name of the catalog that was written to disk in !SMAP_CATS
;;    CHECK_IMS is a keyword which, when set, will write various output maps
;;     including, background, background subtracted, source marked, 
;;     source removed
;;    MAPMASKBITS is a bit mask of unacceptable mask bits.  Pixels
;;     with these mask bits are excluded.  The default is 1uL.  Pixels
;;     with no hits are also masked.
;;    SOURCES_FOUND returns the number of total sources found in all
;;     bands
;;    OBSID and EXNAME are similar to the arguments to
;;     smap_makenaivemap, and affect the name of the file written
;;    PS_SMOOTH is an optional keyword for StarFinder
;;    MIN_CORR  sets the minimum correlation between the object and
;;     psf for StarFinder (def: 0.75).
;; To read the product of this program use:
;; IDL> result=mrdfits(!SMAP_CATS+cat_name,$
;;                     1,hresult)
;;
;; To get information on the structure:
;; IDL> help,result,/str
;;
;; To plot the resulting data:
;; IDL> plot,result.ra,result.dec,psym=3
;; IDL> plot,result.x_cen_250,result.y_cen_250,psym=3
;;
;; To index your results:
;; IDL> print,result[where(result.id lt 4)].x_cen_250
;; will print the x centers of the sources with id's less than 4
;;
;; To find the number of sources detected at 250 microns:
;; IDL> print,n_elements(result[where(results[0,*].det eq 1)])
;;
;; Dependencies: 
;; THE FOLLOWING MUST BE DEFINDED FOR THE PS_EXTRACTOR TO WORK!!
;; DEFINE DIRECTORY FOR  OUTPUT MAPS 
;; DEFSYSV,'!SMAP_CATS','/path/to/output/catalog/directory/'
;; DEFSYSV,'!SMAP_MAPS','/path/to/output/map/directory/'
;; DEFINE THE PATH TO THE SMAP PIPELINE
;; DEFSYSV,'!SMAP_PIPELINE_PATH','/Users/levenson/SPIRE/smap_pipeline/'
;;  ADD THE THE PIPELINE AND SUBDIRECTORIES TO YOUR PATH
;; !PATH=EXPAND_PATH('+'+!SMAP_PIPELINE_PATH,/ALL_DIRS)+':'+!PATH
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-

FUNCTION ps_extractor_mask, map, mapmaskbits
  COMPILE_OPT IDL2, HIDDEN
  ;;The returned mask is 1 where the pixel should
  ;; be skipped, 0 where it's good

  mask = BYTARR( map.xsize, map.ysize )
  IF map.has_mask THEN BEGIN
     wmask = WHERE( (map.mask AND mapmaskbits) NE 0, nmask )
     IF nmask NE 0 THEN mask[wmask]=1b
  ENDIF
  IF map.has_exposure THEN BEGIN
     wmask = WHERE( map.exposure EQ 0, nmask )
     IF nmask NE 0 THEN mask[wmask] = 1b
  ENDIF
  RETURN,mask
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION ps_extractor_doband, inmap, dlimit, band, base_name,$
                              CHECK_IMS=check_ims, $
                              ALG=alg, MAPMASKBITS=mapmaskbits,$
                              PRE_SMOOTH=pre_smooth, MIN_CORR=min_corr,$
                              SUCCESS=success
  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN
  success = 0b

  IF N_ELEMENTS( min_corr ) EQ 0 THEN min_corr = 0.75
  getbg = 1b ;;Required for now

  ;;Deal with masking, including no coverage regions
  mask = ps_extractor_mask( inmap, mapmaskbits )
  wgood = WHERE(mask EQ 0,ngood, COMPLEMENT=wbad, NCOMPLEMENT=nbad)
  IF ngood EQ 0 THEN BEGIN
     MESSAGE,"All pixels masked in "+band+" -- skipping",/INF
     RETURN, !VALUES.F_NAN
  ENDIF

  ;;GET PIXEL SCALE USING SMAP ASTROMETRY STRUCTURE
  getrot,inmap.astrometry,dum,cdelt
  px_scale=cdelt[1]*3600.0


  ;; GET FWHM IN PIXELS
  fwhm_pix=get_spire_beam_fwhm(band)/px_scale

  ;;Get PSF
  ;;This should be adjusted to be large enough
  psf=get_spire_beam(band,px_scale,21,21,/SILENT)

  ;; ESTIMATE BACKGROUND
  ;; This should be adjusted for size of pixel relative to PSF
  IF getbg eq 1 THEN BEGIN
     MESSAGE,'Starting Background Estimator ('+band+')',/INF
     bg=bg_estimator2(inmap.image,mask,32)
     bgsub=inmap.image-bg
  ENDIF ELSE BEGIN
     bgsub = inmap.image
  ENDELSE

  ;; GET BG STDDEV
  ;;  If maps are noiseless, just take stdev of image
  IF getbg EQ 1 THEN BEGIN
     sig=stddev(bg[wgood])
  ENDIF ELSE BEGIN
     sig = 0.0
  ENDELSE

  sig = 0
  ;;Last ditch, use minimum abs
  IF sig EQ 1 THEN BEGIN
     MESSAGE,"Having trouble with background in "+band+", using poor estimate",$
             /INF
     w2 = WHERE( inmap.image[wgood] NE 0.0, n2 )
     IF n2 EQ 0 THEN BEGIN
        MESSAGE,"Completely unable to estimate background in "+band,/INF
        RETURN,!VALUES.F_NAN
     ENDIF
     sig = MIN( inmap.image[wgood[TEMPORARY(w2)]], /ABS )
  ENDIF



  ;;Detect sources
  ;;We don't have errors from find2, so use arbitrary assumptions
  MESSAGE,'Locating '+band+' sources',/INF
  CASE alg OF
     1 : BEGIN
        find2,bgsub,x,y,f,sharp,round,dlimit*sig,$
              fwhm_pix,[0.2,1.0],[-1.0,1.0],/SILENT
        sigx = 0.5
        sigy = 0.5
        sigf = 0.1*f
     END
     2 : BEGIN
        IF inmap.has_error THEN noise_std = inmap.error
        IF nbad NE 0 THEN BEGIN
           ;;Get bad pixel coordinates
           bad_pixel_x = wbad mod inmap.xsize
           bad_pixel_y = wbad / inmap.xsize
        ENDIF
        STARFINDER,inmap.image, psf, dlimit, min_corr, x, y, f,$
                   sigx, sigy, sigf, corr, BACKGROUND=bg,$
                   X_BAD=bad_pixel_x, Y_BAD=bad_pixel_y,$
                   NOISE_STD=inmap.error, PRE_SMOOTH=pre_smooth,$
                   /REL_THRESHOLD, CORREL_MAG=2 ;, BACKGROUND=bg,BACK_BOX=32
        DELVARX, bad_pixel_x, bad_pixel_y, noise_std
     END
  ENDCASE
  nsources = N_ELEMENTS(x)
  ;;Output check images
  IF check_ims EQ 1b THEN BEGIN
     MESSAGE,'Creating Check Images',/INF

     ;;First, re-write input map
     st = write_smap_fitsmap(inmap,base_name+'_input_map')
     IF st EQ 0 THEN MESSAGE,"ERROR writing copy of input map"

     ;;Now background stuff
     map_copy = inmap
     IF getbg THEN BEGIN
        map_copy.image = bg
        st = write_smap_fitsmap(map_copy,base_name+'_bg_map')
        IF st EQ 0 THEN MESSAGE,"ERROR writing background map"
     ENDIF
     map_copy.image = bgsub
     st = write_smap_fitsmap(map_copy,base_name+'_bgsub_map')
     IF st EQ 0 THEN MESSAGE,"ERROR writing background subtracted map"

     ;;Source marked and subtracted maps
     IF nsources NE 0 THEN BEGIN
        map_copy.image = check_image(bgsub,x,y,9)
        st = write_smap_fitsmap(map_copy,base_name+'_checkim_map')
        IF st EQ 0 THEN MESSAGE,"ERROR writing object marked map"

        map_copy.image = remove_stars(inmap.image ,x, y, f, psf, alg)
        st = write_smap_fitsmap(map_copy,base_name+'_nostars_map')
        IF st EQ 0 THEN MESSAGE,"ERROR writing object subtracted map"
     ENDIF
  ENDIF
  IF nsources EQ 0 THEN RETURN,!VALUES.F_NAN

  ;;Now wrap them up as a nice structure
  retstr = REPLICATE( { x: !VALUES.F_NAN, y: !VALUES.F_NAN,$
                        f: !VALUES.F_NAN, dx: !VALUES.F_NAN,$
                        dy: !VALUES.F_NAN, df: !VALUES.F_NAN}, nsources )
  retstr.x = TEMPORARY(x)
  retstr.y = TEMPORARY(y)
  retstr.f = TEMPORARY(f)
  retstr.dx = TEMPORARY(sigx)
  retstr.dy = TEMPORARY(sigy)
  retstr.df = TEMPORARY(sigf)

  success = 1b
  RETURN,retstr

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO ps_extractor,inmap250,inmap350,inmap500,DLIMIT=dlimit,CAT_NAME=cat_name,$
                 CHECK_IMS=check_ims,ALGORITHM=algorithm, $
                 MAPMASKBITS=mapmaskbits, OBSID=obsid, EXNAME=exname,$
                 SOURCES_FOUND=sources_found, PRE_SMOOTH=pre_smooth,$
                 MIN_CORR=min_corr

On_error,2                    ;Return to caller
COMPILE_OPT idl2, strictarrsubs

MESSAGE,'Performing PS extraction',/INF

; DID YOU CHOOSE A SOURCE FINDING ALGORITHM?
IF N_ELEMENTS( algorithm ) EQ 0 THEN alg = 2 ELSE BEGIN ;;starfndr
   IF SIZE( algorithm, /TNAME ) EQ 'STRING' THEN BEGIN
      CASE STRLOWCASE(algorithm) OF
         'find2' : alg = 1
         'starfinder' : alg = 2
         ELSE : MESSAGE,"Unknown algorithm type: "+algorithm
      ENDCASE
   ENDIF ELSE BEGIN
      alg = ROUND(algorithm)
      IF alg LT 1 OR alg GT 2 THEN MESSAGE,"Unknown algorithm number: "+$
                                           STRING(algorithm)
   ENDELSE
ENDELSE

;; DID YOU CHOOSE A DETECTION LIMIT?
IF N_ELEMENTS(dlimit) EQ 0 THEN BEGIN
   MESSAGE,'WARNING: DETECTION LIMIT NOT SET, USING: 15 sigma',/INFORMATIONAL
   MESSAGE,'WARNING: THIS MAY AFFECT YOUR SOURCE DETECTION SUCCESS!',$
           /INFORMATIONAL
   dlimit=15.
ENDIF

IF N_ELEMENTS( mapmaskbits ) EQ 0 THEN mapmaskbits = 1uL
IF N_ELEMENTS( check_ims ) EQ 0 THEN check_ims = 0

;;Figure out base name used to make catalog and check image names
IF N_ELEMENTS( obsid ) NE 0 THEN BEGIN
   IF SIZE(obsid,/TNAME) EQ 'STRING' THEN base_name = obsid ELSE $
      base_name = '0x'+TO_HEX(obsid)
   IF N_ELEMENTS(exname) NE 0 && STRLEN(exname) NE 0 THEN $
      base_name += '_'+exname
   CASE alg OF 
      1 : base_name += '_find'
      2 : base_name += '_starfndr'
   ENDCASE 
ENDIF ELSE BEGIN
   CASE alg OF 
      1 : base_name = 'find'
      2 : base_name = 'starfndr'
   ENDCASE 
ENDELSE

;;Do each band
cat250 = ps_extractor_doband( inmap250, dlimit, 'PSW', base_name,$
                              CHECK_IMS=check_ims, $
                              ALG=alg, MAPMASKBITS=mapmaskbits,$
                              SUCCESS=success250, PRE_SMOOTH=pre_smooth,$
                              MIN_CORR=min_corr )
cat350 = ps_extractor_doband( inmap350, dlimit, 'PMW', base_name,$
                              CHECK_IMS=check_ims, $
                              ALG=alg, MAPMASKBITS=mapmaskbits,$
                              SUCCESS=success350, PRE_SMOOTH=pre_smooth,$
                              MIN_CORR=min_corr )
cat500 = ps_extractor_doband( inmap500, dlimit, 'PLW', base_name,$
                              CHECK_IMS=check_ims, $
                              ALG=alg, MAPMASKBITS=mapmaskbits,$
                              SUCCESS=success500, PRE_SMOOTH=pre_smooth,$
                              MIN_CORR = min_corr )
IF ~ (success250 OR success350 OR success500) THEN GOTO,nosource
n250=N_ELEMENTS(cat250) & n350=N_ELEMENTS(cat350) & n500=N_ELEMENTS(cat500)
ntot = n250+n350+n500
MESSAGE,STRING(ntot,FORMAT='("Found ",I0," total sources")'),/INF
MESSAGE,STRING(n250,n350,n500,FORMAT='(" PSW:",I0," PMW:",I0," PLW:",I0)'),/INF

table = REPLICATE( { id: 0L, $
                     x_cen_250: !VALUES.F_NAN, y_cen_250: !VALUES.F_NAN,$
                     dx_cen_250: !VALUES.F_NAN, dy_cen_250: !VALUES.F_NAN,$
                     x_cen_350: !VALUES.F_NAN, y_cen_350: !VALUES.F_NAN,$
                     dx_cen_350: !VALUES.F_NAN, dy_cen_350: !VALUES.F_NAN,$
                     x_cen_500: !VALUES.F_NAN, y_cen_500: !VALUES.F_NAN,$
                     dx_cen_500: !VALUES.F_NAN, dy_cen_500: !VALUES.F_NAN,$
                     ra: !VALUES.D_NAN, dec: !VALUES.D_NAN,$
                     dra: !VALUES.F_NAN, ddec: !VALUES.F_NAN,$
                     glon: !VALUES.D_NAN, glat: !VALUES.D_NAN,$
                     dglon: !VALUES.F_NAN, dglat: !VALUES.F_NAN,$
                     f_250: !VALUES.F_NAN, df_250: !VALUES.F_NAN,$
                     f_350: !VALUES.F_NAN, df_350: !VALUES.F_NAN,$
                     f_500: !VALUES.F_NAN, df_500: !VALUES.F_NAN,$
                     det: BYTARR(3), conf: BYTARR(3), blend: BYTARR(3) },$
                   ntot )
table.id = LINDGEN( ntot )                     

IF n250 NE 0 THEN BEGIN
   mini = 0 & maxi = n250-1
   table[mini:maxi].x_cen_250 = cat250.x
   table[mini:maxi].y_cen_250 = cat250.y
   table[mini:maxi].dx_cen_250 = cat250.dx
   table[mini:maxi].dy_cen_250 = cat250.dy
   xy2ad, cat250.x, cat250.y, inmap250.astrometry, ra250, dec250
   euler,ra250,dec250,glon250,glat250,1
   getrot,inmap250.astrometry,dum,cdelt
   px_scale=cdelt[1]
   table[mini:maxi].dra = cat250.dx*px_scale/cos(dec250*3.14159/180.)
   table[mini:maxi].ddec = cat250.dy*px_scale
   table[mini:maxi].dglon=cat250.dx*px_scale/cos(glat250*3.14159/180.)
   table[mini:maxi].dglat=cat250.dy*px_scale
   table[mini:maxi].ra = ra250
   table[mini:maxi].dec = dec250
   table[mini:maxi].glat = glat250
   table[mini:maxi].glon = glon250
   table[mini:maxi].det[0]=1b
   table[mini:maxi].f_250 = cat250.f
   table[mini:maxi].df_250 = cat250.df
ENDIF
IF n350 GT 1 THEN BEGIN
   mini = n250 & maxi = n250+n350-1
   table[mini:maxi].x_cen_350 = cat350.x
   table[mini:maxi].y_cen_350 = cat350.y
   table[mini:maxi].dx_cen_350 = cat350.dx
   table[mini:maxi].dy_cen_350 = cat350.dy
   xy2ad, cat350.x, cat350.y, inmap350.astrometry, ra350, dec350
   euler,ra350,dec350,glon350,glat350,1
   getrot,inmap350.astrometry,dum,cdelt
   px_scale=cdelt[1]
   table[mini:maxi].dra = cat350.dx*px_scale/cos(dec350*3.14159/180.)
   table[mini:maxi].ddec = cat350.dy*px_scale
   table[mini:maxi].dglon=cat350.dx*px_scale/cos(glat350*3.14159/180.)
   table[mini:maxi].dglat=cat350.dy*px_scale
   table[mini:maxi].ra = ra350
   table[mini:maxi].dec = dec350
   table[mini:maxi].glat = glat350
   table[mini:maxi].glon = glon350
   table[mini:maxi].det[1]=1b
   table[mini:maxi].f_350 = cat350.f
   table[mini:maxi].df_350 = cat350.df
ENDIF
IF n500 GT 1 THEN BEGIN
   mini = n250+n350 & maxi = n250+n350+n500-1
   table[mini:maxi].x_cen_500 = cat500.x
   table[mini:maxi].y_cen_500 = cat500.y
   table[mini:maxi].dx_cen_500 = cat500.dx
   table[mini:maxi].dy_cen_500 = cat500.dy
   xy2ad, cat500.x, cat500.y, inmap500.astrometry, ra500, dec500
   euler,ra500,dec500,glon500,glat500,1
   getrot,inmap500.astrometry,dum,cdelt
   px_scale=cdelt[1]
   table[mini:maxi].dra = cat500.dx*px_scale/cos(dec500*3.14159/180.)
   table[mini:maxi].ddec = cat500.dy*px_scale
   table[mini:maxi].dglon=cat500.dx*px_scale/cos(glat500*3.14159/180.)
   table[mini:maxi].dglat=cat500.dy*px_scale
   table[mini:maxi].ra = ra500
   table[mini:maxi].dec = dec500
   table[mini:maxi].glat = glat500
   table[mini:maxi].glon = glon500
   table[mini:maxi].det[2]=1b
   table[mini:maxi].f_500 = cat500.f
   table[mini:maxi].df_500 = cat500.df
ENDIF

; CREATE DUMMY FITS BINARY TABLE HEADER
fxbhmake,h,1,/INITIALIZE

; NOW ADD SOME HOUSEKEEPING HEADER INFORMATION

; ADD THINGS LIKE INPUT FILENAME, DATE CREATED, FIELD ID, DEPTH, ETC.
sxaddpar,h,'CR_DATE',systime(),'Date File Created'
       
;;Add units
SXADDPAR,h,'TUNIT2','pixels' ;;x_cen_250
SXADDPAR,h,'TUNIT3','pixels' ;;y_cen_250
SXADDPAR,h,'TUNIT4','pixels' ;;dx_cen_250
SXADDPAR,h,'TUNIT5','pixels' ;;dy_cen_250
SXADDPAR,h,'TUNIT6','pixels' ;;x_cen_350
SXADDPAR,h,'TUNIT7','pixels' ;;y_cen_350
SXADDPAR,h,'TUNIT8','pixels' ;;dx_cen_350
SXADDPAR,h,'TUNIT9','pixels' ;;dy_cen_350
SXADDPAR,h,'TUNIT10','pixels' ;;x_cen_500
SXADDPAR,h,'TUNIT11','pixels' ;;y_cen_500
SXADDPAR,h,'TUNIT12','pixels' ;;dx_cen_500
SXADDPAR,h,'TUNIT13','pixels' ;;dy_cen_500
SXADDPAR,h,'TUNIT14','deg'    ;;ra
SXADDPAR,h,'TUNIT15','deg'    ;;dec
SXADDPAR,h,'TUNIT16','deg'    ;;dra
SXADDPAR,h,'TUNIT17','deg'    ;;ddec
SXADDPAR,h,'TUNIT18','deg'    ;;glon
SXADDPAR,h,'TUNIT19','deg'    ;;glat
SXADDPAR,h,'TUNIT20','deg'    ;;dglon
SXADDPAR,h,'TUNIT21','deg'    ;;dglat
SXADDPAR,h,'TUNIT22','Jy'    ;;f_250
SXADDPAR,h,'TUNIT23','Jy'    ;;df_250
SXADDPAR,h,'TUNIT24','Jy'    ;;f_350
SXADDPAR,h,'TUNIT25','Jy'    ;;df_350
SXADDPAR,h,'TUNIT26','Jy'    ;;f_500
SXADDPAR,h,'TUNIT27','Jy'    ;;df_500

; WRITE THE CATALOG
cat_name=base_name+'_catalog.fits'
mwrfits,table,addslash(!SMAP_CATS)+cat_name,h,/CREATE, STATUS=status
IF status LT 0 THEN MESSAGE,"ERROR outputting catalog"
MESSAGE,'PS_EXTRACTOR: Output written to: '+addslash(!SMAP_CATS)+cat_name,/INF
sources_found = ntot
RETURN
 
nosource: 
MESSAGE,"No sources were found above user specified dlimit",/INF
sources_found=0
RETURN

;;;;;;;;;;;;;;;;;;; THIS IS THE END ;;;;;;;;;;;;;;;;;;;;

END
