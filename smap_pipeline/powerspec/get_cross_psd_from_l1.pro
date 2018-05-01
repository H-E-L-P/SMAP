;+
;NAME
; get_cross_psd_from_l1
;PURPOSE
; To get cross-PSD estimates from a set of L1 time streams, combining
; them all into PSDs for each channel.
;USAGE
; get_cross_psd_from_l1,files,outfile
;INPUTS
; files         Either a list of FITS files to read data from, or the
;                name of the root directory of an HCSS output product.
;                If the former, the files are assumed to be in L1 format
;                as output from HIPE
; outfile       A FITS file containing the output cross-spectra for
;                all of the detectors.  Cross-spectra between
;                different passbands are not considered.
;OUTPUT FILE FORMAT
; A fits file with 4 extensions.  The first is a fits binary table
; giving the name of each combination, the two detectors involved,
; the units of the combined output, and the number of files used.  
; The second extension is a FITS image that gives the actual
; cross-PSDs. The first column is the frequency.  The third extension
; gives the names of the files used.  The fourth extension gives the
; mask bits for each input file (this should be read as unsigned integers
; using, for example, MRDFITS( ..., /UNSIGNED ) ).
;OPTIONAL INPUTS
;  minfreq          The minimum desired non-zero frequency in the 
;                    output PSDs (Hz).  If this can't be accomplished
;                    with the given data files, the code fails
;                    (Def: 0.012, giving 4 samples
;                    below the expected 1/f noise knee of 0.048 Hz).
;  poly_order       Order of polynomial if poly_remove is set.  Must
;                    be from 1 to 6.  The default is 5.
;  badmask          A bitmask set to whatever bits in the masks to
;                    reject.  See construct_mask_bitmask.  This should
;                    be a 32bit unsigned int.  The default is
;                    1uL -- i.e., the master bit
;OPTIONAL OUTPUTS
; success       1 on success, 0 on failure
; errmsg        An error message in case of failure
;KEYWORDS
; poly_remove      Remove a low polynomial from the data before
;                   transforming.
; usehamming       Use a Hamming window function.  This turns on data
;                   overlapping.
; usehanning       Use a Hanning window function.  This turns on data
;                   overlapping.
; nooverlap        Don't overlap data segments.
; nonancheck       Don't check for NaNs in inputs
; fullsize         Always use full available length for estimating PSD.
; nopow2           Don't attempt to round chunk size up to power of 2
; verbose          Run in verbose mode
;NORMALIZATION
;  Note that the cross-PSDs will not fit in a fits binary table, as
;   only 999 columns are allowed.
;  There are a semi-infinite number of conventions for normalizing the
;   PSD.  The one used here is:
;    \int_{0}^{f_c} PSD df = 1/T \int_{0}^{T} f dt
;   where T is the total data sample time, f_c is the Nyquist
;   frequency, and f is the signal.  These are therefore one-sided
;   PSDs.  (note: this differs very slightly from the conventions of 
;   cross_correlate).  So, if f is in Jy, then
;   this has units of Jy^2 Hz^-1.  The cross-PSDs are not guaranteed
;   to be positive.
;NOTES
;  The data must not have unfilled gaps.
;  Normally a check is done for NaN values in the input streams (which
;   sometimes occur in simulator output).  If this check is done, a
;   new maskbit is added for non-Finite values.
;  In order to maximize the signal to noise of the PSD, the code tries
;   to use the shortest length transform it can consistent MINFREQ.
;MODIFICATION HISTORY
; Author: Alex Conley, April 2009
;-
FUNCTION get_cross_psd_getcombos, tags, NCOMBOS=ncombos

  COMPILE_OPT IDL2, HIDDEN, STRICTARRSUBS

  success = 0b
  detlist = ['PSW','PMW','PLW']
  dettags = STRUPCASE(STRMID(tags,0,3))
  ncombos = INTARR(3)

  FOR i=0,N_ELEMENTS(detlist)-1 DO BEGIN
     w = WHERE(dettags EQ STRUPCASE(detlist[i]),n)
     IF n EQ 0 THEN CONTINUE
     ncombos[i] = n*(n-1)/2 ;; n choose 2
     curr_combo = INTARR( ncombos[i], 2 )
     curridx = 0
     FOR j=0,n-1 DO FOR k=j+1,n-1 DO BEGIN
        curr_combo[curridx,*] = [w[j],w[k]]
        curridx += 1
     ENDFOR

     IF N_ELEMENTS(retarr) EQ 0 THEN retarr = curr_combo ELSE $
        retarr = [retarr,curr_combo]

  ENDFOR

  IF N_ELEMENTS(retarr) EQ 0 THEN RETURN,!VALUES.F_NAN
  RETURN,retarr

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION get_cross_psd_from_l1_inner, file, nchunk, badmask,$
                                      mask_image, EXT1_STRUCT=ext1_struct,$
                                      NDATAPOINTS=ndatapoints,$
                                      POLY_REMOVE=poly_remove,$
                                      POLY_ORDER=poly_order, $
                                      USEHANNING=usehanning,$
                                      USEHAMMING=usehamming, $
                                      NOOVERLAP=nooverlap, $
                                      VERBOSE=verbose, SUCCESS=success, $
                                      ERRMSG=errmsg, MASK_INFO=mask_info,$
                                      NONANCHECK=nonancheck

  COMPILE_OPT IDL2, STRICTARRSUBS, HIDDEN

  success = 0b
  DELVARX,outmask

  IF ~ FILE_TEST(file) THEN BEGIN
     errmsg = "Input file "+file+" does not exist"
     RETURN,!VALUES.F_NAN
  END
  IF ~ FILE_TEST(file,/READ) THEN BEGIN
     errmsg = "Input file "+file+" exists but is unreadable"
     RETURN,!VALUES.F_NAN
  END

  primary_head = HEADFITS( file, /SILENT, ERRMSG=errmsg )
  IF errmsg NE '' THEN BEGIN
     errmsg = 'Unable to read primary header '+errmsg
     RETURN,!VALUES.F_NAN
  ENDIF

  FITS_OPEN, file, fcb, /NO_ABORT, MESSAGE=errmsg
  FITS_CLOSE, fcb
  IF errmsg NE '' THEN RETURN,!VALUES.F_NAN
  extnames = STRUPCASE( fcb.extname )
  mask_hdu = ( WHERE( extnames EQ 'MASK', nmask ) )[0]
  IF nmask NE 1 THEN BEGIN
     errmsg = "Error finding mask extension in "+file
     RETURN,!VALUES.F_NAN
  ENDIF
  signal_hdu = ( WHERE( extnames EQ 'SIGNAL', nmask ) )[0]
  IF nmask NE 1 THEN BEGIN
     errmsg = "Error finding signal extension in "+file
     RETURN,!VALUES.F_NAN
  ENDIF
  DELVARX,fcb

  mask_status=0
  mask = MRDFITS( file, mask_hdu, /SILENT, STATUS=mask_status )
  IF mask_status LT 0 THEN BEGIN
     errmsg = "Error reading mask HDU from "+file
     RETURN,!VALUES.F_NAN
  ENDIF

  signal_status = 0
  signal = MRDFITS( file, signal_hdu, signal_head, /SILENT, $
                    STATUS=signal_status )
  IF mask_status LT 0 THEN BEGIN
     errmsg = "Error reading signal HDU from "+file
     RETURN,!VALUES.F_NAN
  ENDIF
  nsignal = N_ELEMENTS(signal)
  IF ARG_PRESENT(ndatapoints) THEN ndatapoints=nsignal

  ;;Get units
  has_units = 0b
  signal_units = SXPAR( TEMPORARY(signal_head), 'TUNIT*' )
  IF N_ELEMENTS(signal_units) NE 0 THEN BEGIN
     signal_units=STRTRIM(signal_units,2)
     has_units=1b
  ENDIF

  ;;Get bit settings for masks
  mask_info = get_mask_bits(primary_head,SUCCESS=mask_bits_success,$
                            ERRMSG=errmsg )
  IF ~ mask_bits_success THEN BEGIN
     errmsg += "In get_maskbits: "+errmsg
     RETURN,!VALUES.F_NAN
  ENDIF
  ;;Add extra bit for NaN mask
  IF ~ KEYWORD_SET( nonancheck ) THEN BEGIN
     maxbit = MAX(mask_info.bits)
     IF maxbit GE 2uL^31 THEN BEGIN
        errmsg = "No room for more bits in mask for "+file
        RETURN,!VALUES.F_NAN
     ENDIF
     new_maskbit = maxbit*2uL
     badmask OR= new_maskbit ;;We can't survive this one, so make sure it's set
     new_mask_info = { name: 'maskNonFinite', bits: new_maskbit, $
                       comment: 'Non-Finite value' }
     IF N_ELEMENTS(mask_info) EQ 0 THEN mask_info = new_mask_info ELSE $
        mask_info = [mask_info,new_mask_info]
  ENDIF


  IF nsignal NE N_ELEMENTS(mask) THEN BEGIN
     errmsg = "Signal and mask not same length"
     RETURN,!VALUES.F_NAN
  ENDIF

  IF N_ELEMENTS( signal ) LT nchunk THEN BEGIN
     errmsg = "Signal length is shorter than nchunk"
     RETURN,!VALUES.F_NAN
  ENDIF

  ;;Test to be sure the sample time is monotonic
  IF ~ TAG_EXIST( signal, 'SAMPLETIME', /TOP_LEVEL ) THEN BEGIN
     errmsg = "Input signal not in right format -- no sample time!"
     RETURN,!VALUES.F_NAN
  ENDIF
  ftol = 1e-3
  delta = signal[1].sampletime - signal[0].sampletime
  IF MAX(ABS(signal[1:*].sampletime - signal[0:nsignal-2].sampletime - delta))/$
     ABS(delta) GT ftol THEN BEGIN
     errmsg = "Input signal time sampling not monotonic"
     RETURN,!VALUES.F_NAN
  ENDIF

  ;;Get list of detector related tags.
  ;; we only look for P[SML]W tags
  ;;It's probably safe to assume that the mask and signal have
  ;; the same set of tags in the same order, but just to be cautious
  ;; the code will not make this assumption
  mask_tags = TAG_NAMES(mask)
  signal_tags = TAG_NAMES(signal)
  ;;Make sure these are unique or things will break down the line
  IF N_ELEMENTS( UNIQ( signal_tags, SORT(signal_tags) ) ) NE $
     N_ELEMENTS(signal_tags) THEN BEGIN
     errmsg = "Non unique signal tags found in "+file
     RETURN,!VALUES.F_NAN
  ENDIF
  IF N_ELEMENTS( UNIQ( mask_tags, SORT(mask_tags) ) ) NE $
     N_ELEMENTS(mask_tags) THEN BEGIN
     errmsg = "Non unique mask tags found in "+file
     RETURN,!VALUES.F_NAN
  ENDIF
  maskf3 = STRUPCASE(STRMID(mask_tags,0,3))
  signalf3 = STRUPCASE(STRMID(signal_tags,0,3))

  ;;These will now be index lists into the mask and signal
  ;; structures giving the indexes of things we want to take
  ;; PSDs of.  So mask.(mask_dets) is all the detectors
  mask_dets = WHERE( maskf3 EQ 'PSW' OR maskf3 EQ 'PMW' OR $
                     maskf3 EQ 'PLW', nmask_dets )
  signal_dets = WHERE( signalf3 EQ 'PSW' OR signalf3 EQ 'PMW' OR $
                       signalf3 EQ 'PLW', nsignal_dets )
  DELVARX,maskf3,signalf3
  IF nmask_dets NE nsignal_dets THEN BEGIN
     errmsg = "Mask and signal structures don't have same number of detectors"
     RETURN,!VALUES.F_NAN
  ENDIF
  IF nmask_dets EQ 0 THEN BEGIN
     errmsg = "No detectors found in mask or signal extensions"
     RETURN,!VALUES.F_NAN
  ENDIF
  signal_det_tags = signal_tags[signal_dets]

  ;;Make sure all names are unique
  nuniq=N_ELEMENTS(UNIQ(signal_tags[signal_dets],$
                        SORT( signal_tags[signal_dets])))
  IF nuniq NE nsignal_dets THEN BEGIN
     errmsg = "Non-unique detector names in signal"
     RETURN,!VALUES.F_NAN
  ENDIF

  ;;Now, we make sure they have the same list of detectors
  wbad = WHERE( mask_tags[mask_dets] NE signal_tags[signal_dets], nbad )
  IF nbad NE 0 THEN BEGIN
     errmsg = "The mask and signal structures don't have all the same tags"
     RETURN,!VALUES.F_NAN
  ENDIF

  ;;Since we aren't going to assume that they are in the same order,
  ;; we replace mask_dets with a reordered version so that it comes out
  ;; in the same order as signal_dets.  
  ;; After this, we have 
  ;;    signal.(signal_dets) is the same detector as mask.(mask_dets)
  mord = MATCH_ORDER( signal_tags[signal_dets], mask_tags[mask_dets] )
  IF mord[0] EQ -1 THEN BEGIN
     errmsg = "Couldn't match order of signal and mask detector lists"
     RETURN,!VALUES.F_NAN
  ENDIF
  mask_dets = mask_dets[mord]
  mask_det_tags = mask_tags[mask_dets]

  ;;Now build the list of all combinations of detectors we want
  ;; This consists of an array of pairs of indicies into 
  ;; signal_dets.  So, for example, one thing we want to calculate
  ;; is the cross PSD of signal.( signal_dets[array[i,0]] ) 
  ;;  and signal.(signal_dets[array[i,1]])
  ;;We don't do cross-band PSDs.
  comb_index = get_cross_psd_getcombos( signal_det_tags, $
                                        NCOMBOS=ncombos )
  ntot = TOTAL(ncombos,/INTEGER)
  npsw = ncombos[0] & npmw = ncombos[1] & nplw = ncombos[2]
  IF ntot EQ 0 THEN BEGIN
     errmsg = "No detectors found when making combos"
     RETURN,!VALUES.F_NAN
  ENDIF
  IF KEYWORD_SET(verbose) THEN BEGIN
     MESSAGE,"Sampling "+STRING(ntot,FORMAT='(I4)')+$
             " detectors",/INFOR
     MESSAGE,STRING(npsw,npmw,nplw,$
                    FORMAT='(" PSW: ",I3," PMW: ",I3," PLW: ",I3)'),/INF
  ENDIF

  ;;Start building the first extension, which will hold the combined
  ;; name, the name of each detector, the units, the number of
  ;; contributing files. 
  ;;We can't fill these all in yet
  ;;The first column will be frequency
  IF ARG_PRESENT( ext1_struct ) THEN BEGIN
     ext1_struct = REPLICATE( { name: '', det1: '', det2: '',$
                                units: '', nfiles: 0}, ntot+1 )
     shorttags = STRMID(signal_det_tags,3) ;;; i.e., PSWA11 -> A11
     ext1_struct[0].name = 'Freq'
     ext1_struct[0].units = 'Hz'

     ;;Put dets always in alphabetic order within each -- i.e., 
     ;;  A2_C3 never C3_A2
     FOR m = 0, ntot-1 DO BEGIN
        idx0 = comb_index[m,0]
        idx1 = comb_index[m,1]
        IF shorttags[idx0] LT shorttags[idx1] THEN BEGIN
           idx0s = idx0 & idx1s = idx1 
        ENDIF ELSE BEGIN
           idx0s = idx1 & idx1s = idx0
        ENDELSE
        band = STRMID( signal_det_tags[idx0], 0, 3 )
        ext1_struct[m+1].name = band + '_' + shorttags[idx0s] + '_' + $
                                shorttags[idx1s]
        ext1_struct[m+1].det1 = signal_det_tags[idx0s]
        ext1_struct[m+1].det2 = signal_det_tags[idx1s]
        IF has_units THEN BEGIN
           unit1 = signal_units[ idx0s ]
           unit2 = signal_units[ idx1s ]
           IF unit1 EQ unit2 THEN BEGIN
              ext1_struct[m+1].units = unit1+'^2/Hz'
           ENDIF ELSE BEGIN
              ext1_struct[m+1].units = unit1+' '+unit2+'/Hz'
           ENDELSE
        ENDIF
     ENDFOR
  ENDIF


  ;;The third extension will be another image, this one giving the
  ;; mask for each input file for each cross channel.  We will ignore
  ;; the first channel, since there's nothing to put there,
  ;; and put the names of the files in the header.  Hopefully there
  ;; are never more than 9999!  The masks from different files will 
  ;; be stitched together later
  mask_image = ULONARR( ntot+1 )
  FOR i=0,ntot-1 DO BEGIN
     comb_mask = ULONG(mask.( mask_dets[comb_index[i,0]] ) OR $
                       mask.( mask_dets[comb_index[i,1]] ))
     IF MAX(comb_mask) GT 0 THEN BEGIN
        bitmask = comb_mask[0]
        FOR j=1, N_ELEMENTS(comb_mask)-1 DO bitmask OR= comb_mask[j]
        mask_image[i+1] = bitmask
     ENDIF
  ENDFOR

  ;;The second extension is just an image of the cross-PSDs in the
  ;; order specified in extension1.  Using a fits binary table would
  ;; be nice, but it turns out you can't have more than 999 columns,
  ;; and there are about 16000 cross spectra, so it work
  psd_image = DBLARR( ntot+1, nchunk/2+1 )

  ;;Main loop for cross PSDs
  ncalculated=0
  FOR i = 0, ntot-1 DO BEGIN
     IF KEYWORD_SET( verbose ) THEN $
        MESSAGE," Doing "+ext1_struct[i+1].name,/INF
     
     idx0 = signal_dets[comb_index[i,0]]
     idx1 = signal_dets[comb_index[i,1]]

     IF ~ KEYWORD_SET( nonancheck ) THEN BEGIN
        ;;The simulator returns NaNs sometimes, so it's good to check this
        wnfin = WHERE( ~ FINITE(signal.(idx0)) OR $
                       ~ FINITE(signal.(idx1)), nnfin )
        IF nnfin NE 0 THEN mask_image[i+1] += new_maskbit
     ENDIF

     ;;Check against badmask to see if we are doing this one or not
     IF (mask_image[i+1] AND badmask) NE 0 THEN BEGIN
        ;;Guess not
        psd_image[i+1,*] = 0.0 ;;These will be NaNd later if all files are bad
        CONTINUE
     ENDIF

     psd_image[i+1,*] = cross_correlate( signal.(idx0),signal.(idx1), nchunk,$
                                         POLY_REMOVE=poly_remove, $
                                         POLY_ORDER=poly_order,$
                                         USEHANNING=usehanning, $
                                         USEHAMMING=usehamming,$
                                         DISCARD_THRESH=0.2,$
                                         NOOVERLAP=nooverlap ) * delta
     ncalculated += 1
  ENDFOR   
  psd_image[0,*] = FINDGEN( nchunk/2+1 )/ (nchunk*delta)

  IF ncalculated EQ 0 && KEYWORD_SET(verbose) THEN $
     MESSAGE,"WARNING: No good data streams found in " + file,/INF
  
  success = 1b

  RETURN,psd_image

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO get_cross_psd_from_l1, firstarg, outfile, MINFREQ=minfreq,$
                           POLY_REMOVE=poly_remove,$
                           POLY_ORDER=poly_order, $
                           USEHANNING=usehanning,$
                           USEHAMMING=usehamming, $
                           NOOVERLAP=nooverlap, $
                           VERBOSE=verbose, SUCCESS=success, $
                           ERRMSG=errmsg, NONANCHECK=nonancheck,$
                           FULLSIZE=fullsize, NOPOW2=nopow2

  COMPILE_OPT IDL2, STRICTARRSUBS
  success=0b
  errmsg = ''

  ;;Default mask is all 1s, meaning reject all datasamples if any
  ;; mask bit is set
  IF N_ELEMENTS(badmask) EQ 0 THEN int_badmask=1uL ELSE $
     int_badmask=badmask
  IF KEYWORD_SET( verbose ) && FILE_TEST( outfile ) THEN $
     MESSAGE,"WARNING: output file "+outfile+" will be overwritten",/INF

  IF N_ELEMENTS(firstarg) EQ 0 THEN BEGIN
     errmsg="Input file list is empty"
     GOTO, err_handler
  ENDIF
  IF SIZE(firstarg,/TNAME) NE 'STRING' THEN BEGIN
     errmsg = "Input file list/directory is not a string but a: "+$
              SIZE(firstarg,/TNAME)
     GOTO, err_handler
  ENDIF

  IF FILE_TEST( firstarg, /DIRECTORY ) THEN BEGIN
     files = get_psp_filelist( firstarg, SUCCESS=psp_success,$
                               ERRMSG=errmsg )
     IF psp_success EQ 0 THEN BEGIN
        errmsg = "While getting file list: "+errmsg
        GOTO, err_handler
     ENDIF
  ENDIF ELSE files=firstarg

  IF N_ELEMENTS(minfreq) EQ 0 THEN minfreq = 0.012
  IF minfreq LE 0.0 THEN BEGIN
     errmsg = "Invalid minimim non-zero frequency -- must be positive"
     GOTO, err_handler
  ENDIF 

  nfiles = N_ELEMENTS(files)
  IF KEYWORD_SET( verbose ) THEN $
     MESSAGE,STRING(nfiles,FORMAT='("Processing ",I," files")'),/INF

  ;;Get chunk size from minfreq
  nchunk = choose_l1_chunksize( files, minfreq, SUCCESS=chunk_success,$
                                ERRMSG=errmsg, FULLSIZE=fullsize,$
                                NOPOW2=nopow2, MINLENGTH=minlength )
  IF chunk_success EQ 0 THEN BEGIN
     errmsg = "While choosing chunk size: "+errmsg
     GOTO, err_handler
  ENDIF 
  IF KEYWORD_SET(verbose) THEN $
     MESSAGE,STRING(nchunk,FORMAT='("Using chunk size: ",I0)'),/INF

  ;;Warn user if chunk size is close to min length and
  ;; overlapping is on
  IF 2*nchunk GT minlength AND ( KEYWORD_SET( usehanning ) OR $
                                 KEYWORD_SET( usehamming ) ) AND $
     ~ KEYWORD_SET( nooverlap ) THEN BEGIN
     MESSAGE,"WARNING -- needed chunk length close to data length and"+$
             " overlapping will occur",/INF
     MESSAGE,STRING(nchunk,minlength,$
                    FORMAT='(" chunk: ",I0," data len: ",I0)'),/INF
  ENDIF

  ;;Main loop
  file_good = BYTARR(nfiles)
  totaldatapoints = 0
  FOR i=0,nfiles-1 DO BEGIN
     IF KEYWORD_SET( verbose ) THEN $
        MESSAGE,STRING(files[i],FORMAT='("Processing file: ",A)'),/INF
     curr_data = get_cross_psd_from_l1_inner(files[i],nchunk,int_badmask,$
                                             curr_mask_image,$
                                             NDATAPOINTS=ndatapoints,$
                                             EXT1_STRUCT=ext1_struct,$
                                             POLY_REMOVE=poly_remove,$
                                             POLY_ORDER=poly_order, $
                                             USEHANNING=usehanning,$
                                             USEHAMMING=usehamming, $
                                             NOOVERLAP=nooverlap, $
                                             SUCCESS=curr_success, $
                                             ERRMSG=currerrmsg,$
                                             MASK_INFO=mask_info,$
                                             NONANCHECK=nonancheck)
     totaldatapoints += ndatapoints
     ;;Build total mask structure, even if the file was bad
     ;; -but- not if we couldn't read it
     IF N_ELEMENTS(curr_mask_image) NE 0 AND $
        N_ELEMENTS(all_mask) EQ 0 THEN BEGIN
        all_mask = ULONARR( N_ELEMENTS(curr_mask_image), nfiles )
        cross_names = ext1_struct.name
     ENDIF
     IF N_ELEMENTS(mask0_info) EQ 0 AND $
        SIZE(mask_info,/TNAME) EQ 'STRUCT' THEN $
        mask0_info = mask_info
     ;;Make sure the tags are all the same if things worked
     IF N_ELEMENTS( curr_mask_image ) NE 0 THEN BEGIN
        wdifftag = WHERE( ext1_struct.name NE cross_names, ndifftag )
        IF ndifftag NE 0 THEN BEGIN
           errmsg = "Somehow the mask tags for "+files[i]+" and "+file[0]+$
                    " don't match"
           GOTO, err_handler
        ENDIF
        all_mask[*,i] = curr_mask_image
        ;;Make sure mask bits have same settings
        IF SIZE(mask_info,/TNAME) EQ 'STRUCT' THEN BEGIN
           wpresent = WHERE_ARRAY( mask_info.bits, mask0_info.bits )
           IF wpresent[0] EQ -1 OR $
              N_ELEMENTS(wpresent) NE N_ELEMENTS(mask0_info) THEN BEGIN
              errmsg = "Mask bits are different in different input images"
              GOTO, err_handler
           ENDIF
        ENDIF
     ENDIF

     ;;Deal with the signal
     IF curr_success LT 1 THEN file_good[i]=0b ELSE file_good[i]=1b
     IF ~ file_good[i] THEN BEGIN
        MESSAGE,"Skipping bad file: "+files[i],/INF
        MESSAGE,currerrmsg,/INF
        CONTINUE
     ENDIF 
     IF N_ELEMENTS( psd_image ) EQ 0 THEN BEGIN
        ;;First one that worked
        freq = curr_data[0,*]
        psd_image = curr_data
     ENDIF ELSE psd_image += curr_data
  ENDFOR

  ;;Make sure we have some good files before writing
  w_goodfiles = WHERE( file_good, ngoodfiles, COMPLEMENT=w_badfiles,$
                       NCOMPLEMENT=nbadfiles )
  IF ngoodfiles EQ 0 THEN BEGIN
     errmsg = "No good files"
     GOTO, err_handler
  ENDIF

  ncross = ( SIZE(psd_image) )[1]

  ;;Figure out how many went into each channel, add that to ext1_struct
  FOR i=1, ncross - 1 DO BEGIN
     wgood = WHERE( file_good AND ~(all_mask[i,*] AND int_badmask), ngood )
     ext1_struct[i].nfiles = ngood
  ENDFOR

  ;;Normalize
  FOR i=1, (SIZE(psd_image))[1]-1 DO IF ext1_struct[i].nfiles GT 1 THEN $
     psd_image[i,*] /= ext1_struct[i].nfiles
  ;;Fix freq
  psd_image[0,*] = freq

  ;;If there were no good samples, NaN it
  wbad = WHERE( ext1_struct[1:*].nfiles EQ 0, nbad )
  IF nbad NE 0 THEN psd_image[wbad+1,*] = !VALUES.D_NAN

  ;;Now write
  ;;Base header
  MKHDR, base_hdr, '', /EXTEND
  SXADDPAR,base_hdr,'MINFREQ',minfreq,'Minimum target non-zero freq'
  SXADDPAR,base_hdr,'CHUNKSIZ',nchunk,'Chunk size used',FORMAT='(I)'
  SXADDPAR,base_hdr,'MNDATLEN',minlength,'Minimum length data sample',$
           FORMAT='(I)'
  IF KEYWORD_SET(usehanning) OR KEYWORD_SET(usehamming) THEN BEGIN
     SXADDPAR,base_hdr,'WINDOW','T','Window function used'
     IF KEYWORD_SET( usehanning ) THEN $
        SXADDPAR, base_hdr, 'WINTYP','HANNING','Window function used' ELSE $
           SXADDPAR, base_hdr, 'WINTYP','HAMMING','Window function used' 
     IF KEYWORD_SET( nooverlap ) THEN $
        SXADDPAR,base_hdr,'OVERLAP','F','Overlapping used?' ELSE $
           SXADDPAR,base_hdr,'OVERLAP','T','Overlapping used?'
  ENDIF ELSE SXADDPAR, base_hdr, 'WINDOW','F','Window function used' 
  SXADDPAR,base_hdr,'NDATAPTS',totaldatapoints,'Total number of data points'
  IF KEYWORD_SET( poly_remove ) THEN BEGIN
     SXADDPAR,base_hdr,'POLYRMV','T','Polynomial removal'
     SXADDPAR,base_hdr,'POLYORDR',poly_order,'Polynomial order'
  ENDIF ELSE BEGIN
     SXADDPAR,base_hdr,'POLYRMV','F','Polynomial removal'
  ENDELSE
;;Z is hexadecimal
  SXADDPAR,base_hdr,'BADMASK',int_badmask,FORMAT='(Z)','Mask fail pattern'
  SXADDPAR,base_hdr,'NCROSS', ncross,'Number of cross-spectra'
  SXADDPAR,header,'COMMENT',$
           'This is a header for an SMAP pipeline product.'
  SXADDPAR,base_hdr,'TIMESYS','UTC     ','All dates are in UTC time'
  SXADDPAR,base_hdr,'CREATOR',getenv('USER'),'User who created this file'
  SXADDPAR,base_hdr,'TELESCOP','Herschel','Name of the telescope'
  SXADDPAR,base_hdr,'INSTRUME','SPIRE','Name of the instrument'
  SXADDPAR,base_hdr,'DESC','CrossPSD','Description of file contents'

  ;;Get the primary header of a good file to get some extra info out of
  primary_hdr = HEADFITS( files[w_goodfiles[0]] )
  copy_vals = ['AOR','AOT','CUSMODE','INSTMODE']
  FOR i=0,N_ELEMENTS(copy_vals)-1 DO BEGIN
     val = SXPAR(primary_hdr,copy_vals[i],COMMENT=comment,COUNT=count)
     IF count NE 0 THEN SXADDPAR,base_hdr,copy_vals[i],val,comment ELSE $
        SXADDPAR,base_hdr,copy_vals[i],'Unknown'
  ENDFOR

  SXADDPAR,base_hdr,'NINPUT',nfiles,'Number of files'
  SXADDPAR,base_hdr,'NGOODFL',ngoodfiles,'Number of good files'
  SXADDPAR,base_hdr,'NBADFL',nbadfiles,'Number of bad files'
  IF ngoodfiles LT 10000 THEN FOR i=0,ngoodfiles-1 DO BEGIN
     idx = w_goodfiles[i]
     lastslash = RSTRPOS( files[idx],PATH_SEP() )
     IF lastslash EQ -1 THEN shortfile=files[idx] ELSE $
        shortfile=STRMID(files[idx],lastslash+1)
     tagname = "GDFL"+STRING(i+1,FORMAT='(I04)')
     SXADDPAR, base_hdr, tagname, shortfile,'Input file'
  ENDFOR
  IF nbadfiles LT 10000 THEN FOR i=0,nbadfiles-1 DO BEGIN
     idx = w_badfiles[i]
     lastslash = RSTRPOS( files[idx],PATH_SEP() )
     IF lastslash EQ -1 THEN shortfile=files[idx] ELSE $
        shortfile=STRMID(files[idx],lastslash+1)
     tagname = "BDFL"+STRING(i+1,FORMAT='(I04)')
     SXADDPAR, base_hdr, tagname, shortfile,'Bad input file'
  ENDFOR

  ;;Add list of all OBS_ID values for good files
  IF ngoodfiles LT 100 THEN FOR i=0,ngoodfiles-1 DO BEGIN
     idx = w_goodfiles[i]
     curr_hdr = HEADFITS(files[idx])
     val = ULONG(SXPAR(primary_hdr,'OBS_ID',COUNT=count))
     sxname = 'OBS_ID'+STRING(i+1,FORMAT='(I02)')

     IF count NE 0 THEN $
        SXADDPAR,base_hdr,sxname,val,'OBS_ID of good files' ELSE $
           SXADDPAR,base_hdr,sxname,'Unknown','OBS_ID of good files'
  ENDFOR

  ;;Add mask info
  IF SIZE(mask0_info,/TNAME) EQ 'STRUCT' THEN $
     add_mask_info_to_hdr, base_hdr, mask0_info

  SXADDPAR,base_hdr, 'HISTORY', 'Created by get_cross_psd_from_l1'
  SXADDPAR,base_hdr, 'HISTORY', ' on '+systime()

  ;;Extension headers
  FXBHMAKE,ext_hdr, N_ELEMENTS(ext1_struct),'DETIDS','IDs of image rows'
  FXBHMAKE,psd_hdr, N_ELEMENTS(outstruct), 'PSD', 'Cross-PSDs'
  SXADDPAR,psd_hdr,'COMMENT','See first extension for detector combinations'
  FXBHMAKE,file_hdr,nfiles,'FILENAMES','Names of files used'
  SXADDPAR,file_hdr,'COMMENT','Corresponds to columns of all mask extension'
  FXBHMAKE,all_mask_hdr, N_ELEMENTS(all_mask), 'FULLMASK', 'Full Mask'

  ;;Write actual file
  IF FILE_TEST( outfile ) THEN FILE_DELETE,outfile

  ;;Make initial extension
  WRITEFITS, outfile, 0, base_hdr, /CHECKSUM

  ;;Add row ID info
  write_status = 0b
  MWRFITS, ext1_struct, outfile, ext_hdr, /SILENT, STATUS=write_status
  IF write_status NE 0 THEN BEGIN
     errmsg = "Unable to write to outfile: "+outfile
     GOTO, err_handler
  ENDIF
  ;;Append psd
  write_status = 0b
  MWRFITS, psd_image, outfile, psd_hdr, /SILENT, STATUS=write_status
  IF write_status NE 0 THEN BEGIN
     errmsg = "Unable to write to outfile: "+outfile
     GOTO, err_handler
  ENDIF

  ;;We have to list all the files
  filename_struct=REPLICATE({file: '', shortfile: ''},nfiles)
  filename_struct.file = files
  FOR i=0,nfiles-1 DO BEGIN
     lastslash = RSTRPOS( files[i],PATH_SEP() )
     IF lastslash EQ -1 THEN shortfile=files[i] ELSE $
        shortfile=STRMID(files[i],lastslash+1)
     filename_struct[i].shortfile=shortfile
  ENDFOR
  write_status=0b
  MWRFITS, filename_struct, outfile, file_hdr, /SILENT, STATUS=write_status,$
           LSCALE=2147483648
  IF write_status NE 0 THEN BEGIN
     errmsg = "Unable to append file name structure to outfile: "+outfile
     GOTO, err_handler
  ENDIF

  ;;Append full mask info
  ;;People should read this with the /UNSIGNED flag
  write_status=0b
  IF SIZE(mask0_info,/TNAME) EQ 'STRUCT' THEN $
     add_mask_info_to_hdr, all_mask_hdr, mask0_info
  MWRFITS, all_mask, outfile, all_mask_hdr, /SILENT, STATUS=write_status
  IF write_status NE 0 THEN BEGIN
     errmsg = "Unable to append full mask to outfile: "+outfile
     GOTO, err_handler
  ENDIF

  success=1b
  RETURN

err_handler:
  IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
  success = 0b
  RETURN

END

                                
                      
