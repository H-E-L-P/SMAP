;+
;NAME
; get_auto_psd_from_l1
;PURPOSE
; To get a PSD estimate from a set of L1 time streams, combining
; them all into PSDs for each channel.
;USAGE
; get_auto_psd_from_l1,files,outfile
;INPUTS
; files         Either a list of FITS files to read data from, or the
;                name of the root directory of an HCSS export product.
;                If the former, the files are assumed to be in L1 format
;                as output from HIPE
; outfile       A FITS file containing the output auto-spectra for
;                all of the detectors.
;OUTPUT FILE FORMAT
; A fits file with 3 extensions.  The first contains the PSDs for
; each detector.  The second is a very basic mask file saying which
; PSDs are bad, and how many files input into each channel.
; The third is more detailed mask information about every channel
; of every input file.  This should be read as unsigned (i.e., using
; MRDFITS( ..., /UNSIGNED )).
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
;                    1uLL -- i.e., the master bit.  
;OPTIONAL OUTPUTS
;  success          1 on success, 0 on failure
;  errmsg           An error message in case of failure
;KEYWORDS
;  poly_remove      Remove a low polynomial from the data before
;                   transforming.
;  usehamming       Use a Hamming window function.  This turns on data
;                   overlapping.
;  usehanning       Use a Hanning window function.  This turns on data
;                   overlapping.
;  nooverlap        Don't overlap data segments.
;  nofullmask       Don't output the 3rd extension
;  nonancheck       Don't check for NaNs in inputs.  You may need to
;                    set this when working with SPS data.
;  fullsize         Always use full available length for estimating PSD.
;  nopow2           Don't attempt to round chunk size up to power of 2
;  verbose          Run in verbose mode
;NORMALIZATION
;  There are a semi-infinite number of conventions for normalizing the
;   PSD.  The one used here is:
;     \int_{0}^{f_c} PSD df = 1/T \int_{0}^{T} f dt
;   where T is the total data sample time, f_c is the Nyquist
;   frequency, and f is the signal.  These are therefore one-sided
;   PSDs.  (note: this differs very slightly from the conventions of 
;   auto_correlate).  So, if f is in Jy, then
;   this has units of Jy^2 Hz^-1.  Since this is always >= 0, you may
;   want to take the square root, although this will spoil the symmetry
;   with the cross-correlated data (which can be negative).
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

FUNCTION get_auto_psd_from_l1_inner, file, nchunk, outmask, badmask, $
                                     SIGNAL_UNITS=signal_units,$
                                     POLY_REMOVE=poly_remove,$
                                     POLY_ORDER=poly_order, $
                                     USEHANNING=usehanning,$
                                     USEHAMMING=usehamming, $
                                     NOOVERLAP=nooverlap, $
                                     NDATAPOINTS=ndatapoints,$
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
  
  ;;Make sure it has the extensions we want
  FITS_OPEN, file, fcb, /NO_ABORT, MESSAGE=errmsg
  FITS_CLOSE,fcb
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
  
  primary_head = HEADFITS( file, /SILENT, ERRMSG=errmsg )
  IF errmsg NE '' THEN BEGIN
     errmsg = 'Unable to read primary header '+errmsg
     RETURN,!VALUES.F_NAN
  ENDIF
  
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
  
  ;;Get units
  has_units = 0b
  signal_units = SXPAR( TEMPORARY(signal_head), 'TUNIT*' )
  IF N_ELEMENTS(signal_units) NE 0 THEN BEGIN
     signal_units=STRMID(STRTRIM(signal_units,2)+'^2/Hz',0,8)
     has_units=1b
  ENDIF
  
  ;;Add extra bit for NaN mask if needed
  IF ~ KEYWORD_SET( nonancheck ) THEN BEGIN
     mask_info = get_mask_bits(primary_head,SUCCESS=mask_bits_success,$
                               ERRMSG=errmsg )
     IF ~ mask_bits_success THEN BEGIN
        errmsg += " in get_maskbits: "+errmsg
        RETURN,!VALUES.F_NAN
     ENDIF
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
  
  ;;Get list of detector related tags.
  ;;We will assume everything that isn't SAMPLETIME is
  ;; something we want to take the PSD of.
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
  
  ;;These will now be index lists into the mask and signal
  ;; structures giving the indexes of things we want to take
  ;; PSDs of.  So mask.(mask_dets) is all the detectors
  mask_dets = WHERE( STRUPCASE(mask_tags) NE 'SAMPLETIME', nmask_dets )
  signal_dets = WHERE( STRUPCASE(signal_tags) NE 'SAMPLETIME', nsignal_dets )
  IF nmask_dets NE nsignal_dets THEN BEGIN
     errmsg = "Mask and signal structures don't have same number of detectors"
     RETURN,!VALUES.F_NAN
  ENDIF
  IF nmask_dets EQ 0 THEN BEGIN
     errmsg = "No detectors found in mask or signal extensions"
     RETURN,!VALUES.F_NAN
  ENDIF
  signal_det_tags = signal_tags[signal_dets]
  IF has_units THEN signal_units = signal_units[signal_dets]
  
  ;;Make sure the Mask and Signal tables have all the same detectors
  ;; and build indicies that make sure we get to them in the same order
  s_mask_tags = SORT(mask_tags)
  s_signal_tags = SORT(signal_tags)
  wbad = WHERE( mask_tags[s_mask_tags] NE signal_tags[s_signal_tags], nbad )
  IF nbad NE 0 THEN BEGIN
     errmsg = "The mask and signal structures don't have all the same tags"
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
  
  npsds = N_ELEMENTS(signal_dets)
  
  IF KEYWORD_SET(verbose) THEN MESSAGE,"Sampling "+STRING(npsds,FORMAT='(I4)')+$
                                       " detectors",/INFOR
  
  ;;Build the data structures
  SMAP_CREATE_STRUCT, outstruct_type, '', ['freq',signal_det_tags],$
                      REPLICATE('D',npsds+1)
  outstruct = REPLICATE( outstruct_type, nchunk/2+1 )
  
  ;;The other structure tells us if there are errors
  ;; It will support 32 bit masks
  ;;Note that this has one fewer index than the signal, since it
  ;; doesn't have the frequency tag
  SMAP_CREATE_STRUCT, outmask, '', signal_det_tags, REPLICATE('U',npsds)
  
  ;;Build the combined mask for every channel
  FOR i=0,npsds-1 DO BEGIN
     curr_mask = ULONG(mask.( mask_dets[i]))
     IF MAX( curr_mask ) NE 0 THEN BEGIN ;;Have at least some mask bits set
        bitmask = curr_mask[0]
        FOR j=1, N_ELEMENTS(curr_mask)-1 DO bitmask OR= curr_mask[j]
        outmask.(i) = bitmask
     ENDIF
  ENDFOR
  
  ;;Main loop for PSDs
  ncalculated=0
  FOR i = 0, npsds-1 DO BEGIN
     IF KEYWORD_SET( verbose ) THEN $
        MESSAGE," Doing "+psd_det_tags[i],/INF
     
     IF ~ KEYWORD_SET( nonancheck ) THEN BEGIN
        ;;The simulator returns NaNs sometimes, so it's good to check this
        wnfin = WHERE( ~ FINITE(signal.(signal_dets[i])), nnfin )
        IF nnfin NE 0 THEN outmask.(i) += new_maskbit
     ENDIF
     
     IF (outmask.(i) AND badmask) NE 0 THEN BEGIN
        outstruct.(i+1) = 0.0
        CONTINUE
     ENDIF
     
     outstruct.(i+1) = auto_correlate( signal.(signal_dets[i]), nchunk,$
                                       POLY_REMOVE=poly_remove, $
                                       POLY_ORDER=poly_order,$
                                       USEHANNING=usehanning, $
                                       USEHAMMING=usehamming,$
                                       FREQUENCIES=frequencies, $
                                       DISCARD_THRESH=0.2,$
                                       NOOVERLAP=nooverlap ) * delta
     ncalculated += 1

  ENDFOR   
  outstruct.freq = FINDGEN( nchunk/2+1 )/ (nchunk*delta)
  
  IF ncalculated EQ 0 && KEYWORD_SET(verbose) THEN $
     MESSAGE,"WARNING: No good data streams found in " + file,/INF
  
  success = 1b
  
  RETURN,outstruct
  
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO get_auto_psd_from_l1, firstarg, outfile, MINFREQ=minfreq,$
                          POLY_REMOVE=poly_remove,$
                          POLY_ORDER=poly_order, $
                          USEHANNING=usehanning,$
                          USEHAMMING=usehamming, $
                          NOOVERLAP=nooverlap, $
                          VERBOSE=verbose, SUCCESS=success, $
                          ERRMSG=errmsg, BADMASK=badmask,$
                          NOFULLMASK=nofullmask, NONANCHECK=nonancheck,$
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
     GOTO,err_handler
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
  totalsamplelen = 0
  FOR i=0,nfiles-1 DO BEGIN
     IF KEYWORD_SET( verbose ) THEN $
        MESSAGE,STRING(files[i],FORMAT='("Processing file: ",A)'),/INF
     curr_data = get_auto_psd_from_l1_inner(files[i],nchunk,curr_mask,$
                                            int_badmask,$
                                            NDATAPOINTS=ndatapoints,$
                                            SIGNAL_UNITS=signal_units,$
                                            POLY_REMOVE=poly_remove,$
                                            POLY_ORDER=poly_order, $
                                            USEHANNING=usehanning,$
                                            USEHAMMING=usehamming, $
                                            NOOVERLAP=nooverlap, $
                                            SUCCESS=curr_success, $
                                            ERRMSG=curr_errmsg,$
                                            MASK_INFO=mask_info,$
                                            NONANCHECK=nonancheck)
     totalsamplelen += ndatapoints
     ;;Build total mask structure, even if the file was bad
     ;; -but- not if we couldn't read it
     IF N_ELEMENTS(curr_mask) NE 0 AND N_ELEMENTS(all_mask) EQ 0 THEN BEGIN
        ;;Note file will be at the end of the tag list
        all_mask_struct = CREATE_STRUCT( curr_mask, 'file', '' ) 
        all_mask = REPLICATE(all_mask_struct,nfiles)
        tag0mask = TAG_NAMES( curr_mask )
     ENDIF
     IF N_ELEMENTS(mask0_info) EQ 0 AND $
        SIZE(mask_info,/TNAME) EQ 'STRUCT' THEN $
           mask0_info = mask_info
     
     ;;Now keep the mask
     IF N_ELEMENTS( curr_mask ) NE 0 THEN BEGIN
        wdifftag = WHERE( tag0mask NE TAG_NAMES( curr_mask ), ndifftag )
        IF ndifftag NE 0 THEN BEGIN
           errmsg = "Somehow the mask tags for "+files[i]+" and "+file[0]+$
                    " don't match"
           GOTO, err_handler
        ENDIF
        curr_val = all_mask[i]
        STRUCT_ASSIGN,curr_mask,curr_val
        all_mask[i] = TEMPORARY(curr_val)

        lastslash = RSTRPOS( files[i],PATH_SEP() )
        IF lastslash EQ -1 THEN shortfile=files[idx] ELSE $
           shortfile=STRMID(files[i],lastslash+1)

        all_mask[i].file=shortfile

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
     IF ~ file_good[i] AND KEYWORD_SET(verbose) THEN BEGIN
        MESSAGE,"Skipping bad file: "+files[i],/INF
        MESSAGE,curr_errmsg,/INF
        CONTINUE
     ENDIF 
     IF N_ELEMENTS(outstruct) EQ 0 THEN BEGIN
        ;;First successful one
        outstruct = TEMPORARY(curr_data)
        ndet = N_ELEMENTS( tag_names(curr_mask) )
        tag0 = tag_names( outstruct )
     ENDIF ELSE BEGIN
        ;;Average in with old one
        curr_tag = tag_names( curr_data )
        wdifftag = WHERE( tag0 NE curr_tag, ndifftag )
        ;;We require that the structure for each file have the same
        ;; format.  If not, give up
        IF ndifftag NE 0 THEN BEGIN
           errmsg = "Somehow tags for "+files[i]+" and "+file[0]+$
                    " don't match"
           GOTO, err_handler
        ENDIF
        FOR j=0,ndet-1 DO IF ~ curr_mask.(j) THEN $
           outstruct.(j+1) += curr_data.(j+1)
     ENDELSE
  ENDFOR

  ;;Figure out how many went into each channel
  nchannels = N_ELEMENTS(tag0mask)
  n_in_average = INTARR(nchannels)
  FOR i=0,nchannels-1 DO BEGIN
     wgood=WHERE( (all_mask.(i) AND int_badmask) EQ 0, ngood ) 
     n_in_average[i]=ngood
  ENDFOR
  ;;Normalize
  FOR i=0,nchannels-1 DO BEGIN
     IF n_in_average[i] GT 1 THEN BEGIN
        outstruct.(i+1) /= n_in_average[i]
     ENDIF ELSE IF n_in_average[i] EQ 0 THEN outstruct.(i+1) = !VALUES.D_NAN
  ENDFOR

  ;;Build the basic mask
  SMAP_CREATE_STRUCT, basic_mask, '', [tag0mask,'N'+tag0mask],$
                      [REPLICATE('B',nchannels),$
                       REPLICATE('I',nchannels)]
  FOR i=0,nchannels-1 DO BEGIN
     IF n_in_average[i] EQ 0 THEN basic_mask.(i)=1b
     basic_mask.(i+nchannels)=n_in_average[i]
  ENDFOR

  ;;Make sure we have some good files before writing
  w_goodfiles = WHERE( file_good, ngoodfiles, COMPLEMENT=w_badfiles,$
                       NCOMPLEMENT=nbadfiles )
  IF ngoodfiles EQ 0 THEN BEGIN
     errmsg = "No good files"
     RETURN
  ENDIF

  ;;Now write the outputs as binary fits tables

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
  SXADDPAR,base_hdr,'NDATAPTS',totalsamplelen,'Total number of data points'
  IF KEYWORD_SET( poly_remove ) THEN BEGIN
     SXADDPAR,base_hdr,'POLYRMV','T','Polynomial removal'
     SXADDPAR,base_hdr,'POLYORDR',poly_order,'Polynomial order'
  ENDIF ELSE BEGIN
     SXADDPAR,base_hdr,'POLYRMV','F','Polynomial removal'
  ENDELSE
  ;;Z is hexadecimal
  SXADDPAR,base_hdr,'BADMASK',int_badmask,FORMAT='(Z)','Mask fail pattern'
  SXADDPAR,base_hdr,'NPSD',N_ELEMENTS(tag0mask),'Number of PSDs'
  SXADDPAR,base_hdr,'COMMENT',$
           'This is a header for an SMAP pipeline product.'
  SXADDPAR,base_hdr,'TIMESYS','UTC     ','All dates are in UTC time'
  SXADDPAR,base_hdr,'CREATOR',getenv('USER'),'User who created this file'
  SXADDPAR,base_hdr,'TELESCOP','Herschel','Name of the telescope'
  SXADDPAR,base_hdr,'INSTRUME','SPIRE','Name of the instrument'
  SXADDPAR,base_hdr,'DESC','AutoPSD','Description of file contents'

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
     tagname = "GDFL"+STRING(idx+1,FORMAT='(I0)')
     SXADDPAR, base_hdr, tagname, shortfile,'Input file'
  ENDFOR
  IF nbadfiles LT 10000 THEN FOR i=0,nbadfiles-1 DO BEGIN
     idx = w_badfiles[i]
     lastslash = RSTRPOS( files[idx],PATH_SEP() )
     IF lastslash EQ -1 THEN shortfile=files[idx] ELSE $
        shortfile=STRMID(files[idx],lastslash+1)
     tagname = "BDFL"+STRING(i+1,FORMAT='(I0)')
     SXADDPAR, base_hdr, tagname, shortfile,'Bad input file'
  ENDFOR

  ;;Add list of all OBS_ID values for good files
  IF ngoodfiles LT 100 THEN FOR i=0,ngoodfiles-1 DO BEGIN
     idx = w_goodfiles[i]
     curr_hdr = HEADFITS(files[idx])
     val = ULONG(SXPAR(primary_hdr,'OBS_ID',COUNT=count))
     sxname = 'OBS_ID'+STRING(i+1,FORMAT='(I02)')
     IF count NE 0 THEN SXADDPAR,base_hdr,sxname,val,$
                                 'OBS_ID of good files' ELSE $
        SXADDPAR,base_hdr,sxname,'Unknown','OBS_ID of good files'
  ENDFOR

  ;;Add mask information
  IF SIZE(mask0_info,/TNAME) EQ 'STRUCT' THEN $
     add_mask_info_to_hdr, base_hdr, mask0_info

  SXADDPAR, base_hdr, 'HISTORY', 'Created by get_auto_psd_from_l1'
  SXADDPAR, base_hdr, 'HISTORY', ' on '+systime()

  ;;PSD header
  FXBHMAKE,psd_hdr, N_ELEMENTS(outstruct), 'PSD', 'Auto-PSDs'
  ;;Add units to PSD
  SXADDPAR, psd_hdr, 'TUNIT1','Hz'
  FOR i=0,N_ELEMENTS(signal_units)-1 DO $
     SXADDPAR,psd_hdr,'TUNIT'+STRING(i+2,FORMAT='(I0)'),signal_units[i]

  FXBHMAKE,basic_mask_hdr, N_ELEMENTS(basic_mask), 'SMPLMASK', 'Basic Mask'

  ;;Write actual file
  IF FILE_TEST( outfile ) THEN FILE_DELETE,outfile
  ;;Make initial extension
  WRITEFITS, outfile, 0, base_hdr, /CHECKSUM

  ;;Append psd
  write_status = 0b
  MWRFITS, outstruct, outfile, psd_hdr, /SILENT, STATUS=write_status
  IF write_status NE 0 THEN BEGIN
     errmsg = "Unable to write to outfile: "+outfile
     GOTO, err_handler
  ENDIF
  ;;Append basic mask
  MWRFITS, basic_mask, outfile, basic_mask_hdr, /SILENT, STATUS=write_status
  IF write_status NE 0 THEN BEGIN
     errmsg = "Unable to append basic mask to outfile: "+outfile
     GOTO, err_handler
  ENDIF
  ;;Append full mask info
  IF ~ KEYWORD_SET( nofullmask ) THEN BEGIN
     FXBHMAKE,all_mask_hdr, N_ELEMENTS(all_mask), 'FULLMASK', 'Full Mask'
     IF SIZE(mask0_info,/TNAME) EQ 'STRUCT' THEN $
        add_mask_info_to_hdr, all_mask_hdr, mask0_info
     MWRFITS, all_mask, outfile, all_mask_hdr, /SILENT, STATUS=write_status
     IF write_status NE 0 THEN BEGIN
        errmsg = "Unable to append full mask to outfile: "+outfile
        GOTO, err_handler
     ENDIF
  ENDIF

  success = 1b
  RETURN

err_handler:
  IF KEYWORD_SET(verbose) THEN MESSAGE,errmsg,/INF
  success = 0b
  RETURN
END
