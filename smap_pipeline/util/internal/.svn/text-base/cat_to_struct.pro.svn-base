;+
;NAME 
; cat_to_struct
;PURPOSE
; Read a catalogue into an array of structures
;CALLING SEQUENCE
; cat=cat_to_struct(filename,tagnames,tagdescrip)
;INPUTS
; filename - the ascii filename to be read in
; tagnames - the name each column will have as a tag in the
;            structure. String array.
; tagdescrip - the data type of each tag in the structure
;               String array descriptor for each element of tagnames, 
;               containing the tag type and dimensions.  For example, 
;               ['A(2)','F(3)','I'], would is the descriptor for a 
;               structure with 3 tags: strarr(2), 
;               fltarr(3) and Integer scalar, respectively.
;               Allowed types are 'A' for strings, 'B' or 'L' for unsigned byte 
;               integers, 'I' for integers, 'J' for longword integers, 
;               'F' or 'E' for floating point, 'D' for double precision.
;               It is allowed to read in arrays. Arrays are specified
;               like 'A(10)' which is a 10 element string array. Array
;               data in filename must be separated by ARRAYSEP with no white
;               spaces. It is an error if there is more array data
;               than the length of the array.
;OPTIONAL INPUTS
; format      - Fixed for mat for reading a line
; nskip       - skip this many lines at the start of the file
; ntagstoread - read in this many tags from each line. Allows large
;               st ructures to be passed which include tags which are
;               not to be populated by this routine.
; arraysep     - Character to use when separating arrays (Def: ',')
;KEYWORDS
; skipcomments - skip lines beginning with '#' or ';'. Def: yes.
;KEYWORDS
; verbose - verbose or not
; rasort - sort by RA if a RADEG tag is present
;RETURNS
; the ascii file as a structure
;MODIFICATION HISTORY: 
; 
;Author: Mark Sullivan
;-

FUNCTION cat_to_struct,filename,tagnames,tagdescrip,VERBOSE=verbose,$
  NSKIP=nskip,NTAGSTOREAD=ntagstoread, RASORT=rasort,$
  SKIPCOMMENTS=skipcomments,PATTERN=pattern, ARRAYSEP=arraysep,$
  FORMAT=format

COMPILE_OPT IDL2, STRICTARRSUBS

IF(N_ELEMENTS(verbose) EQ 0)THEN verbose=0b
IF(N_ELEMENTS(rasort) EQ 0)THEN rasort=0b
IF(N_ELEMENTS(nskip) EQ 0)THEN nskip=0
IF(N_ELEMENTS(skipcomments) EQ 0)THEN skipcomments=1b
regex=1b
IF(N_ELEMENTS(pattern) EQ 0)THEN pattern='[ ' + STRING(9B) + ']+' ELSE regex=0b
IF(N_ELEMENTS(filename) EQ 0)THEN BEGIN
   PRINT,'ERROR in cat_to_struct: filename must be given'
   RETURN,0
ENDIF
IF(N_ELEMENTS(tagnames) EQ 0)THEN BEGIN
   PRINT,'ERROR in cat_to_struct: tagnames must be given'
   RETURN,0
ENDIF
nparams=N_ELEMENTS(tagnames)
IF(N_ELEMENTS(ntagstoread) EQ 0)THEN ntagstoread=nparams

IF(N_ELEMENTS(tagdescrip) EQ 0)THEN BEGIN
   PRINT,'ERROR in cat_to_struct: tagdescrip must be given'
   RETURN,0
ENDIF

IF(nparams NE N_ELEMENTS(tagdescrip))THEN BEGIN
   PRINT,'ERROR in cat_to_struct: tagnames and tagdescrip must have the same number of elements'
   RETURN,0
ENDIF

IF(N_ELEMENTS(format) GT 0 && N_ELEMENTS(format) NE ntagstoread)THEN BEGIN
   PRINT,'ERROR in cat_to_struct: format must have the same number of elements as parameters to read from file.'
   RETURN,0
ENDIF

IF(~ FILE_TEST(filename,/READ,/REGULAR))THEN BEGIN
   PRINT,'ERROR in cat_to_struct: Cannot read '+filename
   RETURN,0
ENDIF

OPENR,unit,filename,/GET_LUN,ERROR=err
IF(err NE 0)THEN BEGIN
   PRINT,'ERROR in cat_to_struct when opening '+filename+' : '
   PRINT,!ERROR_STATE.MSG
   RETURN,0
ENDIF

nlines=FILE_LINES(filename)
create_struct,A,'',tagnames,tagdescrip
cat_struct=REPLICATE(A,nlines)
icount=0L
thisline=''

IF(verbose)THEN PRINT,'Reading '+filename+' with '+STRN(nlines)+' lines.'

IF(N_ELEMENTS(format) GT 0)THEN BEGIN
   fmtstring = STRTRIM(STRUPCASE(format), 2 )
   fmtstring = '(' + format + ')'
   IF(verbose)THEN PRINT,'(Using fixed format)'
ENDIF

IF(nskip GT 0)THEN BEGIN
   FOR i=0L,nskip-1 DO BEGIN
      READF,unit,thisline
   ENDFOR
   nlines=nlines-nskip
ENDIF

usingarrays=0b
arraylength=LONARR(N_ELEMENTS(tagdescrip))
arraytype=STRARR(N_ELEMENTS(tagdescrip))
usingfixed=0b
fixedlength=LONARR(N_ELEMENTS(tagdescrip))
fixedtype=STRARR(N_ELEMENTS(tagdescrip))
FOR j=0L,N_ELEMENTS(tagdescrip)-1 DO BEGIN
   IF(STREGEX(tagdescrip[j],'\(',/BOOLEAN))THEN BEGIN
      usingarrays=1b
      ;; this one is an array
      tmparr=STREGEX(tagdescrip[j],'([ablijfedABLIJFED]+)\(([0-9]+)\)',$
                     /EXTRACT,/SUBEXPR) ;; the maximum length of the array
      IF(N_ELEMENTS(tmparr) NE 3)THEN BEGIN
         PRINT,"ERROR in cat_to_struct: I didn't recognise tag descriptor "+$
               tagdescrip[j]
         RETURN,0
      ENDIF
      arraytype[j]=STRLOWCASE(tmparr[1]) ;; array type
      arraylength[j]=FIX(tmparr[2]) ;; array length
   ENDIF
   IF(STREGEX(tagdescrip[j],'\[',/BOOLEAN))THEN BEGIN
      usingfixed=1b
      ;; this one is a fixed length
      tmparr=STREGEX(tagdescrip[j],'([ablijfedABLIJFED]+)\[([0-9]+)\]',$
                     /EXTRACT,/SUBEXPR) ;; the maximum length of the array
      IF(N_ELEMENTS(tmparr) NE 3)THEN BEGIN
         PRINT,"ERROR in cat_to_struct: I didn't recognise tag descriptor "+$
               tagdescrip[j]
         RETURN,0
      ENDIF
      fixedlength[j]=FIX(tmparr[2]) ;; string length
   ENDIF
ENDFOR
IF N_ELEMENTS(arraysep) EQ 0 THEN arraysep = ','

;IF(N_ELEMENTS(format) GT 0)THEN BEGIN
;
;READFMT,formatstring,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15, $
;                 v16,v17,v18,v19,v20,v21,v22,v23,v24,v25
;ENDIF

FOR i=0L,nlines-1 DO BEGIN
   READF,unit,thisline
   IF(thisline EQ '')THEN CONTINUE
   IF(skipcomments && (STREGEX(thisline,'^#',/BOOLEAN) || $
                       STREGEX(thisline,'^;',/BOOLEAN)))THEN CONTINUE
   array=STRSPLIT(thisline,pattern,/EXTRACT,COUNT=nsplit,REGEX=regex)
   IF(nsplit GE ntagstoread)THEN BEGIN
      FOR j=0L,ntagstoread-1 DO BEGIN
         IF(usingarrays && STREGEX(tagdescrip[j],'\(',/BOOLEAN))THEN BEGIN
            ;; this one is an array
            ;; the data+length of data
            array1=STRSPLIT(array[j],arraysep,/EXTRACT,COUNT=narraysplit) 
            IF(narraysplit GT arraylength[j])THEN BEGIN
               PRINT,'ERROR in cat_to_struct: more data in array than array length'
               RETURN,0
            ENDIF
            IF(arraytype[j] EQ 'a')THEN BEGIN
               array2=STRARR(arraylength[j])
            ENDIF ELSE IF(arraytype[j] EQ 'd')THEN BEGIN
               array2=DBLARR(arraylength[j])
               array1=DOUBLE(array1)
            ENDIF ELSE IF(arraytype[j] EQ 'f' OR arraytype[j] EQ 'e')THEN BEGIN
               array2=FLTARR(arraylength[j])
               array1=FLOAT(array1)
            ENDIF ELSE IF(arraytype[j] EQ 'b' OR arraytype[j] EQ 'l')THEN BEGIN
               array2=BYTARR(arraylength[j])
               array1=BYTE(array1)
            ENDIF ELSE IF(arraytype[j] EQ 'i')THEN BEGIN
               array2=INTARR(arraylength[j])
               array1=FIX(array1)
            ENDIF ELSE IF(arraytype[j] EQ 'j')THEN BEGIN
               array2=LONARR(arraylength[j])
               array1=LONG(array1)
            ENDIF
            IF(narraysplit LT arraylength[j])THEN BEGIN
               array2[0:narraysplit-1]=array1
            ENDIF ELSE BEGIN
               array2=array1
            ENDELSE
            IF (j EQ 0) THEN $
               temp = CREATE_STRUCT(tagnames[j], array2) ELSE  $
                  temp = CREATE_STRUCT(temp, tagnames[j], array2)
         ENDIF ELSE IF(usingfixed)THEN BEGIN
            IF(fixedlength[j] EQ 0)THEN BEGIN
               array=STRSPLIT(thisline,pattern,/EXTRACT,COUNT=nsplit,REGEX=regex)
               array1=STRSPLIT(thisline,pattern,COUNT=nsplit,REGEX=regex)
               IF(j NE ntagstoread-1)THEN thisline=STRMID(thisline,array1[1])
            ENDIF ELSE BEGIN
               array=STRMID(thisline,0,fixedlength[j])
               IF(j NE ntagstoread-1)THEN thisline=STRMID(thisline,fixedlength[j])
            ENDELSE
            IF( j EQ 0)THEN temp = CREATE_STRUCT(tagnames[j], array) ELSE temp = CREATE_STRUCT(temp, tagnames[j], array)
         ENDIF ELSE BEGIN
            ;; simple scalar
            IF (j EQ 0) THEN $
              temp = CREATE_STRUCT(tagnames[j], array[j]) ELSE  $
                temp = CREATE_STRUCT(temp, tagnames[j], array[j])
         ENDELSE
      ENDFOR
      thiscat_struct=A
      STRUCT_ASSIGN,temp,thiscat_struct
      cat_struct[icount]=thiscat_struct
      icount=icount+1
   ENDIF
ENDFOR
IF(icount EQ 0)THEN BEGIN
   PRINT,'ERROR in cat_to_struct: no lines read from '+filename
   RETURN,0
ENDIF
cat_struct=cat_struct[0:icount-1]
FREE_LUN,unit

IF(verbose)THEN PRINT,STRN(icount)+' lines read in from '+filename

IF(rasort AND TAG_EXIST(cat_struct,'radeg'))THEN BEGIN
   sorted=BSORT(cat_struct.radeg)
   cat_struct=cat_struct[sorted]
   IF(verbose)THEN PRINT,'Catalogue sorted by right ascension.'
ENDIF

RETURN,cat_struct
END
