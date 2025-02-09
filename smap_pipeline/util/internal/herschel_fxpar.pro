FUNCTION HERSCHEL_FXPAR, HDR, NAME, ABORT, COUNT=MATCHES, COMMENT=COMMENTS, $
                         START=START, PRECHECK=PRECHECK, POSTCHECK=POSTCHECK, $
                         NOCONTINUE = NOCONTINUE, $
                         DATATYPE=DATATYPE
;+
; NAME: 
;       HERSCHEL_FXPAR()
; CATEGORY:
;       Herschel SPIRE SMAP pipeline
; PURPOSE: 
;       Obtain the value of a parameter in a Herschel FITS header as
;       output by HCSS.
; CALLING SEQUENCE: 
;       Result = HERSCHEL_FXPAR( HDR, NAME  [, ABORT, COUNT=, 
;                                COMMENT=, /NOCONTINUE ] )
; REQUIRED INPUTS: 
;       HDR     = FITS header string array (e.g. as returned by FXREAD).  Each
;                 element should have a length of 80 characters
;       NAME    = String name of the parameter to return.  If NAME is of the
;                 form 'keyword*' then an array is returned containing values
;                 of keywordN where N is an integer.  The value of keywordN
;                 will be placed in RESULT(N-1).  The data type of RESULT will
;                 be the type of the first valid match of keywordN
;                 found, unless DATATYPE is given.
; OPTIONAL INPUT: 
;       ABORT   = String specifying that FXPAR should do a RETALL if a
;                 parameter is not found.  ABORT should contain a string to be
;                 printed if the keyword parameter is not found.  If not
;                 supplied, FXPAR will return with a negative !err if a keyword
;                 is not found.
;       DATATYPE = A scalar value, indicating the type of vector
;                  data.  All keywords will be cast to this type.
;                  Default: based on first keyword.
;                  Example: DATATYPE=0.0D (cast data to double precision)
;       START   = A best-guess starting position of the sought-after
;                 keyword in the header.  If specified, then FXPAR
;                 first searches for scalar keywords in the header in
;                 the index range bounded by START-PRECHECK and
;                 START+POSTCHECK.  This can speed up keyword searches
;                 in large headers.  If the keyword is not found, then
;                 FXPAR searches the entire header.  
;
;                 If not specified then the entire header is searched.
;                 Searches of the form 'keyword*' also search the
;                 entire header and ignore START.
;
;                 Upon return START is changed to be the position of
;                 the newly found keyword.  Thus the best way to
;                 search for a series of keywords is to search for
;                 them in the order they appear in the header like
;                 this:
;
;                       START = 0L
;                       P1 = FXPAR('P1', START=START)
;                       P2 = FXPAR('P2', START=START)
;       PRECHECK = If START is specified, then PRECHECK is the number
;                  of keywords preceding START to be searched.
;                  Default: 5
;       POSTCHECK = If START is specified, then POSTCHECK is the number
;                   of keywords after START to be searched.
;                   Default: 20
; OUTPUT: 
;       The returned value of the function is the value(s) associated with the
;       requested keyword in the header array.
;
;       If the parameter is complex, double precision, floating point, long or
;       string, then the result is of that type.  Apostrophes are stripped from
;       strings.  If the parameter is logical, 1 is returned for T, and 0 is
;       returned for F.
;
;       If NAME was of form 'keyword*' then a vector of values are returned.
;
; OPTIONAL INPUT KEYWORDS: 
;       /NOCONTINUE = If set, then continuation lines will not be read, even
;                 if present in the header
; OPTIONAL OUTPUT KEYWORD:
;       COUNT   = Optional keyword to return a value equal to the number of
;                 parameters found by FXPAR.
;       COMMENTS= Array of comments associated with the returned values.
;
; PROCEDURE CALLS: 
;       GETTOK(), VALID_NUM
; SIDE EFFECTS: 
;
;       The system variable !err is set to -1 if parameter not found, 0 for a
;       scalar value returned.  If a vector is returned it is set to the number
;       of keyword matches found.
;
;       If a keyword occurs more than once in a header, a warning is given,
;       and the first occurence is used.  However, if the keyword is "HISTORY",
;       "COMMENT", or "        " (blank), then multiple values are returned.
;
; NOTES:
;       This is almost identical to FXPAR from the IDL astrolib.  It
;       has been modified in two ways:
;        1) It treats HIERARCH like COMMENT or HISTORY -- that is, it
;            returns all found examples of them.
;        2) It understands the Herschel multi-line comment format of
;            HCSS.  This is just like the OGIP 1.0 convention but also
;            applies to comments.  A multi-line comment occurs when
;            the comment ends with & and the next line is COMMENT
; REVISION HISTORY: 
;       Modified from fxpar by Alex Conley, May 1, 2009
;-

;
;  Check the number of parameters.
;     God, I hate their indentation convention.  Damn FORTRAN
;     programmers -- AC
  IF N_PARAMS() LT 2 THEN BEGIN
     PRINT,'Syntax:  result =  HERSCHEL_FXPAR( HDR, NAME  [, ABORT ])'
     RETURN, -1
  ENDIF
;
;  Determine the abort condition.
;
  VALUE = 0
  IF N_PARAMS() LE 2 THEN BEGIN
     ABORT_RETURN = 0
     ABORT = 'FITS Header'
  END ELSE ABORT_RETURN = 1
  IF ABORT_RETURN THEN ON_ERROR,1 ELSE ON_ERROR,2
;
;  Check for valid header.  Check header for proper attributes.
;
  S = SIZE(HDR)
  IF ( S[0] NE 1 ) OR ( S[2] NE 7 ) THEN $
     MESSAGE,'FITS Header (first parameter) must be a string array'
;
;  Convert the selected keyword NAME to uppercase.
;
  NAM = STRTRIM( STRUPCASE(NAME) )
;
;  Determine if NAME is of form 'keyword*'.  If so, then strip off the '*', and
;  set the VECTOR flag.  One must consider the possibility that NAM is an empty
;  string.
;
  NAMELENGTH1 = (STRLEN(NAM) - 1) > 1
  IF STRPOS( NAM, '*' ) EQ NAMELENGTH1 THEN BEGIN    
     NAM = STRMID( NAM, 0, NAMELENGTH1)  
     VECTOR = 1                          ;Flag for vector output  
     NAME_LENGTH = STRLEN(NAM)           ;Length of name 
     NUM_LENGTH = 8 - NAME_LENGTH        ;Max length of number portion  
     IF NUM_LENGTH LE 0 THEN MESSAGE,    $
        'Keyword length must be 8 characters or less'
;
;  Otherwise, extend NAME with blanks to eight characters.
;
  ENDIF ELSE BEGIN
     WHILE STRLEN(NAM) LT 8 DO NAM = NAM + ' '
     VECTOR = 0
  ENDELSE
;
;  If of the form 'keyword*', then find all instances of 'keyword' followed by
;  a number.  Store the positions of the located keywords in NFOUND, and the
;  value of the number field in NUMBER.
;
  IF N_ELEMENTS(START)     EQ 0 THEN START = -1L
  START = LONG(START[0])
  IF NOT VECTOR AND START GE 0 THEN BEGIN
     IF N_ELEMENTS(PRECHECK)  EQ 0 THEN PRECHECK = 5
     IF N_ELEMENTS(POSTCHECK) EQ 0 THEN POSTCHECK = 20
     NHEADER = N_ELEMENTS(HDR)
     MN = (START - PRECHECK)  > 0
     MX = (START + POSTCHECK) < (NHEADER-1) ;Corrected bug
     KEYWORD = STRMID(HDR[MN:MX], 0, 8)
  ENDIF ELSE BEGIN
     RESTART:
     START   = -1L
     KEYWORD = STRMID( HDR, 0, 8)
  ENDELSE

  IF VECTOR THEN BEGIN
     NFOUND = WHERE(STRPOS(KEYWORD,NAM) GE 0, MATCHES)
     IF ( MATCHES GT 0 ) THEN BEGIN
        NUMST= STRMID(HDR[NFOUND], NAME_LENGTH, NUM_LENGTH)
        NUMBER = INTARR(MATCHES)-1
        FOR I = 0, MATCHES-1 DO         $
           IF VALID_NUM( NUMST[I], NUM) THEN NUMBER[I] = NUM
        IGOOD = WHERE(NUMBER GE 0, MATCHES)
        IF MATCHES GT 0 THEN BEGIN
           NFOUND = NFOUND[IGOOD]
           NUMBER = NUMBER[IGOOD]
        ENDIF
     ENDIF
;
;  Otherwise, find all the instances of the requested keyword.  If more than
;  one is found, and NAME is not one of the special cases, then print an error
;  message.
;
  ENDIF ELSE BEGIN
     NFOUND = WHERE(KEYWORD EQ NAM, MATCHES)
     IF MATCHES EQ 0 AND START GE 0 THEN GOTO, RESTART
     IF START GE 0 THEN NFOUND = NFOUND + MN
     IF (MATCHES GT 1) AND (NAM NE 'HISTORY ') AND $
        (NAM NE 'COMMENT ') AND (NAM NE '') AND $
        (NAM NE 'HIERARCH') THEN $
           MESSAGE,/INFORMATIONAL, 'WARNING- Keyword ' +   $
                   NAM + ' located more than once in ' + ABORT
     IF (MATCHES GT 0) THEN START = NFOUND[MATCHES-1]
  ENDELSE
;
;  Extract the parameter field from the specified header lines.  If one of the
;  special cases, then done.
;
  IF MATCHES GT 0 THEN BEGIN
     LINE = HDR[NFOUND]
     SVALUE = STRTRIM( STRMID(LINE,9,71),2)
;
;  See if we may have to deal with continuation characters
;
     IF (SIZE(FXPAR(HDR, 'LONGSTRN',/NOCONTINUE)))[1] EQ 7 THEN $
        continuation=1b ELSE continuation=0b

     IF (NAM EQ 'HISTORY ') OR (NAM EQ 'COMMENT ') OR    $
        (NAM EQ '        ') OR (NAME EQ 'HIERARCH') THEN BEGIN
        VALUE = STRTRIM( STRMID(LINE,8,72),2)
        COMMENTS = STRARR(N_ELEMENTS(VALUE))

;
;  Otherwise, test to see if the parameter contains a string, signalled by
;  beginning with a single quote character (') (apostrophe).
;
     END ELSE FOR I = 0,MATCHES-1 DO BEGIN
        IF ( STRMID(SVALUE[I],0,1) EQ "'" ) THEN BEGIN
           TEST = STRMID( SVALUE[I],1,STRLEN( SVALUE[I] )-1)
           NEXT_CHAR = 0
           OFF = 0
           VALUE = ''
;
;  Find the next apostrophe.
;
NEXT_APOST:
           ENDAP = STRPOS(TEST, "'", NEXT_CHAR)
           IF ENDAP LT 0 THEN MESSAGE,         $
              'WARNING: Value of '+NAME+' invalid in '+ABORT+ $
              " (no trailing ')", /info
           VALUE = VALUE + STRMID( TEST, NEXT_CHAR, ENDAP-NEXT_CHAR )
;
;  Test to see if the next character is also an apostrophe.  If so, then the
;  string isn't completed yet.  Apostrophes in the text string are signalled as
;  two apostrophes in a row.
;
           IF STRMID( TEST, ENDAP+1, 1) EQ "'" THEN BEGIN    
              VALUE = VALUE + "'"
              NEXT_CHAR = ENDAP+2      
              GOTO, NEXT_APOST
           ENDIF
;
;  Extract the comment, if any.
;
           SLASH = STRPOS(TEST, "/", ENDAP)
           IF SLASH LT 0 THEN COMMENT = '' ELSE        $
              COMMENT = STRMID(TEST, SLASH+1, STRLEN(TEST)-SLASH-1)

;
; CM 19 Sep 1997
; This is a string that could be continued on the next line.  Check this
; possibility with the following four criteria: *1) Ends with '&'
; (2) Next line is CONTINUE  (3) LONGSTRN keyword is present (recursive call to
;  FXPAR) 4. /NOCONTINE is not set

           IF ~ KEYWORD_SET(NOCONTINUE) THEN BEGIN
              OFF = OFF + 1
              VAL = STRTRIM(VALUE,2)
              
              IF (STRLEN(VAL) GT 0) AND $
                 (STRMID(VAL, STRLEN(VAL)-1, 1) EQ '&') AND $
                 (STRMID(HDR[NFOUND[I]+OFF],0,8) EQ 'CONTINUE') THEN BEGIN
                 IF continuation THEN BEGIN
                    VALUE = STRMID(VAL, 0, STRLEN(VAL)-1)
                    TEST = HDR[NFOUND[I]+OFF]
                    TEST = STRMID(TEST, 8, STRLEN(TEST)-8)
                    TEST = STRTRIM(TEST, 2)
                    IF STRMID(TEST, 0, 1) NE "'" THEN MESSAGE, $
                       'ERROR: Invalidly CONTINUEd string in '+ABORT
                    NEXT_CHAR = 1
                    GOTO, NEXT_APOST
                 ENDIF
              ENDIF
           ENDIF

;
;  If not a string, then separate the parameter field from the comment field.
;
        ENDIF ELSE BEGIN
           TEST = SVALUE[I]
           SLASH = STRPOS(TEST, "/")
           IF SLASH GT 0 THEN BEGIN
              COMMENT = STRMID(TEST, SLASH+1, STRLEN(TEST)-SLASH-1)
              TEST = STRMID(TEST, 0, SLASH)
           END ELSE COMMENT = ''
;
;  Find the first word in TEST.  Is it a logical value ('T' or 'F')?
;
           TEST2 = TEST
           VALUE = GETTOK(TEST2,' ')
           TEST2 = STRTRIM(TEST2,2)
           IF ( VALUE EQ 'T' ) THEN BEGIN
              VALUE = 1
           END ELSE IF ( VALUE EQ 'F' ) THEN BEGIN
              VALUE = 0
           END ELSE BEGIN
;
;  Test to see if a complex number.  It's a complex number if the value and the
;  next word, if any, both are valid numbers.
;
              IF STRLEN(TEST2) EQ 0 THEN GOTO, NOT_COMPLEX
              VALUE2 = GETTOK(TEST2,' ')
              IF VALID_NUM(VALUE,VAL1) AND VALID_NUM(VALUE2,VAL2) $
              THEN BEGIN
                 VALUE = COMPLEX(VAL1,VAL2)
                 GOTO, GOT_VALUE
              ENDIF
;
;  Not a complex number.  Decide if it is a floating point, double precision,
;  or integer number.  If an error occurs, then a string value is returned.
;  If the integer is not within the range of a valid long value, then it will 
;  be converted to a double.  
;
NOT_COMPLEX:
              ON_IOERROR, GOT_VALUE
              VALUE = TEST
              IF NOT VALID_NUM(VALUE) THEN GOTO, GOT_VALUE
              IF (STRPOS(VALUE,'.') GE 0) OR (STRPOS(VALUE,'E') $
                                              GE 0) OR (STRPOS(VALUE,'D') GE 0) THEN BEGIN
                 IF ( STRPOS(VALUE,'D') GT 0 ) OR $
                    ( STRLEN(VALUE) GE 8 ) THEN BEGIN
                    VALUE = DOUBLE(VALUE)
                 END ELSE VALUE = FLOAT(VALUE)
              ENDIF ELSE BEGIN
                 LMAX = 2.0D^31 - 1.0D
                 LMIN = -2.0D31
                 VALUE = DOUBLE(VALUE)
                 if (VALUE GE LMIN) and (VALUE LE LMAX) THEN $
                    VALUE = LONG(VALUE)
              ENDELSE
              
;
GOT_VALUE:
              ON_IOERROR, NULL
           ENDELSE
        ENDELSE                 ; if string

; AC May 1, 2009
; See if the comment line continues on the next line.  The criterion
;   is: 1) The comment ends with '&' 2) the next line is COMMENT
;       3) LONGSTRN keyword is present 4) /NOCONTINUE is not set
; This is the convention used by HCSS (Herschel)
; Note: the code assumes that the special values (HISTORY, etc)
;  don't support multi-line comments, since they are essentially all comment
        IF ~ KEYWORD_SET(NOCONTINUE) THEN BEGIN
           com = STRTRIM(comment,2)
           IF (strlen(com) GT 0) AND $
              (strmid(com,STRLEN(com)-1,1) EQ '&') AND $
              (strmid(hdr[nfound[i]+1],0,8) EQ 'COMMENT ') THEN BEGIN
              IF continuation THEN BEGIN
                 test = hdr[nfound[i]+1]
                 test = STRTRIM(STRMID(test, 8, STRLEN(test)-8),2)
                 ;;Strip off the & from the old comment, add the new
                 ;;(no space)
                 comment = STRMID(comment,0,STRLEN(comment)-1)+test
              ENDIF
           ENDIF
        ENDIF

;
;  Add to vector if required.
;
        IF VECTOR THEN BEGIN
           MAXNUM = MAX(NUMBER)
           IF ( I EQ 0 ) THEN BEGIN
              IF N_ELEMENTS(DATATYPE) EQ 0 THEN BEGIN
                 ;; Data type determined from keyword
                 SZ_VALUE = SIZE(VALUE)
              ENDIF ELSE BEGIN
                 ;; Data type requested by user
                 SZ_VALUE = SIZE(DATATYPE[0])
              ENDELSE
              RESULT = MAKE_ARRAY( MAXNUM, TYPE=SZ_VALUE[1])
              COMMENTS = STRARR(MAXNUM)
           ENDIF 
           RESULT[   NUMBER[I]-1 ] =  VALUE
           COMMENTS[ NUMBER[I]-1 ] =  COMMENT
        ENDIF ELSE BEGIN
           COMMENTS = COMMENT
        ENDELSE
     ENDFOR
;
;  Set the value of !ERR for the number of matches for vectors, or simply 0
;  otherwise.
;
     IF VECTOR THEN BEGIN
        !ERR = MATCHES
        RETURN, RESULT
     ENDIF ELSE !ERR = 0
;
;  Error point for keyword not found.
;
  ENDIF ELSE BEGIN
     IF ABORT_RETURN THEN MESSAGE,'Keyword '+NAM+' not found in '+ABORT
     !ERR = -1
  ENDELSE
;
  RETURN, VALUE
END
