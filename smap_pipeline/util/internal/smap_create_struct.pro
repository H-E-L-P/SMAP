pro smap_create_struct, struct, strname, tagnames, tag_descript, $
                        DIMEN = dimen, CHATTER = chatter, NODELETE = nodelete
;+
; NAME:
;       SMAP_CREATE_STRUCT
; PURPOSE:
;       Create an IDL structure from a list of tag names and dimensions
; EXPLANATION:
;       Dynamically create an IDL structure variable from list of tag names 
;       and data types of arbitrary dimensions.   Useful when the type of
;       structure needed is not known until run time.
;
;       Unlike the intrinsic function CREATE_STRUCT(), this procedure does not
;       require the user to know the number of tags before run time.   (Note
;       there is no name conflict since the intrinsic CREATE_STRUCT is a 
;       function, and this file contains a procedure.)
; CALLING SEQUENCE:
;       SMAP_CREATE_STRUCT, STRUCT, strname, tagnames, tag_descript, 
;                             [ DIMEN = , /CHATTER, /NODELETE ]
;
; INPUTS:
;       STRNAME -   name to be associated with structure (string)
;               Must be unique for each structure created.   Set
;               STRNAME = '' to create an anonymous structure
;
;       TAGNAMES -  tag names for structure elements (string or string array)
;                Any strings that are not valid IDL tag names (e.g. 'a\2')
;                will be converted by IDL_VALIDNAME to a valid tagname by 
;                replacing with underscores as necessary (e.g. 'a_2')
;
;       TAG_DESCRIPT -  String descriptor for the structure, containing the
;               tag type and dimensions.  For example, 'A(2),F(3),I', would
;               be the descriptor for a structure with 3 tags, strarr(2), 
;               fltarr(3) and Integer scalar, respectively.
;               Allowed types are 'A' for strings, 'B' or 'L' for unsigned byte 
;               integers, 'I' for integers, 'J' for longword integers, 
;               'K' for 64bit integers, 'F' or 'E' for floating point, 
;               'D' for double precision  'C' for complex, and 'M' for double 
;               complex.   Unsigned long is 'U', and 64-bit unsigned
;               long is 'N'. Uninterpretable characters in a format field are 
;               ignored.
;
;               For vectors, the tag description can also be specified by
;               a repeat count.  For example, '16E,2J' would specify a 
;               structure with two tags, fltarr(16), and lonarr(2)
;
; OPTIONAL KEYWORD INPUTS:
;       DIMEN -    number of dimensions of structure array (default is 1)
;
;       CHATTER -  If set, then CREATE_STRUCT() will display
;                  the dimensions of the structure to be created, and prompt
;                  the user whether to continue.  Default is no prompt.
;
;       /NODELETE - If set, then the temporary file created
;                  CREATE_STRUCT will not be deleted upon exiting.   See below
;
; OUTPUTS:
;       STRUCT -   IDL structure, created according to specifications 
;
; EXAMPLES: 
;
;       IDL> create_struct, new, 'name',['tag1','tag2','tag3'], 'D(2),F,A(1)'
;
;       will create a structure variable new, with structure name NAME
;
;       To see the structure of new:
;
;       IDL> help,new,/struc
;       ** Structure NAME, 3 tags, 20 length:
;          TAG1            DOUBLE         Array(2)
;          TAG2            FLOAT          0.0
;          TAG3            STRING         Array(1)
;
; PROCEDURE:
;       Generates a temporary procedure file using input information with
;       the desired structure data types and dimensions hard-coded.
;       This file is then executed with CALL_PROCEDURE.
;
; NOTES:
;       If CREATE_STRUCT cannot write a temporary .pro file in the current 
;       directory, then it will write the temporary file in the getenv('HOME')
;       directory.
;
;       At present, can fail if a tag_name cannot be used as a proper
;       structure component definition, e.g., '0.10' will not
;       work, but a typical string like 'RA' or 'DEC' will.
;       A partial workaround checks for characters '\' and '/'
;       and '.' and converts them to '_'. in a tag_name.
;
;       Note that 'L' now specifies a LOGICAL (byte) data type and not a
;       a LONG data type for consistency with FITS binary tables
;
; RESTRICTIONS:
;       The name of the structure must be unique, for each structure created.
;       Otherwise, the new variable will have the same structure as the 
;       previous definition (because the temporary procedure will not be
;       recompiled).  ** No error message will be generated  ***
;
; SUBROUTINES CALLED:
;       CONCAT_DIR(), FDECOMP, REPCHR() 
;
; MODIFICATION HISTORY:
;       This is a modified version of create_struct from the
;       IDL/Astrolib library.  It is based on create_Struct 6.4,
;       but 1) uses compile_opt hidden in the temporary file, and 
;       2) supports a few additional format codes
;-
;-------------------------------------------------------------------------------

 compile_opt idl2
 if N_params() LT 4 then begin
   print,'Syntax - CREATE_STRUCT, STRUCT, strname, tagnames, tag_descript,' 
   print,'                  [ DIMEN = , /CHATTER, /NODELETE ]'
   return
 endif

 if not keyword_set( chatter) then chatter = 0        ;default is 0
 if (N_elements(dimen) eq 0) then dimen = 1            ;default is 1

 if (dimen lt 1) then begin
  print,' Number of dimensions must be >= 1. Returning.'
  return
 endif

; For anonymous structure, strname = ''
  anonymous = 0b
  if (strlen( strtrim(strname,2)) EQ 0 ) then anonymous = 1b

 good_fmts = [ 'A', 'B', 'I', 'L', 'F', 'E', 'D', 'J','C','M', 'K','U','N' ]
 fmts = ["' '",'0B','0','0B','0.0','0.0','0.0D0','0L','complex(0)', $
           'dcomplex(0)', '0LL', '0uL', '0ULL' ]
 arrs = [ 'strarr', 'bytarr', 'intarr', 'bytarr', 'fltarr', 'fltarr', $
          'dblarr', 'lonarr','complexarr','dcomplexarr','lon64arr',$
          'ulongarr', 'ulon64arr' ]
 ngoodf = N_elements( good_fmts )

; If tagname is a scalar string separated by commas, convert to a string array

 if size(tagnames,/N_dimensions) EQ 0 then begin
            tagname = strsplit(tagnames,',',/EXTRACT) 
 endif else tagname = tagnames

 Ntags = N_elements(tagname)

; Make sure supplied tag names are valid.

 if !VERSION.RELEASE GE '6.4' then $ 
          tagname = idl_validname( tagname, /convert_all ) else $
 for k = 0, Ntags -1 do $ 
         tagname[k] = idl_validname( tagname[k], /convert_all )

;  If user supplied a scalar string descriptor then we want to break it up
;  into individual items.    This is somewhat complicated because the string
;  delimiter is not always a comma, e.g. if 'F,F(2,2),I(2)', so we need
;  to check positions of parenthesis also.

 sz = size(tag_descript)
 if sz[0] EQ 0 then begin
      tagvar = strarr( Ntags)
      temptag = tag_descript
      for i = 0, Ntags - 1 do begin
         comma = strpos( temptag, ',' )
         lparen = strpos( temptag, '(' )
         rparen = strpos( temptag, ')' )
            if ( comma GT lparen ) and (comma LT Rparen) then pos = Rparen+1 $
                                                         else pos = comma 
             if pos EQ -1 then begin
                 if i NE Ntags-1 then message, $
         'WARNING - could only parse ' + strtrim(i+1,2) + ' string descriptors'
                 tagvar[i] = temptag 
                 goto, DONE
             endif else begin
                    tagvar[i] = strmid( temptag, 0, pos )
                    temptag = strmid( temptag, pos+1, 1000)
              endelse
             endfor
             DONE:
            
 endif else tagvar = tag_descript

; create string array for IDL statements, to be written into 
; 'temp_'+strname+'.pro'

 pro_string = strarr (ntags + 2) 

 if (dimen EQ 1) then begin

   pro_string[0] = "struct =  { " + strname + " $"
   pro_string[ntags+1] = " } "

 endif else begin

   dimen = long(dimen)                ;Changed to LONG from FIX Mar 95
   pro_string[0] = "struct "   + " = replicate ( { " + strname + " $"
   pro_string[ntags+1] = " } , " + string(dimen) + ")"

 endelse

 for i = 0, ntags-1 do begin

   goodpos = -1
   try = strupcase( tagvar[i] )
   for j = 0,ngoodf-1 do begin
         fmt_pos = strpos( try, good_fmts[j] )
         if ( fmt_pos GE 0 ) then begin
              goodpos = j
              goto, FOUND_FORMAT
         endif
   endfor

      print,' Format not recognized: ' + tagvar[i]
      print,' Allowed formats are :',good_fmts
      stop,' Redefine tag format (' + string(i) + ' ) or quit now'


FOUND_FORMAT:

    if fmt_pos GT 0 then begin

           repeat_count = strmid( tagvar[i], 0, fmt_pos )
           if strnumber( repeat_count, value ) then begin
                fmt = arrs[ goodpos ] + '(' + strtrim(fix(value), 2) + ')'
           endif else begin 
                print,' Format not recognized: ' + tagvar[i]
                stop,' Redefine tag format (' + string(i) + ' ) or quit now'
           endelse

    endif else  begin

; Break up the tag descriptor into a format and a dimension
    tagfmts = strmid( tagvar[i], 0, 1)
    tagdim = strtrim( strmid( tagvar[i], 1, 80),2)
    if strmid(tagdim,0,1) NE '(' then tagdim = ''

    if (tagdim EQ '') then fmt = fmts[goodpos] else $
                           fmt = arrs[goodpos] + tagdim 

    endelse

  if anonymous and ( i EQ 0 ) then comma = '' else comma = " , "

      pro_string[i+1] = comma + tagname[i] + ": " + fmt + " $"      

 endfor

; Check that this structure definition is OK (if chatter set to 1)
 
 if keyword_set ( Chatter )  then begin
   ans = ''
   print,' Structure ',strname,' will be defined according to the following:'
   temp = repchr( pro_string, '$', '')
   print, temp
   read,' OK to continue? (Y or N)  ',ans
   if strmid(strupcase(ans),0,1) eq 'N' then begin
      print,' Returning at user request.'
     return
   endif
 endif 

; --- Determine if a file already exists with same name as temporary file

 tempfile = 'temp_' + strlowcase( strname )
 cdhome = 0

TEST_EXIST:
EXIST:  
    list = file_search( tempfile + '.pro', COUNT = Nfile)
     if (Nfile GT 0) then begin
       tempfile = tempfile + 'x'
       goto, EXIST
     endif
 
; ---- open temp file and create procedure
; ---- If problems writing into the current directory, try the HOME directory

 openw, unit, tempfile +'.pro', /get_lun, ERROR = err
 if (err LT 0) and (not cdhome) then begin
      cdhome = 1
      tempfile = concat_dir(getenv('HOME'),tempfile)
      goto, TEST_EXIST
 endif
 fdecomp,tempfile,disk,dir,name
 printf, unit, 'pro ' +  name + ', struct'
 printf, unit, 'COMPILE_OPT HIDDEN'
 for j = 0,N_elements(pro_string)-1 do $
        printf, unit, strtrim( pro_string[j] )
 printf, unit, 'return'
 printf, unit, 'end'
 free_lun, unit

; If using the HOME directory, it needs to be included in the IDL !PATH

 if cdhome then cd,getenv('HOME'),curr=curr
  resolve_routine, name
  Call_procedure, name, struct
 if cdhome then cd,curr

 if keyword_set( NODELETE ) then begin
    message,'Created temporary file ' + tempfile + '.pro',/INF
    return
 endif else file_delete, tempfile + '.pro'
  
  return
  end         ;pro create_struct


