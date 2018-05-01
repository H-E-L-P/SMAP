;+
; NAME:
;  ALTER_TAGS()
;
; PURPOSE:
;  Alter the tag definitions in a structure.  A second structure is input and
;  tags from that structure that match the original struct are altered to the
;  new data type.  The original struct is copied into the new using the
;  struct_assign builtin procedure with the /nozero option.  Thus if the
;  data type is compatible with the old one, the type is converted and 
;  copied in, otherwise they are left as defined.
;
; CALLING SEQUENCE:
;  newstruct = alter_tags(struct, defstruct)
;
; INPUTS:
;  struct: The original structure.  May be an array.
;  defstruct: A struct defining the tags.
;
; OUTPUTS:
;  A new structure with tags altered.
;
; EXAMPLE:
;  ; Convert a tag from int to float.
;  struct = {a:35, b:66}
;  newstruct = alter_tags(struct, {a:0.0})
;  print,newstruct.a
;    35.0000
;
; MODIFICATION HISTORY:
;  Early 2006: Erin Sheldon, NYU
;-
;
;
;
;  Copyright (C) 2005  Erin Sheldon, NYU.  erin dot sheldon at gmail dot com
;
;    This program is free software; you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation; either version 2 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program; if not, write to the Free Software
;    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;
;

FUNCTION alter_tags, struct, defstruct, status=status

  status = 1
  on_error, 2
  IF n_params() LT 2 THEN BEGIN 
      print,'-Syntax: newst = alter_tags(struct, defstruct)'
      print,'Change the definition of tags in a struct.'
      print
      message,'Halting'
  ENDIF 

  ;; All this complication is to make sure the
  ;; tags remain in the same order

  tags = tag_names(struct)
  dtags = tag_names(defstruct)

  ntags = n_elements(tags)
  altertags = intarr(ntags)


  match, tags, dtags, mt, md

  IF mt[0] EQ -1 THEN BEGIN 
      message,'No tags matched',/inf
      return, -1
  ENDIF 

  altertags[mt] = 1

  di = 0
  FOR i=0L, ntags-1 DO BEGIN 

      tagname = tags[i]
      IF altertags[i] THEN BEGIN 
          tagval = defstruct.(md[di])
          di = di+1
      ENDIF ELSE BEGIN 
          tagval = struct[0].(i)
      ENDELSE 

      IF i EQ 0 THEN BEGIN 
          newst = create_struct(tagname, tagval)
      ENDIF ELSE BEGIN 
          newst = create_struct(newst, tagname, tagval)
      ENDELSE 

  ENDFOR 

  num = n_elements(struct)
  IF num GT 1 THEN BEGIN 
      newst = replicate(newst, num)
  ENDIF 

  ;; This will only copy compatible tags.
  struct_assign, struct, newst, /nozero

  return, newst

END 
