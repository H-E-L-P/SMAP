;+
; NAME:
;   struct_concat
; 
; PURPOSE:
;   for concatenating two arrays of structures of the same type
;   but not necessarily the same name or order of tag names.
;
; CALLING SEQUENCE:
;   newst=struct_concat(str1, str2, /different)
;
; INPUTS:
;   str1, str2: 2 structures with the same tags to be concatenated.
;
; OUTPUTS:
;   strsum: concatenetion of str1, str2, with tag names in the order
;        of str1. If tags have same name but different type, then
;        a type conversion is performed, with result having type
;        of tag in str1.  Resulting structure has name of str1.
;        If tags differ and /different is sent, then the common tags 
;        are kept.
;
; KEYWORD PARAMETERS:
;    /different:  Allow different structs keeping the common tags.
;
; Original Author: Dave Johnston  UofM
; Rewrote and included ability to concat different structures keeping
;   common tags. Erin Sheldon, NYU
; 2007-08-09: Renamed struct_concat and made into a function.
;-
;
;
;
;  Copyright (C) 2005  Dave Johnston, Erin Sheldon.  erin dot sheldon at gmail dot com
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


function _struct_concat_different, str1, str2

  nstr1 = n_elements(str1)
  nstr2 = n_elements(str2)
  ntot=nstr1+nstr2

  tags1 = tag_names(str1)
  tags2 = tag_names(str2)

  ntags1=n_elements(tags1)

  ;; tags will be ordered as in str1
  for i=0l, ntags1-1 do begin 
      w=where( tags2 eq tags1[i], nw)
      if nw ne 0 then begin 
          if n_elements(cstr) eq 0 then begin 
              cstr = create_struct(tags1[i], str1[0].(i))
          endif else begin 
              cstr = create_struct(cstr, tags1[i], str1[0].(i))
          endelse 
      endif 
  endfor 

  if n_elements(cstr) eq 0 then begin 
      on_error, 2
      message,'No common tags found'
  endif 

  strsum = replicate(cstr, ntot)
  newtags = tag_names(cstr)
  nmatch=n_elements(newtags)

  match, tags1, newtags, m1, mnew
  for i=0l, nmatch-1 do strsum[0:nstr1-1].(mnew[i]) = str1.(m1[i])
  match, tags2, newtags, m2, mnew
  for i=0l, nmatch-1 do strsum[nstr1:ntot-1].(mnew[i]) = str2.(m2[i])

  return, strsum
end 

function struct_concat, str1, str2, different=different

    if n_params() lt 2 then begin 
        print,'-syntax: strsum=struct_concat(str1, str2, /different)'
        on_error, 2
        message,'Halting'
    endif

    tags1 = tag_names(str1)
    tags2 = tag_names(str2)
    ntags=n_elements(tags1)

    match, tags1, tags2, m1, m2

    if m1[0] eq -1 then begin
        on_error, 2
        message,'No matching tags'
    endif
 
    if n_elements(m1) ne ntags then begin 

        ;; User may request to keep common tags
        if keyword_set(different) then begin 
            strsum=_struct_concat_different(str1, str2)
            return, strsum
        endif 
        on_error, 2
        message,'Structures must have the same tags.  Send /different when structs may differ.'

    endif 

    n1 = n_elements(str1)
    n2 = n_elements(str2)
    ntot = n1 + n2

    strsum = replicate(str1[0], ntot)
    strsum[0:n1-1] = str1

    for i=0, ntags-1 do strsum[n1:ntot-1].(m1[i]) = str2.(m2[i])

    return, strsum
end


 
