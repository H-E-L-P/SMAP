;+
; NAME:
;   struct_addtags
;       
; PURPOSE:
;   Add new tags to the structure. 
;
; CALLING SEQUENCE:
;   newstruct = struct_addtags(oldstruct, tagnames, values, structype=)
;
; INPUTS: 
;   oldstruct: The original structure (or array of structures)
;   tagnames: new tag name(s), can be an array
;   values: string containing values for tagnames. must be same size
;            array as tagnames. Same format as MRD_STRUCT.PRO
;
; KEYWORD PARAMETERS:
;   structyp: a string with the name of the new structure.
;     if already defined the program will crash.
;       
; OUTPUTS: 
;   newstruct: The structure with the new tags in it.
;
; OPTIONAL OUTPUTS:
;   NONE
;
; EXAMPLE: 
;   tagnames=['ra', 'dec', 'image', 'name']
;   values  =['0d', '0d',  'intarr(1000, 1000)', "'NGC3035'"]
;   newst = struct_addtags(oldstruct, tagnames, values)
;
; CALLED ROUTINES:
;   MRD_STRUCT
;   STRUCT_COMBINE
; 
; PROCEDURE: 
;   Use mrd_struct to create a new structure.
;	
;
; REVISION HISTORY:
;   25-OCT-2000, Erin Scott Sheldon
;   2007-08-09, Converted from add_tags to function. Erin Sheldon, NYU
;                                             
;-      
;
;
;
;  Copyright (C) 2005  Erin Sheldon, NYU.  erin dot sheldon at gmail dot com
;
;    This program is free software; you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation; either version 2 of the License
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


function struct_addtags, struct, tagnames, values, structype=structype

    if n_params() lt 3 then begin 
        print,'Syntax - newst=struct_addtags(struct,tagnames,values,structype=)'
        print,'Use doc_library,"struct_addtags"  for more help.'  
        on_error, 2
        message,'Halting'
    END
  
    nt=n_elements(tagnames)
    nv=n_elements(values)
    if nt ne nv then begin 
        message,'Number of tagnames not equal to number of tag values'
    endif 

    if size(tagnames,/tname) ne 'STRING' then begin
        message,'tagnames must be a string array'
    endif 
    if size(values,/tname) ne 'STRING' then begin
        message,'values must be a string array'
    endif 

    n_struct = n_elements(struct)

    tmpstr = mrd_struct(tagnames, values, n_struct)
    if size(tmpstr,/tname) eq 'INT' then begin 
        message,'Error: MRD_STRUCT exited with error'
    endif 

    newstr = struct_combine(struct, tmpstr, structype=structype)

    return, newstr
end
