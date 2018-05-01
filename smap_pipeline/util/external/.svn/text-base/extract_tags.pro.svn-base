
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    EXTRACT_TAGS()
;       
; PURPOSE:
;   Extract the specified tags from input structure and return in a new
;   struct ordered as requested.
;
; CALLING SEQUENCE:
;    newstruct = extract_tags(oldstruct, tagnames)
;
; INPUTS: 
;    oldstruct: the original structure
;    tagnames: the names of tags to keep.
;
; OUTPUTS: 
;   newstruct: the new structure with requested tags. Tags are in the order
;       requested.
;
; CALLED ROUTINES:
;    MATCH
;    
; PROCEDURE: 
;    
;	
;
; REVISION HISTORY:
;   Created: 2007-08-03, Erin Sheldon, NYU
;       
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION extract_tags, struct, tagnames

    if n_params() eq 0 then begin 
        print,'Syntax - newstruct = extract_tags(struct, tagnames)'
        print
        print,'Use doc_library,"extract_tags"  for more help.'  
        return, -1
    end

    ;; Figure out which tags are to be kept

    tags=tag_names(struct)
    n=n_elements(tags)
    tagnames=strupcase(tagnames)
    nt=n_elements(tagnames)
    if nt eq 1 then begin
        t=where(tags eq tagnames[0],nw) 
        if nw eq n then begin
            message,'Tag did not match, returning -1',/inf
            return, -1
        endif 
    endif else begin 
        match, tags, tagnames, m, min, count=nm
        if nm eq 0 then begin
            message,'No tags matched, returning -1',/inf
            return, -1
        endif 
        if nm eq n then begin 
            return, struct
        endif 
    endelse 
      
    ;; create new structure
    s=sort(min)
    m = m[s]
    tags=tags[m]
    n=n_elements(tags)

    newstruct=create_struct(tags[0],struct[0].(m[0]))
  
    for i=1l, n-1 do begin
        newstruct = $
            create_struct(newstruct, tags[i], struct[0].(m[i]) )
    endfor
    newstruct=replicate(newstruct, n_elements(struct) )
    struct_assign, struct, newstruct

    return, newstruct

END
