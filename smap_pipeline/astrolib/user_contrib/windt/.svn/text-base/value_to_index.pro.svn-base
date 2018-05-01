;+
; NAME:
;
;    VALUE_TO_INDEX
;
; PURPOSE:
;
;    Given a (1D) ARRAY and a scalar VALUE, determine the array INDEX
;    corresponding to the element of the array that is closest in
;    magnitude to the specified value.
;
; CALLING SEQUENCE:
;
;   Index=VALUE_TO_INDEX(ARRAY, VALUE)
; 
; INPUTS:
;
;   ARRAY = 1D array of values
;   
;   VALUE = scalar value
;
; EXAMPLE:
;
;  ARRAY=findgen(100)/99.*5          ; create an array of 100 pts from 0 to 5.
;
;  Index=VALUE_TO_INDEX(ARRAY,3.125) ; find the element of ARRAY whose value
;                                    ; is closest to 3.125.
;                                   
;  In this case, Index=62 (i.e., ARRAY(62)=3.13131)
; 
; MODIFICATION HISTORY:
;
;   David L. Windt, Bell Labs, March 1997.
;
;   May 1998 - Realized that this function is hardly necessary, as one
;              can just make wise use of the min function and
;              the !c system variable.  Duh!
;   
;   windt@bell-labs.com
;-
function value_to_index,array,value
on_error,2
if n_elements(value) ne 1 then message,'VALUE must be a scalar.'
sz=size(array)
if sz(0) ne 1 then message,'ARRAY must be a 1-D array.'

mm=min(abs(double(array)-double(value)))
return,!c

end
