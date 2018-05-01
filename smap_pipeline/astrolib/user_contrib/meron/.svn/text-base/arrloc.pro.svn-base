Function Arrloc, inds, siz, expand = exd, contract = cnt

;+
; NAME:
;	ARRLOC
; VERSION:
;	3.3
; PURPOSE:
;	Converts array indices from expanded (multi dimensional) form to 
;	contracted (one dimensional) form and vice versa.
; CATEGORY:
;	Array Function.
; CALLING SEQUENCE:
;	Result = ARRLOC( INDS, SIZ [, keywords])
; INPUTS:
;    INDS
;	Numeric, converted to long integer on input.  Structure depends on 
;	usage mode (EXPAND versus CONTRACT) and on the input parameter SIZ, as
;	follows:
;	    EXPAND mode  :  INDS can be a scalar or a vector (1-dim array).
;	    CONTRACT mode:  If SIZ(0) = 0 then INDS arbitrary.  Else INDS can
;	    be a scalar, vector or a 2-dim array, subject to the following 
;	    restrictions:
;		INDS is scalar:  SIZ(0) must be 1.
;		INDS is vector:  SIZ(0) must equal the length of INDS.
;		INDS is M*N array:  SIZ(0) must equal M (first array dimension).
;    SIZ
;	Vector in the form of the standard output of the system function SIZE.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    /EXPAND
;	Switch, specifies expansion mode.  This is the default.
;    /CONTRACT
;	Switch, specifies contraction mode.
; OUTPUTS:
;	Depending on mode, as follows:
;	EXPAND mode:  Returns an M*N array, where M is the number of dimensions
;	    of the object charactarized by SIZ (i.e. SIZ(0)) and N is the 
;	    number of elements in INDS.  For example, if S = [2,3,8,4,24] 
;	    (corresponding to a 3*8 real array) then ARRLOC(5,S) will return
;	    the vector [2,1] (the indices of the 5th element) while 
;	    ARRLOC([2,5,22]) will return:	2	0
;						2	1
;						1	7
;	CONTRACT mode:  Reverses the action of expand.  When fed an array in 
;	    the form of the output of ARRLOC with EXPAND, returns a vector of
;	    1-dimensional indices.  For example a call to ARRLOC using S and
;	    the output array from the example above will return:	2
;									5
;									22
;    In either mode, if any of the resulting indices is out of bounds for the
;    array specified by SIZ, it is replaced by -1.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None other than those specified in the description of INDS, above.
; PROCEDURE:
;	Straightforward.  Calls ONE_OF from MIDL.
; MODIFICATION HISTORY:
;	Created 30-MARCH-1994 by Mati Meron.
;-

    on_error, 1
    act = One_of(exd,cnt) > 0
    item = long(inds)
    ndim = siz(0)
    if ndim gt 0 then begin
	if act then begin
	    csiz = size(item)
	    case csiz(0) of
		0:	begin
			    if ndim eq 1 then begin
				item = reform(replicate(item,1),1,1)
				csiz = size(item)
			    endif else message, 'Size incompatibility!'
			end
		1:	begin
			    if ndim eq csiz(1) then begin
				item = reform(item,csiz(1),1)
				csiz = size(item)
			    endif else message, 'Size incompatibility!'
			end
		2:	if ndim ne csiz(1) then $
			message, 'Size incompatibility!'
		else:	message, 'Size incompatibility!'
	    endcase
	    res = item(ndim-1,*)
	    for i = ndim - 2, 0, -1 do res = res*siz(i+1) + item(i,*)
	    for j = 0l, csiz(2) - 1 do begin
		tem = item(*,j) lt 0 or item(*,j) ge siz(1:ndim)
		dum = where(tem ne 0, nov)
		if nov ne 0 then res(j) = -1l
	    endfor
	endif else begin
	    nel = n_elements(item)
	    ovfl = where(item lt 0 or item ge siz(ndim+2), nov)
	    res = reform(lonarr(ndim*nel),ndim,nel)
	    i = 0
	    while i lt (ndim - 1) do begin
		res(i,*) = item mod siz(i+1)
		item = (item - res(i,*))/siz(i+1)
		i = i + 1
	    endwhile
	    res(i,*) = item
	    if nov ne 0 then res(*,ovfl) = -1l
	endelse
    endif else begin 
	res = lonarr(n_elements(item))
	jtem = where(item ne 0, ntem)
	if ntem gt 0 then res(jtem) = -1l
    endelse

    return, res
end
