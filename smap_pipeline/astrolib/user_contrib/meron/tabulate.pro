Pro Tabulate, a, b, c, d, e, f, g, h, index = ind, from = fro, to = to, $
    title = tit, header = ehed, noheader = nhed, format = efor, realform = rf, $
    IDF_off = idf, more = mor, file = fnam

;+
; NAME:
;	TABULATE
; VERSION:
;	3.3
; PURPOSE:
;	Accepts data in form of a set (up to 8) one dimensional arrays and 
;	prints it out as a table.
; CATEGORY:
;	Input/Output.
; CALLING SEQUENCE:
;	TABULATE, A [,B ....H] [,keywords]
; INPUTS:
;    A [B ... H]
;	One or more (up to 8) one dimensional arrays.  Type arbitrary 
;	(including string arrays).  If the array lengths are not all equal, the
;	shortest length will be used.
; OPTIONAL INPUT PARAMETERS:
;	All but the first array are arbitrary.
; KEYWORD PARAMETERS:
;    /INDEX
;	If set, the elements indices (0, 1 ... etc.) are printed in the first
;	column of the table.  Set by default if only one column is provided.
;    FROM
;	Specifies the index of the first element to be displayed.  Default is 0.
;    TO
;	Specifies the index of the last element to be displayed.  Default is 
;	end of array.
;    TITLE
;	Character string, used as the title of the table.  Default is no title.
;    HEADER
;	Character array containing the titles of the columns.  Default titles 
;	are the letters A thru H.  The title of the index column, if it appears
;	is N and cannot be changed.  If some of the entries in HEADER are null
;	strings, the default headers will be used for the corresponding columns.
;    /NOHEADER
;	Switch.  If set, no header is printed.  If HEADER is provided and
;	NOHEADER is set, HEADER will be ignored.
;    FORMAT
;	Character array, containing format specifications for the columns.  The
;	default formats are as follows:
;	    BYTE		- I4
;	    INTEGER		- I8
;	    LONG		- I12
;	    FLOATING		- G13.6
;	    DOUBLE		- G16.8
;	    COMPLEX		- G13.6  (twice)
;	    STRING		- A16
;	    DOUBLECOMPLEX 	- G16.8  (twice)
;	If some of the entries in FORMAT are null strings, defaults will be 
;	used for the corresponding columns.  If only partial formats are given
;	(for example 'E', or '16.6') missing fields are filled from the 
;	default.  Valid formats are A, D, E, F, G, I, O, Z.  Nonvalid formats
;	are ignored.
;	Important:  TABULATE may change the formats (either defaults or 
;	provided explicitly through FORMAT) in order to make enough room in the
;	display.
;    REALFORM
;	Character string (only first letter matters).  If given and is one of
;	D, E, F or G, provides the default format for all the real, double and
;	complex data.  If not given, the default is G format.
;    /IDF_OFF
;	Switch.  In normal operation formats I, D and F (if present) are 
;	adjusted to the sizes of numbers displayed.  /IDF_OFF, when set, turns 
;	off this adjustment.
;    FILE
;	String representing a valid file name (if extension is not provided the
;	default is .TAB).  If provided, the output is sent to this file, 
;	otherwise it is sent to the terminal.
;    /MORE
;	Sends output to the screen one page at a time (like UNIX MORE).
; OUTPUTS:
;	None, other then the printed table.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	TABULATE may reduce the widths of the print fields in order to 
;	accomodate all the data.  If after reduction to minimum widths the data
;	still cannot be fitted, no table will be generated.
;	While in theory TABULATE can display up to 8 columns, the actual number
;	depends on the data types and magnitudes.  Approximate maximal numbers 
;	of columns are:
;	    BYTE, INTEGER
;	    or STRING		- 8 columns (strings may be truncated)
;	    LONG		- 6-8 columns (depends on size)
;	    FLOAT		- 8 columns in E/G format.  Size dependent in F.
;	    DOUBLE		- 8 columns in E/G format.  Size dependent in F.
;	    COMPLEX		- 4 columns in E/G format.  Size dependent in F.
;	    DOUBLECOMPLEX	- 4 columns in E/G format.  Size dependent in F.
;
;	Beginning with the SEP-1997 version, undefined columns are ignored 
;	instead of being flagged as an error.  If there are defined columns
;	following an undefined one, they'll be ignored as well.
; PROCEDURE:
;	Straightforward.  Uses ABS_MM, DEFAULT, STREQ, STRPARSE_MM and TYPE 
;	from MIDL.
; MODIFICATION HISTORY:
;	Created 3-MAY-1992 by Mati Meron.
;	Modified 20-JUN-1995 by Mati Meron to accept the new DOUBLECOMPLEX type.
;	Modified 30-MAY-1996 by Mati Meron.  Added keyword NOHEADER.
;	Modified 20-JUL-1997 by Mati Meron.  Adjusted format handling.
;	Modified 20-SEP-1997 by Mati Meron.  Ability to ignore undefined
;	columns added.
;	Modified 15-NOV-1997 by Mati Meron.  Formats adjusted to IDL standards
;	and correction for the errant behavior of G format added.
;	Modified 20-SEP-1998 by Mati Meron.  TABULATE now accepts multiline 
;	titles.  Also, repeated formats, such as '3i' or '5e12.4' are now 
;	recognized.  Enclosing formats in parentheses is legal but not needed.
;-

    on_error, 1

    fdef = [ '','I','I','I','G','G','G','A','NA','G']
    wdef = [ -1,  4,  8, 12, 13, 16, 13, 16, -1 , 16]
    ddef = [ -1, -1, -1, -1,  6,  8,  6, -1, -1 ,  8]
    wmin = [ -1,  3,  4,  4,  7,  7,  7,  4, -1 ,  7]
    compa = 'ADEFGIOZ'
    compn = '0123456789'
    compr = 'DEFG'
    ncomax = 8
    linlen = 80
    linum = 22
    ulin = 95b
    cnams = ['N', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H']

    filfl = Type(fnam) eq 7
    nco = (n_params() < ncomax)
    if nco eq 0 then message, 'No data!'
    infl = keyword_set(ind)
    cf = 1 - infl
    nco = nco + infl
    cinf = intarr(nco)
    for i = infl, nco - 1 do begin
	id = execute('cinf(i)=n_elements('+cnams(i+cf)+')')
	if cinf(i) eq 0 then nco = nco < i
    endfor
    if nco eq infl then message, 'No data!'
    cinf = cinf(0:nco-1)

    nro = min(cinf(infl:*), max = nrox)
    if nro ne nrox then message, 'Incompatible lengths, truncating data!',/con
    if infl then if nro le 2^15-1 then n = indgen(nro) else n = lindgen(nro)
    for i = 0, nco - 1 do id = execute('cinf(i) = Type(' + cnams(i+cf) + ')')
    if (where(cinf eq 8))(0) ge 0 then message, 'Structures not allowed!'
    cxin = intarr(nco)
    cxco = where(cinf eq 6 or cinf eq 9, cxfl)
    cxfl = cxfl gt 0
    if cxfl then cxin(cxco) = 1

    if n_elements(rf) ne 0 then begin
	rfor = strupcase(strmid(strcompress(rf,/remove_all),0,1))
	if strpos(compr,rfor) ne -1 then fdef([4,5,6,9]) = rfor $
	else message, 'Default format ' +rfor+ ' not acceptable, ignored!',/con
    endif
    pf = fdef(cinf) 
    pw = wdef(cinf)
    pd = ddef(cinf)

    nefor = n_elements(efor)
    if nefor ne 0 then begin
	cfor = strupcase(strcompress(efor,/remove_all))
	tfor = ''
	for i = 0, nefor - 1 do begin
	    sdum = Strparse_mm(cfor(i),'()',lis)
	    ftem = lis(0)
	    sdum = Strparse_mm(ftem,'.' + compn,lis)
	    fpos = strpos(ftem,lis(0))
	    frep = fix(strmid(ftem,0,fpos)) > 1
	    sfor = strmid(ftem,fpos,strlen(ftem))
	    tfor = [tfor,replicate(sfor,frep)]
	endfor
	if not infl then tfor = tfor(1:*)
	nefor = n_elements(tfor) < nco
	for i = 0, nefor - 1 do begin
	    ftem = strmid(tfor(i),0,1)
	    if ftem ne '' and strpos(compa + compn,ftem) ne -1 then begin
		roff = 0
		if strpos(compa,ftem) ne -1 then begin
		    if cinf(i) ne 7 then pf(i) = ftem
		    roff = 1
		endif
		nfields = Strparse_mm(strmid(tfor(i),roff,6),'.',fields)
		if nfields ge 0 then pw(i) = fix(fields(0))
		if strpos(compr,pf(i)) eq -1 then pd(i) = -1 else $
	 	if nfields eq 1 then pd(i) = fix(fields(1)) > 0
	    endif else if ftem ne '' then $
	    message, 'Format ' + ftem + ' not acceptable, ignored!', /con
	endfor
    endif

    fwm = intarr(nco)
    if filfl or not keyword_set(idf) then begin
	com = ['fwm(dum(i))=floor(alog10(max(Abs_mm(' , '))>1))']
	dum = where(pf eq 'I', nd)
	for i = 0, nd - 1 do id = execute(com(0)+cnams(dum(i)+cf)+com(1)+'+2')
	dum = where(pf eq 'D' or pf eq 'F', nd)
	for i = 0, nd - 1 do id = execute(com(0)+cnams(dum(i)+cf)+com(1)+'+3')
    endif
    pwm = fwm > wmin(cinf)

    gap = replicate(4,nco)
    gap(0) = 0
    if nco gt 1 then begin
	dum = where(pf eq 'G' and gap ne 0 and cxin eq 0, nd)
	if nd ne 0 then gap(dum) = gap(dum) - 1
    endif else nd = 0
    netlin = linlen - nco + 1 + nd

    cow = pw > pwm
    mcow = pwm
    if cxfl then begin
	cow(cxco) = 2*(cow(cxco) + 2)
	mcow(cxco) = 2*(mcow(cxco) + 2)
    endif
    if fix(total(mcow)) le netlin then begin
	excess = fix(total(cow)) - netlin
	while excess gt 0 do begin
	    dum = where(cinf eq 7, nd)
	    if nd ne 0 then begin
		if (excess - fix(total(cow(dum) - mcow(dum)))) le 0 then begin
		    while excess gt 0 do begin
			cow(dum) = (cow(dum) - (excess/nd > 1)) > mcow(dum)
			excess = fix(total(cow)) - netlin
		    endwhile
		endif else begin
		    cow(dum) = mcow(dum)
		    excess = fix(total(cow)) - netlin
		endelse
	    endif
	    while excess gt 0 do begin
		dum = where(cow gt mcow and cxin eq 0, nd)
		ddum = where(cow gt mcow and cxin ne 0, ndd)
		sub = excess/(nd + 2*ndd) > 1
		if nd gt 0 then cow(dum) = (cow(dum) - sub) > mcow(dum)
		if ndd gt 0 then cow(ddum) = (cow(ddum) - 2*sub) > mcow(ddum)
		excess = fix(total(cow)) - netlin
	    endwhile
	endwhile
	mcow = cow
    endif else message, 'Too much stuff, can''t display!'

    dohed = not(keyword_set(nhed))
    nehed = n_elements(ehed)
    if not (dohed or nehed eq 0) then message, 'External header ignored!', /con
    if dohed then begin
	hed = cnams(cf:nco-infl)
	if nehed ne 0 then begin
	    dum = where(ehed(0:(nehed < (nco-infl)) - 1) ne '',nd)
	    if nd ne 0 then hed(dum + infl) = ehed(dum)
	endif
	cow = fix(cow > strlen(hed))
    endif
    excess = fix(total(cow)) - netlin
    while excess gt 0 do begin
	dum = where(cow gt mcow, nd)
	cow(dum) = (cow(dum) - (excess/nd > 1)) > mcow(dum)
	excess = fix(total(cow)) - netlin
    endwhile

    pw = cow
    if cxfl then pw(cxco) = pw(cxco)/2 - 2
    dum = where(pf eq 'D' or pf eq 'F', nd)
    if nd gt 0 then pd(dum) = 0 > pd(dum) < (pw(dum) - (3 > fwm(dum)))
    dum = where(pf eq 'E' or pf eq 'G', nd)
    if nd gt 0 then pd(dum) = 0 > pd(dum) < (pw(dum) - 7)

    repeat begin
	plen = fix(total(cow + gap))
	if plen gt linlen then gap = (gap - 1) > 0
    endrep until plen le linlen
    pg = strarr(nco)
    dum = where(gap gt 0,nd)
    if nd ne 0 then pg(dum) = strtrim(string(gap(dum)),2) + 'X,'

    pplen = (plen + 2) < linlen
    tlen = max(strlen(Default(tit,'',/dtype))) < linlen
    ttlen = (tlen + 2) < linlen

    pwd = string(pw)
    dum = where(pd ge 0, nd)
    if nd ne 0 then pwd(dum) = pwd(dum) + '.' + string(pd(dum))
    pwd = strcompress(pwd,/remove_all)
    for i = 0, nco - 1 do begin
	pf(i) = pf(i) + pwd(i)
	if cxin(i) then pf(i) ='"(",'+ pf(i) + ',", ",' + pf(i) + ',")"'
    endfor
    pf = pg + pf

    fro = Default(fro,0l,/dtype) < (nro - 1)
    to = Default(to, nro - 1l,/dtype) > fro
    tabl = strarr(to - fro + 1)
    tfro = 0l
    repeat begin
	tto = (tfro + 255) < (to - fro)
	for i = 0, nco - 1 do $
	id = execute("tabl(tfro:tto) = tabl(tfro:tto) + string(" $
	+ cnams(i+cf) + "(tfro+fro:tto+fro), format = '(' + pf(i) + ')')")
	tfro = tto + 1
    endrep until tto eq (to - fro)
    tabh = strarr(3)
    hf = pg + strcompress('A' + string(cow),/remove_all)
    if dohed then begin
	for i = 0, nco - 1 do begin
	    tabh(0) = tabh(0) + string(hed(i), format = '(' + hf(i) + ')')
	    shed = string(replicate(ulin,cow(i)))
	    tabh(1) = tabh(1) + string(shed, format = '(' + hf(i) + ')')
	endfor
	tabl = [tabh, tabl]
    endif
    if ttlen gt pplen then tabl = tabl + string(replicate(32b,ttlen - pplen))
    if tlen ne 0 then begin
	titfor = 'A' +  strcompress(string(tlen),/remove_all)
	tabh = string(tit + string(replicate(32b,tlen)),form='('+titfor+')')
	pad = (plen - tlen + 1)/2 > 0
	titfor = 'A' +  strcompress(string((ttlen > pplen) - pad),/remove_all)
	tabh= string([tabh,string(replicate(ulin,tlen)),''],form='('+titfor+')')
	if pad gt 0 then tabh = tabh + string(replicate(32b, pad))
	tabl = [tabh, tabl]
    endif

    tabfor = strcompress('A' + string(pplen > ttlen), /remove_all)
    if filfl then begin
	if Streq(!version.os,'vms',3) then begin
	    openw, unit, fnam, default = '.tab', /get_lun
	endif else openw, unit, fnam, /get_lun
	printf, unit, tabl, format = '(' + tabfor + ')'
	free_lun, unit
    endif else begin
	if keyword_set(mor) then begin
	    clear_screen
	    rout = n_elements(tabl) - 1
	    i = 0
	    j = 0
	    while j lt rout do begin
		j = (i + linum) < rout
		print, tabl(i:j), format = '(' + tabfor + ')'
		i = j + 1
		if j ne rout then hak
	    endwhile
	endif else print, tabl, format = '(' + tabfor + ')'
    endelse

    return
end
