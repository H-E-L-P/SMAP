; $Id: read_float_data, v 1.1 Mar 2000 e.d. $
;
;+
; NAME:
;	READ_FLOAT_DATA
;
; PURPOSE:
;	Read a Free Format file containing floating-point numbers,
;	ordered by columns.
;
; CATEGORY:
;	Input/Output.
;
; CALLING SEQUENCE:
;	Result = READ_FLOAT_DATA(File, Ncolumns)
;
; INPUTS:
;	File:	File name (string)
;
;	Ncolumns:	Number of columns in the input file
;
; KEYWORD PARAMETERS:
;	MSG:	Set this keyword to display a dialog message in case
;		of input error.
;
; OUTPUTS:
;	Result:	Floating point array of size Ncolumns * Nrows, where Nrows
;		is the number of rows in the input file, determined by the routine.
;		If an error occurs, a scalar string is returned, with the message
;		'I/O error'.
;
; RESTRICTIONS:
;	The numbers in the input file MUST be written in rows of Ncolumns
;	elements each. After the last number, only a 'carriage return' or no
;	characters at all are allowed. If the last number in the last line is
;	followed by one or more lines with one or more blank spaces, an error
;	occurs.
;	In practice this routine may be used to read data previously written by
;	the IDL intrinsic procedure PRINTF.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, September 1999.
;   Updates:
;	1) Added Free_Lun instruction (Emiliano Diolaiti, March 2000).
;-

FUNCTION read_float_data, file, ncolumns, MSG = msg

	catch, i_error
	if  i_error ne 0  then begin
	   if  keyword_set(msg)  then $
	      m = dialog_message(/ERROR, 'Input error')
	   if  n_elements(u) ne 0  then  free_lun, u
	   return, 'I/O error'
	endif
	openr, u, file, /GET_LUN
	line = fltarr(ncolumns)  &  data = fltarr(ncolumns, 2)
	n = 0L
	while  not eof(u)  do begin
	   readf, u, line  &  data[*,n] = line  &  n = n + 1
	   data = extend_array(data, ncolumns, n + 1, /NO_OFF)
	endwhile
	if  n ne 0  then  data = data[*,0:n-1]
	close, u  &  free_lun, u
	return, data
end
