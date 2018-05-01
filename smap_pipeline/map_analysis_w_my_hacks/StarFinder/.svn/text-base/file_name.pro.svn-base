; $Id: file_name.pro, v 1.2 Jun 2000 e.d. $
;
;+
; NAME:
;	FILE_NAME
;
; PURPOSE:
;	Given the name of a file contained in a directory included
;	in the IDL !Path system variable, return the fully specified
;	name of the file. Operating system dependencies are taken
;	into account.
;
; CATEGORY:
;	Input/Output.
;
; CALLING SEQUENCE:
;	Result = FILE_NAME(Dir, File)
;
; INPUTS:
;	Dir:	Scalar string, name of parent directory.
;		It must be a directory included in the !Path variable.
;
;	File:	Scalar string, containing the name of the file with no
;		path divider.
;
; OUTPUTS:
;	Result:	Scalar string with the complete name of the file.
;
; RESTRICTIONS:
;	Apply only to files in directories included in the !Path system
;	variable.
;
; PROCEDURE:
;	Search the substring specified by the input variable Dir into the
;	!Path system variable. Append a path divider and the name of the
;	file specified by the input variable File.
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, November 1999.
;	Updates:
;	1) Fixed bug related to sub-directories
;	   (Emiliano Diolaiti, May 2000).
;	2) Case-insensitive (Emiliano Diolaiti, June 2000).
;-

FUNCTION file_name, dir, file

	on_error, 2
	; Define path of directory
	case  !Version.OS  of
           'vms': begin
              pdiv = ','  &  ddiv = '.'
              end
           'Win32': begin
              pdiv = ';'  &  ddiv = '\'
              end
           'MacOS' : begin
              pdiv = ','  &  ddiv = ':'
              end
           else: begin
              pdiv = ':'  &  ddiv = '/'
              end
	endcase
	lowpath = strlowcase(!Path)  &  lowdir = strlowcase(dir)
	first = strpos(lowpath, lowdir)
	while  first gt 0 and strmid(lowpath, first, 1) ne pdiv  do $
	   first = first - 1
	if  strmid(lowpath, first, 1) eq pdiv  then  first = first + 1
	last = strpos(lowpath, lowdir)  &  length = strlen(lowpath)
	while  last lt length - 1 and strmid(lowpath, last, 1) ne pdiv and $
	       strmid(lowpath, last, 1) ne ddiv  do $
	   last = last + 1
	if  strmid(lowpath, last, 1) eq ddiv or $
	    strmid(lowpath, last, 1) eq pdiv  then  last = last - 1
	path = strmid(!Path, first, last - first + 1)
	return, path + ddiv + file
end
