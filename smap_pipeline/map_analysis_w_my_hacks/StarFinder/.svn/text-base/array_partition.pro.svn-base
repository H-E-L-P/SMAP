; $Id: array_partition.pro, v 1.0 Dec. 1999 e.d. $
;
;+
; NAME:
;	ARRAY_PARTITION
;
; PURPOSE:
;	Define bounds to partition an array into a pre-fixed number of
;	parts along one dimension.
;
; CATEGORY:
;	Array manipulation.
;
; CALLING SEQUENCE:
;	ARRAY_PARTITION, Siz, N, L, U, S
;
; INPUTS:
;	Siz:	(Long) integer, representing the array size along the
;			dimension of interest.
;
;	N:	Number of parts into which the dimension must be partitioned.
;
; OUTPUTS:
;	L, U:	N-element long integer vectors, defined as follows:
;			[L[i], U[i]] are the bounds of the i-th partition, i = 0, N - 1.
;			Notice that L[0] = 0, U[N - 1] = Siz - 1
;
;	S:	N-element long integer vector, with the size of each sub-domain.
;
; EXAMPLE:
;	Given a 512x512 array, partition it into 2x2 sub-regions. Define the
;	bounds of the sub-arrays along one dimension:
;	IDL> ARRAY_PARTITION, 512, 2, L, U, S
;	IDL> PRINT, L, U
;	     0      256
;	     255    511
;
; MODIFICATION HISTORY:
; 	Written by:	Emiliano Diolaiti, December 1999.
;-

PRO array_partition, siz, n, l, u, s

	on_error, 2
	step = siz / n
	l = lindgen(n) * step
	u = l + step - 1  &  u[n - 1] = siz - 1
	s = u - l + 1
	return
end
