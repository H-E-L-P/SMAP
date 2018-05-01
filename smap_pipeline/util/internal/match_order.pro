;+
;NAME
; match_order
;PURPOSE
; Given two lists of unique items that contain the same
; elements in different orders, returns an index into the
; second list that will bring it into the same order as the
; first.
;USAGE
; w = match_order( list1, list2 )
;INPUTS
; list1           The list whose order you want to match list2 into
; list2           The list you want to rearrange to match list1.
;RETURNS
; An index into list2 that brings it into the same order as list1
;EXAMPLE
;  a = REPLICATE( { idx: 0, val: '' }, 9 )
;  b = a
;  a.idx = FINDGEN(9)
;  a.val = STRING( a.idx, FORMAT='(I1)' )
;  b.idx = REVERSE(a.idx)
;  b.val = a.val
;  PRINT,b[ match_order( a.idx, b.idx ) ].val
; Will return 8 7 6 5 4 3 2 1 0
;MODIFICATION HISTORY
; Author: Alex Conley, Feb 2008
;-

FUNCTION match_order, list1, list2
 
COMPILE_OPT IDL2, STRICTARRSUBS
ON_ERROR,2

IF N_ELEMENTS(list1) NE N_ELEMENTS(list2) THEN $
  MESSAGE,"List1 and List2 are different lengths"

sort1 = SORT( list1 )
sort2 = SORT( list2 )

;;Check uniqueness
IF N_ELEMENTS( UNIQ( list1, sort1 ) ) NE N_ELEMENTS(list1) THEN $
  MESSAGE,"List1 has non-unique elements"
;;This will also check to make sure list2 has all unique elements
wbad = WHERE( list1[sort1] NE list2[sort2], nbad )
IF nbad NE 0 THEN MESSAGE,"List1 and List2 have different elements"

;;Get the reverse sort index for list1
reverse_index = SORT( sort1 )

RETURN,sort2[ reverse_index ]

END
