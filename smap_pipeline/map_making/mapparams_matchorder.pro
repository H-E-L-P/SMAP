;+
;NAME
; mapparams_matchorder
;PURPOSE
; Given two sets of mapparams, figures out which TODS in the first
; one match the second one.  Assumes that the TODS of the second are
; a subset of those in the first.
;USAGE
; idx = mapparams_matchorder(mapparam1, mapparam2)
;INPUTS
; mapparam1     The first mapparam (from smap_read_and_filter)
; mapparam2     The second mapparam
;RETURNS
; An index such that mapparam1.file_obsids[idx] = mapparam2.file_obsids
; and mapparam1.file_bbids[idx] = mapparam2.file_bbids, or -1 if the match
; failed.
;OPTIONAL OUTPUTS
; errmsg        Error message if failed
; success       1 on success, 0 on failure
;MODIFICATION HISTORY
; Author: Alex Conley, April 2013
;-

FUNCTION mapparams_matchorder, mapparam1, mapparam2, $
                               ERRMSG=errmsg, SUCCESS=success
  COMPILE_OPT IDL2, STRICTARRSUBS

  success = 0b
  errmsg = ""

  ;;A common case is to be passed two mapparams in the same order with
  ;; the same files/obsids.  So it's worth checking that first
  IF N_ELEMENTS(mapparam1.file_obsids) EQ $
     N_ELEMENTS(mapparam2.file_obsids) THEN BEGIN
     wdisj = WHERE(mapparam1.file_obsids NE mapparam2.file_obsids OR $
                   mapparam1.file_bbids NE mapparam2.file_bbids, ndisj)
     IF ndisj EQ 0 THEN BEGIN
        success = 1b
        RETURN, INDGEN(N_ELEMENTS(mapparam1.file_obsids))
     ENDIF
  ENDIF

  ;;We will compare using the string concatenation of obsid and bbid
  comp1 = TO_HEX(mapparam1.file_obsids) + "_" + TO_HEX(mapparam1.file_bbids)
  comp2 = TO_HEX(mapparam2.file_obsids) + "_" + TO_HEX(mapparam2.file_bbids)

  ;;Make sure they are unique
  ucomp1 = UNIQ(comp1, SORT(comp1))
  IF N_ELEMENTS(ucomp1) NE N_ELEMENTS(comp1) THEN BEGIN
     errmsg = "Non-unique OBSID/BBID combo in mapparam1"
     RETURN,-1
  ENDIF
  ucomp2 = UNIQ(comp2, SORT(comp2))
  IF N_ELEMENTS(ucomp2) NE N_ELEMENTS(comp2) THEN BEGIN
     errmsg = "Non-unique OBSID/BBID combo in mapparam2"
     RETURN, -1
  ENDIF

  ;;Make sure we can actually find everything from mapparam2 in
  ;;mapparam1
  w1 = WHERE_ARRAY(comp2, comp1, nw1)
  IF nw1 NE N_ELEMENTS(comp2) THEN BEGIN
     errmsg = "Can't find matches for all files in mapparam2 in mapparam1"
     RETURN, -1
  ENDIF

  w2 = MATCH_ORDER(comp1[w1], comp2)
  success = 1b
  RETURN, w1[w2]

END
