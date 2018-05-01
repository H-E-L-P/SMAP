; $Id: stack_combine.pro, v 1.1 May 2005 e.d. $
;
;+
; NAME:
;   STACK_COMBINE
;
; PURPOSE:
;   Combine frames in a 3D stack by average, average with sigma-rejection
;  or median.
;
; CATEGORY:
;   Signal processing.
;
; CALLING SEQUENCE:
;   Result = STACK_COMBINE(Stack)
;
; INPUTS:
;   Stack:    3D array, representing the stack of frames to be combined
;
; KEYWORD PARAMETERS:
;   AVGTYPE:  Set this keyword to choose a combination algorithm among
;     the following possibilities:
;     AVGTYPE = 0 [default]: pixel-by-pixel average
;             = any other value: median
;
;   MASK: 3D binary array used to mask the frames in the Stack.
;     The n-th frame of the 3D array Mask passed with this keyword may
;     be defined as follows:
;     Mask[j, i, n] = 1, if  Stack[j, i, n] is a valid pixel
;                   = 0, if  Stack[j, i, n] must be rejected
;     The default is no masking.
;
;   WEIGHTS:  Set this keyword to a N-components vector, where N is the
;     number of frames in the Stack. The n-th component is the weight to
;     apply to the n-th input frame. The weights must be positive and
;     non-zero.
;      If the median is used, then the weights are divided by their minimum
;      value and rounded to integers. Every plane in the Stack is then
;      replicated a number of times equal to the corresponding weight.
;
;   NSTDEV:   when the average with sigma-clipping is used, the outliers to
;     be excluded from the computation are defined as those values whose
;     absolute distance from the median of the sample is larger than NSTDEV
;     times the standard deviation of the sample.
;
; OUTPUTS:
;   Result:   2D array, given by the combination of the planes in the Stack.
;
; MODIFICATION HISTORY:
;   Written by:  Emiliano Diolaiti, September 2001.
;   1) Mean and median average only (E.D. May 2005)
;-

FUNCTION stack_combine, stack, MASK = mask, WEIGHTS = weights, $
                         AVGTYPE = avgtype, _EXTRA = extra

    on_error, 2
    ; Default average type
    if  n_elements(avgtype) eq 0  then  avgtype = 0
    ; Stack size
    if  size52(stack, /N_DIM) ne 3  then  return, stack
    s = size52(stack, /DIM)  &  s0 = s[0]  &  s1 = s[1]  &  n_frames = s[2]
    ; Mask some frames?
    masked = n_elements(mask) ne 0
    ; Weighted average?
    weighted = n_elements(weights) ne 0
    if  weighted  then  weighted = min(weights) gt 0
    if  weighted  then  w = weights  else  w = replicate(1, n_frames)
    w = w / total(w)
    if  weighted and avgtype eq 2  then  w = round(w / min(w))
    ; Combine planes of the stack
    if  avgtype eq 0  then begin
       wstack = replicate(w[0], s0, s1)
       for  n = 1, n_frames - 1  do  $
          wstack = [[[wstack]], [[replicate(w[n], s0, s1)]]]
       m = total(wstack * stack, 3)
    endif else begin
       m = make_array(s0, s1, TYPE = size52(stack, /TYPE))
       for  i = 0L, s1 - 1  do  for  j = 0L, s0 - 1  do begin
          slice = stack[j,i,*]  &  w_ji = w  &  n = n_frames
          ; Mask undesired frames
          if  masked  then begin
             accept = where(mask[j,i,*] ne 0, n)
             if  n ne 0  then begin
                slice = slice[accept]  &  w_ji = w_ji[accept]
             endif
          endif
          ; If median is applied, replicate frames according to their weight
          if  avgtype eq 2 and weighted and n ne 0  then begin
             temp = replicate(slice[0], w_ji[0])
             for  k = 1L, n - 1  do $
                temp = [temp, replicate(slice[k], w_ji[k])]
             slice = temp
          endif
          ; Compute average of pixel [j,i]
          if  n ne 0  then $
             if  avgtype eq 0  then $
                m[j,i] = mean(slice)  else $
                m[j,i] = median(slice, /EVEN)
       endfor
    endelse
    return, m
end
