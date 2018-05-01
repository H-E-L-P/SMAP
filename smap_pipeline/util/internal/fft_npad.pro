;+
; NAME
;  fft_npad
; PURPOSE
;  To figure out how much padding is needed to get some
;  input size to be a product of small primes to speed FFT
;  transformations
; USAGE
;  padsize = fft_npad(n)
; RETURNS
;  The amount to pad n by to get the fastest
; NOTES
;  Uses the time estimate from the FFT help page
; MODIFICATION HISTORY
;  Author: Alex Conley
;-

FUNCTION fft_npad, n
  COMPILE_OPT IDL2

  IF (n LE 1) THEN MESSAGE, "n must be > 1"
  nl = ULONG(n)
  ;; Quick exit on power of 2
  IF (nl AND (nl - 1)) EQ 0 THEN RETURN, 0

  ;; Figure out maximum powers of 2, 3, 5
  lgn = ALOG(nl)
  n2 = CEIL(lgn / ALOG(2))
  n3 = CEIL(lgn / ALOG(3))
  n5 = CEIL(lgn / ALOG(5))

  totn = n2 * n3 * n5
  IF totn GT 5000 THEN MESSAGE, "Overly large n to study"
  cost = REPLICATE(!VALUES.F_NAN, totn) ;; use float for NaNs

  ;; Cost factor from FFT help page, and with T_1 = 1
  FOR i = 0, n2-1 DO BEGIN
     n2val = 2ul^i
     ;; Cost for purely factors of 2
     IF n2val GE nl THEN cost[i * n3 * n5] = n2val * (1 + 2 * i)
     FOR j = 0, n3-1 DO BEGIN
        currn3 = n2val * 3uL^j
        IF currn3 GE nl THEN $
           cost[i * n3 * n5 + j * n5] = currn3 * (1 + 2 * i + 12 * j)
        FOR k = 0, n5-1 DO BEGIN
           currn5 = currn3 * 5uL^k
           IF currn5 GE nl THEN $
              cost[i * n3 * n5 + j * n5 + k] = $
              currn5 * (1 + 2 * i + 12 * j + 20 * k)
        ENDFOR
     ENDFOR
  ENDFOR

  ;; Compute amount of padding to use
  mincost = MIN(cost, wmin, /NAN)
  k5 = wmin MOD n5
  k3 = (wmin / n5) MOD n3
  k2 = wmin / (n3 * n5)
  RETURN, 2uL^k2 * 3uL^k3 * 5uL^k5 - nl
END
