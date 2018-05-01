FUNCTION FIND_INSTNOISE, noisemap, NOISEWID=noisewid

;+
;
; noiseval = FIND_INSTNOISE(noisemap)
;
; Find instrumental noise of deep region in map for passing to matched
; filter. 
;
; INPUTS:
;   noisemap:  noise map (duh)
;
; OUTPUTS:
;   noiseval:  instrumental noise of deep region
;
; OPTIONAL OUTPUTS:
;   noisewid:  width of deep noise peak
;
; CREATED BY: gmarsden 2011-07-20
;
;-

; parameters for algorithm
minpixfrac = 0.5
cutvalfrac = 1.1
nsigma_refit = 1

s = SIZE(noisemap, /DIM)
nx = s[0]
ny = s[1]

finiteind = WHERE(FINITE(noisemap), npix)

; find median of low-noise subset
sortind = SORT(noisemap[finiteind])
upperpix = CEIL(npix * minpixfrac)
medval = MEDIAN(noisemap[finiteind[sortind[0:upperpix-1]]])

; make histogram
binsize = (MAX(noisemap[finiteind]) - MIN(noisemap[finiteind])) / SQRT(npix)
hy = HISTOGRAM(noisemap[finiteind], BIN=binsize, LOC=hx)

; find peak to left of cutvalfrac * medval, fit
cutind = CEIL(INTERPOL(FINDGEN(N_ELEMENTS(hx)), hx, cutvalfrac * medval))

; make new high-res histogram
binsize = (hx[cutind-1] - hx[0]) / SQRT(TOTAL(hy[0:cutind-1]))
hy = HISTOGRAM(noisemap[finiteind], BIN=binsize, LOC=hx)
cutind = CEIL(INTERPOL(FINDGEN(N_ELEMENTS(hx)), hx, cutvalfrac * medval))

peakval = MAX(hy[0:cutind-1], peakind)
maxfitind = CEIL(INTERPOL(FINDGEN(N_ELEMENTS(hx)), hx, $
                          cutvalfrac * hx[peakind]))

xfit = hx[0:maxfitind-1]
yfit = hy[0:maxfitind-1]

pars0 = [peakval, hx[peakind], SQRT(MEAN((xfit - hx[peakind])^2))]
fit = GAUSSFIT(xfit, yfit, pars, NTERMS=3, EST=pars0)

; refit using +/-nsigma_refit data
fitind = WHERE(xfit GE pars[1] - nsigma_refit * pars[2] AND $
               xfit LT pars[1] + nsigma_refit * pars[2])

fit = GAUSSFIT(xfit[fitind], yfit[fitind], pars, NTERMS=3)

noiseval = pars[1]
noisewid = pars[2]

RETURN, noiseval

END
