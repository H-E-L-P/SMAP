;+
; NAME:
; 
;	CONT_IMAGE2
;	
; PURPOSE:
; 
;	Display an image and overlayer the contours from a second image.
;	
; CALLING SEQUENCE:
; 
;	CONT_IMAGE2, IMAGE1, IMAGE2, X, Y
;	
; INPUTS:
; 
;	IMAGE1 = Image to display.
;	
;	IMAGE2 = Image from which contours are drawn.
;	
; KEYWORD PARAMETERS:
; 
;	WINDOW_SCALE = set to scale the window size to the image size,
;		otherwise the image size is scaled to the window size.
;		Ignored when outputting to devices with scalable pixels.
;		
;	ASPECT = set to retain image's aspect ratio.  Assumes square
;		pixels.  If /WINDOW_SCALE is set, the aspect ratio is
;		retained.
;		
;	INTERP = set to bi-linear interpolate if image is resampled.
;	
;	Plus IDL graphics keywords: XTITLE, YTITLE, SUBTITLE, TITLE
;	
; PROCEDURE:
; 
;	If the device has scalable pixels then the image is written over the
;       plot window.
;	
; MODIFICATION HISTORY:
; 
;	Adapted (i.e. stolen) from IMAGE_CONT
;	
;	D. L. Windt, Bell Laboratories, June 1991.
;	
;	April 1994:
;	Changed image scaling to go from 32 to !d.n_colors, so that
;	TEK_COLOR can be called to use first 32 colors for other plotting.
;	Added _EXTRA keyword.
;
;	windt@bell-labs.com
;	
;-

pro cont_image2,a1,a2,x,y,window_scale=window_scale,aspect=aspect, $
        interp=interp, $
        xtitle=xtitle,ytitle=ytitle,title=title,subtitle=subtitle,_extra=e

if keyword_set(xtitle) then xtitle=xtitle else xtitle=' '
if keyword_set(ytitle) then ytitle=ytitle else ytitle=' '
if keyword_set(title) then title=title else title=' '
if keyword_set(subtitle) then subtitle=subtitle else subtitle=' '

; first do image 1...

a=a1

if !d.name eq 'PS' then a=255-a

sz = size(a)			;Size of image
if sz(0) lt 2 then begin
    print,'IMAGE_CONT -- parameter not 2D'
    return
endif

                                ;set window used by contour
contour,[[0,0],[1,1]],/nodata, xstyle=4, ystyle = 4,_extra=e

px = !x.window * !d.x_vsize	;Get size of window in device units
py = !y.window * !d.y_vsize
swx = px(1)-px(0)		;Size in x in device units
swy = py(1)-py(0)		;Size in Y
six = float(sz(1))		;Image sizes
siy = float(sz(2))
aspi = six / siy		;Image aspect ratio
aspw = swx / swy		;Window aspect ratio
f = aspi / aspw			;Ratio of aspect ratios

if (!d.flags and 1) ne 0 then begin ;Scalable pixels?
    if keyword_set(aspect) then begin ;Retain aspect ratio?
                                ;Adjust window size
        if f ge 1.0 then swy = swy / f else swx = swx * f
    endif

    tv,bytscl(a,top=!d.n_colors-32)+32, $
      px(0),py(0),xsize = swx, ysize = swy, /device

endif else begin                ;Not scalable pixels	
    if keyword_set(window_scale) then begin ;Scale window to image?
        tv,bytscl(a,top=!d.n_colors-32)+32,px(0),py(0) ;Output image
        swx = six		;Set window size from image
        swy = siy
    endif else begin		;Scale window
        if keyword_set(aspect) then begin
            if f ge 1.0 then swy = swy / f else swx = swx * f
        endif                   ;aspect
        tv,poly_2d(bytscl(a,top=!d.n_colors-32)+32,$ ;Have to resample image
                   [[0,0],[six/swx,0]], [[0,siy/swy],[0,0]],$
                   keyword_set(interp),swx,swy), $
          px(0),py(0)
    endelse			;window_scale
endelse                         ;scalable pixels

if n_params() ge 3 then ax=x else ax=findgen(sz(1))
if n_params() eq 4 then ay=y else ay=findgen(sz(2))

; now do image 2...

a=a2

;if !d.name eq 'PS' then a=255-a

if n_elements(lev) ne 0 then begin
    contour,a,ax,ay,/noe,/xst,/yst, $ ;Do the contour
      pos=[px(0),py(0),px(0)+swx,py(0)+swy],/dev, $
      lev=lev, $
      xtitle=xtitle,ytitle=ytitle,title=title,subtitle=subtitle
endif else begin
    contour,a,ax,ay,/noe,/xst,/yst, $ ;Do the contour
      pos=[px(0),py(0),px(0)+swx,py(0)+swy],/dev, $
      xtitle=xtitle,ytitle=ytitle,title=title,subtitle=subtitle
endelse

return
end
