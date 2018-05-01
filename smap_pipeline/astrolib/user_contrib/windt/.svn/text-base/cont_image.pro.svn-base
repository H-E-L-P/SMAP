;+
; NAME:
; 
;       CONT_IMAGE
;	
; PURPOSE:
; 
;       Overlay an image and a contour plot.
;	
; CALLING SEQUENCE:
; 
;	CONT_IMAGE, IMAGE[,X,Y]
;	
; INPUTS:
; 
;       IMAGE - 2 dimensional array to display.
;	
; OPTIONAL INPUTS:
; 
;	X - 1 dimensional array of x-axis values.
;	
;	Y - 1 dimensional array of y-axis values.
;	
; KEYWORD PARAMETERS:
; 
;       WINDOW_SCALE - Set to scale the window size to the image size,
;                      otherwise the image size is scaled to the
;                      window size.  Ignored when outputting to
;                      devices with scalable pixels.
;		
;       ASPECT - Set to retain image's aspect ratio.  Assumes square
;                pixels.  If ASPECT is set, the aspect ratio is
;                retained.
;		
;       INTERP - Set to bi-linear interpolate if image is resampled.
;	
;       NOCONTOUR - Set to just display the image with plot axes.
;	
;       INVERT - Set to invert the image scale, ie image=255-image
;
;       TOP - The maximum value of the scaled image. If not set, then
;             it's set to (!d.n_colors < 255)-1.
;
;       MIN_VALUE - The minimum value of IMAGE to be displayed.
;
;       MAX_VALUE - The maximum value of IMAGE to be displayed.
;
;       COLORBAR - Set to display a color bar alongside the image.
;
;       BAR_TITLE - A text string to be used as the colorbar title if
;                   COLORBAR is set.
;
;       BAR_WIDTH - Width of the colorbar, in pixels. Default is 10
;                   pixels for non-scalable pixel devices, or 2% of
;                   the plot width for scalable pixel devices.
; 
;       BAR_OFFSET - Offset spacing between plot and colorbar. Default
;                    is 10.
; 
;	
; MODIFICATION HISTORY:
; 
;       Adapted (i.e. stolen) from IMAGE_CONT.
;	
;       D. L. Windt, Bell Laboratories, Nov 1989.
;	
;       April 1994:
;       Changed image scaling to go from 32 to !d.n_colors, so that
;       TEK_COLOR can be called to use first 32 colors for other plotting.
;	
;       Added _EXTRA keyword.
;
;       March 1998 - Added TOP, MIN_VALUE, MAX_VALUE, COLORBAR, and
;                    BAR_TITLE keywords.  Also fixed quite a few bugs.
;                    Note that setting the XSTYLE, XTYPE, YSTYLE, and
;                    YTYPE keywords has no effect: these parameters
;                    are always set to 0,1,0, and 1, respectively.
;
;       August 1998 - Plots are now drawn properly when !p.multi is
;                     different from 0. Added BAR_OFFSET keyword.
;
;	windt@bell-labs.com
;	
;-
pro cont_image,a,x,y,invert=invert, $
        xrange=xrange,yrange=yrange, $
        window_scale=window_scale,aspect=aspect,interp=interp, $
        top=top,min_value=min_value,max_value=max_value, $
        colorbar=colorbar,bar_title=bar_title,bar_width=bar_width, $
        bar_offset=bar_offset, $
        nocontour=nocontour,_extra=e,xtype=xtype,ytype=ytype, $
        xstyle=xstyle,ystyle=ystyle,title=title,subtitle=subtitle, $
        position=position

on_error,2

sz=size(a)
if sz(0) lt 2 then message,'CONT_IMAGE -- parameter not 2D'

;; define ax and ay
if n_params() ge 2 then ax=x else ax=findgen(sz(1))
if n_params() eq 3 then ay=y else ay=findgen(sz(2))

;; invert image?
if keyword_set(invert) then a=255-a

;; set range parameters:
if n_elements(xrange) ne 2 then xrange=[min(ax),max(ax)]
if n_elements(yrange) ne 2 then yrange=[min(ay),max(ay)]

;; set window used by contour
if keyword_set(colorbar) then xmargin=[10,14] else xmargin=!x.margin
if n_elements(bar_offset) eq 0 then bar_offset=10 

if n_elements(title) eq 0 then title=''
if n_elements(subtitle) eq 0 then subtitle=''

;; save !p.multi(0):
pmulti=!p.multi(0)

contour,a,ax,ay,/nodata, $
  xtype=0,ytype=0, $
  xstyle=5,ystyle=5, $
  xrange=xrange,yrange=yrange, $
  xmargin=xmargin,title=' ', $
  xtitle=' ',ytitle=' ',subtitle=' ',_extra=e

pmulti1=!p.multi(0)

;; determine image indices corresponding to actual x and y ranges:
x0=value_to_index(ax,!x.crange(0))
x1=value_to_index(ax,!x.crange(1)) 
y0=value_to_index(ay,!y.crange(0)) 
y1=value_to_index(ay,!y.crange(1)) 

;; extract sub-image:
if x0 gt x1 then begin
    x2=x1
    x1=x0
    x0=x2
endif
if x0 eq x1 then begin
    if (x1 ne n_elements(ax)-1) then x1=x1+1 else x0=x1-1
endif
if y0 gt y1 then begin
    y2=y1
    y1=y0
    y0=y2
endif
if y0 eq y1 then begin
    if (y1 ne n_elements(ay)-1) then y1=y1+1 else y0=y1-1
endif
ax=ax(x0:x1)
ay=ay(y0:y1)
aa=a(x0:x1,y0:y1)
;; size of sub-image
sz = size(aa)			

;; Get size of window in device units. If position keyword has been
;; passed, then scale plot region accordingly:
if n_elements(position) ne 0 then begin
    px=[position(0),position(2)]*!d.x_vsize
    py=[position(1),position(3)]*!d.y_vsize
endif else begin
    px=!x.window*!d.x_vsize     
    py=!y.window*!d.y_vsize
endelse

swx=px(1)-px(0)                 ; Size in x in device units
swy=py(1)-py(0)                 ; Size in Y
six=float(sz(1))                ; Image sizes
siy=float(sz(2))
aspi=six/siy                    ; Image aspect ratio
aspw=swx/swy                    ; Window aspect ratio
f=aspi/aspw                     ; Ratio of aspect ratios

;; set parameters for bytscl:
if keyword_set(top) eq 0 then top=(!d.n_colors < 255)-1-32
if n_elements(min_value) eq 0 then min_value=min(a)
if n_elements(max_value) eq 0 then max_value=max(a)
if n_elements(bar_title) eq 0 then bar_title=' '

;; scale image
ba=bytscl(aa,top=top,min=min_value,max=max_value)+33

;; reverse image if necessary:
if ax(0) gt ax(1) then ba=reverse(ba,1)
if ay(0) gt ay(1) then ba=reverse(ba,2)

;; scalable pixels:
if (!d.flags and 1) ne 0 then begin 
    ;; retain aspect ratio?
    if keyword_set(aspect) then if (f ge 1.0) then swy=swy/f else swx=swx*f
    ;; display image:
    tv,ba,px(0),py(0),xsize=swx,ysize=swy,/device
endif else begin                
    ;; not scalable pixels:
    ;; scale window to image?
    if keyword_set(window_scale) then begin 
        ;; output image
        tv,ba,px(0),py(0)

        ;; set window size from image
        swx=six		
        swy=siy
    endif else begin		
        ;; scale window
        if keyword_set(aspect) then if (f ge 1.0) then swy=swy/f else swx=swx*f
        ;; have to resample image:
        tv,poly_2d(ba,[[0,0],[six/swx,0]],[[0,siy/swy],[0,0]],$
                   keyword_set(interp),swx,swy),px(0),py(0)
    endelse		
endelse        

if keyword_set(invert) then a=255-a

;; draw the colorbar:
if keyword_set(colorbar) then begin
    ;; default bar width is 10 pixels for non-scalable pixels,
    ;; or 2% of the plot width for scalable pixels:
    if keyword_set(bar_width) eq 0 then begin
        if (!d.flags and 1) then bar_width=.02*swx else bar_width=10 
    endif

    ;; make array to display colors:
    if (!d.flags and 1) then $
      zz=(bytarr(2)+1)#vector(32,32+top,swy) else $
      zz=(bytarr(bar_width)+1)#vector(32,32+top,swy)

    ;; define position vector for colorbar, relative to the main plot
    ;; window:

    pos=[(px(1)+bar_offset*!d.x_ch_size)/!d.x_vsize,py(0)/!d.y_vsize, $
         (px(1)+bar_offset*!d.x_ch_size+bar_width)/!d.x_vsize,(py(0)+swy)/!d.y_vsize]

    contour,[[0,0],[1,1]],/nodata, $
      xrange=[0,1.],xticks=1,xtickname=[' ',' '], $
      yrange=[min_value,max_value], $
      ytitle=bar_title, $
      xstyle=7,ystyle=9, $
      yticklen=-.1, $
      position=pos,/noerase

    tv,zz,pos(0)*!d.x_vsize,pos(1)*!d.y_vsize,xsize=bar_width,ysize=swy,/device 
endif

!p.multi(0)=pmulti
;; now draw contour levels (and titles). do this last, so that the
;; graphics system variables refer to this plot and not the colorbar:
contour,aa,ax,ay,/noerase, $
  pos=[px[0],py[0],px[0]+swx,py[0]+swy],/dev, $
  xtype=0,ytype=0, $
  xstyle=1,ystyle=1, $
  nodata=keyword_set(nocontour), $
  min_value=min_value,max_value=max_value, $
  xrange=xrange,yrange=yrange, $
  xmargin=xmargin,title=title,subtitle=subtitle, $
  _extra=e 

!p.multi(0)=pmulti1
return
end



