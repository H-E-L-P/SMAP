;+
; NAME:
;
;          DGTZ_PLOT
;
; PURPOSE:
;
;          A widget application used to extract (X,Y) values from an
;          image of plot.  For example, you can use this program to
;          extract data from a published plot that you've scanned and
;          converted to an image array.
;
; CALLING SEQUENCE:
; 
;         DGTZ_PLOT,IMAGE,XRANGE,YRANGE
;
; INPUTS:
; 
;         IMAGE - 2D array containing the plot image.
;         
;         XRANGE - 2-element array specifying data range of X axis on
;                  plot image.
;                  
;         YRANGE - 2-element array specifying data range of Y axis on
;                  plot image.
;                               
; KEYWORD PARAMETERS:
; 
;         XTYPE - set if plot image has log x axis.
;
;         YTYPE - set if plot image has log x axis.
;
;         SXMAX - Visible size of draw widget along x direction, in pixels.
;                 Default=512.
;                 
;         SYMAX - Visible size of draw widget along y direction, in pixels.
;                 Default=512.
;
; OUTPUTS:
;
;         The digitized X,Y pairs are listed on a widget. You can
;         also save these data to a file (using MORE.)
;         
; COMMON BLOCKS:
; 
;         DGTZ_PLOT internal to this procedure.
;         
; PROCEDURE:
; 
;         The image of the plot is displayed on a widget, and the user
;         can digitize points which are converted to X,Y values.  The
;         first step,however, is generally to calibrate the X and Y
;         axes; the endpoints of the specified axis are digitized,
;         after pressing the Calibrate X Axis or Calibrate Y Axis
;         button.
;
;         
; MODIFICATION HISTORY:
; 
;         David L. Windt, Bell Laboratories, May, 1997
;
;         September, 1998 - Addex SXMAX and SYMAX keywords.
;
;         windt@bell-labs.com
;-

;-procedure to save results of statistical analysis on topographic surface-
pro dgtz_plot_wt
common dgtz_plot,x,y,dgtz_plot,im,xra,yra,xty,yty,pts,n_pts

file=pickfile(/write)
if file eq '' then return
more,file=file,x,y

;; pickfile messes up the draw widget:
widget_control,dgtz_plot.iwindow,get_value=value & wset,value
tvscl,im

return
end

;-procedure to plot profile---------------------------------------------

pro dgtz_plot_pts

common dgtz_plot

;; realize profile widget.
if (n_pts gt 0) and (xregistered('dgtz_plot_pts') eq 0) then  $
  widget_control,/realize,dgtz_plot.base2,group=dgtz_plot.base1 


widget_control,dgtz_plot.pwindow,get_value=value & wset,value

if n_pts eq 0 then begin
    erase
    a=''
    goto,finish
endif

x=fltarr(n_pts)
y=x

if xty eq 0 then  $
  for i=0,n_pts-1 do $
  x(i)=(pts(i,0)-xra(2))*(xra(1)-xra(0))/abs(xra(3)-xra(2))+xra(0) $
else for i=0,n_pts-1 do $
  x(i)=exp((pts(i,0)-xra(2))*(alog(xra(1)/xra(0)))/abs(xra(3)-xra(2))+ $
           alog(xra(0)))

if yty eq 0 then for i=0,n_pts-1 do $
  y(i)=(pts(i,1)-yra(2))*(yra(1)-yra(0))/abs(yra(3)-yra(2))+yra(0) $
else for i=0,n_pts-1 do $
  y(i)=exp((pts(i,1)-yra(2))*(alog(yra(1)/yra(0)))/abs(yra(3)-yra(2))+ $
           alog(yra(0)))

; plot points
plot,x,y,xtype=xty,ytype=yty,xtitle='X Axis',ytitle='Y Axis',psym=2

a=strarr(n_pts)
for i=0,n_pts-1 do a(i)=strtrim(x(i),2)+', '+strtrim(y(i),2)

finish:;;

;; update widgets:
widget_control,dgtz_plot.npts,set_value=strtrim(n_pts,2)+' points'
widget_control,dgtz_plot.pts,set_value=a 
widget_control,dgtz_plot.save,sensitive=n_pts gt 0

return
end

;---procedure to handle widget events----------------------------------------

pro dgtz_plot_ev,event

common dgtz_plot

widget_control,event.id,get_uvalue=uvalue

;; tracking:
if tag_names(event,/structure) eq 'WIDGET_DRAW' then begin
    widget_control,dgtz_plot.cursor,set_value= $
      'X = '+strtrim(event.x,2)+', Y = '+strtrim(event.y,2)
    if event.press gt 0 then begin
        pts(n_pts,0)=event.x
        pts(n_pts,1)=event.y
        n_pts=n_pts+1
    endif else return
endif else begin
    case uvalue of
        'xloadct': xloadct,group=dgtz_plot.base1

        'xcal': begin
            widget_control,dgtz_plot.iwindow,get_value=value & wset,value
            widget_control,dgtz_plot.status,set_value='Digitize Start of X Axis:'
            cursor,xx,yy,/device
            xra(2)=xx
            wait,.25
            widget_control,dgtz_plot.status,set_value='Digitize End of X Axis:'
            cursor,xx,yy,/device
            xra(3)=xx
            if xra(3) eq xra(2) then xra(3)=xra(2)+1
            widget_control,dgtz_plot.xra, $
              set_value='X Axis: '+strtrim(xra(0),2)+' to '+ $
              strtrim(xra(1),2)+'; '+strtrim(abs(xra(2)-xra(3)),2)+' pixels'
            widget_control,dgtz_plot.status,set_value='Click on plot to digitize X,Y points.'
        end

        'ycal': begin
            widget_control,dgtz_plot.iwindow,get_value=value & wset,value
            widget_control,dgtz_plot.status,set_value='Digitize Start of Y Axis:'
            cursor,xx,yy,/device
            yra(2)=yy
            wait,.25
            widget_control,dgtz_plot.status,set_value='Digitize End of Y Axis:'
            cursor,xx,yy,/device
            yra(3)=yy
            if yra(3) eq yra(2) then yra(3)=yra(2)+1
            widget_control,dgtz_plot.yra, $
              set_value='Y Axis: '+strtrim(yra(0),2)+' to '+ $
              strtrim(yra(1),2)+'; '+strtrim(abs(yra(2)-yra(3)),2)+' pixels'
            widget_control,dgtz_plot.status,set_value='Click on plot to digitize X,Y points.'
        end

        'clear': begin
            if event.value eq 0 then n_pts=((n_pts-1)>0) <500 else n_pts=0
            if n_pts eq 500 then wm=widget_message('Only 500 points allowed.')
        end

        'save': dgtz_plot_wt

        'done': begin
            widget_control,dgtz_plot.base1,/destroy
            return
        end

        else:;;
    endcase
endelse
;; update points widget:
dgtz_plot_pts           

return
end

;-procedure to create main widgets---------------------------------------

pro dgtz_plot,image,xrange,yrange,xtype=xtype,ytype=ytype, $
        sxmax=sxmax,symax=symax

common dgtz_plot

; definitions...
if n_params() lt 3 then message, $
  'usage: DGTZ_PLOT, IMAGE, XRANGE, YRANGE'
if (n_elements(xrange) ne 2) or (n_elements(yrange) ne 2) then $
  message,'XRANGE and YRANGE must be 2-element arrays.'

im=image                        ; plot image.
sz=size(im)                     ; size of image.
sx=sz(1)                        ; x size
sy=sz(2)                        ; y size
if n_elements(sxmax) eq 0 then sxmax=512
if n_elements(symax) eq 0 then symax=512
xra=[xrange,0,sx]
yra=[yrange,0,sy]
xty=keyword_set(xtype)
yty=keyword_set(ytype)

pts=fltarr(500,2)               ; array of pts.
n_pts=0                         ; initialize number of pts defined

; create widget...

dgtz_plot={base1:0L, $
           status:0L, $
           cursor:0L, $
           iwindow:0L, $
           x:0L, $
           y:0L, $
           xra:0L, $
           yra:0L, $
           base2:0L, $
           save:0L, $
           pwindow:0L, $
           pts:0L, $
           npts:0L, $
           clear:0L}

dgtz_plot.base1=widget_base(title='Plot Image',/column,mbar=mbar)

file_menu=widget_button(mbar,/menu,value='File')
button=widget_button(file_menu,value='Adjust Color Table...',uvalue='xloadct')
button=widget_button(file_menu,value='Done',uvalue='done',/separator)

dgtz_plot.status=widget_label(dgtz_plot.base1,/align_left, $
                              value='Click on plot to digitize X,Y points.')

dgtz_plot.cursor=widget_label(dgtz_plot.base1, $
                              value=string(replicate(50B,20)),/align_left)

dgtz_plot.iwindow=widget_draw(dgtz_plot.base1,xsize=sx,ysize=sy, $
                              /button,/motion,/scroll,x_scroll_size=sx<sxmax, $
                              y_scroll_size=sy<symax)

row=widget_base(dgtz_plot.base1,/row)
button=widget_button(row,value='Calibrate',uvalue='xcal')
dgtz_plot.xra=widget_label(row,value='X Axis: '+strtrim(xra(0),2)+' to '+ $
                           strtrim(xra(1),2)+'; '+strtrim(abs(xra(2)-xra(3)),2)+' pixels')

row=widget_base(dgtz_plot.base1,/row)
button=widget_button(row,value='Calibrate',uvalue='ycal')
dgtz_plot.yra=widget_label(row,value='Y Axis: '+strtrim(yra(0),2)+' to '+ $
                           strtrim(yra(1),2)+'; '+strtrim(abs(yra(2)-yra(3)),2)+' pixels')

;; unmap widget:
widget_control,dgtz_plot.base1,map=0

;; realize widget:
widget_control,dgtz_plot.base1,/realize

;; display image:
tvscl,im                        

;; clear cursor area:
widget_control,dgtz_plot.cursor,set_value=''

;; Create plot widget, but don't realize it yet:
dgtz_plot.base2=widget_base(title='Digitized Points',/row,mbar=mbar)

file_menu=widget_button(mbar,/menu,value='File')
dgtz_plot.save=widget_button(file_menu,value='Save...',uvalue='save')
widget_control,dgtz_plot.save,sensitive=0

dgtz_plot.pwindow=widget_draw(dgtz_plot.base2,xsize=512,ysize=384)

colbase=widget_base(dgtz_plot.base2,/column)

dgtz_plot.npts=widget_label(colbase,value='500 points',/align_left)
dgtz_plot.pts=widget_text(colbase,xsize=20,ysize=15,/scroll)
dgtz_plot.clear=cw_bgroup(colbase,['Clear Last','Clear All'],uvalue='clear',/row)

;; map widget:
widget_control,dgtz_plot.base1,map=1

;; manage widgets
xmanager,"dgtz_plot",dgtz_plot.base2,event_handler="dgtz_plot_ev",/just_reg
xmanager,"dgtz_plot",dgtz_plot.base1,event_handler="dgtz_plot_ev" 

end
