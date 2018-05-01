;+
; NAME:
;
;       DGTZ_IMAGE
;       
; PURPOSE:
;
;       A widget application to interactively measure distances in an
;       image, either between two points, two horizontal lines, or two
;       vertical lines.
;       
; CATEGORY:
;
;       Image analysis
;       
; CALLING SEQUENCE:
; 
;       DGTZ_IMAGE,IMAGE
;		
; INPUTS:
; 
;      IMAGE = 2-D array containing image.
;
; OUTPUTS:
;
;      The measured distances are listed on the widget, and
;      can also be saved to a text file (using MORE.)
;		
; KEYWORD PARAMETERS:
; 
;	UNITS - String specifying units.  Default is 'units'.
;	
; COMMON BLOCKS:
; 
;	dgtz_image, internal to this program.
;
; MODIFICATION HISTORY:
; 
;      David L. Windt, Bell Laboratories, May 1997
;      windt@bell-labs.com
;
;      Jul 1997: Corrected problem with widget labels
;-

;---procedure to save results-----------------------------------------------
pro dgtz_image_wt
common dgtz_image,dgtz_image,im,ref,mode, $
  n_dist,dist,unit,xprof,yprof,cursor,sz

file=pickfile(/write)
if file eq '' then return
more,file=file, $
  dist(0:n_dist-1,0), $
  dist(0:n_dist-1,1), $
  dist(0:n_dist-1,2), $
  dist(0:n_dist-1,3), $
  dist(0:n_dist-1,4), $
  dist(0:n_dist-1,5)

;; call dgtz_image_plot, because pickfile erases the windows!
dgtz_image_plot,/init
return
end

;-procedure to draw the image and plots--------------------------------------
;
pro dgtz_image_plot,initialize=initialize
common dgtz_image

sx=float(sz(0))
sxmax=sz(1)
sy=float(sz(2))
symax=sz(3)

;; display image:
if keyword_set(initialize) then begin
    widget_control,dgtz_image.iwindow,get_value=value & wset,value
    tvscl,im        
    tvcrs,sx/2,sy/2,/device     ; position cursor at center.
endif

; draw cross-hairs...
device,set_graphics=6           ; Use XOR.
widget_control,dgtz_image.iwindow,get_value=value & wset,value
plots,[cursor(0),cursor(0)],[0,sy],/device,color=6
plots,[0,sx],[cursor(1),cursor(1)],/device,color=6
device,set_graphics=3

;; plot profiles...
if keyword_set(initialize) then begin
    widget_control,dgtz_image.xwindow,get_value=value & wset,value
    plot,xprof(0,*),xprof(1,*),xmargin=[0,0],/xstyle,ymargin=[0,0],/ystyle
    device,set_graphics=6
    plots,[cursor(0)/sx,cursor(0)/sx],[0,1.],/normal,color=6
    device,set_graphics=3

    widget_control,dgtz_image.ywindow,get_value=value & wset,value
    plot,yprof(1,*),xprof(0,*),xmargin=[0,0],/xstyle,ymargin=[0,0],/ystyle
    device,set_graphics=6
    plots,[0,1.],[cursor(1)/sy,cursor(1)/sy],/normal,color=6
    device,set_graphics=3
endif

return
end

;---procedure to compute distances--------------------------------------------

pro dgtz_image_dist,event
common dgtz_image

if n_dist gt 0 then begin
    ;; initialize periods.
    dist(*,0)=0                
    if ref(1) eq 0 then ref(1)=ref(0)
                                
    ;; compute period in UNITS..
    for i=0,n_dist-1 do dist(i,0)=(ref(0)/ref(1))*dist(i,1)

    ;; create string array.
    a=strarr(n_dist)            
    for i=0,n_dist-1 do a(i)=strtrim(abs(fix(dist(i,1))),2)+ $
      ' pixels = '+strtrim(dist(i,0),2)+' '+unit
endif else a=''

;; label widget:
widget_control,dgtz_image.dist,set_value=a 

;; set Save sensitivity:
widget_control,dgtz_image.save,sensitive=n_dist gt 0

return
end

;---procedure to cursor events----------------------------------------

pro dgtz_image_curs,event
common dgtz_image

; update cursor.

widget_control,dgtz_image.cursor,set_value= $
  'X = '+strtrim(event.x,2)+', Y = '+ $
  strtrim(event.y,2)+', Intensity = '+ $
  strtrim(fix(im(event.x,event.y)),2) 

sx=float(sz(0))
sxmax=sz(1)
sy=float(sz(2))
symax=sz(3)

device,set_graphics=6			; Use XOR.

; draw cross-hairs...

widget_control,dgtz_image.iwindow,get_value=value & wset,value
plots,[cursor(0),cursor(0)],[0,sy],/device,color=6
plots,[event.x,event.x],[0,sy],/device,color=6

plots,[0,sx],[cursor(1),cursor(1)],/device,color=6
plots,[0,sx],[event.y,event.y],/device,color=6

; indicate current cursor position on profiles...

widget_control,dgtz_image.xwindow,get_value=value & wset,value
plots,[cursor(0)/sx,cursor(0)/sx],[0,1.],/normal,color=6
plots,[event.x/sx,event.x/sx],[0,1.],/normal,color=6

widget_control,dgtz_image.ywindow,get_value=value & wset,value
plots,[0,1.],[cursor(1)/sy,cursor(1)/sy],/normal,color=6
plots,[0,1.],[event.y/sy,event.y/sy],/normal,color=6

device,set_graphics=3			; back to default.

cursor=[event.x,event.y,cursor(2)]

return
end

;---procedure to handle widget events----------------------------------------

pro dgtz_image_ev,event
common dgtz_image

on_ioerror,finish

widget_control,event.id,get_uvalue=uvalue

if tag_names(event,/structure) eq 'WIDGET_DRAW' then begin
    ;; draw events:

    ;; call cursor event handler
    dgtz_image_curs,event       

    ;; determine what mouse button was pressed:
    if event.press gt 0 then begin
        case event.press of 
            ;; Left click: measure distance:
            1: begin            
                ;; has 1st point been digitized?
                if cursor(2) eq 1 then begin 
                    dist(n_dist,4)=event.x 
                    dist(n_dist,5)=event.y
                    x1=dist(n_dist,2)  
                    y1=dist(n_dist,3)
                    x2=dist(n_dist,4)  
                    y2=dist(n_dist,5)

                    case mode of
                        0: begin ; pts.
                            if (x2 eq x1) and (y2 eq y1) then goto,finish
                            dist(n_dist,1)=sqrt((x2-x1)^2+(y2-y1)^2)
                        end
                        1: begin ; h-lines
                            if (y2 eq y1) then return
                            dist(n_dist,1)=y2-y1
                        end
                        2: begin ; v-lines
                            if (x2 eq x1) then return
                            dist(n_dist,1)=x2-x1
                        end
                    endcase
                    
                    ;; increment counter and compute new distances:
                    n_dist=n_dist+1 
                    dgtz_image_dist			

                    ;; reset flag:
                    cursor(2)=0 
                    widget_control,dgtz_image.status, $
                      set_value='Click Left for Distances, Right for Conversion Reference Pixels.'
                endif else begin
                    ;; need second point:
                    dist(n_dist,2)=event.x 
                    dist(n_dist,3)=event.y
                    widget_control,dgtz_image.status, $
                      set_value='Digitize Second Point...'
                    ;; set flag:
                    cursor(2)=1 
                endelse
            end

            4: begin             $
              ;; Right click: measure reference distance:
                ;; has 1st point been digitized?
                if cursor(2) eq 1 then begin 
                    ref(4)=event.x 
                    ref(5)=event.y
                    x1=ref(2)  
                    y1=ref(3)
                    x2=ref(4)  
                    y2=ref(5)
                    case mode of
                        0: begin ; pts.
                            if (x2 eq x1) and (y2 eq y1) then goto,finish
                            ref(1)=sqrt((x2-x1)^2+(y2-y1)^2)
                        end
                        1: begin ; h-lines
                            if (y2 eq y1) then return
                            ref(1)=abs(y2-y1)
                        end
                        2: begin ; v-lines
                            if (x2 eq x1) then return
                            ref(1)=abs(x2-x1)
                        end
                    endcase
                    ;; update widgets...
                    widget_control,dgtz_image.ref_pix, $
                      set_value=strtrim(fix(ref(1)),2)+' pixels='
                    widget_control,dgtz_image.ref_con, $
                      set_value=strtrim(ref(0)/ref(1),2)+' '+strtrim(unit,2)+'/pixel'

                    ;; compute new distances:
                    dgtz_image_dist			

                    ;; reset flag:
                    cursor(2)=0 
                    widget_control,dgtz_image.status, $
                      set_value='Click Left for Distances, Right for Conversion Reference Pixels.'
                endif else begin				
                    ;; wait for second point:
                    ref(2)=event.x 
                    ref(3)=event.y
                    widget_control,dgtz_image.status, $
                      set_value='Digitize Second Reference Point...'
                    ;; set flag:
                    cursor(2)=1 
                endelse
            end
            else:;;
        endcase
    endif
    return
endif else begin
    ;; buttons:
    case uvalue of
        'save': dgtz_image_wt

        'xloadct': xloadct,group=dgtz_image.base
        
        'done': begin
            widget_control,event.top,/destroy
            return
        end

        'points': mode=0
        'hlines': mode=1
        'vlines': mode=2

        'clear': begin
            if event.value eq 0 then begin
                ;; clear last:
                n_dist=(n_dist-1) > 0
                dgtz_image_dist
            endif else begin
                ;; clear all:
                n_dist=0
                dgtz_image_dist
            endelse
        end

        'ref_val': begin	
            ;; get new value:
            widget_control,dgtz_image.ref_val,get_value=value
            if value(0) ne 0 then ref(0)=float(value(0))
            ;; update widgets:
            widget_control,dgtz_image.ref_val,set_value= $
              strtrim(ref(0),2)
            widget_control,dgtz_image.ref_con, $
              set_value=strtrim(ref(0)/ref(1),2)+ $
              ' '+strtrim(unit,2)+'/pixel'

            ;; compute new distances...
            dgtz_image_dist
        end
        else:;;
    endcase
endelse

finish:;;
return
end

;---procedure to create main widgets---------------------------------------

pro dgtz_image,image,units=units

common dgtz_image

if n_params() lt 1 then message,'usage: dgtz_image, image'

im=image                        ; image
sz=size(im)                     ; size of image
sx=float(sz(1))                 ; x size
sy=float(sz(2))                 ; y size
sxmax=640                       ; maximum image window size, beyond which
symax=512                       ; scroll bars are implemented.
sz=[sx,sxmax,sy,symax]

; reference: [distance, delta(pixels), pixelx0, pixely0, pixelx1, pixely1]
ref=[1.,1.,0,1./sqrt(2),0.,1./sqrt(2)]

if keyword_set(units) then unit=units else unit='units'
mode=0                          ; initialize measurement mode.
n_dist=0                        ; initialize number of periods defined
dist=fltarr(100,6)              ; array of distances.

; compute average profiles...

xprof=fltarr(2,sx)
yprof=fltarr(2,sy)
xprof(0,*)=findgen(sx)
yprof(0,*)=findgen(sy)
xprof(1,*)=im#replicate(1,sy)/sy
yprof(1,*)=replicate(1,sx)#im/sx

; define cursor position.

cursor=[sx/2,sy/2,0]

; create widget...

dgtz_image={base:0L, $
            save:0L, $          ; save button.
            status:0L, $        ; status label.
            cursor:0L, $        ; cursor position.
            iwindow:0L, $       ; image window.
            xwindow:0L, $       ; x profile.
            ywindow:0L, $       ; y profile.
            mode:0L, $          ; mode buttons.
            ref_val:0L, $       ; reference distance value.
            ref_pix:0L, $       ; reference distance pixels.
            ref_con:0L, $       ; reference conversion factor.
            clear:0L, $         ; clear buttons.
            dist:0L}            ; distances list

dgtz_image.base=widget_base(title='Image Distance Measurement',/col,mbar=mbar)

file_menu=widget_button(mbar,/menu,value='File')
dgtz_image.save=widget_button(file_menu,value='Save...',uvalue='save')
button=widget_button(file_menu,value='Adjust Color Table...',uvalue='xloadct')
button=widget_button(file_menu,value='Done',uvalue='done',/separator)

mode_menu=widget_button(mbar,/menu,value='Mode')
button=widget_button(mode_menu,value='Measure distance between two points',uvalue='points')
button=widget_button(mode_menu,value='Measure distance between two horizontal lines',uvalue='hlines')
button=widget_button(mode_menu,value='Measure distance between two vertical lines',uvalue='vlines')

dgtz_image.status=widget_label(dgtz_image.base,/align_left, $
                               value='Click Left for Distances, ' + $
                               'Right for Conversion Reference Pixels.')

dgtz_image.cursor=widget_label(dgtz_image.base,value=string(replicate(50B,50)),/align_left)

row=widget_base(dgtz_image.base,/row)
col=widget_base(row,/column)
dgtz_image.ywindow=widget_draw(col,xsize=100,ysize=(sy<symax))
col=widget_base(row,/column)
if (sx gt sxmax) or (sy gt symax) then $
  dgtz_image.iwindow=widget_draw(col,xsize=sx, $
                                 ysize=sy,/scroll, $
                                 x_scroll_size=(sx<sxmax), $
                                 y_scroll_size=(sy<symax), $
                                 /button,/motion) $
else dgtz_image.iwindow=widget_draw(col,xsize=sx, $
                                    ysize=sy,/button,/motion) 
dgtz_image.xwindow=widget_draw(col,xsize=(sx < sxmax),ysize=100)

base=widget_base(row,/column)
base=widget_base(base,/column,/frame)
label=widget_label(base,value='Distances:',/align_left)
dgtz_image.dist=widget_text(base,xsize=30,ysize=15,/scroll)
buttons=cw_bgroup(base,['Clear Last','Clear All'],uvalue='clear',/row)

row=widget_base(dgtz_image.base,/row)
dgtz_image.ref_val=cw_field(row,/string,xsize=10,ysize=1,/return_events, $
                            value=strtrim(ref(0),2),uvalue='ref_val', $
                            title='Conversion:')
label=widget_label(row,value=strtrim(unit,2)+' per')
dgtz_image.ref_pix=widget_label(row,value=strtrim(fix(sx>sy),2)+' pixels=')
dgtz_image.ref_con=widget_label(row,value=strtrim(ref(0)/ref(1),2)+' '+$
                                strtrim(unit,2)+'/pixel        ')

;; unmap widget
widget_control,dgtz_image.base,map=0

;; realize widget
widget_control,dgtz_image.base,/realize 

;; clear curser area:
widget_control,dgtz_image.cursor,set_value=''

;; reset conversion pixels:
widget_control,dgtz_image.ref_pix,set_value=strtrim(fix(ref(1)),2)+' pixels='

;; call dgtz_image_plot to paint the draw widgets:
dgtz_image_plot,/init

;; set Save sensitivity:
widget_control,dgtz_image.save,sensitive=0

;; map widget
widget_control,dgtz_image.base,map=1

xmanager,"dgtz_image",dgtz_image.base,event_handler="dgtz_image_ev" 

end
