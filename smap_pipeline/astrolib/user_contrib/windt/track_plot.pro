;+
; NAME:
; 
;       TRACK_PLOT
;		
; PURPOSE:
;
;       A procedure to plot X vs Y in a widget, track the cursor
;       position, and interactively display the Y(X) value.
;
; CALLING SEQUENCE:
;
;       TRACK_PLOT,X,Y
;
; INPUTS:
;
;       X, Y -  1-D arrays
;
; KEYWORD PARAMETERS:
;
;       WXSIZE - Draw widget X size, in pixels. (Default=640)
;
;       WYSIZE - Draw widget Y size, in pixels. (Default=480)
;
;       CROSSHAIR - Set this to display a crosshair at the current
;       Y(X) value.
;
;       plus all valid IDL PLOT keywords.  
;
; RESTRICTIONS:
; 
;       Requires widgets.  Requires use of the VALUE_TO_INDEX function
;       in the windt library.
;
; EXAMPLE:
;
;      Create some X,Y data and plot it using TRACK_PLOT:
; 
;      X=VECTOR(0.,100.,256)
;      Y=SIN(X/5.)*EXP(-X/20.)
;      TRACK_PLOT,X,Y
;
; MODIFICATION HISTORY:
;
;      D. L. Windt, Bell Laboratories, August 1997
;      windt@bell-labs.com
;
;      March, 1998 - Added crosshair display option and CROSSHAIR
;                    keyword.
;      
;-

pro track_plot_event,event
; 
; procedure event handler
;

;; return on errors:
on_error,2

;; get event uvalue:
widget_control,event.id,get_uvalue=uvalue

;; deal with event:
case uvalue of
    'done': begin
        ;; destroy widget
        widget_control,event.top,/destroy
    end
    'draw': begin
        ;; extract variables from base:
        widget_control,event.top,get_uvalue=s,/no_copy

        ;; convert to data coordinates:
        cc=convert_coord(event.x,event.y,/device,/to_data)

        ;; convert from wavelength/energy value to array index:
        i=value_to_index(s.x,cc(0))
        x=strtrim(s.x(i),2)
        y=strtrim(s.y(i),2)
        
        ;; display x,y values:
        widget_control,s.label,set_value='y('+x+') = '+y


        if s.crosshair then begin
            if !x.type then xcrange=10^!x.crange else xcrange=!x.crange
            if !y.type then ycrange=10^!y.crange else ycrange=!y.crange

            ;; draw new crosshair, and erase old ones:
            device,set_graphics=6 ; use XOR

            if s.nolast eq 0 then $
              plots,[s.last_x,s.last_x],ycrange
            plots,[x,x],ycrange

            if s.nolast eq 0 then $
              plots,xcrange,[s.last_y,s.last_y]
            plots,xcrange,[y,y]

            device,set_graphics=3 ; back to default.

            ;; set last x and y:
            s.last_x=x
            s.last_y=y
            s.nolast=0
        endif

        ;; restore variables to base:
        widget_control,event.top,set_uvalue=s,/no_copy
    end
endcase

return
end

pro track_plot,x,y,_extra=_extra,wxsize=wxsize,wysize=wysize, $
         crosshair=crosshair

;; return on errors:
on_error,2

;; check for parameters:
if n_params() ne 2 then message,'usage: track_plot,x,y'

;; define draw widget dimensions:
if n_elements(wxsize) eq 0 then wxsize=640
if n_elements(wysize) eq 0 then wysize=480

;; make widget:
base=widget_base(title='Track Plot',/column)
label=widget_label(base,value=string(replicate(50B,60)),/align_left)
draw=widget_draw(base,xsize=wxsize,ysize=wysize,uvalue='draw',/motion_events)
button=widget_button(base,value='Done',uvalue='done',/align_center)

;; save variables in base uvalue:
widget_control,base,set_uvalue={x:x,y:y,label:label, $
                                last_x:0.,last_y:0., $
                                nolast:1,crosshair:keyword_set(crosshair)}

;; unmap widget:
widget_control,base,map=0

;; realize widget:
widget_control,base,/realize

;; draw plot:
plot,x,y,_extra=_extra

;; clear label:
widget_control,label,set_value=''

;; map widget:
widget_control,base,map=1

;; manage widget
xmanager,'track_plot',base

return
end

