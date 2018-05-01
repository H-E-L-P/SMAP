;+
; NAME:
;       CW_DRAWSIZE
;
; PURPOSE:
; 
;       A compound widget used to change the size of an existing
;       draw widget.  The widget contains fields for the X and Y
;       draw size (in pixels), an Apply button, and optionally a
;       Done button.
;      
; CATEGORY:
; 
;	Compound widgets.
;
; CALLING SEQUENCE:
; 
;	Result = CW_DRAWSIZE(PARENT,DRAW_WIDGET)
;
; INPUTS:
; 
;       PARENT - The ID of the parent widget.  
;
;       DRAW_WIDGET - The id of the draw widget being resized.
; 
; KEYWORD PARAMETERS:
; 
;	UVALUE - Supplies the user value for the widget.
; 
;       FRAME - set to draw a frame around the widget; ignored if
;               PARENT is present.
;
;       ROW - set to place the two window size fields (x,y) in a row.
; 
;       COLUMN - set to place the two window size fields (x,y) in a column.
;
;       FONT - fonts to use for labels and buttons.
;
;       DONE - set this to add a Done button, in addition to the standard
;              Apply button.
;
;       NO_RETURN - The default behavior is that the user must press
;                   <return> after entering new values.  Set this
;                   keyword so that an event is returned even if the
;                   user just changes a value and then moves the
;                   cursor outside of the text entry area.
;
; OUTPUTS:
; 
;       The ID of the created widget is returned.
;
; PROCEDURE/EXAMPLE:
;
;         A widget is created in which the user can specify the X and Y
;         draw widget size in pixels.   By pressing the "Apply" button,
;         the draw widget is resized, and an event is returned, allowing
;         the calling procedure to repaint the window if desired.
;
;         This widget generates an event when the user presses the
;         Apply button or the Done button, if present. The EVENT.TAG
;         keyword will return either "APPLY" or "DONE" accordingly.
;
; MODIFICATION HISTORY:
; 
;       David L. Windt, Bell Labs, March 1997
;       windt@bell-labs.com
;
;       DLW, June 1997, Added NO_RETURN keyword.
;
;       DLW, Sep 1997, Fixed bug that caused initial values of X and Y
;       pixel sizes to be displayed as floating point values rather
;       than integers.
;       
;-
function cw_drawsize_ev,event

;; get parent:
parent=event.handler

;; get child of parent, where state is stored in uvalue:
child=widget_info(parent,/child)

;; get state - uvalue of parents child:
widget_control,child,get_uvalue=state,/no_copy

;; get uservalue of this event:
widget_control,event.id,get_uvalue=eventval 

;; only return an event if the window size actually changes.
ret=0

case eventval of

    'x': begin
        widget_control,state.x,get_value=x_size
        ;; make sure window size is valid
        widget_control,state.x,set_value=x_size > 1
        widget_control,state.apply_button,/sensitive
    end

    'y': begin
        widget_control,state.y,get_value=y_size
        ;; make sure window size is valid
        widget_control,state.y,set_value=y_size > 1
        widget_control,state.apply_button,/sensitive
    end

    'apply': begin
        widget_control,state.x,get_value=x_size
        widget_control,state.y,get_value=y_size
        widget_control,state.draw_widget, $
          draw_xsize=x_size,draw_ysize=y_size
        widget_control,state.apply_button,sensitive=0
        ;; return event structure
        ret={ CW_DRAWSIZE_EVENT,  $
              ID: parent, TOP: event.top, HANDLER: 0L, TAG:"APPLY"}
    end

    'done': ret={ CW_DRAWSIZE_EVENT,  $
              ID: parent, TOP: event.top, HANDLER: 0L, TAG:"DONE"}


endcase
;; restore state - uvalue of parents child:
widget_control,child,set_uvalue=state,/no_copy
return,ret
end

function cw_drawsize,parent,draw_widget, $
           uvalue=uvalue,frame=frame,row=row,column=column,font=font, $
           done=done,no_return=no_return

if n_params() ne 2 then  $
  message,'Must specify a parent and a draw widget for cw_drawsize'

; make sure current device actually  supports windows
if (!d.flags and 256) eq 0 then  $
  message,'Current graphics device does not support windows.'

; set uvalue if not passed:
if n_elements(uvalue) ne 1 then uvalue=0

; get size of draw widget
geom=widget_info(draw_widget,/geometry) 
x_size=fix(geom.draw_xsize)
y_size=fix(geom.draw_ysize)

base=widget_base(parent,uvalue=uvalue,frame=keyword_set(frame), $
                 /column,event_func='cw_drawsize_ev') 

base1=widget_base(base,row=keyword_set(row),column=keyword_set(column))

x_base=widget_base(base1,/row)
x=cw_field(x_base,title='X Size:',/integer,/return_events, $
           value=x_size,uvalue='x',xsize=4,font=font, $
           no_return=keyword_set(no_return))
x_units=widget_label(x_base,value='pixels',font=font) 

y_base=widget_base(base1,/row)
y=cw_field(y_base,title='Y Size:',/integer,/return_events, $
           value=y_size,uvalue='y',xsize=4,font=font, $
           no_return=keyword_set(no_return))
y_units=widget_label(y_base,value='pixels',font=font) 

bbase=widget_base(base,/row,/align_center)
apply_button=widget_button(bbase,value='Apply',uvalue='apply',font=font) 
widget_control,apply_button,sensitive=0 ; apply button starts out insensitive 
if keyword_set(done) then done=widget_button(bbase,value='Done', $
                                             uvalue='done',font=font)

;; stuff the state into first child widget of base (i.e., base1)
state={draw_widget:draw_widget,x:x,y:y,apply_button:apply_button}
widget_control,widget_info(base,/child),set_uvalue=state

return,base
end
