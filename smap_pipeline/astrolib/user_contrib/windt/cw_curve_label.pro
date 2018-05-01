;+
; NAME:
;       CW_CURVE_LABEL
;
; PURPOSE:
;
;       A compound widget used to select the position for a curve
;       label; this widget is intended to be used in conjunction with
;       the CURVE_LABEL procedure in this directory, in that this
;       widget lets the user select a value from a slider from 0 to
;       one, corresponding to the XPOSITION keyword in CURVE_LABEL.
;      
; CATEGORY:
; 
;	Compound widgets.
;
; CALLING SEQUENCE:
; 
;	Result = CW_CURVE_LABEL(PARENT)
;
; INPUTS:
; 
;       PARENT - The ID of the parent widget.  
;
; KEYWORD PARAMETERS:
; 
;	UVALUE - Supplies the user value for the widget.
;
;       VALUE - Initial value for the widget: a floating point between
;               0 and 1, corresponding to the XPOSITION keyword in
;               CURVE_LABEL.
; 
;       TITLE - A title for the widget. 
;
;       FRAME - Set to draw a frame around the widget; ignored if
;               PARENT is present.
;
;       FORMAT - Format string for CW_FSLIDER (default is F5.3)
;
;       FONT - Fonts to use for labels and buttons.
;
;       DONE - Set this to add a Done button, in addition to the
;              standard Apply button.
; 
; OUTPUTS:
; 
;       The ID of the created widget is returned.
;
; PROCEDURE/EXAMPLE:
;
;         A slider widget is created in which the user can select a
;         position value.  By pressing the "Apply" button, an event is
;         returned, allowing the calling procedure to redraw the
;         curve label if desired.
;
;         This widget generates an event when the user presses the
;         Apply button or the Done button, if present. The EVENT.TAG
;         keyword will return either "APPLY" or "DONE" accordingly.
;
; MODIFICATION HISTORY:
; 
;       David L. Windt, Bell Labs, April 1997
;       windt@bell-labs.com
;-

;------------------------------------------------------------------
; event handler

function cw_curve_label_ev,event

parent=event.handler
stash=widget_info(parent,/child)

;; get state
widget_control,stash,get_uvalue=state,/no_copy

widget_control,event.id,get_uvalue=eventval 
case eventval of
    'apply': begin
        widget_control,state.apply,sensitive=0
        ret={CW_CURVE_LABEL_EVENT,ID:parent,TOP:event.top, $
             HANDLER:0L,TAG:"APPLY"}
    end
    'done': ret={CW_CURVE_LABEL_EVENT,ID:parent,TOP:event.top, $
                 HANDLER:0L,TAG:"DONE"}
    else: begin
        widget_control,state.apply,/sensitive
        ret=0
    end
endcase

;; restore state
widget_control,stash,set_uvalue=state,/no_copy
return, ret
end

;------------------------------------------------------------------
; set value procedure

pro cw_curve_label_set_value,id,value

on_error,2
;; retrieve the state, which contains the widget id's
widget_control,widget_info(id,/child),get_uvalue=state

;; set the slider value
widget_control,state.slider,set_value=value

return
end

;------------------------------------------------------------------
; get value function

function cw_curve_label_get_value,id
on_error,2

;; retrieve the state, which contains the widget id's
widget_control,widget_info(id,/child),get_uvalue=state

;; get the slider value
widget_control,state.slider,get_value=value

return,value
end

;------------------------------------------------------------------
; main function

function cw_curve_label,parent,uvalue=uvalue,frame=frame, $
           value=value,font=font,done=done,title=title

on_error,2

if n_params() ne 1 then  $
  message,'Must specify a parent for cw_curve_label.'

if not keyword_set(uvalue) then uvalue=0
if n_elements(font) eq 0 then font=''
if n_elements(format) eq 0 then format='(F5.2)'
if n_elements(title) eq 0 then title=''

; keep widget id's in state:
state={slider:0L,apply:0L}

base=widget_base(parent,uvalue=uvalue,frame=keyword_set(frame), $
                 event_func='cw_curve_label_ev', $
                 pro_set_value='cw_curve_label_set_value', $
                 func_get_value='cw_curve_label_get_value') 
base1=widget_base(base,/column)

;; can't set /edit for IDL-on-Mac, due to BUG:
state.slider=cw_fslider(base1,title=title, $
                        uvalue='slider', $ $
                        edit=!version.os ne 'MacOS', $
                        format=format, $
                        minimum=0.,maximum=1.)

bbase=widget_base(base1,/row,/align_center)
state.apply=widget_button(bbase,value='Apply',uvalue='apply',font=font)
widget_control,state.apply,sensitive=0 ; apply starts out insensitive

if keyword_set(done) then done=widget_button(bbase,value='Done', $
                                             uvalue='done',font=font)

;; stuff the state into first child
widget_control,widget_info(base,/child),set_uvalue=state

return,base
end
