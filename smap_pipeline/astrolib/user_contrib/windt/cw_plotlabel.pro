;+
; NAME:
; 
;       CW_PLOTLABEL
;
; PURPOSE:
;
;       A compound widget used to select the position for a plot label
;       or legend.  This widget is intended to be used in conjunction
;       with the PLOT_TEXT or LEGEND procedures in this directory, in
;       that this widget lets the user select one of 13 pre-defined
;       positions corresponding to the POSITION keyword in PLOT_TEXT
;       and LEGEND.
;      
; CATEGORY:
; 
;	Compound widgets.
;
; CALLING SEQUENCE:
; 
;	Result = CW_PLOTLABEL(PARENT)
;
; INPUTS:
; 
;       PARENT - The ID of the parent widget.  
;
; KEYWORD PARAMETERS:
; 
;	UVALUE - Supplies the user value for the widget.
;
;       VALUE - initial value for the widget: an integer between 0 and
;               12, corresponding to the POSITION keyword in plot_text
;               or legend.
; 
;       TITLE - a title for the widget. 
;
;       FRAME - set to draw a frame around the widget; ignored if
;               PARENT is present.
;
;       FONT - fonts to use for labels and buttons.
;
;       DONE - set this to add a Done button, in addition to the standard
;              Apply button.
; 
;       NO_BELOW - set this to inhibit drawing the three buttons that
;                  correspond to label positions below the plot, i.e.,
;                  position values of 1, 2 and 3.
;
; OUTPUTS:
; 
;       The ID of the created widget is returned.
;
; PROCEDURE/EXAMPLE:
;
;         A widget is created in which the user can select one of 13
;         position values.  By pressing the "Apply" button, an event
;         is returned, allowing the calling procedure to redraw the
;         plot label or legend if desired.
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

function cw_plotlabel_ev,event

parent=event.handler
stash=widget_info(parent,/child)

;; get state
widget_control,stash,get_uvalue=state,/no_copy

widget_control,event.id,get_uvalue=eventval 
case eventval of
    'apply': begin
        widget_control,state.apply,sensitive=0
        ret={CW_PLOTLABEL_EVENT,ID:parent,TOP:event.top, $
             HANDLER:0L,TAG:"APPLY"}
    end
    'done': ret={CW_PLOTLABEL_EVENT,ID:parent,TOP:event.top, $
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

pro cw_plotlabel_set_value,id,value

on_error,2
;; retrieve the state, which contains the widget id's
widget_control,widget_info(id,/child),get_uvalue=state

case value of 
    0: value=12
    1: value=9
    2: value=10
    3: value=11
    4: value=6
    5: value=7
    6: value=8
    7: value=3
    8: value=4
    9: value=5
    10: value=0
    11: value=1
    12: value=2
endcase

if (value eq 12) and (state.no_below eq 1) then value=9

;; set the button value
widget_control,state.buttons,set_value=value

return
end

;------------------------------------------------------------------
; get value function

function cw_plotlabel_get_value,id
on_error,2

;; retrieve the state, which contains the widget id's
widget_control,widget_info(id,/child),get_uvalue=state

;; get the button value
widget_control,state.buttons,get_value=value

case value of 
    0: value=10
    1: value=11
    2: value=12
    3: value=7
    4: value=8
    5: value=9
    6: value=4
    7: value=5
    8: value=6
    9: value=1
    10: value=2
    11: value=3
    12: value=0
endcase

if (value eq 1) and (state.no_below eq 1) then value=0

return,value
end

;------------------------------------------------------------------
; main function

function cw_plotlabel,parent,uvalue=uvalue,frame=frame, $
           value=value,font=font,done=done,title=title, $
           no_below=no_below

on_error,2

if n_params() ne 1 then  $
  message,'Must specify a parent for cw_plotlabel.'

if not keyword_set(uvalue) then uvalue=0
if n_elements(font) eq 0 then font=''
if n_elements(title) eq 0 then title=''

; keep widget id's in state:
state={buttons:0L,apply:0L,no_below:keyword_set(no_below)}

base=widget_base(parent,uvalue=uvalue,frame=keyword_set(frame), $
                 event_func='cw_plotlabel_ev', $
                 pro_set_value='cw_plotlabel_set_value', $
                 func_get_value='cw_plotlabel_get_value') 
base1=widget_base(base,/column)

; create the position buttons:
if keyword_set(no_below) then $
  name=['Upper Left','Upper Center','Upper Right', $
        'Middle Left','Middle Center','Middle Right', $
        'Lower Left','Lower Center','Lower Right', $
        'None'] else $
  name=['Upper Left','Upper Center','Upper Right', $
        'Middle Left','Middle Center','Middle Right', $
        'Lower Left','Lower Center','Lower Right', $
        'Below Left','Below Center','Below Right', $
        'None']

if n_elements(value) eq 0 then begin
    state.buttons=cw_bgroup(base1,name, $
                            row=5-keyword_set(no_below), $
                            uvalue='buttons', $
                            label_top=title, $
                            font=font,/frame, $
                            /exclusive,/no_release) 
endif else begin
    case value of 
        0: value=12
        1: value=9
        2: value=10
        3: value=11
        4: value=6
        5: value=7
        6: value=8
        7: value=3
        8: value=4
        9: value=5
        10: value=0
        11: value=1
        12: value=2
    endcase
    if (value eq 12) and (state.no_below eq 1) then value=9
    state.buttons=cw_bgroup(base1,row=4-keyword_set(no_below),name, $
                            set_value=value, $
                            uvalue='buttons', $
                            label_top=title, $
                            font=font,/frame, $
                            /exclusive,/no_release) 
endelse

bbase=widget_base(base1,/row,/align_center)
state.apply=widget_button(bbase,value='Apply',uvalue='apply',font=font)
widget_control,state.apply,sensitive=0 ; apply starts out insensitive
if keyword_set(done) then done=widget_button(bbase,value='Done', $
                                             uvalue='done',font=font)

;; stuff the state into first child
widget_control,widget_info(base,/child),set_uvalue=state

return,base
end
