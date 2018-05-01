;+
; NAME:
; 
;       CW_PLOTAXIS
;
; PURPOSE:
; 
;       A compound widget used to change the type, range and style
;       (bit 0 only) values of plot axis structure variable.
;      
; CATEGORY:
; 
;	Compound widgets.
;
; CALLING SEQUENCE:
; 
;	Result = CW_PLOTAXIS(PARENT,LABEL)
;
; INPUTS:
; 
;       PARENT - The ID of the parent widget.  
;
;       LABEL - a label to be drawn to the left of the widget.
;
; OPTIONAL KEYWORD PARAMETERS:
; 
;	UVALUE - Supplies the user value for the widget.
; 
;       FRAME - set to draw a frame around the widget.
;
;       VALUE - a 4-element array of the form [type,min,max,style].
;               type and min,max correspond to the !axis.type and
;               !axis.range variables, and style is bit 0 of
;               !axis.style.
;
;       FONT - font keyword for labels etc.
;       
;       ROW - set to orient the subwidgets in a row (default.)
;
;       COLUMN - set to orient the subwidgets in a column.
;
;       NO_RETURN - The default behavior is that the user must press
;                   <return> after entering new values.  Set this
;                   keyword so that new values are accepted even if
;                   the user just changes a value and then moves the
;                   cursor outside of the text entry area.
;
; OUTPUTS:
; 
;       The ID of the created widget is returned.
;
; PROCEDURE/EXAMPLE:
;
;       The idea is that one or more instances of this cw would be
;       used in a widget intended to allow the user to interactively
;       adjust the settings for a plot.  For instance, you might have
;       a menu item such as Plot Options->Scaling, which would create
;       a popup widget containing CW_PLOTAXIS subwidgets for the X and
;       Y plot variables.  When the user makes changes to the Type,
;       Range, and Style values, the popup widget event handler would
;       re-draw the plot accordingly.
;
;       The widget returns events when any of it's children generate
;       events.  The returned event has the form
;       {CW_PLOTAXIS_EVENT,ID:id,TOP:top,HANDLER:handler,TAG:tag}
;       where TAG indicates which child widget generated the event:
;       possible values for EVENT.TAG are TYPE, MIN, MAX, and STYLE.
;
;       NOTE: you probably really want to use the CW_PLOTAXES (plural)
;       widget, not this one.
;
; MODIFICATION HISTORY:
; 
;       David L. Windt, Bell Labs, March 1997
;       windt@bell-labs.com
;
;       DLW, June 1997, Added NO_RETURN keyword.
;
;       DLW, November 1997, Text fields for Range values are now
;       updated when the user makes a change; the specified values are
;       converted to floating point.
;       
;-

;------------------------------------------------------------------
; event handler

function cw_plotaxis_ev,event

widget_control,event.id,get_uvalue=eventval 

;; set the widget values of text widgets:
if (eventval eq 'min') or (eventval eq 'max') then begin
    widget_control,event.id,get_value=val
    widget_control,event.id,set_value=float(val(0))
endif

return,{CW_PLOTAXIS_EVENT,ID:event.handler,TOP:event.top, $
        HANDLER:0L,TAG:eventval}
end

;------------------------------------------------------------------
; set value procedure

pro cw_plotaxis_set_value,id,value

on_error,2
;; retrieve the state, which contains the widget id's
widget_control,widget_info(id,/child),get_uvalue=state

;; set the widget values:
widget_control,state.type,set_droplist_select=value(0)
widget_control,state.min,set_value=value(1)
widget_control,state.max,set_value=value(2)
widget_control,state.style,set_droplist_select=value(3)

return
end

;------------------------------------------------------------------
; get value function

function cw_plotaxis_get_value,id
on_error,2

;; retrieve the state, which contains the widget id's
widget_control,widget_info(id,/child),get_uvalue=state

;; get the widget values:

type=widget_info(state.type,/droplist_select)
widget_control,state.min,get_value=min
widget_control,state.max,get_value=max
style=widget_info(state.style,/droplist_select)

return,[type,min,max,style]
end

;------------------------------------------------------------------
; main function

function cw_plotaxis,parent,label,uvalue=uvalue,frame=frame,value=value, $
           font=font,row=row,column=column,no_return=no_return

on_error,2

if n_params() eq 0 then message,'Must specify a parent for cw_plotaxis.'
if not keyword_set(uvalue) then uvalue=0
if (keyword_set(column) eq 0) then row=1

;; check that value is set properly
if n_elements(value) ne 4 then value=[0,0,0,0] 

; keep widget id's in state:
state={type:0L, min:0L, max:0L, style:0L}

if n_elements(font) eq 0 then font=''

base=widget_base(parent,uvalue=uvalue,frame=keyword_set(frame), $
                 event_func='cw_plotaxis_ev', $
                 pro_set_value='cw_plotaxis_set_value', $
                 func_get_value='cw_plotaxis_get_value') 
base1=widget_base(base,row=keyword_set(row),column=keyword_set(column))

if n_elements(label) eq 1 then $
  label=widget_label(base1,value=label,font=font) 

state.type=widget_droplist(base1,value=['Linear','Log'], $
                           uvalue='type',font=font) 
widget_control,state.type,set_droplist_select=value(0)

state.min=cw_field(base1,title='Min:',/float,/return_events, $
                   value=value(1),uvalue='min',xsize=10, $
                   font=font,no_return=keyword_set(no_return))
state.max=cw_field(base1,title='Max:',/float,/return_events, $
                   value=value(2),uvalue='max',xsize=10, $
                   font=font,no_return=keyword_set(no_return))

state.style=widget_droplist(base1,value=["Don't Force Exact Range", $
                                         "Force Exact Range"], $
                            uvalue='style',font=font) 
widget_control,state.style,set_droplist_select=value(3)

;; stuff the state into first child (i.e., base1)
widget_control,widget_info(base,/child),set_uvalue=state

return,base
end
