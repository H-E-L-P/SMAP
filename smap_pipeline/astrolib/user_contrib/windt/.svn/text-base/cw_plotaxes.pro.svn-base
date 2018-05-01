;+
; NAME:
;       CW_PLOTAXES
;
; PURPOSE:
; 
;       A compound widget used to change the type, range and style
;       (bit 0) values of one or more plot axis structure variable.  A
;       CW_PLOTAXIS (single axis) widget is created for each element
;       of the LABELS input parameter. The widget also includes an
;       Apply button, and (optionally) a Done button.
;      
; CATEGORY:
; 
;	Compound widgets.
;
; CALLING SEQUENCE:
; 
;	Result = CW_PLOTAXES(PARENT,LABELS)
;
; INPUTS:
; 
;       PARENT - The ID of the parent widget.  
;
;       LABELS - A string array of labels to be drawn to the left (or
;                top) of each of the CW_PLOTAXIS widgets.
;
; OPTIONAL KEYWORD PARAMETERS:
; 
;	UVALUE - Supplies the user value for the widget.
; 
;       FRAME - Set to draw a frame around the widget.
;
;       VALUE - An (n,4) array of initial values, where
;               n = n_elements(LABELS), and each row has
;               the form VALUE(i,*)=[type,min,max,style]
;
;       FONT - Font keyword for labels etc.
;
;       ROW - Set to create a row of column-oriented CW_PLOTAXIS widgets.
;
;       COLUMN - Set to create a column of row-oriented CW_PLOTAXIS
;                widgets. (default)
; 
;       DONE - Set this to add a Done button, in addition to the standard
;              Apply button.
;
;       AXIS_IDS - An array of widget id's for the individual
;                  CW_PLOTAXIS widgets.
;
;       X_SCROLL_SIZE, Y_SCROLL_SIZE - if these values are non-zero,
;                                      then the base widget which
;                                      holds the CW_PLOTAXIS widgets
;                                      will include scroll bars.
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
;         The idea is that this cw would be used in a widget intended
;         to allow the user to interactively adjust the settings for a
;         plot.  For instance, you might have a menu item such as Plot
;         Options->Scaling, which would create a popup widget
;         containing a CW_PLOTAXES subwidget for the X and Y plot
;         variables.  When the user makes changes to the Type, Range,
;         and Style values, and then presses the Apply button, the
;         popup widget event handler would re-draw the plot
;         accordingly.
;
;         This widget generates an event when the user presses the
;         Apply button or the Done button, if present. The EVENT.TAG
;         keyword will return either "APPLY" or "DONE" accordingly.
;
;         Example:
; 
;         axes=CW_PLOTAXES(BASE,['X','Y'],/DONE, $
;                      VALUE=TRANSPOSE([[FLTARR(4)],[FLTARR(4)]]))
;
; MODIFICATION HISTORY:
; 
;       David L. Windt, Bell Labs, March 1997
;       windt@bell-labs.com
;
;       DLW, June 1997, Added NO_RETURN keyword.
;-

;------------------------------------------------------------------
; event handler

function cw_plotaxes_ev,event

parent=event.handler
stash=widget_info(parent,/child)

;; get state
widget_control,stash,get_uvalue=state,/no_copy

widget_control,event.id,get_uvalue=eventval 
case eventval of
    'apply': begin
        widget_control,state.apply,sensitive=0
        ret={CW_PLOTAXES_EVENT,ID:parent,TOP:event.top, $
             HANDLER:0L,TAG:"APPLY"}
    end
    'done': ret={CW_PLOTAXES_EVENT,ID:parent,TOP:event.top, $
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

pro cw_plotaxes_set_value,id,value

on_error,2
;; retrieve the state, which contains the widget id's
widget_control,widget_info(id,/child),get_uvalue=state

;; set the widget values:
for i=0,n_elements(state.axis)-1 do  $
  widget_control,state.axis(i),set_value=value(i,*)

return
end

;------------------------------------------------------------------
; get value function

function cw_plotaxes_get_value,id
on_error,2

;; retrieve the state, which contains the widget id's
widget_control,widget_info(id,/child),get_uvalue=state

;; number of axes:
a_n=n_elements(state.axis)

;; make an array to hold the values:
value=fltarr(a_n,4)

;; get the widget values:
for i=0,a_n-1 do begin
    widget_control,state.axis(i),get_value=val
    value(i,*)=val
endfor

return,value
end

;------------------------------------------------------------------
; main function

function cw_plotaxes,parent,labels,uvalue=uvalue,frame=frame, $
           value=value,font=font,row=row,column=column,done=done, $
           axis_ids=axis_ids, $
           x_scroll_size=x_scroll_size, $
           y_scroll_size=y_scroll_size, $
           no_return=no_return


on_error,2

if n_params() ne 2 then  $
  message,'Must specify a parent and labels for cw_plotaxes.'
if not keyword_set(uvalue) then uvalue=0
if (keyword_set(row) eq 0) then column=1

;; number of axes:
a_n=n_elements(labels)

;; check that value is set properly
if n_elements(value) eq 0 then value=fltarr(a_n,4)
sz=size(value)
if (sz(0) ne 2) or (sz(1) ne a_n) or (sz(2) ne 4) $
  then message,'invalid VALUE keword.'
; keep widget id's in state:
state={axis:lonarr(a_n),apply:0L}

if n_elements(font) eq 0 then font=''

base=widget_base(parent,uvalue=uvalue,frame=keyword_set(frame), $
                 event_func='cw_plotaxes_ev', $
                 pro_set_value='cw_plotaxes_set_value', $
                 func_get_value='cw_plotaxes_get_value') 
base1=widget_base(base,row=keyword_set(row),column=keyword_set(column))

if keyword_set(x_scroll_size) and keyword_set(y_scroll_size) then $
  base2=widget_base(base1,row=keyword_set(row), $
                    column=keyword_set(column), $
                    x_scroll_size=x_scroll_size, $
                    y_scroll_size=y_scroll_size) $
else base2=base1

; create all the axis widgets:
for i=0,a_n-1 do state.axis(i)=cw_plotaxis(base2,labels(i), $
                                           row=keyword_set(column),  $
                                           column=keyword_set(row),  $
                                           value=value(i,*),/frame, $
                                           uvalue='axis'+strtrim(i,2), $
                                           font=font, $
                                           no_return=keyword_set(no_return))
axis_ids=state.axis
bbase=widget_base(base1,row=keyword_set(column), $
                  column=keyword_set(row),/align_center)
state.apply=widget_button(bbase,value='Apply',uvalue='apply',font=font)
widget_control,state.apply,sensitive=0 ; apply starts out insensitive
if keyword_set(done) then done=widget_button(bbase,value='Done', $
                                             uvalue='done',font=font)

;; stuff the state into first child
widget_control,widget_info(base,/child),set_uvalue=state

return,base
end
