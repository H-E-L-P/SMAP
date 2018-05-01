;+
; NAME:
; 
;       CW_PLOTSTYLES
;
; PURPOSE:
; 
;       A compound widget used to set values for the graphics keywords
;       COLOR, LINESTYLE, THICK, PSYM, and SYMSIZE, for several plot
;       variables.  A CW_PLOTSTYLE (single variable) widget is created
;       for each element of the LABELS input parameter. The widget
;       also includes an Apply button, and (optionally) a Done button.
;      
; CATEGORY:
; 
;	Compound widgets.
;
; CALLING SEQUENCE:
; 
;	Result = CW_PLOTSTYLES(PARENT,LABEL)
;
; INPUTS:
; 
;       PARENT - The ID of the parent widget.  
;
;       LABELS - a string array of labels to be drawn to the left
;               (or top) of each of the CW_PLOTSTYLE widgets.
;
; OPTIONAL KEYWORD PARAMETERS:
; 
;	UVALUE - Supplies the user value for the widget.
;
;       VALUE - an (n,5) array of initial values, where 
;               n = n_elements(LABELS), and each row has
;               the form VALUE(i,*)=[color,linestyle,thick,psym,symsize]
;
;       FRAME - set to draw a frame around the widget.
;       
;       FONT - font keyword for labels etc.
;
;       ROW - set to create a row of column-oriented CW_PLOTSTYLE widgets.
;
;       COLUMN - set to create a column of row-oriented CW_PLOTSTYLE
;                widgets. (default)
; 
;       DONE - set this to add a Done button, in addition to the standard
;              Apply button.
;
;       STYLE_IDS - an array of widget id's for the individual
;                   CW_PLOTSTYLE widgets.
;
;       X_SCROLL_SIZE, Y_SCROLL_SIZE - if these values are non-zero,
;                                      then the base widget which
;                                      holds the CW_PLOTSTYLE widgets
;                                      will include scroll bars.
;
;
; OUTPUTS:
; 
;       The ID of the created widget is returned.
;
; PROCEDURE/EXAMPLE:
;
;         The idea is that this compound widget would be used in a
;         widget intended to allow the user to interactively adjust
;         the style settings for several variables contained in a
;         plot.  For instance, you might have a menu item such as Plot
;         Options->Styles, which would create a popup widget
;         containing a CW_PLOTSTYLES subwidget, allowing the user to
;         affect each of the variables in the plot.  When the user
;         makes changes to the Color, Linestyle, Thick, Psym, and
;         Symsize values, the popup widget event handler would re-draw
;         the plot accordingly.
; 
;         This widget generates an event when the user presses the
;         Apply button or the Done button, if present. The EVENT.TAG
;         keyword will return either "APPLY" or "DONE" accordingly.
;
;         Example:
;
;         style=CW_PLOTSTYLE(BASE,['A','B'],/DONE, $
;                            VALUE=TRANSPOSE([[FLTARR(5)],[FLTARR(5)]]))
;
; MODIFICATION HISTORY:
; 
;       David L. Windt, Bell Labs, March 1997
;       windt@bell-labs.com
;-

;------------------------------------------------------------------
; event handler

function cw_plotstyles_ev,event

parent=event.handler
stash=widget_info(parent,/child)

;; get state
widget_control,stash,get_uvalue=state,/no_copy

widget_control,event.id,get_uvalue=eventval 
case eventval of
    'apply': begin
        widget_control,state.apply,sensitive=0
        ret={CW_PLOTSTYLE_EVENT,ID:parent,TOP:event.top, $
             HANDLER:0L,TAG:"APPLY"}
    end
    'done': ret={CW_PLOTSTYLE_EVENT,ID:parent,TOP:event.top, $
                 HANDLER:0L,TAG:"DONE"}
    else: begin
        widget_control,state.apply,/sensitive
        ret=0
    end
endcase

;; restore state
widget_control,stash,set_uvalue=state,/no_copy
return,ret
end

;------------------------------------------------------------------
; set value procedure

pro cw_plotstyles_set_value,id,value

on_error,2
;; retrieve the state, which contains the widget id's
widget_control,widget_info(id,/child),get_uvalue=state

;; set the widget values:
for i=0,n_elements(state.var)-1 do  $
  widget_control,state.var(i),set_value=value(i,*)

return
end

;------------------------------------------------------------------
; get value function

function cw_plotstyles_get_value,id
on_error,2

;; retrieve the state, which contains the widget id's
widget_control,widget_info(id,/child),get_uvalue=state

;; number of vars:
v_n=n_elements(state.var)

;; make an array to hold the values:
value=fltarr(v_n,5)

;; get the widget values:
for i=0,v_n-1 do begin
    widget_control,state.var(i),get_value=val
    value(i,*)=val
endfor

return,value
end

;------------------------------------------------------------------
; main function

function cw_plotstyles,parent,labels,uvalue=uvalue, $
           frame=frame,value=value,font=font,row=row,column=column, $
           done=done,style_ids=style_ids, $
           x_scroll_size=x_scroll_size, $
           y_scroll_size=y_scroll_size

on_error,2

if n_params() eq 0 then message,'Must specify a parent for cw_plotstyles.'
if not keyword_set(uvalue) then uvalue=0
if keyword_set(row) eq 0 then column=1

;; number of variables:
v_n=n_elements(labels)

;; check that value is set properly
if n_elements(value) eq 0 then value=fltarr(v_n,5)
sz=size(value)
if (sz(0) ne 2) or (sz(1) ne v_n) or (sz(2) ne 5)  $
  then message,'invalid VALUE keyword.' 

; keep widget id's in state:
state={var:lonarr(v_n),apply:0L}

if n_elements(font) ne 1 then font=''

base=widget_base(parent,uvalue=uvalue,frame=keyword_set(frame), $
                 event_func='cw_plotstyles_ev', $
                 pro_set_value='cw_plotstyles_set_value', $
                 func_get_value='cw_plotstyles_get_value') 
base1=widget_base(base,row=keyword_set(row),column=keyword_set(column))

if keyword_set(x_scroll_size) and keyword_set(y_scroll_size) then $
  base2=widget_base(base1,row=keyword_set(row), $
                    column=keyword_set(column), $
                    x_scroll_size=x_scroll_size, $
                    y_scroll_size=y_scroll_size) $
else base2=base1

; create all the variable style widgets:
for i=0,v_n-1 do state.var(i)=cw_plotstyle(base2,labels(i), $
                                           row=keyword_set(column),  $
                                           column=keyword_set(row),  $
                                           value=value(i,*),/frame, $
                                           uvalue='var'+strtrim(i,2), $
                                           font=font)
style_ids=state.var
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
