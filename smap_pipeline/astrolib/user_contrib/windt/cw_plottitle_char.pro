;+
; NAME:
; 
;       CW_PLOTTITLE_CHAR
;
; PURPOSE:
; 
;       A compound widget used to set values for the graphics keywords
;       CHARSIZE, SUBTITLE, and TITLE.  The widget contains fields for
;       these parameters, an Apply button, and (optionally) a Done
;       button.
;      
; CATEGORY:
; 
;	Compound widgets.
;
; CALLING SEQUENCE:
; 
;	Result = CW_PLOTTITLE_CHAR(PARENT)
;
; INPUTS:
; 
;       PARENT - The ID of the parent widget.  
;
; OPTIONAL KEYWORD PARAMETERS:
; 
;	UVALUE - Supplies the user value for the widget.
; 
;       FRAME - set to draw a frame around the widget.
;
;       VALUE - a structure, containing initial values for
;               the charsize, subtitle and title fields, of
;               the form {charsize:_float_, subtitle: _string_,
;               title:_string_}
;
;       FONT - font keyword for labels etc.
;
;       DONE - set this to add a Done button, in addition to the standard
;              Apply button.
; 
;       IDS - widget ids of the title, subtitle, and charsize cw_field
;             widgets, and the apply button widget.
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
;         Options->Titles/Charsize, which would create a popup widget
;         containing a CW_PLOTTITLE_CHAR subwidget.  When the user
;         makes changes to the CW_PLOTTITLE_CHAR fields, and then
;         presses the Apply button, the popup widget event handler
;         would re-draw the plot accordingly.
;
;         This widget generates an event when the user presses the
;         Apply button or the Done button, if present. The EVENT.TAG
;         keyword will return either "APPLY" or "DONE" accordingly.
;
;         Example:
; 
;         title_char=CW_PLOTTITLE_CHAR(BASE,/DONE, $
;                          VALUE={CHARSIZE:!P.CHARSIZE,SUBTITLE:!P.SUBTITLE,
;                                 TITLE:!P.TITLE}
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

function cw_plottitle_char_ev,event

parent=event.handler
stash=widget_info(parent,/child)

;; get state
widget_control,stash,get_uvalue=state,/no_copy

widget_control,event.id,get_uvalue=eventval 
case eventval of
    'apply': begin
        widget_control,state.apply,sensitive=0
        ret={CW_PLOTTITLE_CHAR_EVENT,ID:parent,TOP:event.top, $
             HANDLER:0L,TAG:"APPLY"}
    end
    'done': ret={CW_PLOTTITLE_CHAR_EVENT,ID:parent,TOP:event.top, $
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

pro cw_plottitle_char_set_value,id,value
on_error,2

;; retrieve the state, which contains the widget id's
widget_control,widget_info(id,/child),get_uvalue=state

;; set the widget values:
widget_control,state.charsize,set_value=value.charsize
widget_control,state.subtitle,set_value=value.subtitle
widget_control,state.title,set_value=value.title

return
end

;------------------------------------------------------------------
; get value function

function cw_plottitle_char_get_value,id
on_error,2

;; retrieve the state, which contains the widget id's
widget_control,widget_info(id,/child),get_uvalue=state

;; make a structure to hold the values
value={charsize:0.,subtitle:'',title:''}

;; get the widget values:
widget_control,state.charsize,get_value=val
value.charsize=val(0)
widget_control,state.subtitle,get_value=val
value.subtitle=val(0)
widget_control,state.title,get_value=val
value.title=val(0)

return,value
end

;------------------------------------------------------------------
; main function

function cw_plottitle_char,parent,uvalue=uvalue,frame=frame, $
           value=value,font=font,done=done,ids=ids,no_return=no_return

on_error,2

if n_params() ne 1 then  $
  message,'Must specify a parent for cw_plottitle_char.'
if not keyword_set(uvalue) then uvalue=0

;; check that value is set properly
if n_elements(value) eq 0 then value={charsize:!p.charsize, $
                                      subtitle:!p.subtitle, $
                                      title:!p.title}
tags=tag_names(value)
if (n_elements(tags) ne 3) then message,'invalid VALUE keword.'
if (tags(0) ne 'CHARSIZE') or $
  (tags(1) ne 'SUBTITLE') or $
  (tags(2) ne 'TITLE') then $
  message,'invalid VALUE keword.'

; keep widget id's in state:
state={charsize:0L,subtitle:0L,title:0L,apply:0L}

if n_elements(font) eq 0 then font=''

base=widget_base(parent,uvalue=uvalue,frame=keyword_set(frame), $
                 event_func='cw_plottitle_char_ev', $
                 pro_set_value='cw_plottitle_char_set_value', $
                 func_get_value='cw_plottitle_char_get_value') 
base1=widget_base(base,/column)

base2=widget_base(base1,/column,/frame)

state.title=cw_field(base2,/string,title='   Title:',/return_event, $
                     font=font,/row,value=value.title,uvalue='title', $
                     no_return=keyword_set(no_return))
state.subtitle=cw_field(base2,/string,title='Subtitle:',/return_event, $
                        font=font,/row,value=value.subtitle,uvalue='subtitle', $
                        no_return=keyword_set(no_return))
state.charsize=cw_field(base2,/float, title='Charsize:',/return_event, $
                        font=font,/row,value=value.charsize,uvalue='charsize', $
                        no_return=keyword_set(no_return))

bbase=widget_base(base1,/row,/align_center)
state.apply=widget_button(bbase,value='Apply',uvalue='apply',font=font)
widget_control,state.apply,sensitive=0 ; apply starts out insensitive

if keyword_set(done) then done=widget_button(bbase,value='Done', $
                                             uvalue='done',font=font)

;; return widget ids:
ids=[state.title,state.subtitle,state.charsize,state.apply]

;; stuff the state into first child
widget_control,widget_info(base,/child),set_uvalue=state

return,base
end
