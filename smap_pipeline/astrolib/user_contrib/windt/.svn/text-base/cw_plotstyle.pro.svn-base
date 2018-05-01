;+
; NAME:
; 
;       CW_PLOTSTYLE
;
; PURPOSE:
; 
;       A compound widget used to set values for the graphics keywords
;       COLOR, LINESTYLE, THICK, PSYM, and SYMSIZE.
;      
; CATEGORY:
; 
;	Compound widgets.
;
; CALLING SEQUENCE:
; 
;	Result = CW_PLOTSTYLE(PARENT,LABEL)
;
; INPUTS:
; 
;       PARENT - The ID of the parent widget.  
;
;       LABEL - a label to be drawn to the left (or top, for /column)
;               of the widget.
;
; OPTIONAL KEYWORD PARAMETERS:
; 
;	UVALUE - Supplies the user value for the widget.
;
;       VALUE - an array of initial values:
;               [COLOR,LINESTYLE,THICK,PSYM,SYMSIZE]
;
;       FRAME - set to draw a frame around the widget.
;       
;       FONT - font keyword for labels etc.
;
;       ROW - set to orient the subwidgets in a row (default.)
;
;       COLUMN - set to orient the subwidgets in a column.
;
;       NO_PSYM - set to delete the PSYM and SYMSIZE widgets. If set,
;                 PSYM=0 and SYMSIZE=0 will be returned when using
;                 WIDGET_CONTROL,GET_VALUE; PSYM and SYMSIZE are
;                 ignored when using WIDGET_CONTROL,SET_VALUE
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
;       a menu item such as Plot Options->Styles, which would create a
;       popup widget containing CW_PLOTSTYLE subwidgets for each of
;       the variables being plotted.  When the user makes changes to
;       the Color, Linestyle, Thick, Psym, and Symsize values, the
;       popup widget event handler would re-draw the plot accordingly.
; 
;       The user is presented with:
;
;       - a droplist of color choices, by name, referring to the first
;         19 colors (i.e., the ones I was able to describe by name)
;         defined by the TEK_COLOR procedure.  It's therefore
;         necessary for the plotting program to call TEK_COLOR before
;         drawing the plot using these color settings.
;
;       - a droplist of linestyle choices.
;
;       - a droplist of thickness choices, from 1 to 9
;
;       - a droplist of psym choices, by name.
;
;       - a droplist of symsize choices, from 0.25 to 2.00, in
;         0.25 increments.
;
;       The widget returns events when any of it's children generate
;       events.  The returned event has the form
;       {CW_PLOTSTYLE_EVENT,ID:id,TOP:top,HANDLER:handler,TAG:tag}
;       where TAG indicates which child widget generated the event:
;       Possible values for EVENT.TAG are COLOR, LINESTYLE, THICK
;       PSYM, and SYSMSIZE.
;
;       NOTE: you probably really want to use the CW_PLOTSTYLES (plural)
;       widget, not this one.
;
; MODIFICATION HISTORY:
; 
;       David L. Windt, Bell Labs, March 1997
;       windt@bell-labs.com
;-

;------------------------------------------------------------------
; event handler

function cw_plotstyle_ev,event

;; retrieve the state, which contains the widget id's
widget_control,widget_info(event.handler,/child),get_uvalue=state

;; get psym value:
if widget_info(state.psym,/valid) then $
  psym=widget_info(state.psym,/droplist_select) else $
  psym=0

;; linestyle is insensitive if psym eq 0 or psym gt 7
widget_control,state.linestyle,sensitive=(psym eq 0) or (psym gt 7)

;; symsize is only sensitive if psym ne 0
if widget_info(state.symsize,/valid) then $
  widget_control,state.symsize,sensitive=(psym ne 0)

widget_control,event.id,get_uvalue=eventval 

return,{CW_PLOTSTYLE_EVENT, ID: event.handler,  $
        TOP: event.top, HANDLER: 0L, TAG: eventval}
end

;------------------------------------------------------------------
; set value procedure

pro cw_plotstyle_set_value,id,value

on_error,2
;; retrieve the state, which contains the widget id's
widget_control,widget_info(id,/child),get_uvalue=state

;; set the widget values:
widget_control,state.color,set_droplist_select=value(0)

widget_control,state.linestyle,set_droplist_select=value(1)
;; linestyle is insensitive if psym gt 0
widget_control,state.linestyle,sensitive=(value(3) le 0)

widget_control,state.thick,set_droplist_select=value(2)-1

if widget_info(state.psym,/valid) then begin
    ;; convert from psym to droplist integer:
    psymdrop=value(3)
    if value(3) gt 7 then psymdrop=0
    if value(3) lt 0 then psymdrop=7-value(3)
    widget_control,state.psym,set_droplist_select=psymdrop

    ;; convert from symsize (floating) to droplist integer:
    case 1 of
        value(4) le 0.25: symsdrop=0
        value(4) le 0.50: symsdrop=1
        value(4) le 0.75: symsdrop=2
        value(4) le 1.00: symsdrop=3
        value(4) le 1.25: symsdrop=4
        value(4) le 1.50: symsdrop=5
        value(4) le 1.75: symsdrop=6
        else: symsdrop=7
    endcase
    widget_control,state.symsize,set_droplist_select=symsdrop
    ;; symsize is only sensitive if pysm ne 0
    widget_control,state.symsize,sensitive=(value(3) ne 0)
endif

return
end

;------------------------------------------------------------------
; get value function

function cw_plotstyle_get_value,id
on_error,2

;; retrieve the state, which contains the widget id's
widget_control,widget_info(id,/child),get_uvalue=state

;; get the widget values:

color=widget_info(state.color,/droplist_select)

linestyle=widget_info(state.linestyle,/droplist_select)

thick=widget_info(state.thick,/droplist_select)+1

if widget_info(state.psym,/valid) then begin
    psymdrop=widget_info(state.psym,/droplist_select)
    ;; convert from droplist integer to psym:
    psym=psymdrop
    if psymdrop gt 7 then psym=7-psymdrop

    symsdrop=widget_info(state.symsize,/droplist_select)
    ;; convert from droplist integer to symsize (floating):
    case symsdrop of
        0: symsize=0.25
        1: symsize=0.50
        2: symsize=0.75
        3: symsize=1.00
        4: symsize=1.25
        5: symsize=1.50
        6: symsize=1.75
        7: symsize=2.00
    endcase
endif else begin
    psym=0
    symsize=0
endelse

return,[color,linestyle,thick,psym,symsize]
end

;------------------------------------------------------------------
; main function

function cw_plotstyle,parent,label,uvalue=uvalue, $
           frame=frame,value=value,font=font,row=row,column=column, $
           no_psym=no_psym

on_error,2

if n_params() eq 0 then message,'Must specify a parent for cw_plotstyle.'
if not keyword_set(uvalue) then uvalue=0
if keyword_set(column) eq 0 then row=1

;; check that value is set properly
if n_elements(value) ne 5 then  $
  value=[!p.color,!p.linestyle,!p.thick,!p.psym,!p.symsize] 

; keep widget id's in state:
state={color:0L, linestyle:0L, thick:0L, psym:0L, symsize:0L}

colors=['Black','White','Red','Green','Blue','Cyan', $
        'Magenta','Yellow','Orange','Olive','Light Green', $
        'Light Blue','Violet','Pink','Dark Grey','Light Grey', $
        'Pale Yellow']

lines=['_____','........','- - -','-.-.-','-...-...','__ __ _']

thicks='Thick: '+['1','2','3','4','5','6','7','8','9']

psyms=['Line','+','*','.','Diamond','Triangle','Square','X', $
       'Line/+','Line/*','Line/.','Line/Dia', $
       'Line/Tri','Line/Squ','Line/X']

symss='Size: '+['0.25','0.50','0.75','1.00','1.25','1.50','1.75','2.00']

if n_elements(font) ne 1 then font=''

base=widget_base(parent,uvalue=uvalue,frame=keyword_set(frame), $
                 event_func='cw_plotstyle_ev', $
                 pro_set_value='cw_plotstyle_set_value', $
                 func_get_value='cw_plotstyle_get_value') 
base1=widget_base(base,row=keyword_set(row), $
                  column=keyword_set(column),/base_align_bottom)

if n_elements(label) eq 1 then  $
  label=widget_label(base1,value=label,font=font,/align_center) 

state.color=widget_droplist(base1,value=colors,uvalue='color',font=font) 
widget_control,state.color,set_droplist_select=value(0)

state.linestyle=widget_droplist(base1,value=lines,uvalue='linestyle',font=font)
widget_control,state.linestyle,set_droplist_select=value(1)
;; linestyle is insensitive if psym gt 0
widget_control,state.linestyle,sensitive=(value(3) le 0)

state.thick=widget_droplist(base1,value=thicks,uvalue='thick',font=font)
widget_control,state.thick,set_droplist_select=value(2)

if keyword_set(no_psym) eq 0 then begin
    state.psym=widget_droplist(base1,value=psyms,uvalue='psym',font=font) 

    ;; convert from psym to droplist integer:
    psymdrop=value(3)
    if value(3) gt 7 then psymdrop=0
    if value(3) lt 0 then psymdrop=7-value(3)
    widget_control,state.psym,set_droplist_select=psymdrop

    state.symsize=widget_droplist(base1,value=symss,uvalue='symsize',font=font) 
    ;; convert from symsize (floating) to droplist integer:
    case 1 of
        value(4) le 0.25: symsdrop=0
        value(4) le 0.50: symsdrop=1
        value(4) le 0.75: symsdrop=2
        value(4) le 1.00: symsdrop=3
        value(4) le 1.25: symsdrop=4
        value(4) le 1.50: symsdrop=5
        value(4) le 1.75: symsdrop=6
        else: symsdrop=7
    endcase
    widget_control,state.symsize,set_droplist_select=symsdrop
    ;; symsize is only sensitive if pysm ne 0
    widget_control,state.symsize,sensitive=(value(3) ne 0)
endif

;; stuff the state into first child
widget_control,widget_info(base,/child),set_uvalue=state

return,base
end
