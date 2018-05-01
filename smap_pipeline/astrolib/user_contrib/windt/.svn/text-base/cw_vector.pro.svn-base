;+
; NAME:
; 
;       CW_VECTOR
;
; PURPOSE:
; 
;       A compound widget used to get input necessary to create a
;       "vector", in the spirit of the VECTOR function in this
;       directory, i.e, get input for the MIN, MAX, and PTS values.
;       The widget also lets the user specify the increment between
;       points, and whether the point spacing is linear or
;       logarithmic.
;      
; CATEGORY:
; 
;	compound widgets.
;
; CALLING SEQUENCE:
; 
;	Result = CW_VECTOR(PARENT)
;
; INPUTS:
; 
;       PARENT - the id of the parent widget.  
;
; OPTIONAL KEYWORD PARAMETERS:
; 
;	UVALUE - Supplies the user value for the widget.
; 
;       FRAME - Set to draw a frame around the widget.
;
;       FONT - Font keyword for labels etc.
;
;       TITLE - A string used to label the widget
;
;       XSIZE - An explicit horizontal size (in characters) for the
;               min, max and increment input areas.  The default is to
;               let the window manager size the widget.  
;
;       NXSIZE - An explicit horizontal size (in characters) for the
;                pts field area.  The default is to let the window
;                manager size the widget.  
;
;       YSIZE - An explicit vertical size (in lines) for the text input
;               areas.  The default is 1.
;
;       VALUE - A structure used to set the initial value of the
;               widget, containing the following tags:
;
;               min, max, n and log - the parameters used to specify a
;                                     vector (see vector.pro)
;
;               format - a valid format command string used to format
;                        the min, max, and increment values. a null
;                        string will result in default floating-point
;                        formatting.
;
;               nformat - a valid format command string to format the
;                         pts field.  a null string will result in
;                         default integer formatting.
; 
;               units - a string used to label the vector units. for
;                       example, if the CW_VECTOR widget is being used
;                       to get input to create a vector of lengths in
;                       feet, then set value.units='feet'
;
;               uunits - a flag to indicate whether or not to actually
;                        update the units label.
;             
;               The same value structure is used with WIDGET_CONTROL
;               to set the value of a CW_VECTOR, as in
;               WIDGET_CONTROL,WIDGET,SET_VALUE=VALUE
;
;               When using the GET_VALUE keyword with WIDGET_CONTROL,
;               however, the returned value is a structure with only
;               four tags: {min,max,pts,log}
;
;      MINRANGE, MAXRANGE - These keywords define the range of
;                           acceptable values for the min and max
;                           fields.  If not set, any values for min
;                           and max are allowed; otherwise, (min >
;                           MINRANGE) < MAXRANGE, and (max > MINRANGE)
;                           < MAXRANGE.  None, one or both of these
;                           keywords can be specified.
;
;      MINN - The minimum allowable value for n. default is 1.
;
;      NO_RETURN - The default behavior is that the user must press
;                  <return> after entering new values.  Set this
;                  keyword so that an event is returned even if the
;                  user just changes a value and then moves the
;                  cursor outside of the text entry area.
;
; OUTPUTS:
; 
;       The id of the created widget is returned.
;
; PROCEDURE:
; 
;      Entering a value in the pts, min or max fields will set the
;      increment field.  Entering a value in the increment field will
;      set the points field, and possibly the max field if the
;      increment doesn't divide evenly into the range specified by min
;      and max.
;
; EXAMPLE:
; 
;     Create a CW_VECTOR to get input to create a vector of lengths in
;     [feet]:
;     
;      
;        base=WIDGET_BASE()
;        length_widget=CW_VECTOR(BASE,VALUE={MIN:0.,MAX:10.,N:11,LOG:0, $
;                                            UNITS:'feet', $
;                                            FORMAT:'(F10.2)',
;                                            NFORMAT:'(I4)',
;                                            UUNITS:1}
;                                TITLE='LENGTHS')
;
;     Later, get the widget values and create the length vector:
; 
;        WIDGET_CONTROL,length_widget,GET_VALUE=value
;        lengths=VECTOR(value.min,value.max,value.n,log=value.log)
;
; MODIFICATION HISTORY:
; 
;       David L. Windt, Bell Labs, March 1997
;       windt@bell-labs.com
;
;       DLW, June 1997, Added NO_RETURN keyword.
;
;       DLW, November 1997, Removed TRACKING keyword; corrected bug
;       that caused improper updates when NO_RETURN was set and the
;       user toggled between linear and logarithmic step sizes.
;
;-
;-procedure to set widget value--------------------------------------------
;
pro cw_vector_set_value,id,value

on_error,2

;; get stash id:
stash=widget_info(id,/child)

;; get state:
widget_control,stash,get_uvalue=state,/no_copy

;; make sure values are in range, and of correct type:
n=fix(value.n) > state.minn
v1=(value.min > state.minrange) < state.maxrange
v2=(value.max > state.minrange) < state.maxrange
log=( (value.log > 0) < 1)

;; for log spacing, can't have non-positive values:
if log then begin
    if v1 le 0. then v1=1.
    if v2 le 0. then v2=1.
endif

;; save formats and units in state:
state.units=value.units
state.nformat=value.nformat
state.format=value.format

;; set increment:
inc=0.
if n gt 1 then inc=(v2-v1)/(n-1)

;; set n:
widget_control,state.n,set_value=strtrim(string(n,format=value.nformat),2)
widget_control,state.n,set_uvalue=n

;; set min1:
widget_control,state.min1,set_value=strtrim(string(v1,format=value.format),2)
widget_control,state.min1,set_uvalue=v1

;; set unit label:
if value.uunits then widget_control,state.unit,set_value=value.units

;; set min:
widget_control,state.min,set_value=strtrim(string(v1,format=value.format),2)
widget_control,state.min,set_uvalue=v1

;; set max:
widget_control,state.max,set_value=strtrim(string(v2,format=value.format),2)
widget_control,state.max,set_uvalue=v2

;; set inc:
widget_control,state.inc,set_value=strtrim(string(inc,format=value.format),2)

;; set units label:
if value.uunits then widget_control,state.unitp,set_value=value.units

;; set increment droplist label:
if value.uunits then begin
    if value.units eq '' then $
      widget_control,state.log, $
      set_value=['step size:','logarithmic steps']  $
    else $
      widget_control,state.log, $
      set_value=['step size ['+value.units+']:','logarithmic steps']
    widget_control,state.log,set_droplist_select=log
endif

;; map/unmap unit or max, inc, and units:
widget_control,state.row3,map=(n eq 1)
widget_control,state.row4,map=(n gt 1)
widget_control,state.inc,map=(log eq 0)

;; restore state:
widget_control,stash,set_uvalue=state,/no_copy

return
end

;-function to get widget value---------------------------------------------
;
function cw_vector_get_value,id

on_error,2

;; get stash id:
stash=widget_info(id,/child)

;; get state:
widget_control,stash,get_uvalue=state,/no_copy

;; get n:
widget_control,state.n,get_uvalue=val
n=fix(val(0))

;; get min1:
widget_control,state.min,get_uvalue=val
if n eq 1 then v1=double(val(0))

;; get min:
widget_control,state.min,get_uvalue=val
if n gt 1 then v1=double(val(0))

;; get max:
widget_control,state.max,get_uvalue=val
v2=double(val(0))

;; get log:
log=widget_info(state.log,/droplist_select)

;; restore the state.
widget_control,stash,set_uvalue=state,/no_copy

value={min:v1,max:v2,n:n,log:log}

return,value
end

;-function to handle cw_vector widget events---------------------------------

function cw_vector_event,event

;; get parent id:
parent=event.handler

;; get stash id:
stash=widget_info(parent,/child)

;; get state:
widget_control,stash,get_uvalue=state,/no_copy

;; deal with no_return stuff:
update=1
if keyword_set(state.no_return) then begin
    ;; set new_text flag if it's a tracking event or a carriage
    ;; return:
    new_text=(tag_names(event,/str) eq 'WIDGET_TRACKING')
    if new_text eq 0 then $
      if (strmid(tag_names(event,/str),0,11) eq 'WIDGET_TEXT') then $
      if (event.type eq 0) then new_text=event.ch eq 10B 
    ;; or a droplist event:
    if new_text eq 0 then $
      if (strmid(tag_names(event,/str),0,15) eq 'WIDGET_DROPLIST') then $
      new_text=1

    ;; if there's new text, then get it and (a) turn off tracking
    ;; events and (b) turn on all text events. otherwise, turn on
    ;; tracking and turn off all text events:
    if new_text then begin
        if (strmid(tag_names(event,/str),0,11) ne 'WIDGET_DROPLIST') then begin
            widget_control,event.id,tracking_events=0
            widget_control,event.id,all_text_events=1
        endif
    endif else begin
        if (strmid(tag_names(event,/str),0,11) eq 'WIDGET_TEXT') then begin
            widget_control,event.id,tracking_events=1
            widget_control,event.id,all_text_events=0
        endif
        ;; no new text, so just exit:
        update=0
        goto,finish
    endelse
endif 

;; get n:
widget_control,state.n,get_value=n
n=fix(n(0))

;; get min and mix values:
if n eq 1 then widget_control,state.min1,get_value=v1 else $
  widget_control,state.min,get_value=v1
widget_control,state.max,get_value=v2
v1=double(v1(0))
v2=double(v2(0))

;; get log:
log=widget_info(state.log,/droplist_select)

widget_control,event.id,get_uvalue=uvalue

;; specifying increment?
if string(uvalue) eq 'inc' then begin
    ;; get increment:
    widget_control,state.inc,get_value=inc
    inc=double(inc(0)) 
    ;; set n, based on valid increment:
    if inc ne 0 then n1=1+fix((v2-v1)/inc) else n1=0
    ;; if n1 is negative, ignore the specified increment:
    if n1 lt 1 then inc=double((v2-v1)/(n-1)) else n=n1
    ;; set v2, based on valid n, v1 and inc:
    v2=double(v1+inc*(n-1))
endif

units=state.units
nformat=state.nformat
format=state.format

finish:;;
;; restore the state structure
widget_control,stash,set_uvalue=state,/no_copy

;; update the values:
if update then $
  cw_vector_set_value,parent,{min:v1,max:v2,n:n,log:log,units:units, $
                              nformat:nformat,format:format,uunits:0}

if update eq 0 then return,0 else $
  return,{CW_VECTOR_EVENT, id:parent,top:event.top,handler:0l}
end

;-cw_vector-------------------------------------------------------------------

function cw_vector,parent,uvalue=uval,font=font, $
           frame=frame,title=title,  $
           xsize=xsize,nxsize=nxsize,ysize=ysize, $
           value=value,minn=minn, $
           minrange=minrange,maxrange=maxrange, $
           no_return=no_return

if n_elements(value) ne 1 then $
  value={min:0.,max:0.,n:1,log:0,units:'',nformat:'',format:'',uunits:0}

if (n_params() ne 1) then message,'usage - widget_id=cw_vector(parent)'

on_error, 2

if n_elements(uval) ne 1 then uval=0
if n_elements(font) ne 1 then font=''
if n_elements(xsize) ne 1 then xsize=0
if n_elements(nxsize) ne 1 then nxsize=0
if n_elements(ysize) ne 1 then ysize=1
;; get xmin and xmax values using machar:
cm=check_math(0,1)
ma=machar()
if n_elements(minn) ne 1 then minn=1
if n_elements(minrange) ne 1 then minrange=-ma.xmax
if n_elements(maxrange) ne 1 then maxrange=ma.xmax

state={units:value.units, $
       format:value.format, $
       nformat:value.nformat, $
       minn:minn, $
       minrange:minrange, $
       maxrange:maxrange, $
       title:0l,  $
       row1:0l, $
       row2:0l, $
       n:0l, $
       base:0l, $
       row3:0l, $
       value:0l, $
       min1:0l, $
       unit:0l, $
       row4:0l, $
       from:0l, $
       min:0l, $
       to:0l, $
       max:0l, $
       unitp:0l, $
       log:0l, $
       inc_base:0l, $
       inc:0l, $
       no_return:keyword_set(no_return)}

mainbase=widget_base(parent,uvalue=uval,frame=keyword_set(frame),/column, $
                     event_func="cw_vector_event", $
                     func_get_value="cw_vector_get_value", $
                     pro_set_value="cw_vector_set_value")

if n_elements(title) eq 1 then $
  state.title=widget_label(mainbase,value=title,font=font,/align_left)

state.row1=widget_base(mainbase,/row)

state.row2=widget_base(state.row1,/row)
state.n=widget_text(state.row2,uvalue='n',font=font,/editable, $
                    xsize=nxsize,ysize=ysize, $
                    all_event=keyword_set(no_return))

state.base=widget_base(state.row1)

state.row3=widget_base(state.base,/row)
state.value=widget_label(state.row3,font=font,value='value:')
state.min1=widget_text(state.row3,uvalue='min1',font=font,/editable, $
                       xsize=xsize,ysize=ysize, $
                       all_event=keyword_set(no_return))
state.unit=widget_label(state.row3,font=font)

state.row4=widget_base(state.base,/row)
state.from=widget_label(state.row4,font=font,value='values, from')
state.min=widget_text(state.row4,uvalue='min',font=font,/editable, $
                      xsize=xsize,ysize=ysize, $
                      all_event=keyword_set(no_return))
state.to=widget_label(state.row4,font=font,value='to ')
state.max=widget_text(state.row4,uvalue='max',font=font,/editable, $
                      xsize=xsize,ysize=ysize, $
                      all_event=keyword_set(no_return))
state.unitp=widget_label(state.row4,font=font)
state.log=widget_droplist(state.row4,uvalue='log',font=font)
state.inc_base=widget_base(state.row4,/row)
state.inc=widget_text(state.inc_base,uvalue='inc',font=font,/editable, $
                      xsize=xsize,ysize=ysize, $
                      all_event=keyword_set(no_return))

;; stash state into first child widget:
widget_control,widget_info(mainbase,/child),set_uvalue=state,/no_copy

cw_vector_set_value,mainbase,value

return,mainbase
end

