;+
; NAME:
;          PLOT_PRINT
;
;
; PURPOSE:
; 
;          A widget-based interface for creating and printing IDL
;          graphics output files.  The widget allows the user to
;          select an output device type (PS, PCL, or HP), and specify
;          whether or not to use color, the color depth (for PS), the
;          plot orientation (landscape or portrait), the size of the
;          plot, whether or not to use Vector or PS fonts, the name of
;          the file to create, whether to send the file to a printer,
;          and the print command.
;
; CALLING SEQUENCE:
; 
;          PLOT_PRINT,PLOT_PROCEDURE
; 
; INPUTS:
;          PLOT_PROCEDURE - A string containing the name of the
;                           procedure - or the executable IDL code - that
;                           creates the desire graphics. See EXAMPLE
;                           below for more details.
;
;
; KEYWORD PARAMETERS:
; 
;          PRINTPARS - a structure of the following form (default
;                      values indicated), whose values are used to
;                      set the initial value of this quasi-compound widget:
;
;          PRINTPARS={ $
;                      device:0, $  ; 0=PS, 1=HP, 2=PCL, 3=CGM
;                      psfont:0, $  ; 0=use vector fonts, 1=use PS fonts.
;                      color:1, $   ; 0=B&W, 1=color
;                      depth:2, $   ; PS resolution: 0=>1, 1=>2, 2=>4, 4=>8.
;                      orient:0, $  ; 0=portrait, 1=landscape
;                      size:1, $    ; 0=small, 1=large, 2=custom (i.e., use
;                                     plotsize)
;                      plotsize:[xsize,ysize,xoffset,yoffset], $
;                                   ; keyword to the DEVICE command,
;                                   ; in inches. only used if size=2.
;                      file_name:'idl.ps', $ ; default file name
;                      print:1, $   ; 0=print only to file, 1=send to printer.
;                      command:'lpr -Plp'} ; print command
;
;          GROUP - the standard GROUP_LEADER keyword, which is passed directly
;                  to XMANAGER when the PLOT_PRINT widget is created.
;
; COMMON BLOCKS:
; 
;          PLOT_PRINT, plot_printpars
; 
;                     where plot_printpars = the current state of
;                     the printpars structure.
;
; SIDE EFFECTS:
; 
;          The returned value of the printpars structure, if passed,
;          is changed to reflect the settings changes made by the
;          user.  Thus, settings shown in the widget upon subsequent
;          calls to plot_print with the same printpars structure will
;          show the same settings as the last call to PLOT_PRINT.
;
; RESTRICTIONS:
; 
;          Requires widgets.  Requires several programs in the windt
;          library, including a modified version of CW_FIELD!
; 
;          The PLOT_PRINT widget is modal.
;
; EXAMPLE:
; 
;         This program is intended to be used from within another
;         widget application, where a procedure is already defined
;         that creates the graphics.  You can then add a WIDGET_BUTTON
;         to this application, labelled "Print", for example, that
;         when pressed calls PLOT_PRINT, with the name of the plot
;         creation procedure as the input. i.e., pressing the "Print"
;         button would execute the IDL code < PLOT_PRINT,"myplot" >.
;
;         For example:
;
;         PRO MYPLOT_EV,event
;
;         widget_control,event.id,get_uvalue=eventval
;         if eventval eq 'print' then plot_print,'myplot_plot'
;         if eventval eq 'done' then widget_control,event.top,/destroy
;         return
;         end
;
;         PRO MYPLOT_PLOT
;
;         plot,[1,2],title='My Plot'
;         return
;         end
;
;         PRO MYPLOT
;
;         base=widget_base(mbar=menubar)
;         file=widget_button(menubar,/menu,value='File')
;         print=widget_button(file,value='Print...',uvalue='print')
;         done=widget_button(file,value='Quit',uvalue='done')
;         window=widget_draw(base,xsize=400,ysize=300)
;         widget_control,base,/realize
;         myplot_plot
;         xmanager,'myplot',base,event='myplot_ev'
;         return
;         end
; 
;         Of course, you can call the program right from the command
;         line too, as in
;
;         IDL> plot_print,'x=vector(0.,!pi,100) & y=sin(5*x) & plot,x,y,/xstyle'
;
; MODIFICATION HISTORY:
; 
;         David L. Windt, Bell Labs, March 1997
;
;         May 1997 - Modified use of MODAL keyword to work with
;                    changes in IDL V5.0.
;
;         June 1997 - Changed text and field widgets so that it's no
;                     longer necessary to hit <return> after entering
;                     text.  But this requires use of the modified
;                     CW_FIELD widget.
;
;         January 1998 - Added support for CGM graphics; switched
;                        plot_print.device values for HP and PCL
;                        output, to be consistent with the SP.PRO
;                        routine.
;
;         March 1998 - Made some attempt to include a better default
;                      print command for HP-UX and Win95 platforms.
;
;         May 1998 - The user is now prompted before attempting to
;                    write over an existing file.
;
;         windt@bell-labs.com
;-

;----procedure to handle plot_print widget events-----------------------------
;
pro plot_print_ev,event

common plot_print,plot_printpars

;; get state
widget_control,event.top,get_uvalue=s

;; get uvalue of event
widget_control,event.id,get_uvalue=uvalue

status=''
no_update=0

case uvalue of

    'device': plot_printpars.device=event.index

    'font': plot_printpars.psfont=event.value

    'color': plot_printpars.color=event.index

    'depth': plot_printpars.depth=event.index

    'size': plot_printpars.size=event.index

    'orient': plot_printpars.orient=event.index

    'xsize': begin
        widget_control,s.w.xsize,get_value=value
        plot_printpars.plotsize(0)=float(value(0))
        if plot_printpars.plotsize(0) le 0 then  $
           plot_printpars.plotsize(0)=7.
    end

    'ysize': begin
        widget_control,s.w.ysize,get_value=value
        plot_printpars.plotsize(1)=float(value(0))
        if plot_printpars.plotsize(1) le 0 then  $
           plot_printpars.plotsize(1)=5.
    end

    'xoffset': begin
        widget_control,s.w.xoffset,get_value=value
        plot_printpars.plotsize(2)=float(value(0))
    end

    'yoffset': begin
        widget_control,s.w.yoffset,get_value=value
        plot_printpars.plotsize(3)=float(value(0))
    end

    'file': begin
        ;; set new_text flag if it's a tracking event or a carriage
        ;; return:
        new_text=(tag_names(event,/str) eq 'WIDGET_TRACKING')
        if new_text eq 0 then  $
          if (event.type eq 0) then new_text=event.ch eq 10B 

        ;; if there's new text, then get it and (a) turn off tracking
        ;; events and (b) turn on all text events. otherwise, turn off
        ;; tracking and turn on all text events:
        if new_text then begin
            widget_control,event.id,get_value=value
            file=string(value(0))
            if file eq '' then begin
                case plot_printpars.device of
                    0: file='idl.ps'
                    1: file='idl.hgl'
                    2: file='idl.pcl'
                    3: file='idl.cgm'
                endcase
            endif
            plot_printpars.file_name=file
            widget_control,event.id,set_value=plot_printpars.file_name
        endif
        widget_control,event.id,tracking_events=new_text eq 0, $
          all_text_events=new_text
        no_update=1
    end

    'print': plot_printpars.print=(plot_printpars.print+1) mod 2

    'command': begin
        ;; set new_text flag if it's a tracking event or a carriage
        ;; return:
        new_text=(tag_names(event,/str) eq 'WIDGET_TRACKING')
        if new_text eq 0 then  $
          if (event.type eq 0) then new_text=event.ch eq 10B 

        ;; if there's new text, then get it and (a) turn off tracking
        ;; events and (b) turn on all text events. otherwise, turn off
        ;; tracking and turn on all text events:
        if new_text then begin
            widget_control,event.id,get_value=value
            command=string(value(0))
            if command eq '' then command='lp '
            plot_printpars.command=command
            widget_control,event.id,set_value=plot_printpars.command
        endif
        widget_control,event.id,tracking_events=new_text eq 0, $
          all_text_events=new_text
        no_update=1
    end

    'print_button': begin
        ;; the code that actually creates the output file:

        ;; check to see if the file exists:
        ff=findfile(plot_printpars.file_name,count=count)
        if count ne 0 then $
          if widget_message(/question,'File exists - overwrite?', $
                            /default_no,dialog_parent=event.top)  $
          eq 'No' then goto,finish

        ;; use sp to set the output file settings:
        ;; convert plot_printpars.device value to the
        ;; corresponding 'sp.pro' values:
        case plot_printpars.device of
            0: dev=0
            1: dev=1
            2: dev=2
            3: dev=8
        endcase
        if plot_printpars.size eq 2 then  $
          sp,dev,hard=plot_printpars.psfont, $
          color=plot_printpars.color,land=plot_printpars.orient, $
          plotsize=plot_printpars.plotsize,file=plot_printpars.file_name $
        else $
          sp,dev,hard=plot_printpars.psfont, $
          color=plot_printpars.color,land=plot_printpars.orient, $
          small=(1+plot_printpars.size) mod 2,file=plot_printpars.file_name

        ;; make sure isolatin is set:
        if !d.name eq 'PS' and (plot_printpars.psfont eq 1) then device,/isolatin
        
        ;; set postscript depth:
        if !d.name eq 'PS' then case plot_printpars.depth of 
            0: device,bits=1
            1: device,bits=2
            2: device,bits=4
            3: device,bits=8
        endcase

        ;; procedure string is in uvalue of top widget:
        e=execute(s.procedure)

        ;; update status:
        status=' File '+plot_printpars.file_name+' created.'

        ;; use lprint to close the file, print it, and return to windows:
        if plot_printpars.print then begin
          lprint,command=plot_printpars.command+' '+plot_printpars.file_name
          status=' File '+plot_printpars.file_name+' created and printed.'
        endif else begin
            ;; otherwise just close and return to windows.
            device,/close
            sp
            !p.font=-1
        endelse
    end

    'done_button': begin
        widget_control,event.top,/destroy
        return
    end

endcase

finish:;;

;; update widget:
if no_update eq 0 then plot_print_ud,event.top         

;; status widget is first child:
widget_control,widget_info(event.top,/child),set_value=status
return
end

;----procedure to update the plot_print widget------------------------------
;
pro plot_print_ud,id

; id - widget id of base

common plot_print,plot_printpars

;; get state - uvalue of base
widget_control,id,get_uvalue=s,/no_copy

if plot_printpars.device ne 0 then begin
    plot_printpars.psfont=0
    widget_control,s.w.font,set_value=plot_printpars.psfont
endif
widget_control,s.w.font,sensitive=plot_printpars.device eq 0

widget_control,s.w.color,sensitive=plot_printpars.device ne 3

widget_control,s.w.depth,set_droplist=plot_printpars.depth
widget_control,s.w.depth_base,sensitive=plot_printpars.device eq 0

widget_control,s.w.size,set_droplist=plot_printpars.size
widget_control,s.w.size,sensitive=plot_printpars.device ne 3

widget_control,s.w.orient,set_droplist=plot_printpars.orient
widget_control,s.w.orient,sensitive=plot_printpars.device ne 3

widget_control,s.w.plotsize_base, $
  sensitive=(plot_printpars.size eq 2) and $
  (plot_printpars.device ne 3)

if plot_printpars.size ne 2 then begin
    plot_printpars.plotsize(0)=0.
    plot_printpars.plotsize(1)=0.
    plot_printpars.plotsize(2)=0.
    plot_printpars.plotsize(3)=0.
endif else begin
    if plot_printpars.plotsize(0) le 0 then  $
      plot_printpars.plotsize(0)=7.
    if plot_printpars.plotsize(1) le 0 then  $
      plot_printpars.plotsize(1)=5.
endelse

format='(F6.3)'
widget_control,s.w.xsize,set_value=string(plot_printpars.plotsize(0),format=format)
widget_control,s.w.ysize,set_value=string(plot_printpars.plotsize(1),format=format)
widget_control,s.w.xoffset,set_value=string(plot_printpars.plotsize(2),format=format)
widget_control,s.w.yoffset,set_value=string(plot_printpars.plotsize(3),format=format)

widget_control,s.w.file_name,set_value=plot_printpars.file_name

widget_control,s.w.print,set_button=plot_printpars.print

widget_control,s.w.command,set_value=plot_printpars.command
widget_control,s.w.command_base,sensitive=plot_printpars.print

;; restore state
widget_control,id,set_uvalue=s,/no_copy
return
end

;----procedure to create and manage the plot_print widget-----------------------
;
pro plot_print,plot_procedure,printpars=printpars,group=group

common plot_print,plot_printpars

if n_params() ne 1 then message,'usage - plot_print,plot_procedure'

procedure=plot_procedure

;; create default printing settings:
if n_elements(plot_printpars) ne 1 then begin
    plot_printpars={ $
                     device:0, $ ; 0=PS, 1=HP, 2=PCL, 3=CGM
                     psfont:1, $ ; 0=use vector fonts, 1=use PS fonts.
                     color:1, $ ; 0=no color, 1=color
                     depth:2, $ ; PS resolution: 0=>1, 1=>2, 2=>4, 4=>8.
                     orient:0, $ ; 0=portrait, 1=landscape
                     size:1, $  ; 0=small, 1=large, 2=custom
                     plotsize:[0.,0.,0.,0.], $ ; default [xsize,ysize,xoff,yoff]
                     file_name:'idl.ps', $ ; default file name
                     print:1, $ ; 0=print only to file, 1=send to printer.
                     command:'lpr -Plp'} ; print command
    ;; try to get better printer commands if possible:
    case !version.os of
        'hp-ux': plot_printpars.command='lp -dlp'
        'Win32': plot_printpars.command='PRINT /D:lpt1'
        else: ;;
    endcase
endif

;; if the passed printer settings are valid, then use them:

if n_elements(printpars) eq 1 then begin
    sz=size(printpars)
    if sz(2) ne 8 then begin
        message,'printpars must be a structure variable'
        return
    endif
    tags=tag_names(printpars)
    if n_elements(tags) ne 10 then begin
        message,'printpars structure defined incorrectly'
        return
    endif
    plot_printpars.device=(printpars.device > 0) < 3
    plot_printpars.psfont=(printpars.psfont > 0) < 1
    plot_printpars.color=(printpars.color > 0) < 1
    plot_printpars.depth=(printpars.depth > 0) < 3
    plot_printpars.orient=(printpars.orient > 0) < 1
    plot_printpars.size=(printpars.size > 0) < 2
    plot_printpars.plotsize=printpars.plotsize
    plot_printpars.file_name=printpars.file_name
    plot_printpars.print=(printpars.print > 0) < 1
    plot_printpars.command=printpars.command
endif

;; create the widget

w={ $
    base:0L, $
    status:0L, $
    base1:0L, $
    device_base:0L, $
    device_label:0L, $
    device:0L, $
    font:0L, $
    vefont_button:0L, $
    psfont_button:0L, $
    color_depth_base:0L, $
    color:0L, $
    depth_base:0L, $
    depth_label:0L, $
    depth:0L, $
    orient_size_base:0L, $
    orient_base:0L, $
    orient_label:0L, $
    orient:0L, $
    size_base:0L, $
    size_label:0L, $
    size:0L, $
    plotsize_base:0L, $
    xsize:0L, $
    ysize:0L, $
    xoffset:0L, $
    yoffset:0L, $
    base2:0L, $
    file_base:0L, $
    file_label:0L, $
    file_name:0L, $
    print_base:0L, $
    print_label:0L, $
    print:0L, $
    command_base:0L, $
    command_label:0L, $
    command:0L, $
    button_base:0L, $
    done_button:0L, $
    print_button:0L}

if keyword_set(group) eq 0 then group=0L
if (strmid(!version.release,0,1) eq '5') and $
  widget_info(long(group),/valid) then $
  w.base=widget_base(title='Printer Settings',/column,/modal,group=group) $
else w.base=widget_base(title='Printer Settings',/column)

; status widget must be first child of base:
w.status=widget_label(w.base,value=string(replicate(32B,80)),/align_left)

w.base1=widget_base(w.base,/column,/frame)

w.device_base=widget_base(w.base1,/row)
w.device_label=widget_label(w.device_base,value='Output Device:')
w.device=widget_droplist(w.device_base, $
                         value=['PostScript Printer','HP Plotter', $
                                'PCL Printer','CGM File'], $
                         uvalue='device')
widget_control,w.device,set_droplist=plot_printpars.device

w.font=cw_bgroup(w.base1,['Vector Fonts','PostScript Fonts'], $
                 /row,/exclusive,/no_release,ids=ids,uvalue='font', $
                 set_value=plot_printpars.psfont)
w.vefont_button=ids(0)
w.psfont_button=ids(1)

w.color_depth_base=widget_base(w.base1,/row)

w.color=widget_droplist(w.color_depth_base, $
                        value=['B+W','Color'], $
                        uvalue='color')
widget_control,w.color,set_droplist=plot_printpars.color

w.depth_base=widget_base(w.color_depth_base,/row)
w.depth_label=widget_label(w.depth_base,value='Bits/Pixel:')
w.depth=widget_droplist(w.depth_base, $
                        value=['1','2','4','8'], $
                        uvalue='depth')

w.orient_size_base=widget_base(w.base1,/row)

w.size_base=widget_base(w.orient_size_base,/row)
w.size_label=widget_label(w.size_base,value='Size:')
w.size=widget_droplist(w.size_base, $
                       value=['Small','Large','Custom'], $
                       uvalue='size')

w.orient_base=widget_base(w.orient_size_base,/row)
w.orient_label=widget_label(w.orient_base,value='Orientation:')
w.orient=widget_droplist(w.orient_base, $
                         value=['Portrait','Landscape'], $
                         uvalue='orient')

w.plotsize_base=widget_base(w.base1,/col,/frame)
row=widget_base(w.plotsize_base,/row)
w.xsize=cw_field(row,/return_events,/row,title='X Size [in.]:', $
                 /string,xsize=5,ysize=1,uvalue='xsize', $
                 /no_return)
w.xoffset=cw_field(row,/return_events,/row,title='X Offset [in.]:', $
                   /string,xsize=5,ysize=1,uvalue='xoffset', $
                   /no_return)

row=widget_base(w.plotsize_base,/row)
w.ysize=cw_field(row,/return_events,/row,title='Y Size [in.]:', $
                 /string,xsize=5,ysize=1,uvalue='ysize', $
                 /no_return)
w.yoffset=cw_field(row,/return_events,/row,title='Y Offset [in.]:', $
                   /string,xsize=5,ysize=1,uvalue='yoffset', $
                   /no_return)

w.base2=widget_base(w.base,/column,/frame)

w.file_base=widget_base(w.base2,/row)
w.file_label=widget_label(w.file_base,value='File Name: ')
w.file_name=widget_text(w.file_base,/editable,xsize=20,ysize=1, $
                        uvalue='file',/all_event)

w.print_base=widget_base(w.base2,/col,/nonexclusive)
w.print=widget_button(w.print_base, $
                      value='Send File To Printer', $
                      uvalue='print')

w.command_base=widget_base(w.base2,/row)
w.command_label=widget_label(w.command_base,value='Printer Command: ')
w.command=widget_text(w.command_base,/editable,xsize=25,ysize=1, $
                      uvalue='command',/all_event)

w.button_base=widget_base(w.base,/row,/align_center)
w.print_button=widget_button(w.button_base, $
                             value='Print',uvalue='print_button')
w.done_button=widget_button(w.button_base, $
                            value='Done',uvalue='done_button')

; stuff state into base widget uvalue:
s={w:w,procedure:procedure}
widget_control,w.base,set_uvalue=s

plot_print_ud,w.base            ; update widget
widget_control,/realize,w.base  ; realize widget

; call xmanager - need /modal in order to return changed settings.
if (strmid(!version.release,0,1) eq '5') and  $
  widget_info(long(group),/valid) then $
  xmanager,'plot_print',w.base,event_handler='plot_print_ev', $
  group=group $
else xmanager,'plot_print',w.base,event_handler='plot_print_ev', $
  group=group,/modal

; return changed settings
printpars=plot_printpars        
return
end



