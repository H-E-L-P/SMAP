;+
; NAME:
;
;       SP
;
; PURPOSE:  
;
;       Execute SET_PLOT, and optionally some handy settings.
;
; CALLING SEQUENCE:
;
;       SP[,DEVICE,N_PLOTS]
;
; OPTIONAL INPUTS:
; 
;	    DEVICE = 0 for set_plot,'PS' 
;                    1 for set_plot,'HP'
;                    2 for set_plot,'PCL'
;                    3 for set_plot,'X'
;                    4 for set_plot,'MAC'
;                    5 for set_plot,'WIN'
;                    6 for set_plot,'SUN'
;                    7 for set_plot,'TEK'
;                    8 for set_plot,'CGM'
; 
;           if DEVICE is not set, the graphics device will be set
;           to the platform-dependent default.
;
;           N_PLOTS = 1 for !p.multi=0
;                     2 for !p.multi=[0,1,2]
;                     3 for !p.multi=[0,1,3]
;                     4 for !p.multi=[0,2,2]
;                     
; KEYWORD PARAMETERS:
; 
;           SMALL - Set to make a small plot.
;           
;           LANDSCAPE - Set for landscape mode when device=0,1, or 2.
;           
;           FULL_PAGE - Set for full page plotting when device=0, 1,
;                       or 2. Only has an effect when in portrait mode
;                       (landscape=0) for PS and PCL devices.
;                       FULL_PAGE is set automatically if N_PLOTS is
;                       greater than 1.
;                       
;           HARDWARE - Set for hardware fonts.
;           
;           FILE - Name of output file.
;           
;           ISOTROPIC - Set for isotropic (equal x and y) scaling.
;           
;           COLOR - Set to enable color for PS and PCL devices.
;
;           PLOTSIZE - A four-element array specifying the
;                      [XSIZE,YSIZE,XOFFSET,YOFFSET] keywords (in
;                      INCHES) to the DEVICE procedure.  If PLOTSIZE
;                      is set, then the SMALL and FULL_PAGE keywords
;                      are ignored.  If PLOTSIZE is not set, then
;                      default values are used for these parameters
;                      that make decent-looking plots on 8-1/2 x 11"
;                      paper.
;
; MODIFICATION HISTORY:
; 
;       David L. Windt, Bell Labs November 1989
;       Added DEVICE=4, November 1990.
;       Added ISOTROPIC keyword, August 1991.
;       Added COLOR keyword, Sept 1991.
;       Added pcl support, completely changed device<->number mapping,
;             and changed functionality of small/full_page/landscape/size
;             keywords, May 1997.
;
;       DLW, September 1997: On Unix platforms, if DEVICE is not
;       set, the graphics device is set to 'X' if the IDL_DEVICE
;       environment variable is not defined.
;
;       DLW, January 1998: Added support for CGM graphics; this
;       routine will do nothing more than issue the SET_PLOT,'CGM'
;       command, but is included for compatability with the PLOT_PRINT
;       routine.  When using the CGM device, you will likely want to
;       set the color table entry for !p.color to black; otherwise
;       you'll get a white plot on a white background.
;
;       Also, fixed bug that caused graphics output to anything but PS
;       to fail!  (Doh!)
;
;       windt@bell-labs.com
;       
;-
pro sp,dev,n_plots,landscape=lan,full_page=full, $
       hardware=hard,small=small, $
       file=file,isotropic=isotropic,color=color, $
       plotsize=plotsize

on_error,2                      ; return to caller on error.

if n_params() lt 1 then dev=-1
case dev of                     ; select device.
    0: device='ps'
    1: device='hp'
    2: device='pcl'
    3: device='x'
    4: device='mac'
    5: device='win'
    6: device='sun'
    7: device='tek'
    8: device='cgm'
    else: begin
        case 1 of
            !version.os_family eq 'unix': begin
                device=getenv("IDL_DEVICE")
                if device eq '' then device='x'
            end
            !version.os eq 'MacOS': device='mac'
            !version.os eq 'Win32': device='win'
            else: device='x'
        endcase
    end
endcase

set_plot,device
if keyword_set(file) then device,file=file

if keyword_set(isotropic) then begin
    xmax=float(!d.x_size)
    ymax=float(!d.y_size)
    if xmax gt ymax then begin
        scale=ymax/xmax
        !p.region=[(1-scale)/2.,0.,1-(1-scale)/2.,1.]
    endif else begin
        scale=xmax/ymax
        !p.region=[0.,(1-scale)/2.,1.,scale-(1-scale)/2.]
    endelse
endif else !p.region=0

if n_params() lt 2 then n_plots=1 ; default is 1 plot per page.

case n_plots of
    1: !p.multi=0
    2: !p.multi=[0,1,2]
    3: !p.multi=[0,1,3]
    4: !p.multi=[0,2,2]
    else: !p.multi=0
endcase

;; default is software fonts
if keyword_set(hard) then !p.font=0 else !p.font=-1

;; use full page for PS, PCL, and CGM:
if ((device eq 'ps') or (device eq 'pcl') or (device eq 'cgm') ) $
  and n_plots gt 1 then full=1  

if n_elements(plotsize) eq 4 then begin
    xsi=plotsize(0)
    ysi=plotsize(1)
    xoff=plotsize(2)
    yoff=plotsize(3)
endif

case 1 of 
    (device eq 'ps') or (device eq 'pcl'): begin
        device,color=keyword_set(color)
        if (device eq 'ps') then device,/iso
        if n_elements(plotsize) eq 0 then begin
            if keyword_set(lan) then begin
                if keyword_set(small) then begin 
                    xsi=7.
                    ysi=5.
                    xoff=1.75
                    yoff=9.
                endif else begin
                    xsi=10.
                    ysi=7.
                    xoff=.75
                    yoff=10.5
                endelse
            endif else begin
                if keyword_set(full) then begin
                    if keyword_set(small) then begin
                        xsi=5.
                        ysi=7.
                    endif else begin
                        xsi=7.
                        ysi=10.
                    endelse
                    xoff=.75
                    yoff=.5
                endif else begin
                    if keyword_set(small) then begin
                        xsi=5.
                        ysi=3.5
                    endif else begin
                        xsi=7.
                        ysi=5.
                    endelse
                    xoff=.75
                    yoff=5.5
                endelse
            endelse
        endif
        device,lan=keyword_set(lan),por=(keyword_set(lan) eq 0), $
          /inc,xsi=xsi,ysi=ysi,xoff=xoff,yoff=yoff
    end
    (device eq 'cgm'): begin                 
    end
    device eq 'hp': begin                 
        if n_elements(plotsize) eq 0 then begin
            if keyword_set(lan) then begin
                if keyword_set(small) then begin 
                    xsi=7.
                    ysi=5.
                    xoff=1.25
                    yoff=1.25
                endif else begin
                    xsi=9.5
                    ysi=7.
                    xoff=.25
                    yoff=.5
                endelse
            endif else begin
                if keyword_set(small) then begin
                    xsi=6.
                    ysi=8.
                    xoff=.635
                    yoff=1.125
                endif else begin
                    xsi=7.
                    ysi=10.
                    xoff=.125
                    yoff=.125
                endelse
            endelse
        endif
        device,lan=keyword_set(lan),por=(keyword_set(lan) eq 0), $
          /inc,xsi=xsi,ysi=ysi,xoff=xoff,yoff=yoff
    end
    else:;;
endcase
return
end
