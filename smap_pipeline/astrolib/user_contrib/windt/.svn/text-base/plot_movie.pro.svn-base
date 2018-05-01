;+
; NAME:
; 
;	PLOT_MOVIE
;
; PURPOSE:
;
;	Display an animated sequence of X-Y plots in a window.
;
; CALLING SEQUENCE:
;
;	PLOT_MOVIE,X,Y[,Y1,Y2,Y3,Y4]
; 
; INPUT PARAMETERS:
; 
;	X = N_x-element array, common to all Y vectors
;	
;	Y = 2D array, N_x x N_plots
; 
; OPTIONAL INPUT PARAMETERS:
;
;       Y1, Y2, Y3, Y4 - additional Y arrays to be overplotted; these
;                        must all have the same dimensions as Y.
;
; KEYWORD PARAMETERS:
;
;       XRANGE- A two-element vector specifying the xrange of the
;               plot.  Default = [0, max(x)]
;
;       YRANGE- A two-element vector specifying the yrange of the
;               plot. Default = [0 < min(y)*1.05,max(y)*1.05]
;
;       COLOR- array of colors for each Y plot
;
;       LINESTYLE - array of linestyles for each Y plot
;
;       THICK - array of thicknesses for each Y plot
;
;       _EXTRA - This keyword is use to pass additional parameters to
;                the plot command.
;
; EXAMPLE:
;
;      Make a movie of two 'travelling' sin waves:
;
;      X=VECTOR(0.,100.,100)
;      Y=FLTARR(100,30)
;      for i=0,29 do Y(*,i)=sin((x+i*!pi)/!pi)
;      Y1=-Y
;      PLOT_MOVIE,X,Y,Y1
;
; MODIFICATION HISTORY:
;
; 	Written by:	David L. Windt, Bell Labs, April 1994
;
; 	windt@bell-labs.com
;-

pro plot_movie,x,y,y1,y2,y3,y4, $
        xrange=xrange,yrange=yrange, $
        color=color,linestyle=linestyle,thick=thick,_extra=e

; First, read in new data if desired or neccessary:

sx=size(x)                      ; get sizes of data vectors
sy=size(y)

if sx(1) ne sy(1) then message,'X and Y have incompatible dimensions.'

; set y-axis plot range for a and f plots.

if keyword_set(xrange) eq 0 then xrange=[0,max(x)]
if keyword_set(yrange) eq 0 then yrange=[0 < min(y)*1.05,max(y)*1.05]

; make sure the plotting system variables are set OK...

!p.multi=0
!x.range=0
!y.range=0

loadct,0
tek_color

n_plots=n_params()-1
if keyword_set(color) eq 0 then color=indgen(n_plots)+2
if keyword_set(linestyle) eq 0 then linestyle=intarr(n_plots)+0
if keyword_set(thick) eq 0 then thick=intarr(n_plots)+1

device,set_write_mask=255       ; set write mask so all bits are used.

; make the a plot sans data:
plot,x,y(*,0),/nodata,xrange=xrange,yrange=yrange,_extra=e

; initialize the counter.
i=-1

; initialize the delay factor.
delay=0.001

; Now set write mask so only lowest 64 bits are used. This will ensure that
; plot axes are not erased.
device,set_write_mask=2^5-1	

print,"Type 'q' to quit, 'f' to go faster, and 's' to go slower..."

while 1 do begin                ; loop forever...
    k=strupcase(get_kbrd(0))    ; grab the next keystroke.
    i=(i+1) mod sy(2)           ; increment the counter.
    erase                       ; erase old data.

                                ; draw the new data...

    oplot,x,y(*,i),color=color(0),linestyle=linestyle(0),thick=thick(0)
    if n_plots ge 2 then $
      oplot,x,y1(*,i),color=color(1),linestyle=linestyle(1),thick=thick(1)
    if n_plots ge 3 then $
      oplot,x,y2(*,i),color=color(2),linestyle=linestyle(2),thick=thick(2)
    if n_plots ge 4 then $
      oplot,x,y3(*,i),color=color(3),linestyle=linestyle(3),thick=thick(3)
    if n_plots ge 5 then $
      oplot,x,y4(*,i),color=color(4),linestyle=linestyle(4),thick=thick(4)

                                ; Now check for keystrokes...

    case k of

        'Q': goto,exit          ; Quit.
        'S': begin              ; Slower.
            delay=delay*2.
            print
            print,'Delay = ',delay,' seconds.'
        end
        'F': begin              ; Faster.
            delay=delay/2.
            print
            print,'Delay = ',delay,' seconds.'
        end
        else:                   ;
    endcase

    wait,delay

endwhile

exit: device,set_write_mask=255 ; restore the write mask.
return
end
