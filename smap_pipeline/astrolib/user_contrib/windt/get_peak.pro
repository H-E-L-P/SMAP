;+
; NAME:
;
;        GET_PEAK
;        
; PURPOSE:
; 
;        Interactively find the local maximum of a previously plotted
;        curve, and indicate it on the plot.
;        
; CALLING SEQUENCE:
; 
;        Result=GET_PEAK(XAXIS,YAXIS)
;		
; INPUTS:
; 
;         XAXIS = the x axis variable which has been plotted.
;         
;         YAXIS = the y axis variable which has been plotted.
;         
; KEYWORD PARAMETERS:
; 
;         COLOR - the color index for marking the local maximum.
;         
;         NOMARK - set to disable marking the location of the peak.
;		    
;         NOHIGHLIGHT - set to disable highlighting the region of
;                       interest.
;			
;         H_COLOR - the color index for highlighting the region of
;                   interest. Default is 7 (Yellow).
;			
;         H_THICK- the thickness for highlighting the region ; of
;                  interest.
;			
;         PRINT - set to print the x,y values of the peak.
;		
; OUTPUTS:
;
;         Result = the array subscript of the local max.
;         
; SIDE EFFECTS:
; 
;         TEK_COLOR is used to load in the tektronix colors.
;         The region of interest of the curve is highlighted.
;         A vertical line is drawn through the local maximum.
;		
; PROCEDURE:	
;
;        The user is asked to digitize the endpoints of the region of
;        interest with the mouse using GET_ROI.
;		
; MODIFICATION HISTORY:
; 
;        D. L. Windt, Bell Laboratories, February 1990.
;
;        windt@bell-labs.com
;-
function get_peak,xaxis,yaxis,color=color, $
            nomark=nomark,nohighlight=nohighlight, $
            h_color=h_color,h_thick=h_thick, $
            print=print
on_error,2

if keyword_set(color) eq 0 then color=!p.color
if keyword_set(h_color) eq 0 then h_color=7
if keyword_set(h_thick) eq 0 then h_thick=3
roi=get_roi(xaxis,yaxis,nohighlight=keyword_set(nohighlight), $
            h_color=h_color,h_thick=h_thick)
x=xaxis(roi) & y=yaxis(roi) 
max=max(y)

if keyword_set(nomark) eq 0 then begin
    if !y.type then plots,[x(!c),x(!c)],10^!y.crange else $
      plots,[x(!c),x(!c)],!y.crange,color=color
    plots,[x(!c),x(!c)],[y(!c),y(!c)],psym=2,symsize=.5,color=2
endif
if keyword_set(print) then $
  print,'X: '+strtrim(x(!c),2)+', Y: '+strtrim(y(!c),2)
peak=roi(0)+!c
!c=0
return,peak
end
