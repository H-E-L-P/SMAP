;+
; NAME:
;
;      ROI_WIDTH
;      
; PURPOSE:
; 
;	Measure the width of a region of curve that has been previously
;       plotted. The region is defined to be within ymin and ymax of a
;       digitized region of the curve.
;	
; CALLING SEQUENCE:
; 
;	Result=ROI_WIDTH(XAXIS,YAXIS)
;	
; INPUTS:
; 
;       XAXIS - the x axis variable which has been plotted.
;	
;       YAXIS - the y axis variable which has been plotted.
;	
; KEYWORD PARAMETERS:
; 
;       YMIN - minimum value of digitized region of interest.
;	
;       YMAX - maximum value of digitized region of interest.
;	
;       NOHIGHLIGHT - set to inhibit highlighting the region of 
;                     interest.
;		
;       H_COLOR - the color index for highlighting the region of 
;                 interest. Default is 7 (Yellow).
;		
;       H_THICK - the thickness for highlighting the region of
;                 interest.
;		
;       NOLABEL - set to inhibit labelling fwhm.
;	
;       MANUAL - set to disable automatic location selection for
;                labels.
;	    
;       L_HEADER - string specifying the label header. Default-''.
;	
;       L_COLOR - color index for the label.
;	
;       L_FORMAT - format string for label (eg. '(f4.2)').
;	
;       UNITS - string specifying units along x axis.
;	
;       CHARSIZE - size of label text.
;	
;       PSYM - psym
;   
; OUTPUTS:
;
;       Result - the full-with-half-max of the region of interest of
;                the curve, in x-axis data units.
;		
; OPTIONAL OUTPUT PARAMETERS:
; 
;       ROI - the subscripts of the digitized region of interest.
;	
;       WIDTH_ROI - the subscripts of the region between the ymin and
;                   ymax points.
;	    
;       LINE_PTS - a 4-element array containing the coordinates of the
;                  line drawn on the plot: [x0,x1,y0,y1]
;	    
;       LABEL - the label for the plot.
;	
;       L_POS - a two element array containing the x,y coordinates of
;               the label, in data coords.
;	    
;		
; SIDE EFFECTS:
; 
;       TEK_COLOR is used to load in the tektronix colors.
;       The region of interest of the curve is highlighted.
;       The width is labelled.
;	
; RESTRICTIONS:
; 
;	The data must be plotted prior to calling ROI_WIDTH
;	
; PROCEDURE:
; 
;	The user is asked to digitize the endpoints of the
;	region of interest with the mouse.  The region is
;	highlighted, and the width is labelled.
;	
; MODIFICATION HISTORY:
; 
;	D. L. Windt, Bell Laboratories, October 1990.
;	windt@bell-labs.com
;	
;-
function roi_width,xaxis,yaxis,ymin=ymin,ymax=ymax, $
            roi=roi,width_roi=width_roi,psym=psym,line_pts=line_pts, $
            nohighlight=nohighlight,h_color=h_color,h_thick=h_thick, $
            nolabel=nolabel,manual=manual,units=units, $
            charsize=charsize,l_header=l_header,l_color=l_color,label=label, $
            l_pos=l_pos,l_format=l_format,range=range

on_error,0
if n_params() ne 2 then message,'usage: result=roi_width(x,y)'

; get the region of interest...

if keyword_set(h_color) eq 0 then h_color=7
if keyword_set(h_thick) eq 0 then h_thick=!p.thick
if keyword_set(psym) eq 0 then psym=!p.psym


if keyword_set(range) eq 0 then $
  roi=get_roi(xaxis,yaxis,nohighlight=keyword_set(nohighlight), $
              h_color=h_color,h_thick=h_thick,psym=psym) else roi=range
xroi=float(xaxis(roi))
yroi=float(yaxis(roi))

n_roi=n_elements(roi)		; get number of elements in roi.

; ymin and ymax
if n_elements(ymin) eq 0 then ymin=.1
if n_elements(ymax) eq 0 then ymax=.9

y1=min(yroi,max=y2)             ; get min and max of roi.
; get width_roi
width_roi=where((yroi ge (ymin*(y2-y1)+y1)) and (yroi le (ymax*(y2-y1)+y1)))
n=width_roi(n_elements(width_roi)-1)-width_roi(0)+1
width_roi=findgen(n)+width_roi(0)

; get width in data coordinates.
n_wroi=n_elements(width_roi)    ; number of points in width roi.
width=n_wroi*(max(xaxis)-min(xaxis))/n_elements(xaxis) ; in data coords.

if keyword_set(nolabel) eq 0 then begin ; label the line...
    if keyword_set(l_color) eq 0 then l_color=1	
    if keyword_set(l_header) eq 0 then l_header=''
    if keyword_set(units) eq 0 then units=''
    if keyword_set(charsize) eq 0 then charsize=1 > !p.charsize

                                ; plot a line across the roi...
    x1=xroi(width_roi(0)) & x2=xroi(width_roi(n_wroi-1))
    y5=.5*(ymin+ymax)*(y2-y1)+y1
    line_pts=[x1,x2,y5,y5]
    plots,[x1,x2],[y5,y5],color=l_color
    plots,[x1,x1],[y1,y2],color=l_color
    plots,[x2,x2],[y1,y2],color=l_color

                                ; make label...
    if keyword_set(l_format) eq 0 then $
      label=l_header+strtrim(string(width),2)+' '+units $
    else label=l_header+strtrim(string(width,format=l_format),2)+' '+units
                                ; make label position and place the label...
    if keyword_set(manual) then begin
        make_arrow,label,x=xpos,y=ypos,color=l_color,size=charsize 
        l_pos=[xpos,ypos]
    endif else begin
        l_pos=[.1*(x2-x1)+x2,y5]
        xyouts,l_pos(0),l_pos(1),label,color=l_color,size=charsize
    endelse
endif
width_roi=roi(0)+width_roi

return,width
end

