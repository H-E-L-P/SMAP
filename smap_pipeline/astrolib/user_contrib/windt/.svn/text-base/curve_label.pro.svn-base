;+
; NAME:
;       CURVE_LABEL
;
; PURPOSE:
;
;       Draw labels close to one or more (up to 30) curves that have
;       been previously plotted.
;
; CALLING SEQUENCE:
;
;       CURVE_LABEL,X,Y1,[Y2,Y3,Y4,Y5],LABELS=LABELS, $
;                  [COLOR=COLOR,XPOSITION=XPOSITION,YOFFSET=YOFFSET]
; 
; INPUTS:
;       X - xaxis vector (1D array)
;       Y1 - 1st y axis vector to be labelled.
;
; OPTIONAL INPUTS:
;       Y2 - 2nd y axis vector to be labelled.
;       Y3 - 3rd y axis vector to be labelled.
;       ....etc....
;	
; KEYWORD PARAMETERS:
;
;       LABELS - String array of labels.  The size of the LABELS array
;                must match the number of y variables passed.  This
;                keyword is required.
;
;       XPOSITION - A scalar variable specifying where along the x
;                   axis the labels are to be drawn, in normal
;                   coordinates.  Default = 0.25.  Unless the
;                   NO_REPOSITION keyword is set, this might get
;                   changed if the procedure determines that the
;                   labels are too close together when drawing
;                   multiple labels.  Setting XPOSITION to -1
;                   will inhibit drawing the curve label altogether.
;
;       YOFFSET - A scalar specifying the distance in Y between the
;                 labels and the curves, in normal
;                 coordinates. Default = 0.01
;
;       NO_REPOSITION - Set this to inhibit moving the label positions
;                       if the labels are too close together when
;                       drawing multiple labels.
;
;       COLOR - Integer array of color indices for the labels.
;
;       _EXTRA - The idl _EXTRA keyword, for additional graphics
;                keywords to the XYOUTS procedure.
;       
;
; PROCEDURE:
;
;       All labels are lined up at one point along the xaxis.  The
;       procedure will try to find a position along the xaxis for
;       which the labels are not too close to each other.  If it fails
;       at this, it will just stick the labels at x=0.25 (normal).
;
; EXAMPLE:
;
;       To label a plot containing three curves, try something like
;       this:
;
;       plot,x,y1,/nodata
;       oplot,x,y1,color=2
;       oplot,x,y2,color=3
;       oplot,x,y3,color=4
;       curve_label,x,y1,y2,y3,labels=['Y1','Y2','Y3'],color=[2,3,4]
;
; MODIFICATION HISTORY:
;
;       David L. Windt, Bell Labs, March, 1997.
;       windt@bell-labs.com
;
;       February, 1998 - Added the ability to inhibit labelling
;       the curve by specifying a value of -1 for XPOSITION.
;-
pro curve_label,x,y0,y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11,y12,y13,y14,y15, $
         y16,y17,y18,y19,y20,y21,y22,y23,y24,y25,y26,y27,y28,y29, $
         labels=labels,color=color, $
         xposition=xposition,yoffset=yoffset,_extra=_extra, $
         no_reposition=no_reposition

on_error,2
cm=check_math(0,1)

n_y=n_params()-1                ; number of y curves to label.
if n_y lt 1 then message,'usage: curve,x,y1,[y2,y3,y4,y5]'

labels=[labels]

s_labels=size(labels)
if s_labels(0) ne 1 or s_labels(1) ne n_y then  $
  message,'size of labels array must equal number of curves to label'
if s_labels(2) ne 7 then message,'labels must be string array'

if n_elements(color) eq 0 then color=intarr(n_y)+!p.color
if n_elements(xposition) eq 0 then xposition=.25 ; normal coords.
if n_elements(yoffset) eq 0 then yoffset=.01 ; normal coords.

if xposition eq -1 then return

get_pt:                         ; find the y-axis points...
; convert to normal coords...
if !x.type then xaxis=!x.s(0)+!x.s(1)*alog10(x) else xaxis=!x.s(0)+!x.s(1)*x
index=value_to_index(xaxis,xposition)

; now make sure the points are separated enough...
; get positions in normal coordinates:
    
if !y.type then for i=0,n_params()-2 do $
  ee=execute('y'+strtrim(i,2)+'pos=!y.s(0)+' + $
             '!y.s(1)*alog10(y'+strtrim(i,2)+'(index))') else $
  for i=0,n_params()-2 do $
  ee=execute('y'+strtrim(i,2)+'pos=!y.s(0)+' + $
             '!y.s(1)*y'+strtrim(i,2)+'(index)') 


if (n_y gt 1) and (keyword_set(no_reposition) eq 0) then begin
; get smallest spacing between curves at this x point:
    s=1.
    for i=0,n_params()-3 do for j=i+1,n_params()-2 do $
      ee=execute('s=s < abs(y'+strtrim(i,2)+'pos-y'+strtrim(j,2)+'pos)')
    if (s lt .025) and (index ne n_elements(xaxis)-1) then begin
        xposition=xposition+.025
        goto,get_pt
    endif else if (s lt .025) and (index eq n_elements(xaxis)-1)  $
      then index=index/2.
endif

; get positions in data coords...

if !y.type then for i=0,n_params()-2 do $
  ee=execute('y'+strtrim(i,2)+'pos=10^(alog10(y'+strtrim(i,2)+ $
             '(index))+yoffset/!y.s(1))') else $
  for i=0,n_params()-2 do $
  ee=execute('y'+strtrim(i,2)+'pos=y'+strtrim(i,2)+ $
             '(index)+yoffset/!y.s(1)')

xpos=x(index)   

; label:
for i=0,n_params()-2 do  $
  ee=execute('xyouts,xpos,y'+strtrim(i,2)+ $
             'pos,labels(i),/data,color=color(i),_extra=_extra')

cm=check_math(0,1)
return
end






