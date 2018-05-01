;+
; NAME:
;
;        GET_ROI
;        
; PURPOSE:
;
;       Get a region-of-interest of a previously plotted curve.
;       
; CALLING SEQUENCE:
; 
;	Result=GET_ROI(XAXIS,YAXIS)
;	
; INPUTS:
; 
;         XAXIS = the x axis variable which has been plotted.
;         
;         YAXIS = the y axis variable which has been plotted.
;         
; KEYWORD PARAMETERS:
;
;         NOHIGHLIGHT - set to disable highlighting the region of
;                       interest.
;			
;         H_COLOR - the color index for highlighting the region of
;                   interest. Default is 7 (Yellow).
;			
;         H_THICK - the thickness for highlighting the region ; of
;                  interest.
;			
;         PSYM - PSYM.
;         
; OUTPUTS:
;
;         Result = the array of subscripts of the roi.
;         
; SIDE EFFECTS:
; 
;       TEK_COLOR is used to load in the tektronix colors.
;       The region of interest of the curve is highlighted.
;       
; PROCEDURE:
; 
;       The user is asked to digitize the endpoints of the region of
;       interest with the mouse using GET_PT.  The region is
;       highlighted (unless nohighlight is set.)
;		
; MODIFICATION HISTORY:
; 
;       D. L. Windt, Bell Laboratories, November 1989
;
;       windt@bell-labs.com
;-
function get_roi,xaxis,yaxis,psym=psym, $
            nohighlight=nohighlight,h_color=h_color,h_thick=h_thick

on_error,2

get_pts:                        ; get the endpoints...
s_pt=get_pt(xaxis,yaxis,nohighlight=keyword_set(nohighlight), $
            message='Digitize the start of the region of interest:')
wait,.1                         ; Openwindows is flakey.
e_pt=get_pt(xaxis,yaxis,nohighlight=keyword_set(nohighlight), $
            message='Digitize the end of the region of interest:',/noinit)
if e_pt eq s_pt then begin
    print,'get_roi: Two distinct points must be digitized.'
    goto,get_pts
endif    

roi=indgen(abs(e_pt-s_pt)+1)+ (s_pt<e_pt) ; define roi.
xroi=xaxis(roi)
yroi=yaxis(roi)

if keyword_set(nohighlight) eq 0 then begin ; highlight the roi.
    if keyword_set(h_color) eq 0 then h_color=7 ; use Yellow.
    if keyword_set(h_thick) eq 0 then h_thick=2 ; triple thickness.
    if keyword_set(psym) eq 0 then psym=!p.psym
    oplot,xroi,yroi,color=h_color,thick=h_thick,psym=psym
endif

return,roi
end
