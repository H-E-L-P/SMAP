;+
; NAME:
;
;       GET_PT
;       
; PURPOSE:
; 
;        Digitize a point on a previously plotted curve, and return
;        the corresponding array element.
;
; CALLING SEQUENCE:
; 
;        Result = GET_PT(XAXIS,YAXIS,XPOINT,YPOINT)
;        
; INPUTS:
; 
;        XAXIS - the x axis vector which was used to make the plot.
;        
;        YAXIS - the y axis vector which was used to make the plot.
;		
; KEYWORD PARAMETERS:
; 
;        NOHIGHLIGHT - set to inhibit putting a red mark on the curve
;                      at the digitized point.
;			
;        MESSAGE - a string to print as the message to the user.
;                  Default = 'Digitize a point: '
;			
;        NOINIT - set to inhibit placing the cursor in the center of
;                 the plot window.
;		    
; OUTPUTS:
; 
;        Result - The array subscript of the digitized point.
;
; OPTIONAL OUTPUT PARAMETERS:
; 
;        XPOINT, YPOINT - the digitized points.
;
; SIDE EFFECTS:
; 
;        A mark is drawn on the plot at the digitized point.
;
; PROCEDURE:
;
;        The user is asked to digitize a point on the curve using the
;        mouse.  The VALUE_TO_INDEX function is used to find the
;        closest array element.
;		
; MODIFICATION HISTORY:
; 
;        D. L. Windt, Bell Laboratories, November 1989
;        Feb. 1991, Removed call to TEK_COLOR
;        Mar. 1997, replaced index search code with call to
;        VALUE_TO_INDEX function.
;
;        windt@bell-labs.com
;-
function get_pt,xaxis,yaxis,xpoint,ypoint,nohighlight=nohighlight, $
            message=message,noinit=noinit
on_error,2

if keyword_set(message) then print,message else print,'Digitize a point: '

if keyword_set(noinit) eq 0 then tvcrs,.5,.5,/norm ; move cursor to window.
cursor,xpt,ypt,/data            ; digitize the point.
index=value_to_index(xaxis,xpt) ; find the index
xpoint=xaxis(index)             ; get the x and y values of the point.
ypoint=yaxis(index)

if keyword_set(nohighlight) eq 0 then $
  plots,[xpoint,xpoint],[ypoint,ypoint],psym=2,symsize=.5,color=2

return,index
end
