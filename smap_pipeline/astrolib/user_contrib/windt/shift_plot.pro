;+
; NAME:
;
;       SHIFT_PLOT
;       
; PURPOSE:
; 
;       Interactively slide a previously plotted array using the mouse.
;   
; CALLING SEQUENCE:
; 
;       SHIFT_PLOT,X[,Y,SHIFT=SHIFT]
;   
; INPUTS:
; 
;        X,Y - array variables
;   
; KEYWORD PARAMETERS:
; 
;        Same as for oplot
;   
; OPTIONAL OUTPUT PARAMETERS:
; 
;        SHIFT - the shift along the x-axis
;   
; PROCEDURE:
; 
;   MENUS is used to get input. The previously plotted array is first
;   erased, then oplot'ed, with the incremental shift.
;   
; MODIFICATION HISTORY:
; 
;   David L. Windt, Bell Labs, February, 1990
;   windt@bell-labs.com
;-
pro shift_plot,x,y,shift=shift,color=color,linestyle=linestyle,noclip=noclip,$
         psym=psym,symsize=symsize,t3d=t3d,thick=thick,xrange=xrange,$
         xstyle=xstyle,xtype=xtype,yrange=yrange,ystyle=ystyle,ytype=ytype,$
         zrange=zrange,zstyle=zstyle,zvalue=zvalue

on_error,2                      ; Return to caller on error.

if n_params() eq 1 then begin
    xp=findgen(n_elements(x))
    yp=x
endif else begin
    if n_elements(x) ne n_elements(y) then begin
        print,'shift_plot: X and Y dimensions inconsistent.'
        print,'shift_plot: Aborted.
        retall
    endif
    xp=x
    yp=y
endelse

if keyword_set(color) eq 0 then color=!p.color
if keyword_set(linestyle) eq 0 then linestyle=!p.linestyle
if keyword_set(noclip) eq 0 then noclip=!p.noclip
if keyword_set(psym) eq 0 then psym=!p.psym
if keyword_set(symsize) eq 0 then symsize=1
if keyword_set(t3d) eq 0 then t3d=!p.t3d
if keyword_set(thick) eq 0 then thick=!p.thick
if keyword_set(xrange) eq 0 then xrange=!x.range
if keyword_set(xstyle) eq 0 then xstyle=!x.style
if keyword_set(xtype) eq 0 then xtype=!x.type
if keyword_set(yrange) eq 0 then yrange=!y.range
if keyword_set(ystyle) eq 0 then ystyle=!y.style
if keyword_set(ytype) eq 0 then ytype=!y.type
if keyword_set(zrange) eq 0 then zrange=!z.range
if keyword_set(zstyle) eq 0 then zstyle=!z.style
if keyword_set(zvalue) eq 0 then zvalue=0

choices=['Shift Left','Shift Right','Restore','Quit']
dum=strarr(4)

shift=0                         ; initialize shift
choice=menus(0,['Shift Left','Shift Right','Restore','Quit'],strarr(4))
while 1 do begin
    choice=menus(1,['Shift Left','Shift Right','Restore','Quit'],strarr(4))
;    tvcrs,.5,.5,/norm
    case choice of
        0:  begin               ; shift left...
            oplot,xp,shift(yp,shift),color=!p.background,linestyle=linestyle, $
              noclip=noclip,psym=psym,symsize=symsize,t3d=t3d,thick=thick, $
              zvalue=zvalue
            shift=shift-1       ; decrement shift.
            oplot,xp,shift(yp,shift),color=color,linestyle=linestyle, $
              noclip=noclip,psym=psym,symsize=symsize,t3d=t3d,thick=thick, $
              zvalue=zvalue
        end
        1:  begin               ; shift right...
            oplot,xp,shift(yp,shift),color=!p.background,linestyle=linestyle, $
              noclip=noclip,psym=psym,symsize=symsize,t3d=t3d,thick=thick, $
              zvalue=zvalue
            shift=shift+1       ; increment shift.
            oplot,xp,shift(yp,shift),color=color,linestyle=linestyle, $
              noclip=noclip,psym=psym,symsize=symsize,t3d=t3d,thick=thick, $
              zvalue=zvalue
        end
        2:  begin               ; restore
            oplot,xp,shift(yp,shift),color=!p.background,linestyle=linestyle, $
              noclip=noclip,psym=psym,symsize=symsize,t3d=t3d,thick=thick, $
              zvalue=zvalue
            shift=0             ; increment shift.
            oplot,xp,shift(yp,shift),color=color,linestyle=linestyle, $
              noclip=noclip,psym=psym,symsize=symsize,t3d=t3d,thick=thick, $
              zvalue=zvalue
        end
        3:  begin
                                ; erase menu...
            nst1 = 1./4.
            ych =  1.0 * !d.y_ch_size / !d.y_vsize ;Char ht in normal units
            boxx = [0.,0.,nst1-0.05,nst1-0.05] ;Box for highlight
            boxy = [0.,ych,ych,0.]
            polyfill,(3. * nst1) + boxx,boxy,col=!p.background,/norm
            for i=0,3 do xyouts,i*nst1,0.,choices(i),/norm,/noclip, $
              color=!p.background
            tvcrs,0
            return
        end
        else:                   ;
    endcase
endwhile	

end




