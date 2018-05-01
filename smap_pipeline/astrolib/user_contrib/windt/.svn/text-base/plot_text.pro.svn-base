;+
; NAME:
; 
;        PLOT_TEXT
;
; PURPOSE:
; 
;        Add text in a box to a plot.  The text is located in one
;        of 12 possible positions (i.e., upper left corner, lower
;        right corner, etc.)
;
; CALLING SEQUENCE:
; 
;       PLOT_TEXT,TEXT_ARRAY[,POSITION=POSITION]
; 
; INPUTS:
; 
;       TEXT_ARRAY - a string array of text
;
; KEYWORD PARAMETERS:
;
;       POSITION - an integer, specifying the location of the text box:
; 
;                  0: no text is drawn
;                  1: below plot, left
;                  2: below plot, center
;                  3: below plot, right
;                  4: lower left
;                  5: lower center
;                  6: lower right
;                  7: middle left
;                  8: middle center
;                  9: middle right
;                  10: upper left
;                  11: upper center
;                  12: upper right
;         
;                  if not specified, default position=10
;
;        CHARSIZE - the charsize value for the text
;
;        COLOR - an array of colors to be used for each line of text
;
;        NOBOX - set to inhibit drawing a box around the text
;
;        BOXPADX - padding in character units, between text and box,
;                  in x. default=2.0
;
;        BOXPADY - padding in character units, between text and box,
;                  in y. default=0.5
;
;        FONT - Set to an integer from 3 to 20, corresponding to the
;               Hershey vector font sets, referring to the font used
;               to display the text.  If a font other than !3 is used
;               in the text string, then FONT should be set
;               accordingly. (Any font commands embedded in the text
;               string are ignored.)
;
;        BOXFUDGEX - A scaling factor, used to fudge the width of the
;                    box surrounding the text.  Default=1.0.
;
;        Plus all valid graphics keywords for xyouts and plots
;       
; RESTRICTIONS:
; 
;       When specifying a position of 1,2 or 3, you'll need to (a) use
;       the same charsize value for the plot and for the plot_text,
;       and (b) draw the plot with an extra ymargin(0).  i.e., set
;       ymargin(0)=7+n_elements(text_array)
;
;
; MODIFICATION HISTORY:
; 
;       David L. Windt, Bell Labs, March 1997
;       windt@bell-labs.com
;
;       October, 1997, dlw:
;
;       Now using the TEXT_WIDTH function, in order to do a somewhat
;       better job of drawing the box around the text.
;
;       NONPRINTER_SCALE keyword parameter is now obsolete.
;
;       BOXFUDGEX keyword parameter added.
;
;-

pro plot_text,text_array,position=position,charsize=charsize, $
        nobox=nobox,boxpadx=boxpadx,boxpady=boxpady,boxfudgex=boxfudgex, $
        nonprinter_scale=nonprinter_scale,_extra=_extra, $
        color=color,font=font

if n_params() ne 1 then message,'Must supply a string array of text'

if n_elements(text_array) eq 1 then text_array=[text_array]

if n_elements(color) ne n_elements(text_array) then  $
  color=intarr(n_elements(text_array))+!p.color

if n_elements(position) ne 1 then position=10
if position eq 0 then return
position=position < 12

;; get/set charsize
if n_elements(charsize) eq 0 then charsize=!p.charsize
if charsize eq 0 then charsize=1.

;; scale factor for text, relative to charsize
scale=0.85

;; scale factors for "character" sizes:
sx=float(scale*charsize)
sy=float(charsize)

;; compute number of lines of text:
n_lines=n_elements(text_array)

;; fudge factor to get right side of box close to end of text:
if n_elements(boxfudgex) ne 1 then boxfudgex=1.

;; determine maximum number of printing characters:
width=0
for i=0,n_lines-1 do width=width > text_width(text_array(i),font=font)

;; width of text region, in characters, including boxpadx spaces for
;; padding, and fudge factor:
if n_elements(boxpadx) ne 1 then boxpadx=2.
lxc=(width+boxpadx)*sx*boxfudgex

;; height of text region, in characters:
;; (add boxpady characters for padding)
if n_elements(boxpady) ne 1 then boxpady=.5
lyc=-(n_lines+boxpady)*sy

;; coordinates of plot area, in device units:
xcrange=!x.crange
if !x.type then xcrange=10^xcrange
ycrange=!y.crange
if !y.type then ycrange=10^ycrange
lower_left=convert_coord(xcrange(0),ycrange(0),/data,/to_device)
upper_right=convert_coord(xcrange(1),ycrange(1),/data,/to_device)

;; width of plot area, in characters:
pxc=float(upper_right(0)-lower_left(0))/!d.x_ch_size

;; height of plot area, in characters:
pyc=-float(upper_right(1)-lower_left(1))/!d.y_ch_size

;; define padding: distance (in characters) of edge of
;; text box from plot axes:
padxc=3.*sx
padyc=1.5*sy

;; define coordinates of upper left corner of text region, in
;; characters, relative to upper left corner of plot data area.
case position of
    0: return
    1: begin                                 ; below plot, left
        x1c=0.
        y1c=pyc-4.*sy-padyc
    end
    2: begin                                 ; below plot, center
        x1c=.5*(pxc-lxc)
        y1c=pyc-4.*sy-padyc
    end
    3: begin                                 ; below plot, right
        x1c=pxc-lxc
        y1c=pyc-4.*sy-padyc
    end
    4: begin                                 ; lower left
        x1c=padxc
        y1c=pyc-lyc+padyc
    end
    5: begin                                 ; lower center
        x1c=.5*(pxc-lxc)
        y1c=pyc-lyc+padyc
    end
    6: begin                                 ; lower right
        x1c=pxc-lxc-padxc
        y1c=pyc-lyc+padyc
    end
    7: begin                                 ; middle left
        x1c=padxc
        y1c=.5*(pyc-lyc)
    end
    8: begin                                 ; middle center
        x1c=.5*(pxc-lxc)
        y1c=.5*(pyc-lyc)
    end
    9: begin                                 ; middle  right
        x1c=pxc-lxc-padxc
        y1c=.5*(pyc-lyc)
    end
    10: begin                                 ; upper left
        x1c=padxc
        y1c=-padyc
    end
    11: begin                                 ; upper center
        x1c=.5*(pxc-lxc)
        y1c=-padyc
    end
    12: begin                                 ; upper right
        x1c=pxc-lxc-padxc
        y1c=-padyc
    end
endcase

;; define bottom right corner of text box, in characters:

x2c=x1c+lxc
y2c=y1c+lyc

;; define bottom right corner of each line of text:

txc=fltarr(n_lines)+x1c+sx*boxpadx/2.
tyc=fltarr(n_lines)
for i=0,n_lines-1 do tyc(i)=y1c-(i+.85)*charsize-boxpady/2.

;; get coordinates of upper left corner ("origin")
;; of plot in device coords:

x0=lower_left(0)
y0=upper_right(1)

;; convert (x1,y1), (x2,y2), and (tx,ty) to device coords:

x1=x0+x1c*!d.x_ch_size
y1=y0+y1c*!d.y_ch_size

x2=x0+x2c*!d.x_ch_size
y2=y0+y2c*!d.y_ch_size

tx=x0+txc*!d.x_ch_size
ty=y0+tyc*!d.y_ch_size

;; draw the text_array:
for i=0,n_lines-1 do xyouts,tx(i),ty(i),text_array(i), $
  charsize=scale*charsize,_extra=_extra,/device,color=color(i)

;; draw the box around the text:

if keyword_set(nobox) eq 0 then  $
  plots,[x1,x2,x2,x1,x1],[y1,y1,y2,y2,y1],/device,_extra=_extra

return
end
