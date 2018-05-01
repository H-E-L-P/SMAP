;+
; NAME:
; 
;        TEXT_WIDTH
;
; PURPOSE:
; 
;        Function to determine the actual displayed width
;        (approximately!) of a string of text, in normalized character
;        units, accounting for the fact that non-equal spacing is used
;        when such a string is displayed on a plot using XYOUTS.
;
;        This function is used, for example, by the PLOT_TEXT and
;        LEGEND procedures to ~correctly draw a box around displayed
;        text.
;
; CALLING SEQUENCE:
; 
;       Result=TEXT_WIDTH(TEXT_STRING)
; 
; INPUTS:
; 
;       TEXT_STRING - a string of text
;
; KEYWORD PARAMETERS:
;
;       FONT - Set to an integer from 3 to 20 (corresponding to the
;              Hershey vector font sets,) referring to the font that
;              will be used to display the text. (Any font commands
;              embedded in the text string are ignored.)
;
; RESTRICTIONS:
;
;       This function hardly works perfectly, especially when the text
;       string contains a mix of fonts; superscripts and subscripts
;       will really mess things up as well.  But it comes close in
;       many instances.
;
; PROCEDURE:
;
;       A table of normalized character widths (determined using the
;       !3 font) is used to determine the width of the text string.
;       In order to account for the use of IDL font manipulation
;       commands, the '!' symbol and the character immediately
;       following it are not counted, except for the case of two
;       consecutive '!' symbols.
;
; EXAMPLE:
;
;       Determine the width of a text string:
;
;       width=TEXT_WIDTH('!3This is some displayed text',font=3)
;
; MODIFICATION HISTORY:
; 
;       David L. Windt, Bell Labs, October 1997
;       windt@bell-labs.com
;
;-

function text_width,text_string,font=font

on_error,2

;; make a table of normalized widths for every character from #32 to
;; #127, as determined using the !3 font set.

c_ids=bindgen(128-32)+32B

widths=$
  [.91,.27,.84,1.095,1.05,1.26,1.37,.42,.74,.74,.84,1.37, $
   .525,1.365,.525,1.15, $ 
   1.05,1.05,1.05,1.05,1.05,1.05,1.05,1.05,1.05,1.05, $
   .525,.525,1.26,1.365,1.26,.95,1.42, $
   .95,1.1,1.1,1.1,1.,.95,1.1,1.16,.415,.84,1.1,.9]
widths=[widths, $
        1.26,1.16,1.16,1.1,1.16,1.1,1.05,.845,1.16,.95,1.26,1.05, $
        .95,1.05,.74,.75,.74,.53,.95,.42,1.,1.,.95,1., $
        .95,.63,1.,1.,.42,.52,.89,.42,1.57,1.,1.,1., $
        1.,.68,.89,.63,1.,.845,1.15,.895,.85,.89, $
        .74,.52,.74,.53,1.]

;; sp  !   "   #   $   %   &   '   (   )   *   +
;; ,   -   .   /
;; 0   1   2   3   4   5   6   7   8   9
;; :   ;   <   =   >   ?   @   
;; A   B   C   D   E   F   G   H   I   J   K   L
;; M   N   O   P   Q   R   S   T   U   V   W   X
;; Y   Z   [  \    ]   ^   _   '   a   b   c   d
;; e   f   g   h   i   j   k   l   m   n   o   p
;; q   r   s   t   u   v   w   x   y   z  
;; {   |   }   ^ 

;; compute average character width:
st=stdev(widths,avg)

;; initialize values:
width=0.
last_was_bang=0
for i=0,strlen(text_string)-1 do begin
    character=byte(strmid(text_string,i,1))
    wh=where(character(0) eq c_ids,count)
    case 1 of
        ;; other character? add avg. width
        count eq 0: if last_was_bang then last_was_bang=0 $
          else width=width+avg

        ;; '!' character: count the width of this character
        ;; if the last character was also a '!', otherwise
        ;; just set the last_was_bang flag.
        character(0) eq 33B: if last_was_bang then  $
          width=width+widths(wh(0)) $
        else last_was_bang=1

        ;; otherwise just add character width:
        else: if last_was_bang then last_was_bang=0  $
          else width=width+widths(wh(0))
    endcase
endfor

;; make some attempt to scale for different font sets:
if keyword_set(font) then begin
    case font of
        4: width=width*1.04
        5: width=width*1.03
        6: width=width*1.09
        7: width=width*1.1
        8: width=width*1.14
        9: width=width*1.16
        10: width=width*1.45
        11: width=width*1.07
        12: width=width*.97
        13: width=width*.96
        14: width=width*1.12
        15: width=width*1.12
        16: width=width*1.21
        17: width=width*1.08
        18: width=width*1.14
        19: ;;
        20: width=width*.87
        else: ;;
    endcase
endif

;; adjust for output using PostScript hardware fonts:
if (!d.name eq 'PS') and (!p.font eq 0) then width=width*.925

return,width
end



 


