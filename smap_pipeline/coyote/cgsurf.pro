; docformat = 'rst'
;
; NAME:
;   cgSurf
;
; PURPOSE:
;   The purpose of cgSurf is to create a wrapper for the traditional IDL graphics
;   commands, Surface and Shade_Surf. The primary purpose of this is to create surface 
;   commands that work and look identically both on the display and in PostScript files.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2010, by Fanning Software Consulting, Inc. All rights reserved.           ;
;                                                                                          ;
;  Redistribution and use in source and binary forms, with or without                      ;
;  modification, are permitted provided that the following conditions are met:             ;
;                                                                                          ;
;      * Redistributions of source code must retain the above copyright                    ;
;        notice, this list of conditions and the following disclaimer.                     ;
;      * Redistributions in binary form must reproduce the above copyright                 ;
;        notice, this list of conditions and the following disclaimer in the               ;
;        documentation and/or other materials provided with the distribution.              ;
;      * Neither the name of Fanning Software Consulting, Inc. nor the names of its        ;
;        contributors may be used to endorse or promote products derived from this         ;
;        software without specific prior written permission.                               ;
;                                                                                          ;
;  THIS SOFTWARE IS PROVIDED BY FANNING SOFTWARE CONSULTING, INC. ''AS IS'' AND ANY        ;
;  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES    ;
;  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT     ;
;  SHALL FANNING SOFTWARE CONSULTING, INC. BE LIABLE FOR ANY DIRECT, INDIRECT,             ;
;  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED    ;
;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;         ;
;  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             ;
;  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT              ;
;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS           ;
;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                            ;
;******************************************************************************************;
;
;+
; :Description:
;   The purpose of cgSurf is to create a wrapper for the traditional IDL graphics
;   commands, Surface and Shade_Surf. The primary purpose of this is to create surface 
;   commands that work and look identically both on the display and in PostScript files.
;
; :Categories:
;    Graphics
;    
; :Params:
;    data: in, required, type=any
;         A two-dimensional array of data to be displayed.
;    x: in, optional, type=any
;         A vector or two-dimensional array specifying the X coordinates of the
;         surface grid.
;    y: in, optional, type=any
;         A vector or two-dimensional array specifying the Y coordinates of the
;         surface grid.
;       
; :Keywords:
;     addcmd: in, optional, type=boolean, default=0
;        Set this keyword to add the command to an cgWindow. Setting this keyword
;        automatically sets the WINDOW keyword, but the command does not erase the
;        graphics window as it would normally.
;     axiscolor: in, optional, type=string/integer, default='black'
;        If this keyword is a string, the name of the axis color. By default, 'black'.
;        Otherwise, the keyword is assumed to be a color index into the current color table.
;     axescolor: in, hidden, type=string/integer
;        Provisions for bad spellers.
;     background: in, optional, type=string/integer, default='white'
;        If this keyword is a string, the name of the background color. By default, 'white'.
;        Otherwise, the keyword is assumed to be a color index into the current color table.
;     bottom: in, optional, type=string/integer, default='black'
;        If this keyword is a string, the name of the bottom color. By default, same as COLOR.
;        Otherwise, the keyword is assumed to be a color index into the current color table.
;     charsize: in, optional, type=float, default=cgDefCharSize*1.25
;        The character size for axes annotations. Uses cgDefCharSize()*1.25 to select default
;        character size, unless !P.Charsize is set, in which case !P.Charsize*1.25 is always used.
;     color: in, optional, type=string/integer, default='blu6'
;        If this keyword is a string, the name of the data color. By default, "BLU6".
;        Otherwise, the keyword is assumed to be a color index into the current color table.
;     elevation_shading: in, optional, type=boolean, default=0
;        Set this keyword to put elevation shading into effect for the surface.
;     font: in, optional, type=integer, default=-1
;        The type of font desired. If undefined, and the current graphics device is PostScript,
;        the FONT keyword will be set to 1, indicating true-type fonts. The FONT keyword must
;        be set to -1 (Hershey fonts) or 1 (true-type fonts) for surface annotations to be
;        rotated correctly in PostScript output.
;     layout: in, optional, type=intarr(3)
;         This keyword specifies a grid with a graphics window and determines where the
;         graphic should appear. The syntax of LAYOUT is three numbers: [ncolumns, nrows, location].
;         The grid is determined by the number of columns (ncolumns) by the number of 
;         rows (nrows). The location of the graphic is determined by the third number. The
;         grid numbering starts in the upper left (1) and goes sequentually by column and then
;         by row.
;     noerase: in, optional, type=boolean, default=0
;        Set this keyword to prevent the window from erasing the contents before displaying
;        the surface plot.
;     palette: in, optional, type=bytarr(3,N)
;         Set this keyword to a 3 x N or N x 3 byte array containing the RGB color vectors 
;         to be loaded before the surface is displayed. Such vectors can be obtained, for 
;         example, from cgLoadCT with the RGB_TABLE keyword:
;               
;             cgLoadCT, 33, RGB_TABLE=palette
;             cgSurf, Loaddata(2), PALETTE=palette, /Elevation
;                    
;     rotx: in, optional, type=float, default=30
;        The rotation about the X axis.
;     rotz: in, optional, type=float, default=30
;        The rotation about the Z axis.
;     shaded: in, optional, type=boolean, default=0
;        Set this keyword if you wish to display a shaded surface. To display shaded surfaces
;        in a device-independent way, the shading values are confined to indices 0 to 253 with
;        SET_SHADING, and the background color is placed in color index 254. The color table vectors
;        are reduced to 254 elements when this happens. This all happens behind the stage, 
;        and the original color table is restore upon exit. Because I can't tell how many values
;        SET_SHADING is using on entering the program, I just set it back to its default 256 values
;        on exiting the program.
;     shades: in, optional, type=byte
;        Set this keyword to a byte scaled 2D array of the same size as data to shade the surface
;        with these color indices.
;     skirt: in, optional, type=any
;        Set this keyword to a Z value where a skirt will be drawn for the surface.
;     title: in, optional, type=string
;        The title of the plot. It will be written "flat to the screen", rather than rotated.
;     traditional: in, optional, type=boolean, default=0
;         If this keyword is set, the traditional color scheme of a black background for
;         graphics windows on the display is used and PostScript files always use a white background.
;     tsize: in, optional, type=float
;        The character size for the title. Normally, the title character size is 1.1 times
;        the character size of the surface annotation.
;     tspace: in, optional, type=float
;        The title Y spacing. This should be a number, between 0 and 1 that is the fraction 
;        of the distance between !Y.Window[1] and !Y.Window[0] to locate the title above 
;        !Y.Window[1]. When Total(!P.MULTI) EQ 0, the default is 0.005, and it is 0.0025 otherwise.
;     window: in, optional, type=boolean, default=0
;         Set this keyword if you want to display the plot in a resizable graphics window.
;     xstyle: in, hidden
;         The normal XSTYLE keyword.
;     ystyle: in, hidden
;         The normal YSTYLE keyword.
;     zstyle: in, hidden
;         The normal ZSTYLE keyword.
;     _extra: in, optional, type=any
;        Any keyword appropriate for the IDL Plot command is allowed in the program.
;
; :Examples:
;    Use as you would use the IDL SURFACE of SHADE_SURF command::
;       data = Dist(200)
;       LoadCT, 33
;       cgSurf, data
;       cgSurf, data, Shades=BytScl(data)
;       cgSurf, data, /Shaded
;       cgSurf, data, /Shaded, Shades=BytScl(data) 
;       
; :Author:
;       FANNING SOFTWARE CONSULTING::
;           David W. Fanning 
;           1645 Sheely Drive
;           Fort Collins, CO 80526 USA
;           Phone: 970-221-0438
;           E-mail: davidf@dfanning.com
;           Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; :History:
;     Change History::
;        Written, 13 November 2010. DWF.
;        Now setting decomposition state by calling SetDecomposedState. 16 November 2010. DWF.
;        Added TSIZE and TSPACE keywords to treak title size and placement, 
;           as necessary. 17 November 2010. DWF.
;        Background keyword now applies in PostScript file as well. 17 November 2010. DWF.
;        Many changes after BACKGROUND changes to get !P.MULTI working again! 18 November 2010. DWF.
;        Changes so that color variables don't change type. 23 Nov 2010. DWF.
;        Added ELEVATION_SHADING keyword. 26 Nov 2010. DWF.
;        I had keyword conflicts with the AX and AZ rotation keywords. Now perform rotation with
;           ROTX and ROTZ keywords. 7 Dec 2010. DWF.
;        Added WINDOW keyword to allow graphic to be displayed in a resizable graphics window. 8 Dec 2010. DWF
;        Changed the Title size to 1.1 times the character size of the plot. 14 Dec 2010. DWF.
;        Modifications to allow cgSurf to be drop-in replacement for old Surface commands in 
;            indexed color mode. 24 Dec 2010. DWF.
;        Previous changes introduced problems with OVERPLOT that have now been fixed. 28 Dec 2010. DWF.
;        Set NOERASE keyword from !P.NoErase system variable when appropriate. 28 Dec 2010. DWF.
;        Additional problems with NOERASE discovered and solved. 29 Dec 2010. DWF.
;        Change to DECOMPOSED color was using incorrect color tables. 29 Dec 2010. DWF.
;        In some cases, I was turning BYTE values to strings without converting to 
;            INTEGERS first. 30 Dec 2010. DWF.
;        Moved setting to decomposed color before color selection process to avoid PostScript
;             background problems when passed 24-bit color integers. 12 Jan 2011. DWF. 
;        Fixed a problem in which I assumed the background color was a string. 18 Jan 2011. DWF.  
;        Added ADDCMD keyword. 26 Jan 2011. DWF.
;        Added LAYOUT keyword. 28 Jan 2011. DWF.
;        Added PALETTE keyword. 3 Feb 2011. DWF.
;        Color table vectors must be obtained AFTER loading the color palette. 6 March 2011. DWF.
;        
; :Copyright:
;     Copyright (c) 2010, Fanning Software Consulting, Inc.
;-
PRO cgSurf, data, x, y, $
    ADDCMD=addcmd, $
    AXISCOLOR=saxiscolor, $
    AXESCOLOR=saxescolor, $
    BACKGROUND=sbackground, $
    BOTTOM=sbottom, $
    CHARSIZE=charsize, $
    COLOR=scolor, $
    ELEVATION_SHADING=elevation_shading, $
    FONT=font, $
    LAYOUT=layout, $
    NOERASE=noerase, $
    PALETTE=palette, $
    ROTX=rotx, $
    ROTZ=rotz, $
    SHADED=shaded, $
    SHADES=shades, $
    SKIRT=skirt, $
    TITLE=title, $
    TRADITIONAL=traditional, $
    TSIZE=tsize, $
    TSPACE=tspace, $
    WINDOW=window, $
    XSTYLE=xstyle, $
    YSTYLE=ystyle, $
    ZSTYLE=zstyle, $
    _Ref_Extra=extra
    
    Compile_Opt idl2

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        IF N_Elements(thisMulti) NE 0 THEN !P.Multi = thisMulti
        RETURN
    ENDIF
    
    ; Did user pass parameters?
    IF N_Elements(data) EQ 0 THEN BEGIN
        Print, 'USE SYNTAX: cgSurf, data, x, y, [SHADED=1]'
        RETURN
    ENDIF
    
    ; Set up PostScript device for working with colors.
    IF !D.Name EQ 'PS' THEN Device, COLOR=1, BITS_PER_PIXEL=8
    
    ; Do they want this plot in a resizeable graphics window?
    IF Keyword_Set(addcmd) THEN window = 1
    IF Keyword_Set(window) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
    
        ; If you are using a layout, you can't ever erase.
        IF N_Elements(layout) NE 0 THEN noerase = 1
        
        currentWindow = cgQuery(/CURRENT, COUNT=wincnt)
        IF wincnt EQ 0 THEN replaceCmd = 0 ELSE replaceCmd=1
        
        ; If adding a command, have to do this differently.
        IF Keyword_Set(addcmd) THEN BEGIN
            cgWindow, 'cgSurf', data, x, y, $
                AXISCOLOR=saxiscolor, $
                AXESCOLOR=saxescolor, $
                BACKGROUND=sbackground, $
                BOTTOM=sbottom, $
                CHARSIZE=charsize, $
                COLOR=scolor, $
                ELEVATION_SHADING=elevation_shading, $
                FONT=font, $
                LAYOUT=layout, $
                NOERASE=noerase, $
                PALETTE=palette, $
                ROTX=rotx, $
                ROTZ=rotz, $
                SHADED=shaded, $
                SHADES=shades, $
                SKIRT=skirt, $
                TITLE=title, $
                TRADITIONAL=traditional, $
                TSIZE=tsize, $
                TSPACE=tspace, $
                XSTYLE=xstyle, $
                YSTYLE=ystyle, $
                ZSTYLE=zstyle, $
                ADDCMD=1, $
                _Extra=extra
                
            RETURN
        ENDIF
        
        ; Otherwise, we are just replacing the commands in a new or existing window.
        cgWindow, 'cgSurf', data, x, y, $
            AXISCOLOR=saxiscolor, $
            AXESCOLOR=saxescolor, $
            BACKGROUND=sbackground, $
            BOTTOM=sbottom, $
            CHARSIZE=charsize, $
            COLOR=scolor, $
            ELEVATION_SHADING=elevation_shading, $
            FONT=font, $
            LAYOUT=layout, $
            NOERASE=noerase, $
            PALETTE=palette, $
            ROTX=rotx, $
            ROTZ=rotz, $
            SHADED=shaded, $
            SHADES=shades, $
            SKIRT=skirt, $
            TITLE=title, $
            TRADITIONAL=traditional, $
            TSIZE=tsize, $
            TSPACE=tspace, $
            XSTYLE=xstyle, $
            YSTYLE=ystyle, $
            ZSTYLE=zstyle, $
            REPLACECMD=replaceCmd, $
            _Extra=extra
            
         RETURN
    ENDIF
    
    ; Going to draw in decomposed color, if possible to avoid dirtying the color table.
    SetDecomposedState, 1, CURRENTSTATE=currentState

    ; Pay attention to !P.Noerase in setting the NOERASE kewyord. This must be
    ; done BEFORE checking the LAYOUT properties.
    IF !P.NoErase NE 0 THEN noerase = !P.NoErase ELSE noerase = Keyword_Set(noerase)

    ; Set up the layout, if necessary.
    IF N_Elements(layout) NE 0 THEN BEGIN
       thisMulti = !P.Multi
       totalPlots = layout[0]*layout[1]
       !P.Multi = [0,layout[0], layout[1], 0, 0]
       IF layout[2] EQ 1 THEN BEGIN
            noerase = 1
            !P.Multi[0] = 0
       ENDIF ELSE BEGIN
            !P.Multi[0] = totalPlots - layout[2] + 1
       ENDELSE
    ENDIF

    ; Check parameters.
    ndims = Size(data, /N_DIMENSIONS)
    IF ndims NE 2 THEN Message, 'Data must be 2D.'
    s = Size(data, /DIMENSIONS)
    IF N_Elements(x) EQ 0 THEN x = Findgen(s[0])
    IF N_Elements(y) EQ 0 THEN y = Findgen(s[1])
    noerase = Keyword_Set(noerase)
    
    ; Load a color palette, if you have one.
    IF N_Elements(palette) NE 0 THEN BEGIN
        IF Size(palette, /N_DIMENSIONS) NE 2 THEN Message, 'Color palette is not a 3xN array.'
        dims = Size(palette, /DIMENSIONS)
        threeIndex = Where(dims EQ 3)
        IF ((threeIndex)[0] LT 0) THEN Message, 'Color palette is not a 3xN array.'
        IF threeIndex[0] EQ 0 THEN palette = Transpose(palette)
        TVLCT, palette
    ENDIF
    
    ; Get the current color table vectors.
    TVLCT, rr, gg, bb, /GET
    
    ; Check the keywords.
    IF N_Elements(sbackground) EQ 0 THEN BEGIN
        IF Keyword_Set(overplot) || Keyword_Set(noerase) THEN BEGIN
           IF !D.Name EQ 'PS' THEN BEGIN
                background = 'WHITE' 
           ENDIF ELSE BEGIN
                IF ((!D.Flags AND 256) NE 0) THEN BEGIN
                    IF (!D.Window LT 0) &&  Keyword_Set(noerase) THEN BEGIN
                        Window
                        IF ~Keyword_Set(traditional) THEN cgErase, 'WHITE'
                    ENDIF
                    pixel = cgSnapshot(!D.X_Size-1,  !D.Y_Size-1, 1, 1)
                    IF (Total(pixel) EQ 765) THEN background = 'WHITE'
                    IF (Total(pixel) EQ 0) THEN background = 'BLACK'
                    IF N_Elements(background) EQ 0 THEN background = 'OPPOSITE'
                ENDIF ELSE background = 'OPPOSITE'
           ENDELSE
        ENDIF ELSE BEGIN
           IF Keyword_Set(traditional) THEN BEGIN
              IF ((!D.Flags AND 256) NE 0) THEN background = 'BLACK' ELSE background = 'WHITE'
           ENDIF ELSE background = 'WHITE' 
        ENDELSE
    ENDIF ELSE background = sbackground
    IF Size(background, /TYPE) EQ 3 THEN IF GetDecomposedState() EQ 0 THEN background = Byte(background)
    IF Size(background, /TYPE) LE 2 THEN background = StrTrim(Fix(background),2)

    ; Choose an axis color.
    IF N_Elements(saxisColor) EQ 0 AND N_Elements(saxescolor) NE 0 THEN saxiscolor = saxescolor
    IF N_Elements(saxiscolor) EQ 0 THEN BEGIN
       IF (Size(background, /TNAME) EQ 'STRING') && (StrUpCase(background) EQ 'WHITE') THEN BEGIN
            IF !P.Multi[0] EQ 0 THEN saxisColor = 'BLACK'
       ENDIF
       IF N_Elements(saxiscolor) EQ 0 THEN BEGIN
           IF !D.Name EQ 'PS' THEN BEGIN
                IF StrUpCase(background) EQ 'BLACK' THEN background = 'WHITE'
                saxisColor = 'BLACK' 
           ENDIF ELSE BEGIN
                IF ((!D.Flags AND 256) NE 0) THEN BEGIN
                    IF !D.Window LT 0 THEN Window
                    IF (!P.Multi[0] EQ 0) && (~Keyword_Set(overplot) && ~noerase) THEN cgErase, background
                    pixel = cgSnapshot(!D.X_Size-1,  !D.Y_Size-1, 1, 1)
                    IF (Total(pixel) EQ 765) OR (StrUpCase(background) EQ 'WHITE') THEN saxisColor = 'BLACK'
                    IF (Total(pixel) EQ 0) OR (StrUpCase(background) EQ 'BLACK') THEN saxisColor = 'WHITE'
                    IF N_Elements(saxisColor) EQ 0 THEN saxisColor = 'OPPOSITE'
                ENDIF ELSE saxisColor = 'OPPOSITE'
          ENDELSE
       ENDIF
    ENDIF
    IF N_Elements(saxisColor) EQ 0 THEN axisColor = !P.Color ELSE axisColor = saxisColor
    IF Size(axisColor, /TYPE) EQ 3 THEN IF GetDecomposedState() EQ 0 THEN axisColor = Byte(axisColor)
    IF Size(axisColor, /TYPE) LE 2 THEN axisColor = StrTrim(Fix(axisColor),2)

    
    ; Choose a color.
    IF N_Elements(sColor) EQ 0 THEN BEGIN
       IF (Size(background, /TNAME) EQ 'STRING') && (StrUpCase(background) EQ 'WHITE') THEN BEGIN
            IF !P.Multi[0] EQ 0 THEN BEGIN
                IF Keyword_Set(traditional) THEN sColor = 'BLACK' ELSE sColor = 'BLU6' 
            ENDIF
       ENDIF
       IF N_Elements(sColor) EQ 0 THEN BEGIN
           IF !D.Name EQ 'PS' THEN BEGIN
                IF StrUpCase(background) EQ 'BLACK' THEN background = 'WHITE'
                IF Keyword_Set(traditional) THEN sColor = 'BLACK' ELSE sColor = 'BLU6' 
           ENDIF ELSE BEGIN
                IF ((!D.Flags AND 256) NE 0) THEN BEGIN
                    IF !D.Window LT 0 THEN Window
                    IF (!P.Multi[0] EQ 0) && (~Keyword_Set(overplot) && ~noerase) THEN cgErase, background
                    pixel = cgSnapshot(!D.X_Size-1,  !D.Y_Size-1, 1, 1)
                    IF (Total(pixel) EQ 765) OR (StrUpCase(background) EQ 'WHITE') THEN BEGIN
                        IF Keyword_Set(traditional) THEN sColor = 'BLACK' ELSE sColor = 'BLU6'
                    ENDIF
                    IF (Total(pixel) EQ 0) OR (StrUpCase(background) EQ 'BLACK') THEN sColor = 'WHITE'
                    IF N_Elements(sColor) EQ 0 THEN BEGIN
                        IF Keyword_Set(traditional) THEN sColor = 'BLACK' ELSE sColor = 'BLU6' 
                    ENDIF
                ENDIF ELSE sColor = 'OPPOSITE'
           ENDELSE
       ENDIF
    ENDIF
    IF N_Elements(sColor) EQ 0 THEN BEGIN
        IF Keyword_Set(traditional) THEN sColor = 'BLACK' ELSE color = 'BLU6' 
    ENDIF ELSE  color = sColor
    IF Size(color, /TYPE) EQ 3 THEN IF GetDecomposedState() EQ 0 THEN color = Byte(color)
    IF Size(color, /TYPE) LE 2 THEN color = StrTrim(Fix(color),2)

    ; If color is the same as background, do something.
    ; If color is the same as background, do something.
    IF ColorsAreIdentical(background, color) THEN BEGIN
        IF ((!D.Flags AND 256) NE 0) THEN BEGIN
            IF (!P.Multi[0] EQ 0) && (~Keyword_Set(overplot) && ~noerase) THEN cgErase, background
        ENDIF
        color = 'OPPOSITE'
    ENDIF
    IF ColorsAreIdentical(background, axiscolor) THEN BEGIN
        IF ((!D.Flags AND 256) NE 0) THEN BEGIN
            IF (!P.Multi[0] EQ 0) && (~Keyword_Set(overplot) && ~noerase) THEN cgErase, background
        ENDIF
        axiscolor = 'OPPOSITE'
    ENDIF

    IF N_Elements(sbottom) EQ 0 THEN bottom = color ELSE bottom = sbottom
    IF Size(bottom, /TYPE) EQ 3 THEN IF GetDecomposedState() EQ 0 THEN bottom = Byte(bottom)
    IF Size(bottom, /TYPE) LE 2 THEN bottom = StrTrim(Fix(bottom),2)
    elevation_shading = Keyword_Set(elevation_shading)
    
    ; Character size has to be determined *after* the layout has been decided.
    IF N_Elements(font) EQ 0 THEN IF (!D.Name EQ 'PS') THEN font = 1 ELSE font = !P.font
    IF N_Elements(charsize) EQ 0 THEN charsize = cgDefCharSize(FONT=font) * 1.25
    
    ; Other properties.
    IF N_Elements(rotx) EQ 0 THEN rotx = 30
    IF N_Elements(rotz) EQ 0 THEN rotz = 30
    IF N_Elements(xstyle) EQ 0 THEN xstyle = 0
    IF N_Elements(ystyle) EQ 0 THEN ystyle = 0
    IF N_Elements(zstyle) EQ 0 THEN zstyle = 0
            
    IF Size(axiscolor, /TNAME) EQ 'STRING' THEN BEGIN
        axiscolor = cgColor(axiscolor)
    ENDIF ELSE BEGIN
         IF currentState EQ 0 THEN axiscolor = Color24(rr[axiscolor], gg[axiscolor], bb[axiscolor])
    ENDELSE
    IF Size(bottom, /TNAME) EQ 'STRING' THEN BEGIN
        bottom = cgColor(bottom)
    ENDIF ELSE BEGIN
         IF currentState EQ 0 THEN bottom = Color24(rr[bottom], gg[bottom], bb[bottom])
    ENDELSE
    IF Size(color, /TNAME) EQ 'STRING' THEN BEGIN
        color = cgColor(color)
    ENDIF ELSE BEGIN
         IF currentState EQ 0 THEN color = Color24(rr[color], gg[color], bb[color])
    ENDELSE
    IF Size(background, /TNAME) EQ 'STRING' THEN BEGIN
        originalbg = background
        background = cgColor(background)
        shadebackground = cgColor(originalbg, DECOMPOSED=0, 254)
    ENDIF ELSE BEGIN
         ; Different values based on current state of the device. Indexed color mode here.
         IF currentState EQ 0 THEN BEGIN
            originalbg = [rr[background], gg[background], bb[background]]
            background = Color24(rr[background], gg[background], bb[background])
         ENDIF
         ; Decomposed color mode here. Not sure how this should be handled. Just
         ; do white. If it is not right, then use strings for color values!
         IF currentState EQ 1 THEN BEGIN
            orginalbg = 'white'
            shadebackground = cgColor('white', DECOMPOSED=0, 254)
         ENDIF
    ENDELSE
    
    ; Get the color table with loaded drawing colors.
    TVLCT, rl, gl, bl, /GET
    
    ; Are you doing elevation shading?
    IF elevation_shading THEN BEGIN
        IF N_Elements(shades) EQ 0 THEN shades = BytScl(data, /NAN)
    ENDIF
    
    ; Going to draw the axes in decomposed color if we can.
    IF currentState THEN SetDecomposedState,1
    
    ; Do you need a PostScript background color? Lot's of problems here!
    ; Basically, I MUST draw a plot to advance !P.MULTI. But, drawing a
    ; plot of any sort erases the background color. So, I have to draw a 
    ; plot, store the new system variables, then draw my background, etc.
    ; I have tried LOTS of options. This is the only one that worked.
    IF !D.Name EQ 'PS' THEN BEGIN
       IF ~noerase THEN BEGIN
       
           ; I only have to do this, if this is the first plot.
           IF !P.MULTI[0] EQ 0 THEN BEGIN
           
                ; Save the current system variables. Will need to restore later.
                bangx = !X
                bangy = !Y
                bangz = !Z
                bangp = !P
                
                ; Make sure axis are turned off. I don't really want to draw anything,
                ; just advance !P.MULTI or "erase" the display for the next plot.
                IF BitGet(xstyle, 2) NE 1 THEN xxstyle = xstyle + 4 ELSE xxstyle = xstyle
                IF BitGet(ystyle, 2) NE 1 THEN yystyle = ystyle + 4 ELSE yystyle = ystyle
                IF BitGet(zstyle, 2) NE 1 THEN zzstyle = zstyle + 4 ELSE zzstyle = zstyle
                
                ; Draw the plot that doesn't draw anything.
                Surface, data, x, y, XSTYLE=xxstyle, YSTYLE=yystyle, ZSTYLE=zzstyle, $
                    CHARSIZE=charsize, SKIRT=skirt, _STRICT_EXTRA=extra, $
                    AX=rotx, AZ=rotz  
                
                ; Save the "after plot" system variables. Will use later. 
                afterx = !X
                aftery = !Y
                afterz = !Z
                afterp = !P     
                
                ; Draw the background color and set the variables you will need later.
                PS_Background, background
                psnodraw = 1
                tempNoErase = 1
                
                ; Restore the original system variables so that it is as if you didn't
                ; draw the invisible plot.
                !X = bangx
                !Y = bangy
                !Z = bangz
                !P = bangp
                
            ENDIF ELSE tempNoErase = noerase
        ENDIF ELSE tempNoErase = noerase
     ENDIF ELSE tempNoErase = noerase

    bangx = !X
    bangy = !Y
    bangz = !Z
    bangp = !P

    ; Draw the surface axes.
    Surface, data, x, y, COLOR=axiscolor, BACKGROUND=background, BOTTOM=bottom, $
        /NODATA, XSTYLE=xstyle, YSTYLE=ystyle, ZSTYLE=zstyle, $
        FONT=font, CHARSIZE=charsize, NOERASE=tempNoErase, _STRICT_EXTRA=extra, $
        AX=rotx, AZ=rotz  
        
    ; Draw the title, if you have one.
    IF N_Elements(title) NE 0 THEN BEGIN
       IF N_Elements(tsize) EQ 0 THEN BEGIN
           IF (!P.Charsize EQ 0) AND (N_Elements(charsize) EQ 0) THEN BEGIN
                titleSize = 1.10 
           ENDIF ELSE BEGIN
               IF (!P.Charsize NE 0) THEN titleSize = !P.Charsize * 1.10
               IF (N_Elements(charsize) NE 0) THEN titleSize = charsize * 1.10
           ENDELSE
       ENDIF ELSE titleSize = tsize
       xloc = (!X.Window[1] - !X.Window[0]) / 2.0 + !X.Window[0]
       distance = !Y.Window[1] - !Y.Window[0]
       IF N_Elements(tspace) EQ 0 THEN tspace = (Total(!P.Multi) EQ 0) ? 0.0025 : 0.00125
       yloc = !Y.Window[1] + (distance * tspace)
        XYOutS, xloc, yloc, /NORMAL, ALIGNMENT=0.5, CHARSIZE=titleSize, $
            title, FONT=font, COLOR=axiscolor
    ENDIF

    ; Storing these system variable is *required* to make !P.MULTI work correct.
    ; Do not delete!
    newx = !X
    newy = !y
    newz = !Z
    newP = !P
    !X = bangx
    !Y = bangy
    !Z = bangz
    !P = bangp
    
    ; Turn the axes off to draw the surface itself. Start by making sure bit 4 in
    ; the [XYZ]Style bits are turned on.
    IF BitGet(xstyle, 2) NE 1 THEN xxstyle = xstyle + 4 ELSE xxstyle = xstyle
    IF BitGet(ystyle, 2) NE 1 THEN yystyle = ystyle + 4 ELSE yystyle = ystyle
    IF BitGet(zstyle, 2) NE 1 THEN zzstyle = zstyle + 4 ELSE zzstyle = zstyle
         
    ; Make absolutely sure the colors are fresh.
    IF N_Elements(palette) NE 0 THEN TVLCT, palette ELSE TVLCT, rr, gg, bb
    
    ; Draw either a wire mesh or shaded surface. Care has to be taken if
    ; the SHADES keyword is used, because this also has to be done in indexed
    ; color mode.
    IF Keyword_Set(shaded) THEN BEGIN
    
        ; All shaded surfaces have to be done in indexed color mode.
        SetDecomposedState, 0
        
        ; We have to get the background color out of the surface color
        ; range to do this in a device independent way.
        Set_Shading, VALUES=[0,253]
        
        ; Depending upon the original background color, load the color
        ; in color table index 254.
        IF Size(originalbg, /TNAME) EQ 'STRING' $
            THEN orignalbg = cgColor(originalBg, 254) $
            ELSE TVLCT, Reform(origialbg), 254
            
        ; Restrict the current color table vectors to the range 0-253.
        IF N_Elements(palette) NE 0 THEN BEGIN
            TVLCT, Congrid(palette[*,0],254), Congrid(palette[*,1],254), Congrid(palette[*,2],254)
        ENDIF ELSE BEGIN
            TVLCT, Congrid(rr,254), Congrid(gg,254), Congrid(bb,254)
        ENDELSE
        
        ; If shades is defined, then we have to make sure the values there
        ; are in the range 0-253.
        IF N_Elements(shades) NE 0 THEN BEGIN
            IF Max(shades,/NAN) GT 253 $
                THEN checkShades = BytScl(shades, TOP=253) $
                ELSE checkShades = shades
        ENDIF
        
        ; Shaded surface plot.
         Shade_Surf, data, x, y, /NOERASE, COLOR=color, BOTTOM=bottom, SHADES=checkShades, $
            XSTYLE=xxstyle, YSTYLE=yystyle, ZSTYLE=zzstyle, _STRICT_EXTRA=extra, $
            BACKGROUND=shadebackground, AX=rotx, AZ=rotz, CHARSIZE=charsize  
            
        ; Have to repair the axes. Do this in decomposed color mode, if possible.
        ; If its not possible, you have to reload the color table that has the drawing
        ; colors in it.
        IF currentState THEN BEGIN
            SetDecomposedState, 1 
        ENDIF ELSE BEGIN
            IF N_Elements(palette) NE 0 THEN TVLCT, palette ELSE TVLCT, rl, gl, bl
        ENDELSE
        Surface, data, x, y, COLOR=axiscolor, BACKGROUND=background, BOTTOM=bottom, $
            /NODATA, /NOERASE, XSTYLE=xstyle, YSTYLE=ystyle, ZSTYLE=zstyle, $
            FONT=font, CHARSIZE=charsize, SKIRT=skirt, _STRICT_EXTRA=extra, AX=rotx, AZ=rotz 
            
        ; Have to repair the title, too.
        IF N_Elements(title) NE 0 THEN BEGIN
           IF (!P.Charsize EQ 0) AND (N_Elements(charsize) EQ 0) THEN BEGIN
                titleSize = 1.10 
           ENDIF ELSE BEGIN
               IF (!P.Charsize NE 0) THEN titleSize = !P.Charsize * 1.10
               IF (N_Elements(charsize) NE 0) THEN titleSize = charsize * 1.10
           ENDELSE
           xloc = (!X.Window[1] - !X.Window[0]) / 2.0 + !X.Window[0]
           distance = !Y.Window[1] - !Y.Window[0]
           IF N_Elements(tspace) EQ 0 THEN tspace = (Total(!P.Multi) EQ 0) ? 0.0025 : 0.00125
           yloc = !Y.Window[1] + (distance * tspace)
           XYOutS, xloc, yloc, /NORMAL, ALIGNMENT=0.5, CHARSIZE=titleSize, $
                title, FONT=font, COLOR=axiscolor
        ENDIF
            
        ; Shading parameters are "sticky", but I can't tell what they
        ; were when I came into the program. Here I just set them back
        ; to their default values.
        Set_Shading, VALUES=[0,255]
            
    ENDIF ELSE BEGIN
    
        
        ; We can draw the surface in decomposed color mode, unless the SHADES
        ; keyword is being used. Then we have to use indexed color mode.         
        IF N_Elements(shades) NE 0 THEN BEGIN
            SetDecomposedState, 0
            IF N_Elements(palette) NE 0 THEN TVLCT, palette ELSE TVLCT, rr, gg, bb
            Surface, data, x, y, NOERASE=1, SHADES=shades, $
                XSTYLE=xxstyle, YSTYLE=yystyle, ZSTYLE=zzstyle, $
                FONT=font, CHARSIZE=charsize, _STRICT_EXTRA=extra, AX=rotx, AZ=rotz         
        ENDIF ELSE BEGIN
            IF currentState THEN BEGIN
                SetDecomposedState, 1 
            ENDIF ELSE BEGIN
                IF N_Elements(palette) NE 0 THEN TVLCT, palette ELSE TVLCT, rl, gl, bl
            ENDELSE
            Surface, data, x, y, NOERASE=1, COLOR=color, BOTTOM=bottom, $
                BACKGROUND=background, SHADES=shades, SKIRT=skirt, $
                XSTYLE=xxstyle, YSTYLE=yystyle, ZSTYLE=zzstyle, $
                FONT=font, CHARSIZE=charsize, _STRICT_EXTRA=extra, AX=rotx, AZ=rotz 
        ENDELSE
        
    ENDELSE
    
    ; If this is the first plot in PS, then we have to make it appear that we have
    ; drawn a plot, even though we haven't.
    IF N_Elements(psnodraw) EQ 1 THEN BEGIN
        newx = afterX
        newy = afterY
        newz = afterZ
        newp = afterP
    ENDIF

    ; Restore the decomposed color state to the input state.
    SetDecomposedState, currentState

    ; Restore the color table. Can't do this for the Z-buffer or
    ; the snap shot will be incorrect.
    IF (!D.Name NE 'Z') THEN TVLCT, rr, gg, bb
    
    ; Update the system variables.
    !X = newx 
    !Y = newy 
    !Z = newz 
    !P = newP

    ; Clean up if you are using a layout.
    IF N_Elements(layout) NE 0 THEN !P.Multi = thisMulti

END
    