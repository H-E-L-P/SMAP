;+
; NAME:
;       FSC_Barplot
;
; PURPOSE:
;
;       This program is used to draw a a bar plot in an IDL direct graphics window.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; CATEGORY:
;
;       Graphics
;
; CALLING SEQUENCE:
;
;      FSC_Barplot, depData
;      FSC_Barplot, indepData, depData
;
; ARGUMENTS:
;
;       depData:         The dependent data to plot.
;       
;       indepData:       The independent data to plot.
;
; INPUT KEYWORDS:
;
;       AXISCOLOR:       The name of the axis color. Default: "Black". (All color names
;                        derived from cgColor.)
;
;       BACKGROUND:      The name of the background color. Default: "White".
;       
;       BARCOLOR:        The name of the color to fill the bars. Default: "Gray".
;
;
;       COLOR:            The name of the data color for drawing the histogram outlines.
;                         Default: "Black".
;
;       FILL:             Set this keyword to fill the bars with a color.
;
;       LINE_FILL:        If set, the polygons are filled with lines instead of solid color. If
;                         this keyword is set, the following keywords can also be used.
;
;                         ORIENTATION:  The orientation of the lines in line-filled polygons in degrees.
;                         PATTERN:      Set to rectangular array of pixel giving fill pattern.
;                         BARCOLOR:     The name, or vector of names, of line colors.
;                                       If a vector, the names are cycled though, as needed.
;                         SPACING:      The spacing, in centimeters, between parallel lines.
;
;       OPLOT:            Set this keyword if you want to overplot data on already established axes.
;       
;       OUTLINE:          Set this keyword if you wish to draw only the outline of the histogram plot,
;                         in a manner similar to setting PSYM=10 on a PLOT command.
;                         
;       THICK:            Set this keyword to a value greater than 1 to draw thicker axes and lines.
;
;       The user may also enter any other keywords suitable for the PLOT and POLYFILL commands in IDL.
;
; EXAMPLES:
;
;      IDL> Histoplot, Dist(256)
;      IDL> Histoplot, Fix(RandomU(seed, 200)*20), POLYCOLOR=['charcoal', 'steel blue'], /FILLPOLYGON
;      IDL> Histoplot, Fix(RandomU(seed, 200)*20), POLYCOLOR=['navy', 'forest green'], /LINE_FILL, ORIENTATION=[45,-45]
;
; REQUIRES:
;
;     Requires programs from the Coyote Library:
;
;        http://www.dfanning.com/programs/coyotelibrary.zip
;        
; MODIFICATION HISTORY:
;
;       Written by:  David W. Fanning, 3 January 2011.
;-
;******************************************************************************************;
;  Copyright (c) 2011, by Fanning Software Consulting, Inc.                                ;
;  All rights reserved.                                                                    ;
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
PRO cgBarplot , $                   ; The program name.
   dataToHistogram, $               ; The data to draw a histogram of.
   AXISCOLORNAME=axisColorName, $   ; The axis color.
   BACKCOLORNAME=backcolorName, $   ; The background color.
   DATACOLORNAME=datacolorName, $   ; The data color.
   _REF_EXTRA=extra, $              ; For passing extra keywords.
   FILE=file, $                     ; For specifying a color name file.
   FREQUENCY=frequency, $           ; Plot relative frequency, rather than density.
   MAX_VALUE=max_value, $           ; The maximum value to plot.
   MIN_VALUE=min_value, $           ; The minimum value to plot.
   MISSING=missing, $               ; The value that indicates "missing" data to be excluded from the histgram.
   OPLOT=overplot, $                ; Set if you want overplotting.
   OPROBABILITY=oprob, $            ; Overplot the cummulative probability distribution.
   OUTLINE=outline, $               ; Set this keyword if you wish to draw only the outline of the plot.
   PROBCOLORNAME=probColorName, $   ; The color for the probability plot, if it is used. By default, "blue".
   THICK=thick, $                   ; Set to draw thicker lines and axes.
   ;
   ; POLYFILL KEYWORDS
   ;
   FILLPOLYGON=fillpolygon, $       ; Set if you want filled polygons
   LINE_FILL=line_fill, $           ; Set if you want line-filled polygons.
   ORIENTATION=orientation, $       ; The orientation of the lines.
   PATTERN=pattern, $               ; The fill pattern.
   POLYCOLOR=polycolorname, $           ; The name of the polygon draw/fill color.
   SPACING=spacing, $               ; The spacing of filled lines.
   ;
   ; HISTOGRAM OUTPUT KEYWORDS
   ;
   HISTDATA=histdata, $
   LOCATIONS=locations, $
   OMAX=omax, $
   OMIN=omin, $
   PROBABLITY_FUNCTION=probability, $
   REVERSE_INDICES=ri, $
   ;
   ; HISTOGRAM INPUT KEYWORDS
   ;
   BINSIZE=binsize, $               ; The histogram bin size.
   L64=l64, $                       ; Input for HISTOGRAM.
   MAXINPUT=maxinput, $             ; The maximum value to HISTOGRAM.
   MININPUT=mininput, $             ; The minimum value to HISTOGRAM.
   NAN=nan, $                       ; Check for NAN.
   NBINS=nbins                      ; The number of bins to display.


   ; Catch any error in the HistoPlot program.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(!Error_State.Msg + '. Returning...')
      IF N_Elements(nancount) EQ 0 THEN BEGIN
            IF N_Elements(_dataToHistogram) NE 0 THEN dataToHistogram = Temporary(_dataToHistogram)
      ENDIF ELSE BEGIN
            IF nancount EQ 0 THEN BEGIN
                IF N_Elements(_dataToHistogram) NE 0 THEN dataToHistogram = Temporary(_dataToHistogram)
            ENDIF
      ENDELSE
      RETURN
   ENDIF

   ; Set up PostScript device for working with colors.
   IF !D.Name EQ 'PS' THEN Device, COLOR=1, BITS_PER_PIXEL=8
    
   ; Check for positional parameter.
   IF N_Elements(dataToHistogram) EQ 0 THEN Message, 'Must pass data to histogram.'
   
   ; What kind of data are we doing a HISTOGRAM on?
   dataType = Size(dataToHistogram, /TYPE)
      
   ; Check the data for NANs and alert the user if the NAN keyword is not set.
   IF dataType EQ 4 OR datatype EQ 5 THEN BEGIN
        goodIndices = Where(Finite(dataToHistogram), count, NCOMPLEMENT=nancount, COMPLEMENT=nanIndices)
        IF nancount GT 0 THEN BEGIN
           IF ~Keyword_Set(nan) THEN BEGIN
               Message, 'NANs found in the data. NAN keyword is set to 1.', /INFORMATIONAL
               nan = 1
           ENDIF
        ENDIF 
   ENDIF 
   
   ; The only sensible way to proceed is to make a copy of the data. Otherwise, I'll have
   ; a devil of a time putting it back together again at the end.
   IF N_Elements(_dataToHistogram) EQ 0 THEN _dataToHistogram = dataToHistogram
   
   ; If you have any "missing" data, then the data needs to be converted to float
   ; and the missing data set to F_NAN.
   IF N_Elements(missing) NE 0 THEN BEGIN
      missingIndices = Where(_dataToHistogram EQ missing, missingCount)
      IF missingCount GT 0 THEN BEGIN
         CASE datatype OF
            4: _dataToHistogram[missingIndices] = !Values.F_NAN
            5: _dataToHistogram[missingIndices] = !Values.D_NAN
            ELSE: BEGIN
                _dataToHistogram = Float(_dataToHistogram)
                dataType = 4
                _dataToHistogram[missingIndices] = !Values.F_NAN
                END
         ENDCASE
         nan = 1
      ENDIF ELSE BEGIN
        IF missingCount EQ N_Elements(_dataToHistogram) THEN $
            Message, 'All values are "missing"!'
      ENDELSE
   ENDIF
   
   ; Did someone pass the number of bins?
   IF N_Elements(nbins) NE 0 THEN theseBins = DOUBLE(nbins)

   ; Check for histogram keywords.
   IF N_Elements(binsize) EQ 0 THEN BEGIN
      range = Max(_dataToHistogram, /NAN) - Min(_dataToHistogram, /NAN)
      IF N_Elements(nbins) EQ 0 THEN BEGIN  ; Scott's Choice
         binsize = (3.5D * StdDev(_dataToHistogram, /NAN))/N_Elements(_dataToHistogram)^(1./3.0D) 
         IF (dataType LE 3) OR (dataType GE 12) THEN binsize = Round(binsize) > 1
         binsize = Convert_To_Type(binsize, dataType)
      ENDIF ELSE BEGIN
        binsize = range / (nbins -1)
         IF dataType LE 3 THEN binsize = Round(binsize) > 1
        binsize = Convert_To_Type(binsize, dataType)
      ENDELSE
   ENDIF ELSE BEGIN
       IF Size(binsize, /TYPE) NE dataType THEN BEGIN
         IF dataType LE 3 THEN binsize = Round(binsize) > 1
         binsize = Convert_To_Type(binsize, dataType)
       ENDIF
   ENDELSE

   ; Check for keywords.
   IF N_Elements(backColorName) EQ 0 THEN backColorName = "White"
   IF N_Elements(dataColorName) EQ 0 THEN dataColorName = "Indian Red"
   
   ; Choose an axis color.
   IF N_Elements(axisColorName) EQ 0 AND N_Elements(saxescolor) NE 0 THEN axisColorName = saxescolor
   IF N_Elements(axisColorName) EQ 0 THEN BEGIN
       IF (Size(backColorName, /TNAME) EQ 'STRING') && (StrUpCase(backColorName) EQ 'WHITE') THEN BEGIN
            IF !P.Multi[0] EQ 0 THEN axisColorName = 'BLACK'
       ENDIF
       IF N_Elements(axisColorName) EQ 0 THEN BEGIN
           IF !D.Name EQ 'PS' THEN BEGIN
                axisColorName = 'BLACK' 
           ENDIF ELSE BEGIN
                IF (!D.Window GE 0) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
                    pixel = cgSnapshot(!D.X_Size-1,  !D.Y_Size-1, 1, 1)
                    IF Total(pixel) EQ 765 THEN axisColorName = 'BLACK'
                    IF Total(pixel) EQ 0 THEN axisColorName = 'WHITE'
                    IF N_Elements(axisColorName) EQ 0 THEN axisColorName = 'OPPOSITE'
                ENDIF ELSE axisColorName = 'OPPOSITE'
           ENDELSE
       ENDIF
   ENDIF
   IF N_Elements(axisColorName) EQ 0 THEN axisColor = !P.Color ELSE axisColor = axisColorName
    
   IF N_Elements(polycolorname) EQ 0 THEN polycolorname = "Rose"
   IF N_Elements(probColorname) EQ 0 THEN probColorname = "Blue"
   frequency = Keyword_Set(frequency)
   line_fill = Keyword_Set(line_fill)
   IF line_fill THEN fillpolygon = 1
   fillpolygon = Keyword_Set(fillpolygon)
   IF fillpolygon THEN BEGIN
      IF N_Elements(orientation) EQ 0 THEN orientation = 0
      IF N_Elements(spacing) EQ 0 THEN spacing = 0
   ENDIF
   IF N_Elements(mininput) EQ 0 THEN mininput = Min(_dataToHistogram, NAN=nan)
   IF N_Elements(maxinput) EQ 0 THEN maxinput = Max(_dataToHistogram, NAN=nan)
   IF N_Elements(thick) EQ 0 THEN thick = 1.0

   ; Load plot colors.
   TVLCT, r, g, b, /GET
   axisColor = cgColor(axisColorName, FILE=file)
   dataColor = cgColor(datacolorName, FILE=file)
   backColor = cgColor(backColorName, FILE=file)
   polyColor = cgColor(polyColorName, FILE=file)
   probColor = cgColor(probColorName, FILE=file)

   ; Set up some labels.
   IF frequency THEN BEGIN
      ytitle = 'Relative Frequency'
      ytickformat = '(F6.4)'
   ENDIF ELSE BEGIN
      ytitle = 'Histogram Density'
      ytickformat = '(I)'
   ENDELSE

  ; Calculate the histogram.
   histdata = Histogram(_dataToHistogram, $
      BINSIZE=binsize, $
      L64=l64, $
      MAX=maxinput, $
      MIN=mininput, $
      NAN=nan, $
      LOCATIONS=locations, $
      OMAX=omax, $
      OMIN=omin, $
      REVERSE_INDICES=ri)
   IF frequency THEN histdata = Float(histdata)/N_Elements(_dataToHistogram)
   
   ; Need a probability distribution?
   IF Arg_Present(probablity) OR Keyword_Set(oprob) THEN BEGIN
       cumTotal = Total(histData, /CUMULATIVE)
       probability = Scale_Vector(cumTotal, 0, 1)
   ENDIF

   ; Calculate the range of the plot output.
;   ymin = 0
;   ymax = Max(histData)
;   ymax = ymax + (ymax * 0.05)
   IF N_Elements(min_value) EQ 0 THEN min_value = 0
   IF N_Elements(max_value) EQ 0 THEN max_value = Max(histData) * 1.05
   ymin = min_value
   ymax = max_value
   xmin = Double(omin) - binsize
   xmax = Double(omax) + (binsize * 2)

   ; Save the current system variables, if doing multiple plots.
   IF Total(!P.MULTI) NE 0 THEN BEGIN
      bangp = !P
      bangx = !X
      bangy = !Y
      bangmap = !MAP
   ENDIF
   
   ; Unless we are overplotting, draw the plot to establish a data coordinate system.
   ; Don't actually display anything yet, because we may have to repair damage caused
   ; by polygon filling.
   xrange = [xmin, xmax]
   yrange = [ymin, ymax]
   IF ~Keyword_Set(overplot) THEN BEGIN
       Plot, xrange, yrange, $             
             Background=backColor, $
             Color=axisColor, $                       ; The color of the axes.
             NoData=1, $                              ; Draw the axes only. No data.
             XTHICK=thick, $                          ; Axes thicker, if needed.
             YTHICK=thick, $
             XStyle=5, $                              ; Exact axis scaling. No autoscaled axes.
             YMinor=0, $                              ; No minor tick mark on X axis.
             YStyle=5, $                              ; Exact axis scaling. No autoscaled axes.
             XTickformat='(A1)', $                    ; No format. Nothing drawn
             YTickformat='(A1)', $                    ; No format. Nothing drawn
             _Strict_Extra=extra                      ; Pass any extra PLOT keywords.
   ENDIF

   ; Save the after-plot system variables, if doing multiple plots.
   ; You will need it to advance the plots in !P.MULTI, since you draw
   ; the plots with NOERASE.
   IF Total(!P.MULTI) NE 0 THEN BEGIN
       bangAfterp = !P
       bangAfterx = !X
       bangAftery = !Y
       bangAftermap = !MAP
   ENDIF

   ; Do we need to have things be filled?
   IF Keyword_Set(fillpolygon) THEN BEGIN

       ncolors = N_Elements(polycolor)

      ; Are we line filling?
      IF line_fill THEN BEGIN

         norient = N_Elements(orientation)
         nspace = N_Elements(spacing)
         step = (xrange[1] - xrange[0]) / (binsize + 1)
         start = xrange[0] + binsize
         endpt = start + binsize

         FOR j=0,N_Elements(histdata)-1 DO BEGIN
            x = [start, start, endpt, endpt, start]
            y = [0, histdata[j], histdata[j], 0, 0]
            fillcolor = polycolor[j MOD ncolors]
            orient = orientation[j MOD norient]
            space = spacing[j MOD nspace]
            PolyFill, x, y, COLOR=fillColor, /LINE_FILL, ORIENTATION=orient, $
               PATTERN=pattern, SPACING=space, NOCLIP=0
            start = start + binsize
            endpt = start + binsize
         ENDFOR

      ENDIF ELSE BEGIN ; Normal polygon color fill.

         step = (xrange[1] - xrange[0]) / (binsize + 1)
         start = xrange[0] + binsize
         endpt = start + binsize
         FOR j=0,N_Elements(histdata)-1 DO BEGIN
            x = [start, start, endpt, endpt, start]
            y = [0, histdata[j], histdata[j], 0, 0]
            fillcolor = polycolor[j MOD ncolors]
            PolyFill, x, y, COLOR=fillColor, NOCLIP=0
            start = start + binsize
            endpt = start + binsize
         ENDFOR

      ENDELSE
   ENDIF
      
   ; Restore the pre-plot system variables.
   IF Total(!P.MULTI) NE 0 THEN BEGIN
       !P = bangp
       !X = bangx
       !Y = bangy
       !MAP = bangmap
    ENDIF

   IF ~Keyword_Set(overplot) THEN BEGIN
       xrange = [xmin, xmax]
       yrange = [ymin, ymax]
       Plot, xrange, yrange, $             
             Background=backColor, $
             Color=axisColor, $                       ; The color of the axes.
             NoData=1, $                              ; Draw the axes only. No data.
             XThick=thick, $  
             YThick=thick, $
             XStyle=9, $                              ; Exact axis scaling. No autoscaled axes.
             YMinor=1, $                              ; No minor tick mark on X axis.
             YStyle=1, $                              ; Exact axis scaling. No autoscaled axes.
             YTickformat=ytickformat, $               ; Y Tickformat
             YTitle=ytitle, $                         ; Y Title
             NoErase=1, $
             XTicklen=-0.025, $
             _Strict_Extra=extra                      ; Pass any extra PLOT keywords.
             
        Axis, !X.CRange[0], !Y.CRange[1], XAXIS=1, XTickformat='(A1)', XMINOR=1, $
            COLOR=axisColor, XSTYLE=1, XTHICK=thick
    ENDIF
    step = (xrange[1] - xrange[0]) / (binsize + 1)
    start = xrange[0] + binsize
    endpt = start + binsize
    ystart = 0
    jend = N_Elements(histdata)-1
    FOR j=0,jend DO BEGIN
        IF Keyword_Set(outline) THEN BEGIN
           PLOTS, [start, start], [ystart, histdata[j]], COLOR=dataColor, THICK=thick, NOCLIP=0
           PLOTS, [start, endpt], [histdata[j], histdata[j]], COLOR=dataColor, THICK=thick, NOCLIP=0
           IF j EQ jend THEN $
              Plots, [endpt, endpt], [histdata[j], 0], COLOR=dataColor, THICK=thick, NOCLIP=0
           start = start + binsize
           endpt = start + binsize
           ystart = histdata[j]
        ENDIF ELSE BEGIN
           x = [start, start, endpt, endpt, start]
           y = [0, histdata[j], histdata[j], 0, 0]
           PLOTS, x, y, COLOR=dataColor, NOCLIP=0, THICK=thick
           start = start + binsize
           endpt = start + binsize
        ENDELSE
    ENDFOR
   
   ; Need to overplot probability function?
   IF Keyword_Set(oprob) THEN BEGIN
        proby = Scale_Vector(cumTotal, !Y.CRange[0], !Y.CRange[1])
        IF Keyword_Set(oplot) THEN bsize = 0 ELSE bsize = binsize
        probx = Scale_Vector(Findgen(N_Elements(proby)), !X.CRange[0] + bsize, !X.CRange[1] - bsize)
        Oplot, probx, proby, COLOR=probcolor
   ENDIF

   ; Advance the plot for !P.Multi purposes.
   IF Total(!P.MULTI) NE 0 THEN BEGIN
       !P = bangAfterp 
       !X = bangAfterx 
       !Y = bangAftery
       !MAP = bangAftermap
   ENDIF

   ; Clean up. But you really can't do this in the Z-buffer. 
   IF !D.Name NE 'Z' THEN TVLCT, r, g, b
   

END
