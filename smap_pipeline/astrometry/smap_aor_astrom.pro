;+
; SMAP_AOR_ASTROM, datadir, srccatfile, outfile, thumbsize, $
;                  GOODRAD=goodrad, GOODFRAC=goodfrac
;
; Measure astrometry offsets on set of maps in datadir/ by stacking
; on source catalog give in srccatfile. Offsets are output to file in 
; a format that is used by the map maker.
;
; CREATED BY gmarsden 2011-02-25
;
; CHANGELOG:
;   20110321, GM: add GOODRAD and GOODFRAC params (for stack_thumb.pro)
;   20110323, GM: add WIDTH keyword to OPENW call
;   20110711, GM: flip sign of shift_ra for updated shift calculation i
;                 (see smap_astrom_stack.pro)
;-

PRO SMAP_AOR_ASTROM, mapdir, srccatfile, outfile, thumbsize, $
                     GOODRAD=goodrad, GOODFRAC=goodfrac

  ; find all 250 micron maps in datadir
  maps = FILE_SEARCH(ADDSLASH(mapdir)+"*_PSW.fits", COUNT=nmaps)

  IF nmaps EQ 0 THEN BEGIN
      MESSAGE, "No maps found in '" + mapdir + "'.", /INF
      RETURN
  ENDIF

  ; find maps matching format <field>_itermap_<date>_<obsid>_PSW.fits
  regex = '[^_]+_itermap_[0-9]+_([0-9]+)_PSW\.fits'
  aormapind = WHERE(STREGEX(FILE_BASENAME(maps), '^'+regex+'$', /BOOL), $
                    naormaps)

  ; read src catalog
  READCOL, srccatfile, racat, deccat, fluxcat, errorcat, COMM="#", /SILENT

  ; open output file
  OPENW, lun, outfile, /GET_LUN, WIDTH=256

  ; write header
  PRINTF, lun, $
          "#obsid     delta_RA[arcsec] delta_Dec[arcsec] RA_tan[deg] Dec_tan[deg] N_srcs"

  ; loop over aormaps
  FOR i=0,naormaps-1 DO BEGIN

      thismap = maps[aormapind[i]]

      PRINT, "Stacking '", FILE_BASENAME(thismap), "' on ", $
             STRTRIM(STRING(N_ELEMENTS(racat)),2), ' sources...'

      ; extract obsid name
      temp = STREGEX(thismap, regex, /EXTRACT, /SUBEXPR)
      obsid = temp[1]

      ; run stack
      SMAP_ASTROM_STACK, thismap, racat, deccat, thumbsize, $
                         sigthumb, errthumb, pars, shift, width, $
                         theta, nsrcs, GOODRAD=goodrad, GOODFRAC=goodfrac

      PRINT, "... used ", STRTRIM(STRING(nsrcs),2), " sources."

      ; shift values
      ; negative sign so that shift can be added to pointing
      shift_ra = -shift[0]
      shift_dec = -shift[1]

      PRINT, "shifts are: ", shift_ra, shift_dec
      PRINT

      ; tan point of map
      maphead = HEADFITS(thismap, EXT=1)
      ra_tan = SXPAR(maphead, "CRVAL1")
      dec_tan = SXPAR(maphead, "CRVAL2")

      ; print to file
      PRINTF, lun, obsid, shift_ra, shift_dec, ra_tan, dec_tan, nsrcs
  ENDFOR

  ; close file
  FREE_LUN, lun

END
