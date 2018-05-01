;+
;NAME
; general_cutout
;PURPOSE
; To create a cutout of a FITS image around a specified
;location and write it to disk
;USAGE
; general_cutout, image, head, ra, dec, size, outfile
;INPUTS
; image   image as array
; head    header of image
; ra      RA of center of new map
; dec     DEC of center of new map
; size    Size (in arcsec) of cutout
; outfile File to write new image to (with WRITEFITS)
;MODIFICATION HISTORY
; Author: Alex Conley, June 2011
;-

PRO general_cutout, image, head, ra, dec, sz, outfile
  COMPILE_OPT IDL2

  IF N_ELEMENTS(image) EQ 0 THEN MESSAGE,"No input image"
  IF N_ELEMENTS(head) EQ 0 THEN MESSAGE,"Must provide header"
  IF sz LE 0 THEN MESSAGE,"Size is invalid"

  EXTAST,head, astr, noparams
  IF noparams EQ -1 THEN MESSAGE,"Header had no astrometry information"

  imsz = SIZE(image)
  IF imsz[0] NE 2 THEN MESSAGE,"Image is not 2D"
  ixsize = imsz[1]
  iysize = imsz[2]

  ;;Figure out the clip area
  IF SIZE(ra,/TNAME) EQ 'STRING' THEN work_ra = ra_to_radeg(ra) ELSE $
     work_ra = DOUBLE(ra)
  IF SIZE(dec,/TNAME) EQ 'STRING' THEN work_dec = dec_to_decdeg(dec) ELSE $
     work_dec = DOUBLE(dec)


  AD2XY, work_ra, work_dec, astr, xcen, ycen
  xcen = ROUND(xcen)
  ycen = ROUND(ycen)

  IF xcen LT 0 OR ycen LT 0 OR xcen GT ixsize-1 OR $
     ycen GT iysize-1 THEN MESSAGE,"Provided coordinates outside map"

  GETROT, astr, rot, cdelt
  xpixscale = 3600.0 * ABS(cdelt[0])
  ypixscale = 3600.0 * ABS(cdelt[1])

  xpix_extent = CEIL( 0.5 * sz / xpixscale )
  ypix_extent = CEIL( 0.5 * sz / ypixscale )

  xlow = (xcen - xpix_extent) > 0
  xhigh = (xcen + xpix_extent) < (ixsize-1)
  ylow = (ycen - ypix_extent) > 0
  yhigh = (ycen + ypix_extent) < (iysize-1)

  xsize = xhigh-xlow+1
  ysize = yhigh-ylow+1

  IF xsize LT 5 OR ysize LT 5 THEN MESSAGE,"Requested extent is too small"

  astr.crpix[0] -= xlow
  astr.crpix[1] -= ylow

  newhd = head
  PUTAST, newhd, astr


  newimage = image[xlow:xhigh,ylow:yhigh]

  WRITEFITS, outfile, newimage, newhd
END
