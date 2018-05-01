;+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  function fix_fft.pro
;;  Jan 13, 2010
;;  Mike Zemcov
;;  This function takes a 2D FFT'd array and shuffles it around to look 
;;   more like we're used to seeing it with low modes towards the 
;;   middle of the plot. 
;;  Inputs: data - 2d fft of some image
;;  Outputs: data - reshuffled 2d fft of the image - low modes are at 
;;   the middle now, not the edge.
;;  Options: none
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-


FUNCTION fix_fft,data

  sizedata = size(data)
  lims = INTARR(4,2)
  FOR i=0,1 DO BEGIN
     lims[1,i] = sizedata[i + 1] / 2 - 1
     lims[2,i] = sizedata[i + 1] / 2
     lims[3,i] = sizedata[i + 1] - 1
  ENDFOR

  data[lims[0,0]:lims[1,0],lims[0,1]:lims[1,1]] = $
     ROTATE(data[lims[0,0]:lims[1,0],lims[0,1]:lims[1,1]],2)
  data[lims[2,0]:lims[3,0],lims[0,1]:lims[1,1]] = $
     ROTATE(data[lims[2,0]:lims[3,0],lims[0,1]:lims[1,1]],2)
  data[lims[0,0]:lims[1,0],lims[2,1]:lims[3,1]] = $
     ROTATE(data[lims[0,0]:lims[1,0],lims[2,1]:lims[3,1]],2)
  data[lims[2,0]:lims[3,0],lims[2,1]:lims[3,1]] = $
     ROTATE(data[lims[2,0]:lims[3,0],lims[2,1]:lims[3,1]],2)

  RETURN,data


END
