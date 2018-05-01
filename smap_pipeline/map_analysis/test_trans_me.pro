; test the transfunction code

;dir_data='/data/spiredaq/reprocessed/LockmanSw_L1e/' + $                                                                                       
;         ['1342186108', '1342186109'] + $ 
;         '/level1/herschel.spire.ia.dataset.PointedPhotTimeline/'

; give the location of the L1 data we want to compare to
dir_data='/data/spiredaq/reprocessed/LockmanSw_L1b/' + $
         ['1342186108', '1342186109'] + $
         '/level1/herschel.spire.ia.dataset.PointedPhotTimeline/'
;map_template='/data/spire/release/v2.0/lockman-swire/LockmanSw_image_'+['250','350','500']+'_SMAP_v2.0.fits'
; give the location of the map we want to compare too, be careful to be consistent with L1 data
map_template='/data/spire/release/v1.0/lockman-swire/LockmanSw_image_'+['250','350','500']+'_SMAP_v1.0.fits'
; first index of the simulated sky to run on
startnum=0
; last index of the simulated sky to run on
endnum=9
; extension name of the produced maps and of the Cls file
skypix_pixsize=2.0
skypix_side=2.^(13.)
skypix_side=10580.
exnamebase=strcompress('_'+string(skypix_pixsize,format='(i10)')+'x'+ $
   ;string(skypix_side,format='(i10)')+'x'+string(skypix_side,format='(i10)')+'',/remove_all)
   string(skypix_side,format='(i10)')+'x'+string(skypix_side,format='(i10)')+'_cs',/remove_all)
;exnamebase='_test'
; directory where the simulated sky signal is located 
;dirsim='/data/amblard/sim_transfunc/locksw/'
;dirsim='/data/viero/sim_transfunc/lockman-swire/'
dirsim='/data/viero/sim_transfunc/clean_skies/'
; location of the files containing the mask applied to the data and the correction matrix mkk (to be computed beforehand)
maskfile='~/work/pk_hermes/locksw-mkkbis_50mJy_'+['500','350','250']+'mi.save'
outputfiles='/data/viero/tf/'


test=SMAP_TRANSFUN_PIPELINE(dir_data,map_template,startnum,endnum,exnamebase,maskfile, $
   dirsim=dirsim,dirsave=outputfiles,hi_cut=0.5,skypix_side=skypix_side, $
   skypix_pixsize=skypix_pixsize,clean_skies=1);,lmin=0)


end
