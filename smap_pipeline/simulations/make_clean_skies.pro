dir='/data/viero/testdir/'
outmapdir=dir;'/data/viero/sim_transfunc/source_filled_skies/'
cleanskymapdir='/data/viero/sim_transfunc/clean_skies/'

file_in=strcompress('/home/viero/hers/data/Cl_zbin-'+$
   string(0.9,format='(d10.2)')+'_'+$
   string(250,format='(i10)')+'.sav',/remove_all)
restore, file_in

its=1
it0=0
side=2.*9600.0;2.*2048.
res=6.;3.0

exname='_fls'
exname='_helms'

for it=0+it0,its+it0-1 do begin
   clean_sky_filename=strcompress(cleanskymapdir+'clean_sky_'+$
      string(res, format='(i10)')+'x'+$
      string(side, format='(i10)')+'x'+$
      string(side, format='(i10)')+'_'+$
      string(it, format='(i10)')+$
      exname+'.fits',/remove_all)
   ind_p=where(cl_cib ne 0)
   probmap=float(biased_sky(cl_cib[ind_p], ell[ind_p], res, side, $
      hd=hd,writefileto=clean_sky_filename)) 
endfor

end
