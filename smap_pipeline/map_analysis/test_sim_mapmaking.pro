filename='/data/viero/deleteme/test_fls_sims.sav'
restore, filename;, /verbose

stop
exnamebasewnum=exnamebase+'_'+strcompress(ii,/remove_all)
CREATE_ITERMAP, datadirbase, dataname, dataversion, $
   SAVETODS=savetods, TODS=tods, MAPPARAM=mapparam, $
   EXNAME=exnamebasewnum, EXAPP=exapp, $
   DO_PTRS=do_ptrs, DO_ONEBAND=do_oneband, DO_TDC=do_tdc, $
   ASTROMNAME=astromname, NOASTROM=noastrom, $
   JKLIST=jklist, DO_JKBOLO=do_jkbolo, DO_AORMAPS=do_aormaps, $
   DO_MATCHED=do_matched, INSTNOISE=instnoise,$
   BADBOLOS=badbolos, EXCLUDEMASK=excludemask, $
   ITERPARAMS=iterparams, NITER=niter, FIRST_OFFS=first_offs, $
   FIRST_GAIN=first_gain, FIRST_WT=first_wt, $
   FIRST_CLIP=first_clip, FIXED_NT=fixed_nt, NTERMS=nterms, $
   NT_SCALE=nt_scale, MIN_HITS=min_hits, $
   GROW_CLIP=grow_clip, CLIPSIGMA=clipsigma,$
   ITERDIAG=iterdiag, PIXSIZE=pixsize, SHORTNAME=shortname, $
   NO250=no250, NO350=no350, NO500=no500, $
   NXPIX=nxpix, NYPIX=nypix, CRVALX=crvalx, CRVALY=crvaly, $
   CRPIXX=crpixx, CRPIXY=crpixy, SPEEDCUT=speedcut, $
   BADOBSID=badobsid, FIXEDPARAMS=fixedparams,$
   LINEARCORR=linearcorr

end
