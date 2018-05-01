function cumulative_counts_given_flux, s_lo, wv=wv, verbose=verbose

;s_lo in Jy

dir_counts='/data/viero/counts/counts_ascii/'

if not(keyword_set(wv)) then wv=250

filename=strcompress(dir_counts+'Bethermin_model_counts_'+$
	string(wv,format='(i10)')+'um.txt',/remove_all)

readcol, filename, format='d,d,d,d,d,d',$
	s,dnds,dnds_1sig_up,dnds_1sig_down,dnds_2sig_up,dnds_2sig_down,/silent
ns=n_elements(s)
ind_min=closest(s,s_lo)
ngs=0d
for i=ns-2,ind_min,-1 do $
   ngs+=dnds[i]*(s[i+1]-s[i])

if keyword_set(verbose) then $
   print,string(ngs)+ ' sources with flux greater than '+$
   strcompress(string(s_lo*1000.,format='(e10.2)'),/remove_all)+' mJy'

return, ngs

end
