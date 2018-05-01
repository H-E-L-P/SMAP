function make_k, sidex,sidey=sidey,res=res

IF NOT(KEYWORD_SET(res)) THEN res = 1.
IF NOT(KEYWORD_SET(sidey)) THEN sidey = sidex

taillei = res/60. * !pi/10800.

kx=FINDGEN(sidex/2+1)/(taillei*sidex)*2*!pi
ky=FINDGEN(sidey/2+1)/(taillei*sidey)*2*!pi
IF (sidex mod 2 eq 0) THEN $
	kx=[kx,-1.*REVERSE(kx[1:sidex/2-1])] $
	ELSE $
	kx=[kx,-1.*REVERSE(kx[1:sidex/2])]
IF (sidey mod 2 eq 0) THEN $
	ky=[ky,-1.*REVERSE(ky[1:sidey/2-1])] $
	ELSE $
	ky=[ky,-1.*REVERSE(ky[1:sidey/2])]

kx=kx#(ky*0+1)
ky=(kx[*,0]*0+1)#ky
k=sqrt(kx^2+ky^2)
kx=0b
ky=0b

RETURN,k

end

