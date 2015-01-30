module vrble
use prmtr
implicit none
integer i,j,k,t
integer con
double precision time
double precision omegam,omegas,csp
double precision,dimension(1:5   ,xin  :xMax  ,yin  :yMax  ,zin  :zMax  )::Vari
double precision,dimension(0:prtl,xin-1:xMax+1,yin-1:yMax+1,zin-1:zMax+1)::Fun,Feq,Gun,Geq
double precision,dimension(       xin  :xMax  ,yin  :yMax  ,zin  :zMax  )::x,y,z
double precision,dimension(0:prtl                                       )::wght,cx,cy,cz,cs
character tmpstring*20
end module vrble
