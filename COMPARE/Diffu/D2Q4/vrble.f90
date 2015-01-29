module vrble
use prmtr
implicit none
integer i,j,t,out_freq
double precision x,y
double precision omega,csp
double precision,dimension(    xin:xMax,yin:yMax)::Tmpra,FDM,LBM
double precision,dimension(1:4,xin:xMax,yin:yMax)::Fun,Feq
double precision,dimension(1:4                  )::wght
character tmpstring*20
end module vrble
