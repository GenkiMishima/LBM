module vrble
use prmtr
implicit none
integer i,j,t,out_freq
integer con
double precision x,y,time
double precision omega,csp
double precision,dimension(    xin:xMax,yin:yMax)::Tmpra,FDM,LBM
double precision,dimension(0:8,xin-1:xMax+1,yin-1:yMax+1)::Fun,Feq,Ftmp
double precision,dimension(0:8                  )::wght,cx,cy,cs
character tmpstring*20
end module vrble
