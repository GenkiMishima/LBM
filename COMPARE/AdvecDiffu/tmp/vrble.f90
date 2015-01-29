module vrble
use prmtr
implicit none
integer i,j,t,out_freq
integer con
double precision x,y,time
double precision omega,csp
double precision,dimension(             xin:xMax,yin:yMax)::Tmpra,FDM,LBM
double precision,dimension(0:Particle-1,xin-1:xMax+1,yin-1:yMax+1)::Fun,Feq
double precision,dimension(0:Particle-1                  )::wght,cx,cy,cs
character tmpstring*20
end module vrble
