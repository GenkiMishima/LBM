module vrble
use prmtr
implicit none
integer i,j,t
double precision x,y
double precision omega,csp
double precision,dimension(xin:xMax)::Tmpra,FDM,LBM
double precision,dimension(1:2,xin:xMax)::Fun,Feq
double precision,dimension(1:2)::wght
character tmpstring*20
end module vrble
