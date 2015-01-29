module prmtr
implicit none
integer         ,parameter::  xin=0
integer         ,parameter:: xMax=1e2
integer         ,parameter::  yin=0
integer         ,parameter:: yMax=1e2
integer         ,parameter::  tin=1
integer         ,parameter:: tMax=5e5
integer         ,parameter::  Dmn=1
!double precision,parameter::   dt=1d0
double precision,parameter::   dx=1d0
double precision,parameter::   dy=1d0
double precision,parameter::alpha=2.5d-1
double precision,parameter::Twall=1d0
double precision,parameter:: Uvel=1d-1
double precision,parameter:: Vvel=4d-1

end module prmtr
