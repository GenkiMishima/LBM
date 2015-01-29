module prmtr
implicit none
integer         ,parameter::     xin=0
integer         ,parameter::    xMax=1e2
integer         ,parameter::     yin=0
integer         ,parameter::    yMax=1e2
integer         ,parameter::     tin=1
integer         ,parameter::    tMax=5e6
integer         ,parameter::     Dmn=1
integer         ,parameter::out_freq=1e5
double precision,parameter::      uo=1d-1
double precision,parameter:: sumvelo=0d0
double precision,parameter::    rhoo=1d0
double precision,parameter::      dt=1d0
double precision,parameter::      dx=dt
double precision,parameter::      dy=dx
double precision,parameter::   alpha=1d-2
double precision,parameter::      Re=uo*xMax/alpha

end module prmtr
