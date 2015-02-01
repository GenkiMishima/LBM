module prmtr
implicit none
integer         ,parameter::     xin=0
integer         ,parameter::    xMax=2e2
integer         ,parameter::     yin=0
integer         ,parameter::    yMax=1e2
integer         ,parameter::     zin=0
integer         ,parameter::    zMax=1e2
integer         ,parameter::     tin=1
integer         ,parameter::    tMax=5e5
integer         ,parameter::    prtl=14e0
integer         ,parameter::     Dmn=1
integer         ,parameter::out_freq=5e2
integer         ,parameter::    ob_x=5e1
integer         ,parameter::    ob_y=5e1
integer         ,parameter::    ob_z=5e1
integer         ,parameter:: ob_size=5e0
double precision,parameter::      uo=2d-1
double precision,parameter::      TH=1d0
double precision,parameter::      TC=0d0
double precision,parameter:: sumvelo=0d0
double precision,parameter::    rhoo=1d0
double precision,parameter::      dt=1d0
double precision,parameter::      dx=dt
double precision,parameter::      dy=dx
double precision,parameter::      dz=dx
double precision,parameter::      nu=2d-2
double precision,parameter::      Pr=0.71d0
double precision,parameter::   alpha=nu/Pr
double precision,parameter:: gravity=1d1
double precision,parameter::      Ra=1d5
double precision,parameter::      Re=uo*xMax/alpha

end module prmtr
