program main
use prmtr
use vrble
implicit none
double precision dt
call IC(dt)
dt=2d-1
con=0
do t=tin,tMax
time=dble(t)*dt
  call FiniDifMet(dt)
  call output('F',FDM)
  if(out_freq.eq.1)exit
end do
print *,'FDM'
dt=1d0
call IC(dt)
do t=tin,tMax
time=dble(t)*dt
  call LattBolMet(dt)
  call output('L',LBM)
  if(out_freq.eq.1)exit
end do
print *,'LBM'
end program main
