program main
use prmtr
use vrble
implicit none
call IC
print *,'Renolds',Re,'alpha',alpha
print *,'LBM'
!do t=tin,1
do t=tin,tMax
time=dble(t)*dt
  call collision
  call streaming
  call BC
  call set_vari
  call output
end do
end program main
