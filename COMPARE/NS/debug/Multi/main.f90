program main
use prmtr
use vrble
!$ use omp_lib
implicit none
call IC
print *,'Renolds',Re,'alpha',alpha
print *,'out_freq',out_freq
print *,'LBM'
do t=tin,tMax
time=dble(t)*dt
  call collision
  call streaming
  call BC
  !call OBJECT
  call set_vari
  call output
end do
end program main
