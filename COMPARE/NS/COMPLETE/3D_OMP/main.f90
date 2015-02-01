program main
use prmtr
use vrble
implicit none
print *,'Renolds',Re,'alpha',alpha
call IC
print *,'out_freq',out_freq
print *,'LBM'
do t=tin,tMax
time=dble(t)*dt
  call collision
  call streaming
  call BC
  call set_vari
  call output
end do
end program main
