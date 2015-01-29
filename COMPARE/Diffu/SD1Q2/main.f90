program main
use prmtr
use vrble
implicit none
call IC
do t=tin,tMax
  call FiniDifMet
  call LattBolMet
  call output
end do

  
end program main
