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

open(50,file='test.d')
do j=yin,yMax
  do i=xin,xMax
write(50,*) FDM(i,j)-LBM(i,j)
end do
end do
close(50)

  
end program main
