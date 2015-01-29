subroutine FiniDifMet
use prmtr
use vrble
implicit none
!FDM
do i=xin+1,xMax-1
  FDM(i)=FDM(i)+alpha*dt/dx**2*(FDM(i+1)+FDM(i-1)-2d0*FDM(i))
end do
!BC
FDM(xMax)=0d0
end subroutine FiniDifMet

