subroutine FiniDifMet
use prmtr
use vrble
implicit none
!FDM
do j=yin+1,yMax-1
do i=xin+1,xMax-1
  FDM(i,j)=FDM(i,j)+alpha*dt/dx**2*(FDM(i+1,j)+FDM(i-1,j)-2d0*FDM(i,j))+alpha*dt/dx**2*(FDM(i,j+1)+FDM(i,j-1)-2d0*FDM(i,j))
end do
end do
!BC
FDM(xMax,   :)=0d0
FDM(   :,yMax)=0d0
end subroutine FiniDifMet

