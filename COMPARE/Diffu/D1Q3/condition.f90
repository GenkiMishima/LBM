subroutine IC
use prmtr
use vrble
implicit none
wght(0)=2d0/3d0
wght(1)=1d0/6d0
wght(2)=1d0/6d0
csp=dx**2/dt**2
!omega = alpha*(dt*dble(Dmn)/dx**2)+5d-1
!omega = 1d0/omega
omega = 1d0/(4.5d0*alpha/(dt*csp)+5d-1)
j=0
!IC
Tmpra=0d0
Tmpra(0)=Twall
FDM=Tmpra
LBM=Tmpra
do i=xin,xMax
  Fun(:,i)=wght(:)*LBM(i)
end do

end subroutine IC
