subroutine IC
use prmtr
use vrble
implicit none
wght(1)=5d-1
wght(2)=5d-1
csp=dx**2/dt**2
!omega = alpha*(dt*csp*dble(Dmn)/dx**2)+5d-1
!omega = 1d0/omega
omega = 1d0/(1.5d0*alpha/(dt*csp)+5d-1)
source= 1d-2
j=0
!IC
Tmpra=0d0
Tmpra(0)=Twall
FDM=Tmpra
LBM=Tmpra
do i=xin,xMax
  Fun(1,i)=wght(1)*LBM(i)
  Fun(2,i)=wght(2)*LBM(i)
end do

end subroutine IC
