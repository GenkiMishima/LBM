subroutine IC(dt)
use prmtr
use vrble
implicit none
double precision,intent(in):: dt
wght(0)=2d0/6d0
wght(1)=1d0/6d0
wght(2)=1d0/6d0
wght(3)=1d0/6d0
wght(4)=1d0/6d0
  cx(0)= 0d0
  cx(1)= 1d0
  cx(2)=-1d0
  cx(3)= 0d0
  cx(4)= 0d0
  cy(0)= 0d0
  cy(1)= 0d0
  cy(2)= 0d0
  cy(3)= 1d0
  cy(4)=-1d0
  cs(:)=sqrt(cx(:)**2+cy(:)**2)/sqrt(2d0)
csp=dx**2/dt**2
omega = 1d0/(4d0*alpha/(dt*csp)+5d-1)
out_freq=0
!IC
Tmpra=0d0
Tmpra(0,:)=Twall
Tmpra(:,0)=Twall
FDM=Tmpra
LBM=Tmpra
do j=yin,yMax
  do i=xin,xMax
    Fun(0,i,j)=wght(0)*LBM(i,j)
    Fun(1,i,j)=wght(1)*LBM(i,j)
    Fun(2,i,j)=wght(2)*LBM(i,j)
    Fun(3,i,j)=wght(3)*LBM(i,j)
    Fun(4,i,j)=wght(4)*LBM(i,j)
  end do
end do
end subroutine IC
