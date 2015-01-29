subroutine IC(dt)
use prmtr
use vrble
implicit none
double precision,intent(in):: dt
wght(0)=4d0/9d0
wght(1)=1d0/9d0
wght(2)=1d0/9d0
wght(3)=1d0/9d0
wght(4)=1d0/9d0
wght(5)=1d0/36d0
wght(6)=1d0/36d0
wght(7)=1d0/36d0
wght(8)=1d0/36d0
  cx(0)= 0d0
  cx(1)= 1d0
  cx(2)= 0d0
  cx(3)=-1d0
  cx(4)= 0d0
  cx(5)= 1d0
  cx(6)=-1d0
  cx(7)=-1d0
  cx(8)= 1d0
  cy(0)= 0d0
  cy(1)= 0d0
  cy(2)= 1d0
  cy(3)= 0d0
  cy(4)=-1d0
  cy(5)= 1d0
  cy(6)= 1d0
  cy(7)=-1d0
  cy(8)=-1d0
  cs(:)=sqrt(cx(:)**2+cy(:)**2)/sqrt(2d0)
csp=dx**2/dt**2
omega = 1d0/(9d0*alpha/(dt*csp)+5d-1)
out_freq=0
!IC
Tmpra=0d0
Tmpra(0,:)=Twall
!Tmpra(:,0)=Twall
FDM=Tmpra
LBM=Tmpra
do j=yin,yMax
  do i=xin,xMax
    Fun(:,i,j)=wght(:)*LBM(i,j)
  end do
end do
Fun(:, xin-1,:)=Fun(:, xin,:)
Fun(:,xMax+1,:)=Fun(:,xMax,:)
Fun(:,:, yin-1)=Fun(:,:, yin)
Fun(:,:,yMax+1)=Fun(:,:,yMax)
end subroutine IC
