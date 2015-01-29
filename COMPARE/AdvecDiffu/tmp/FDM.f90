subroutine FiniDifMet(dt)
use prmtr
use vrble
implicit none
double precision,dimension(    xin:xMax,yin:yMax)::Advec,Diffu
double precision tmp(5)
double precision,intent(in):: dt
!FDM
do j=yin+1,yMax-1
  do i=xin+1,xMax-1
    !Advection
    if(Uvel.gt.0d0)then
      tmp(1)=(FDM(i,j)-FDM(i-1,j))/dx
    elseif(Uvel.lt.0d0)then
      tmp(1)=(FDM(i+1,j)-FDM(i,j))/dx
    endif
    if(Vvel.gt.0d0)then
      tmp(2)=(FDM(i,j)-FDM(i,j-1))/dy
    elseif(Uvel.lt.0d0)then
      tmp(2)=(FDM(i,j+1)-FDM(i,j))/dy
    endif
    Advec(i,j)=Uvel*tmp(1)+Vvel*tmp(2)
    !Diffusion
    tmp(1)=(FDM(i+1,j)+FDM(i-1,j)-2d0*FDM(i,j))/dx**2
    tmp(2)=(FDM(i,j+1)+FDM(i,j-1)-2d0*FDM(i,j))/dy**2
    Diffu(i,j)=alpha*(tmp(1)+tmp(2))
  end do
end do
do j=yin+1,yMax-1
  do i=xin+1,xMax-1
    FDM(i,j)=FDM(i,j)+dt*(-Advec(i,j)+Diffu(i,j))
  end do
end do

if(con.eq.0)then
open(50,file='test.d')
do j=yin,yMax
  do i=xin,xMax
write(50,*) Advec(i,j)-Diffu(i,j)
end do
end do
con=1
close(50)
end if

!do j=yin+1,yMax-1
!do i=xin+1,xMax-1
!  FDM(i,j)=FDM(i,j)+alpha*dt/dx**2*(FDM(i+1,j)+FDM(i-1,j)-2d0*FDM(i,j))+alpha*dt/dx**2*(FDM(i,j+1)+FDM(i,j-1)-2d0*FDM(i,j))
!end do
!end do
!BC
FDM(xMax,   :)=FDM(xMax-1,     :)
FDM(   :,yMax)=FDM(     :,yMax-1)
end subroutine FiniDifMet

