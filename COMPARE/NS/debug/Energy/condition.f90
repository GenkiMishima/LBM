subroutine IC !{{{
use prmtr
use vrble
implicit none
integer la,ln
double precision tmp(5)
tmp(2)=0d0
do j=yin,yMax
  tmp(1)=0d0
  do i=xin,xMax
    x(i,j) = tmp(1)
    r(i,j) = tmp(2)
    tmp(1)=dx+tmp(1)
  end do 
    tmp(2)=dy+tmp(2)
end do 
open(50,file='grid.d')
do j=yin,yMax
  do i=xin,xMax
    write(50,*) x(i,j),r(i,j)
  end do 
end do 
close(50)

omegam= 1d0/(3d0*nu+5d-1)
omegas= 1d0/(3d0*alpha+5d-1)
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
cy(0)= 0d0
tmp(1)=dx/dt
tmp(2)=dx/dt
do la=1,4
  cx(la)=cos((dble(la)-1d0)*pi*5d-1)*tmp(1)
  cy(la)=sin((dble(la)-1d0)*pi*5d-1)*tmp(2)
  ln=4+la
  cx(ln)=sqrt(2d0)*cos((dble(ln)-5d0)*pi*5d-1+pi*25d-2)*tmp(1)
  cy(ln)=sqrt(2d0)*sin((dble(ln)-5d0)*pi*5d-1+pi*25d-2)*tmp(2)
end do
  cx(1)= 1d0
  cx(2)= 0d0
  cx(3)=-1d0
  cx(4)= 0d0
  cx(5)= 1d0
  cx(6)=-1d0
  cx(7)=-1d0
  cx(8)= 1d0
  cy(1)= 0d0
  cy(2)= 1d0
  cy(3)= 0d0
  cy(4)=-1d0
  cy(5)= 1d0
  cy(6)= 1d0
  cy(7)=-1d0
  cy(8)=-1d0
  cs(:)=sqrt(cx(:)**2+cy(:)**2)/sqrt(2d0)
!IC
Vari(1,:,:)=rhoo
Vari(2,:,:)=0d0
Vari(3,:,:)=0d0
Vari(4,:,:)=0d0
!Vari(2,:,yMax)=uo
!Vari(4,:,yMax)=To
do j=yin,yMax
  do i=xin,xMax
    Fun(:,i,j)=wght(:)*Vari(1,i,j)
  end do 
end do 

end subroutine IC
!}}}
subroutine BC !{{{
use prmtr
use vrble
implicit none
integer la
double precision tmp(5)
double precision,dimension(0:8)::tmp_vec,force
do j=yin,yMax
  !BounceBack BCWest
  tmp(1)=(Fun(0, xin,j)+Fun(2, xin,j)+Fun(4, xin,j)+2d0*(Fun(3, xin,j)+Fun(6, xin,j)+Fun(7, xin,j)))/(1d0-uo)
  Fun(1, xin,j)=Fun(3, xin,j)+2d0/3d0*tmp(1)*uo
  Fun(5, xin,j)=Fun(7, xin,j)-5d-1*(Fun(2, xin,j)-Fun(4, xin,j))+1d0/6d0*tmp(1)*uo
  Fun(8, xin,j)=Fun(6, xin,j)+5d-1*(Fun(2, xin,j)-Fun(4, xin,j))+1d0/6d0*tmp(1)*uo
  !Scaler BCWest
end do
do j=yin,yMax
    tmp(1)=0d0
    do la=0,8
      tmp(1)=tmp(1)+Fun(la,xin,j)
    end do
    Fun(:,xin,j)=Fun(:,xin,j)/tmp(1)
end do

do j=yin,yMax
  !BounceBack BCEast
  Fun(:,xMax,j)=Fun(:,xMax-1,j)
end do
do j=yin,yMax
    tmp(1)=0d0
    do la=0,8
      tmp(1)=tmp(1)+Fun(la,xMax,j)
    end do
    Fun(:,xMax,j)=Fun(:,xMax,j)/tmp(1)
end do
do i=xin+1,xMax-1
!do i=xin,xMax
  !BounceBack BCSouth
  Fun(2,i, yin)=Fun(4,i,yin)
  Fun(5,i, yin)=Fun(7,i,yin)
  Fun(6,i, yin)=Fun(8,i,yin)
  !Fun(2,i, yin)=Fun(4,i,yin)
  !Fun(5,i, yin)=Fun(8,i,yin)
  !Fun(6,i, yin)=Fun(7,i,yin)

!end do
!do i=xin+1,xMax-1
  !MovingLid BCNorth
  Fun(4,i,yMax)=Fun(2,i,yMax)
  Fun(7,i,yMax)=Fun(6,i,yMax)
  Fun(8,i,yMax)=Fun(5,i,yMax)

end do

end subroutine BC
!}}}
subroutine set_vari!{{{
use prmtr
use vrble
implicit none
integer la
double precision tmp(5)
do j=yin,yMax
  do i=xin,xMax
    tmp(1)=0d0
    do la=0,8
      tmp(1)=tmp(1)+Fun(la,i,j)
    end do
    Vari(1,i,j)=tmp(1)
    if(Vari(1,i,j).lt.0d0)then
      print *,i,j,Vari(1,i,j)
      print *,'rho is under zero.'
      call exit
    end if
  end do
end do
do j=yin,yMax
  do i=xin,xMax-1
    tmp(2)=0d0
    tmp(3)=0d0
    do la=0,8
      tmp(2)=tmp(2)+Fun(la,i,j)*cx(la)
      tmp(3)=tmp(3)+Fun(la,i,j)*cy(la)
    end do
    Vari(2,i,j)=tmp(2)/Vari(1,i,j)
    Vari(3,i,j)=tmp(3)/Vari(1,i,j)
    tmp(4)=0d0
    do la=0,8
      tmp(4)=tmp(4)+Fun(la,i,j)*(cx(la)**2+cy(la)**2)
    end do
    tmp(1)=Vari(2,i,j)**2+Vari(3,i,j)**2
    Vari(4,i,j)=5d-1*(tmp(4)/Vari(1,i,j)-tmp(1))
    
  end do
end do
!do j=yin,yMax
!  do i=xin,xMax
!    Vari(4,i,j)=Gun(0,i,j)+Gun(1,i,j)+Gun(2,i,j)+Gun(3,i,j)+Gun(4,i,j)+Gun(5,i,j)+Gun(6,i,j)+Gun(7,i,j)+Gun(8,i,j)
!  end do
!end do

end subroutine set_vari
!}}}
