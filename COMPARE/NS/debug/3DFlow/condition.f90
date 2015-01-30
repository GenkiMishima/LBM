subroutine IC !{{{
use prmtr
use vrble
implicit none
double precision tmp(5)
tmp(3)=0d0
do k=zin,zMax
  tmp(2)=0d0
  do j=yin,yMax
    tmp(1)=0d0
    do i=xin,xMax
      x(i,j,k) = tmp(1)
      y(i,j,k) = tmp(2)
      z(i,j,k) = tmp(3)
      tmp(1)=dx+tmp(1)
    end do 
    tmp(2)=dy+tmp(2)
  end do 
  tmp(3)=dz+tmp(3)
end do 
open(50,file='grid.d')
do k=zin,zMax
  do j=yin,yMax
    do i=xin,xMax
      write(50,*) x(i,j,k),y(i,j,k),z(i,j,k)
    end do 
  end do 
end do 
close(50)
open(50,file='ob_grid.d')
write(50,*) dx*dble(ob_x-ob_size),dy*dble(ob_y-ob_size),dz*dble(ob_z-ob_size) !0
write(50,*) dx*dble(ob_x+ob_size),dy*dble(ob_y-ob_size),dz*dble(ob_z-ob_size) !1
write(50,*) dx*dble(ob_x-ob_size),dy*dble(ob_y-ob_size),dz*dble(ob_z+ob_size) !2
write(50,*) dx*dble(ob_x+ob_size),dy*dble(ob_y-ob_size),dz*dble(ob_z+ob_size) !3
write(50,*) dx*dble(ob_x-ob_size),dy*dble(ob_y+ob_size),dz*dble(ob_z-ob_size) !4
write(50,*) dx*dble(ob_x+ob_size),dy*dble(ob_y+ob_size),dz*dble(ob_z-ob_size) !5
write(50,*) dx*dble(ob_x-ob_size),dy*dble(ob_y+ob_size),dz*dble(ob_z+ob_size) !6
write(50,*) dx*dble(ob_x+ob_size),dy*dble(ob_y+ob_size),dz*dble(ob_z+ob_size) !7
close(50)



omegam= 1d0/(3d0*nu+5d-1)
omegas= 1d0/(3d0*alpha+5d-1)
wght( 0)=2d0/9d0
wght( 1)=1d0/9d0
wght( 2)=1d0/9d0
wght( 3)=1d0/9d0
wght( 4)=1d0/9d0
wght( 5)=1d0/9d0
wght( 6)=1d0/9d0
wght( 7)=1d0/72d0
wght( 8)=1d0/72d0
wght( 9)=1d0/72d0
wght(10)=1d0/72d0
wght(11)=1d0/72d0
wght(12)=1d0/72d0
wght(13)=1d0/72d0
wght(14)=1d0/72d0
fght( :)=3d0*wght(:)
fght( 0)=-7d0/3d0

  cx( 0)= 0d0
  cx( 1)= 1d0
  cx( 2)= 0d0
  cx( 3)= 0d0
  cx( 4)=-1d0
  cx( 5)= 0d0
  cx( 6)= 0d0
  cx( 7)= 1d0
  cx( 8)=-1d0
  cx( 9)= 1d0
  cx(10)= 1d0
  cx(11)=-1d0
  cx(12)= 1d0
  cx(13)=-1d0
  cx(14)=-1d0

  cy( 0)= 0d0
  cy( 1)= 0d0
  cy( 2)= 1d0
  cy( 3)= 0d0
  cy( 4)= 0d0
  cy( 5)=-1d0
  cy( 6)= 0d0
  cy( 7)= 1d0
  cy( 8)= 1d0
  cy( 9)=-1d0
  cy(10)= 1d0
  cy(11)=-1d0
  cy(12)=-1d0
  cy(13)= 1d0
  cy(14)=-1d0

  cz( 0)= 0d0
  cz( 1)= 0d0
  cz( 2)= 0d0
  cz( 3)= 1d0
  cz( 4)= 0d0
  cz( 5)= 0d0
  cz( 6)=-1d0
  cz( 7)= 1d0
  cz( 8)= 1d0
  cz( 9)= 1d0
  cz(10)=-1d0
  cz(11)=-1d0
  cz(12)=-1d0
  cz(13)=-1d0
  cz(14)= 1d0

  cs(:)=sqrt(cx(:)**2+cy(:)**2+cz(:)**2)/sqrt(3d0)
!IC
Vari(1,:,:,:)=rhoo
Vari(2,:,:,:)=0d0
Vari(3,:,:,:)=0d0
Vari(4,:,:,:)=0d0
Vari(5,:,:,:)=0d0
!Vari(2,:,yMax)=uo
!Vari(4,:,yMax)=To
!do j=yin,20
!  do i=xin,20
!    Vari(1,i,j)=0d0
!  end do 
!end do 
do k=zin,zMax
  do j=yin,yMax
    do i=xin,xMax
      Fun(:,i,j,k)=wght(:)*Vari(1,i,j,k)
    end do 
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
do k=zin,zMax
  do j=yin,yMax
    !X- In
    !tmp(1)=Fun(0, xin,j,k)+Fun(2, xin,j,k)+Fun(3, xin,j,k)+Fun(5, xin,j,k)+Fun(6, xin,j,k)
    !tmp(2)=Fun(4, xin,j,k)+Fun(8, xin,j,k)+Fun(11, xin,j,k)+Fun(13, xin,j,k)+Fun(14, xin,j,k)
    !tmp(3)=(tmp(1)+2d0*tmp(2))/(1d0-uo)
    tmp(3)=Vari(1,xin,j,k)
    Fun( 1, xin,j,k)=Fun( 4, xin,j,k)+2d0/3d0*tmp(3)*uo
    Fun( 7, xin,j,k)=Fun(11, xin,j,k)+1d0/12d0*tmp(3)*(uo)
    Fun( 9, xin,j,k)=Fun(13, xin,j,k)+1d0/12d0*tmp(3)*(uo)
    Fun(10, xin,j,k)=Fun(14, xin,j,k)+1d0/12d0*tmp(3)*(uo)
    Fun(12, xin,j,k)=Fun( 8, xin,j,k)+1d0/12d0*tmp(3)*(uo)
  end do
end do
do k=zin,zMax
  do j=yin,yMax
      tmp(1)=0d0
      do la=0,prtl
        tmp(1)=tmp(1)+Fun(la,xin,j,k)
      end do
      Fun(:,xin,j,k)=Fun(:,xin,j,k)/tmp(1)
  end do
end do

do j=yin,yMax
  !x+ Out
  Fun(:,xMax,j,:)=Fun(:,xMax-1,j,:)
end do
do k=zin,zMax
  do j=yin,yMax
      tmp(1)=0d0
      do la=0,prtl
        tmp(1)=tmp(1)+Fun(la,xMax,j,k)
      end do
      Fun(:,xMax,j,k)=Fun(:,xMax,j,k)/tmp(1)
  end do
end do



do i=xin+1,xMax-1
  !Y- BounceBack
  Fun( 2,i, yin,:)=Fun( 5,i,yin,:)
  Fun( 7,i, yin,:)=Fun(11,i,yin,:)
  Fun( 8,i, yin,:)=Fun(12,i,yin,:)
  Fun(10,i, yin,:)=Fun(14,i,yin,:)
  Fun(13,i, yin,:)=Fun( 9,i,yin,:)

  !Z- BounceBack
  Fun( 3,i,:, zin)=Fun( 6,i,:,zin)
  Fun( 7,i,:, zin)=Fun(11,i,:,zin)
  Fun( 8,i,:, zin)=Fun(12,i,:,zin)
  Fun( 9,i,:, zin)=Fun(13,i,:,zin)
  Fun(14,i,:, zin)=Fun(10,i,:,zin)


  !Y+ BounceBack
  Fun( 5,i,yMax,:)=Fun( 2,i,yMax,:)
  Fun(11,i,yMax,:)=Fun( 7,i,yMax,:)
  Fun(12,i,yMax,:)=Fun( 8,i,yMax,:)
  Fun(14,i,yMax,:)=Fun(10,i,yMax,:)
  Fun( 9,i,yMax,:)=Fun(13,i,yMax,:)

  !Z+ BounceBack
  Fun( 6,i,:,zMax)=Fun( 3,i,:,zMax)
  Fun(11,i,:,zMax)=Fun( 7,i,:,zMax)
  Fun(12,i,:,zMax)=Fun( 8,i,:,zMax)
  Fun(13,i,:,zMax)=Fun( 9,i,:,zMax)
  Fun(10,i,:,zMax)=Fun(14,i,:,zMax)
end do

!!Object
!do j=ob_y-5,ob_y+5
!  do k=ob_z-5,ob_z+5
!    Fun( 1,ob_x+5,j,k)=Fun( 4,ob_x+5,j,k)
!    Fun( 7,ob_x+5,j,k)=Fun(11,ob_x+5,j,k)
!    Fun( 9,ob_x+5,j,k)=Fun(13,ob_x+5,j,k)
!    Fun(10,ob_x+5,j,k)=Fun(14,ob_x+5,j,k)
!    Fun(12,ob_x+5,j,k)=Fun( 8,ob_x+5,j,k)
!    Fun( 4,ob_x-5,j,k)=Fun( 1,ob_x-5,j,k)
!    Fun(11,ob_x-5,j,k)=Fun( 7,ob_x-5,j,k)
!    Fun(13,ob_x-5,j,k)=Fun( 9,ob_x-5,j,k)
!    Fun(14,ob_x-5,j,k)=Fun(10,ob_x-5,j,k)
!    Fun( 8,ob_x-5,j,k)=Fun(12,ob_x-5,j,k)
!  end do
!end do
!do i=ob_x-5,ob_x+5
!  do k=ob_z-5,ob_z+5
!    !Y- BounceBack
!    Fun( 2,i,ob_y+5,k)=Fun( 5,i,ob_y+5,k)
!    Fun( 7,i,ob_y+5,k)=Fun(11,i,ob_y+5,k)
!    Fun( 8,i,ob_y+5,k)=Fun(12,i,ob_y+5,k)
!    Fun(10,i,ob_y+5,k)=Fun(14,i,ob_y+5,k)
!    Fun(13,i,ob_y+5,k)=Fun( 9,i,ob_y+5,k)
!    !Y+ BounceBack
!    Fun( 5,i,ob_y-5,k)=Fun( 2,i,ob_y-5,k)
!    Fun(11,i,ob_y-5,k)=Fun( 7,i,ob_y-5,k)
!    Fun(12,i,ob_y-5,k)=Fun( 8,i,ob_y-5,k)
!    Fun(14,i,ob_y-5,k)=Fun(10,i,ob_y-5,k)
!    Fun( 9,i,ob_y-5,k)=Fun(13,i,ob_y-5,k)
!  enddo
!  do j=ob_y-5,ob_y+5
!    !Z- BounceBack
!    Fun( 3,i,j,ob_z+5)=Fun( 6,i,j,ob_z+5)
!    Fun( 7,i,j,ob_z+5)=Fun(11,i,j,ob_z+5)
!    Fun( 8,i,j,ob_z+5)=Fun(12,i,j,ob_z+5)
!    Fun( 9,i,j,ob_z+5)=Fun(13,i,j,ob_z+5)
!    Fun(14,i,j,ob_z+5)=Fun(10,i,j,ob_z+5)
!    !Z+ BounceBack
!    Fun( 6,i,j,ob_z-5)=Fun( 3,i,j,ob_z-5)
!    Fun(11,i,j,ob_z-5)=Fun( 7,i,j,ob_z-5)
!    Fun(12,i,j,ob_z-5)=Fun( 8,i,j,ob_z-5)
!    Fun(13,i,j,ob_z-5)=Fun( 9,i,j,ob_z-5)
!    Fun(10,i,j,ob_z-5)=Fun(14,i,j,ob_z-5)
!  enddo
!  !Fun(4,i,ob_y-5)=Fun(2,i,ob_y-5)
!  !Fun(2,i,ob_y+5)=Fun(4,i,ob_y+5)
!end do
!!do j=45,55
!!  Fun(1,60,j)=Fun(3,60,j)
!!  Fun(5,60,j)=Fun(7,60,j)
!!  Fun(8,60,j)=Fun(6,60,j)
!!  Fun(3,50,j)=Fun(1,50,j)
!!  Fun(7,50,j)=Fun(5,50,j)
!!  Fun(6,50,j)=Fun(8,50,j)
!!end do
!!
!do k=ob_z-4,ob_z+4
!  do j=ob_y-4,ob_y+4
!    do i=ob_x-4,ob_x+4
!      Fun(:,i,j,k)=wght(:)*Vari(1,i,j,k)
!    end do 
!  end do 
!end do 

end subroutine BC
!}}}
subroutine set_vari!{{{
use prmtr
use vrble
implicit none
integer la
double precision tmp(5)
do k=zin,zMax
 do j=yin,yMax
   do i=xin,xMax
     tmp(1)=0d0
     do la=0,prtl
       tmp(1)=tmp(1)+Fun(la,i,j,k)
     end do
     Vari(1,i,j,k)=tmp(1)
   end do
 end do
end do
do k=zin,zMax
  do j=yin,yMax
    do i=xin,xMax
      tmp(2)=0d0
      tmp(3)=0d0
      tmp(4)=0d0
      do la=0,prtl
        tmp(2)=tmp(2)+Fun(la,i,j,k)*cx(la)
        tmp(3)=tmp(3)+Fun(la,i,j,k)*cy(la)
        tmp(4)=tmp(4)+Fun(la,i,j,k)*cz(la)
      end do
      Vari(2,i,j,k)=tmp(2)/Vari(1,i,j,k)
      Vari(3,i,j,k)=tmp(3)/Vari(1,i,j,k)
      Vari(4,i,j,k)=tmp(4)/Vari(1,i,j,k)
      tmp(5)=0d0
      do la=0,prtl
        tmp(5)=tmp(5)+Fun(la,i,j,k)*(cx(la)**2+cy(la)**2+cz(la)**2)
      end do
      tmp(1)=Vari(2,i,j,k)**2+Vari(3,i,j,k)**2+Vari(4,i,j,k)**2
      Vari(5,i,j,k)=5d-1*(tmp(5)/Vari(1,i,j,k)-tmp(1))
      
    end do
  end do
end do

end subroutine set_vari
!}}}
subroutine OBJECT !{{{
use prmtr
use vrble
implicit none
integer la
double precision tmp(5)
double precision,dimension(0:8)::tmp_vec,force
!Object
do j=ob_y-5,ob_y+5
  do k=ob_z-5,ob_z+5
    Fun( 1,ob_x+5,j,k)=Fun( 4,ob_x+5,j,k)
    Fun( 7,ob_x+5,j,k)=Fun(11,ob_x+5,j,k)
    Fun( 9,ob_x+5,j,k)=Fun(13,ob_x+5,j,k)
    Fun(10,ob_x+5,j,k)=Fun(14,ob_x+5,j,k)
    Fun(12,ob_x+5,j,k)=Fun( 8,ob_x+5,j,k)
    Fun( 4,ob_x-5,j,k)=Fun( 1,ob_x-5,j,k)
    Fun(11,ob_x-5,j,k)=Fun( 7,ob_x-5,j,k)
    Fun(13,ob_x-5,j,k)=Fun( 9,ob_x-5,j,k)
    Fun(14,ob_x-5,j,k)=Fun(10,ob_x-5,j,k)
    Fun( 8,ob_x-5,j,k)=Fun(12,ob_x-5,j,k)
  end do
end do
do i=ob_x-5,ob_x+5
  do k=ob_z-5,ob_z+5
    !Y- BounceBack
    Fun( 2,i,ob_y+5,k)=Fun( 5,i,ob_y+5,k)
    Fun( 7,i,ob_y+5,k)=Fun(11,i,ob_y+5,k)
    Fun( 8,i,ob_y+5,k)=Fun(12,i,ob_y+5,k)
    Fun(10,i,ob_y+5,k)=Fun(14,i,ob_y+5,k)
    Fun(13,i,ob_y+5,k)=Fun( 9,i,ob_y+5,k)
    !Y+ BounceBack
    Fun( 5,i,ob_y-5,k)=Fun( 2,i,ob_y-5,k)
    Fun(11,i,ob_y-5,k)=Fun( 7,i,ob_y-5,k)
    Fun(12,i,ob_y-5,k)=Fun( 8,i,ob_y-5,k)
    Fun(14,i,ob_y-5,k)=Fun(10,i,ob_y-5,k)
    Fun( 9,i,ob_y-5,k)=Fun(13,i,ob_y-5,k)
  enddo
  do j=ob_y-5,ob_y+5
    !Z- BounceBack
    Fun( 3,i,j,ob_z+5)=Fun( 6,i,j,ob_z+5)
    Fun( 7,i,j,ob_z+5)=Fun(11,i,j,ob_z+5)
    Fun( 8,i,j,ob_z+5)=Fun(12,i,j,ob_z+5)
    Fun( 9,i,j,ob_z+5)=Fun(13,i,j,ob_z+5)
    Fun(14,i,j,ob_z+5)=Fun(10,i,j,ob_z+5)
    !Z+ BounceBack
    Fun( 6,i,j,ob_z-5)=Fun( 3,i,j,ob_z-5)
    Fun(11,i,j,ob_z-5)=Fun( 7,i,j,ob_z-5)
    Fun(12,i,j,ob_z-5)=Fun( 8,i,j,ob_z-5)
    Fun(13,i,j,ob_z-5)=Fun( 9,i,j,ob_z-5)
    Fun(10,i,j,ob_z-5)=Fun(14,i,j,ob_z-5)
  enddo
  !Fun(4,i,ob_y-5)=Fun(2,i,ob_y-5)
  !Fun(2,i,ob_y+5)=Fun(4,i,ob_y+5)
end do
!do j=45,55
!  Fun(1,60,j)=Fun(3,60,j)
!  Fun(5,60,j)=Fun(7,60,j)
!  Fun(8,60,j)=Fun(6,60,j)
!  Fun(3,50,j)=Fun(1,50,j)
!  Fun(7,50,j)=Fun(5,50,j)
!  Fun(6,50,j)=Fun(8,50,j)
!end do
!
do k=ob_z-4,ob_z+4
  do j=ob_y-4,ob_y+4
    do i=ob_x-4,ob_x+4
      Fun(:,i,j,k)=wght(:)*Vari(1,i,j,k)
    end do 
  end do 
end do 

end subroutine OBJECT
!}}}
