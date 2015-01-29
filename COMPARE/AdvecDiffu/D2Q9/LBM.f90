subroutine LattBolMet(dt)
use prmtr
use vrble
implicit none
double precision,intent(in):: dt
double precision tmp(5)
!LBM
!collision
do j=yin,yMax
  do i=xin,xMax
    Feq(:,i,j)=wght(:)*LBM(i,j)*(1d0+(cx(:)*Uvel+cy(:)*Vvel)/cs(:)**2)
    Feq(0,i,j)=wght(0)*LBM(i,j)!*(1d0+(cx(0)*Uvel+cy(0)*Vvel)/cs(0)**2)
    Fun(:,i,j)=(1d0-omega)*Fun(:,i,j)+omega*Feq(:,i,j)
  end do
end do
!streaming
!do j=yMax,yin
!  do i=xin,xMax
!    Fun(2,i,j)=Fun(2,i  ,j-1)
!    Fun(6,i,j)=Fun(6,i+1,j-1)
!    Fun(1,i,j)=Fun(1,i-1,j  )
!    Fun(5,i,j)=Fun(5,i-1,j-1)
!    Fun(4,i,j)=Fun(4,i  ,j+1)
!    Fun(8,i,j)=Fun(8,i-1,j+1)
!    Fun(3,i,j)=Fun(3,i  ,j+1)
!    Fun(7,i,j)=Fun(7,i+1,j+1)
!  end do
!end do
do j=yMax,yin,-1
  do i=xin,xMax
    Fun(2,i,j)=Fun(2,i  ,j-1)
    Fun(6,i,j)=Fun(6,i+1,j-1)
  end do
end do
do j=yMax,yin,-1
  do i=xMax,xin,-1
    Fun(1,i,j)=Fun(1,i-1,j  )
    Fun(5,i,j)=Fun(5,i-1,j-1)
  end do
end do
do j=yin,yMax
  do i=xMax,xin,-1
    Fun(4,i,j)=Fun(4,i  ,j+1)
    Fun(8,i,j)=Fun(8,i-1,j+1)
  end do
end do
do j=yin,yMax
  do i=xin,xMax
    Fun(3,i,j)=Fun(3,i  ,j+1)
    Fun(7,i,j)=Fun(7,i+1,j+1)
  end do
end do
!BC
!Left InCondition
Fun(1,xin,:)=wght(1)*Twall+wght(3)*Twall-Fun(3,xin,:)
Fun(5,xin,:)=wght(5)*Twall+wght(7)*Twall-Fun(7,xin,:)
Fun(8,xin,:)=wght(8)*Twall+wght(6)*Twall-Fun(6,xin,:)

!!Right Adiadic
!Fun(:,xMax,:)=-Fun(:,xMax-1,:)
!Right T=0d0
Fun(6,xMax,:)=-Fun(8,xMax,:)
Fun(3,xMax,:)=-Fun(1,xMax,:)
Fun(7,xMax,:)=-Fun(5,xMax,:)
Fun(2,xMax,:)=-Fun(4,xMax,:)
Fun(0,xMax,:)=0d0

!!Top  Adiadic
!Fun(:,:,yMax)=-Fun(:,:,yMax-1)
!Top  T=0d0
Fun(8,:,yMax)=-Fun(6,:,yMax)
Fun(7,:,yMax)=-Fun(5,:,yMax)
Fun(4,:,yMax)=-Fun(2,:,yMax)
Fun(1,:,yMax)=-Fun(3,:,yMax)
Fun(0,:,yMax)=0d0

!Bottom Adiadic
Fun(:,:,yin)=-Fun(:,:,yin+1)
!!Bottom InCondition
!Fun(2,:,yin)=wght(2)*Twall+wght(4)*Twall-Fun(4,xin,:)
!Fun(5,:,yin)=wght(5)*Twall+wght(7)*Twall-Fun(7,xin,:)
!Fun(6,:,yin)=wght(6)*Twall+wght(8)*Twall-Fun(8,xin,:)
!!Bottom T=0d0
!Fun(2,:,yin)=-Fun(4,:,yin)
!Fun(6,:,yin)=-Fun(8,:,yin)
!Fun(5,:,yin)=-Fun(7,:,yin)
!Fun(1,:,yin)=-Fun(3,:,yin)
!Fun(0,:,yin)=0d0

Fun(:, xin-1,:)=Fun(:, xin,:)
Fun(:,xMax+1,:)=Fun(:,xMax,:)
Fun(:,:, yin-1)=Fun(:,:, yin)
Fun(:,:,yMax+1)=Fun(:,:,yMax)


do j=yin,yMax
  do i=xin,xMax
    LBM(i,j)=Fun(0,i,j)+Fun(1,i,j)+Fun(2,i,j)+Fun(3,i,j)+Fun(4,i,j)+Fun(5,i,j)+Fun(6,i,j)+Fun(7,i,j)+Fun(8,i,j)
  end do
end do
!LBM(:,:)=Fun(0,:,:)+Fun(1,:,:)+Fun(2,:,:)+Fun(3,:,:)+Fun(4,:,:)+Fun(5,:,:)+Fun(6,:,:)+Fun(7,:,:)+Fun(8,:,:)

end subroutine LattBolMet
