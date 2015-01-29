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
    Fun(:,i,j)=(1d0-omega)*Fun(:,i,j)+omega*Feq(:,i,j)
  end do
end do
!streaming
do j=yin+1,yMax-1
  do i=xin+1,xMax-1
    Fun(1,xMax-i,j)=Fun(1,xMax-i-1,j)
    Fun(2,   i-1,j)=Fun(2,       i,j)
  end do
end do
do j=yin+1,yMax-1
  do i=xin+1,xMax-1
    Fun(3,i,yMax-j)=Fun(3,i,yMax-j-1)
    Fun(4,i,   j-1)=Fun(4,i,       j)
  end do
end do
!BC

Fun(1, xin,   :)=Twall-Fun(2,xin,:)-Fun(3,xin,:)-Fun(4,xin,:)
Fun(1,xMax,   :)=Fun(1,xMax-1,   :)
Fun(2,xMax,   :)=Fun(2,xMax-1,   :)
Fun(3,   :, yin)=Twall-Fun(4,:,yin)-Fun(1,:,yin)-Fun(2,:,yin)
Fun(3,   :,yMax)=Fun(3,   :,yMax-1)
Fun(4,   :,yMax)=Fun(4,   :,yMax-1)

LBM(:,:)=Fun(1,:,:)+Fun(2,:,:)+Fun(3,:,:)+Fun(4,:,:)

end subroutine LattBolMet
