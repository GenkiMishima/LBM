subroutine LattBolMet(dt)
use prmtr
use vrble
implicit none
double precision,intent(in):: dt

!LBM
!collision
do j=yin,yMax
  do i=xin,xMax
    !equilibrium
    Feq(:,i,j)=wght(:)*LBM(i,j)*(1d0+(cx(:)*Uvel+cy(:)*Vvel)/cs(:)**2)
    Feq(0,i,j)=wght(0)*LBM(i,j)!*(1d0+(cx(0)*Uvel+cy(0)*Vvel)/cs(0)**2)
    Fun(:,i,j)=(1d0-omega)*Fun(:,i,j)+omega*Feq(:,i,j)
  end do
end do

!streaming{{{
do j=yin,yMax
  do i=xin,xMax
    Fun(1,xMax-i,     j)=Fun(1,xMax-i-1,       j) !Fun(1,:) streaming
    Fun(2,     i,yMax-j)=Fun(2,       i,yMax-j-1) !Fun(1,:) streaming
    Fun(3,   i-1,     j)=Fun(3,       i,       j) !Fun(2,:) streaming
    Fun(4,     i,   j-1)=Fun(4,       i,       j) !Fun(2,:) streaming
    Fun(5,xMax-i,yMax-j)=Fun(5,xMax-i-1,yMax-j-1) !Fun(1,:) streaming
    Fun(6,   i-1,yMax-j)=Fun(6,       i,yMax-j-1) !Fun(1,:) streaming
    Fun(7,   i-1,   j-1)=Fun(7,       i,       j) !Fun(1,:) streaming
    Fun(8,xMax-i,   j-1)=Fun(8,xMax-i-1,       j) !Fun(1,:) streaming
  end do
end do
!}}}

!BC{{{
Fun(0, xin,   :)=Twall*wght(0)
Fun(1, xin,   :)=Twall*wght(1)
Fun(2, xin,   :)=Twall*wght(2)
Fun(3, xin,   :)=Twall*wght(3)
Fun(4, xin,   :)=Twall*wght(4)
Fun(5, xin,   :)=Twall*wght(5)
Fun(6, xin,   :)=Twall*wght(6)
Fun(7, xin,   :)=Twall*wght(7)
Fun(8, xin,   :)=Twall*wght(8)
Fun(1,xMax,   :)=Fun(1,xMax-1,   :)
Fun(2,xMax,   :)=Fun(2,xMax-1,   :)
Fun(3,xMax,   :)=Fun(3,xMax-1,   :)
Fun(4,xMax,   :)=Fun(4,xMax-1,   :)
Fun(5,xMax,   :)=Fun(5,xMax-1,   :)
Fun(6,xMax,   :)=Fun(6,xMax-1,   :)
Fun(7,xMax,   :)=Fun(7,xMax-1,   :)
Fun(8,xMax,   :)=Fun(8,xMax-1,   :)

Fun(0,   :, yin)=Twall*wght(0)
Fun(1,   :, yin)=Twall*wght(1)
Fun(2,   :, yin)=Twall*wght(2)
Fun(3,   :, yin)=Twall*wght(3)
Fun(4,   :, yin)=Twall*wght(4)
Fun(5,   :, yin)=Twall*wght(5)
Fun(6,   :, yin)=Twall*wght(6)
Fun(7,   :, yin)=Twall*wght(7)
Fun(8,   :, yin)=Twall*wght(8)
Fun(1,   :,yMax)=Fun(1,   :,yMax-1)
Fun(2,   :,yMax)=Fun(2,   :,yMax-1)
Fun(3,   :,yMax)=Fun(3,   :,yMax-1)
Fun(4,   :,yMax)=Fun(4,   :,yMax-1)
Fun(5,   :,yMax)=Fun(5,   :,yMax-1)
Fun(6,   :,yMax)=Fun(6,   :,yMax-1)
Fun(7,   :,yMax)=Fun(7,   :,yMax-1)
Fun(8,   :,yMax)=Fun(8,   :,yMax-1)
!}}}

Fun(:, xin-1,:)=Fun(:, xin,:)
Fun(:,xMax+1,:)=Fun(:,xMax,:)
Fun(:, yin-1,:)=Fun(:, yin,:)
Fun(:,yMax+1,:)=Fun(:,yMax,:)

do j=yin,yMax
  do i=xin,xMax
    LBM(i,j)=Fun(0,i,j)+Fun(1,i,j)+Fun(2,i,j)+Fun(3,i,j)+Fun(4,i,j)+Fun(5,i,j)+Fun(6,i,j)+Fun(7,i,j)+Fun(8,i,j)
  end do
end do
!LBM(0,:)=Twall
!LBM(:,0)=Twall
end subroutine LattBolMet
