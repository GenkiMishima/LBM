subroutine LattBolMet
use prmtr
use vrble
implicit none
!LBM
!collision
do i=xin,xMax
  LBM(i)=Fun(0,i)+Fun(1,i)+Fun(2,i)
  !equilibrium
  Feq(:,i)=wght(:)*LBM(i)
  Fun(:,i)=(1d0-omega)*Fun(:,i)+omega*Feq(:,i)
end do
!streaming
do i=xin+1,xMax-1
  Fun(1,xMax-i)=Fun(1,xMax-i-1) !Fun(1,:) streaming
  Fun(2,   i-1)=Fun(2,       i) !Fun(2,:) streaming
end do
!BC
Fun(0, xin)=Twall*wght(0)
Fun(1, xin)=Twall-Fun(0,xin)-Fun(2,xin)
!Fun(2, xin)=Twall*wght(2)
Fun(1,xMax)=Fun(1,xMax-1)
Fun(2,xMax)=Fun(2,xMax-1)

end subroutine LattBolMet
