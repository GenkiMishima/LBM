subroutine LattBolMet
use prmtr
use vrble
implicit none
!LBM
!collision
do i=xin+1,xMax-1
  LBM(i)=Fun(1,i)+Fun(2,i)
  !equilibrium
  Feq(:,i)=0.5d0*LBM(i)
  Fun(1,i)=(1d0-omega)*Fun(1,i)+omega*Feq(1,i)+dt*wght(1)*source
  Fun(2,i)=(1d0-omega)*Fun(2,i)+omega*Feq(2,i)+dt*wght(2)*source
end do
!streaming
do i=xin+1,xMax-1
  Fun(1,xMax-i)=Fun(1,xMax-i-1) !Fun(1,:) streaming
  Fun(2,   i-1)=Fun(2,       i) !Fun(2,:) streaming
end do
!BC
Fun(1, xin)=Twall-Fun(2,xin)
Fun(1,xMax)=Fun(1,xMax-1)
Fun(2,xMax)=Fun(2,xMax-1)

end subroutine LattBolMet
