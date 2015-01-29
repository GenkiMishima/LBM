subroutine collesion
use prmtr
use vrble
implicit none
integer la
double precision tmp(5)
double precision,dimension(0:8)::tmp_vec
do j=yin,yMax
  do i=xin,xMax
    tmp(1)=Vari(2,i,j)**2+Vari(3,i,j)**2
    tmp_vec(:)=Vari(2,i,j)*cx(:)+Vari(3,i,j)*cy(:)
    Feq(:,i,j)=Vari(1,i,j)*wght(:)*(1d0+3d0*tmp_vec(:)+4.5d0*tmp_vec(:)**2-1.5d0*tmp(1))
    Fun(:,i,j)=omega*Feq(:,i,j)+(1d0-omega)*Fun(:,i,j)
  end do
end do
end subroutine collesion
