subroutine collision
use prmtr
use vrble
implicit none
integer la
double precision tmp(5),tmp_rho,tmp_u,tmp_v,tmp_T,beta
double precision,dimension(0:8)::tmp_vec,force,tmp_cv
do j=yin,yMax
  do i=xin,xMax
    tmp_rho=Vari(1,i,j)
      tmp_u=Vari(2,i,j)
      tmp_v=Vari(3,i,j)
      tmp_T=Vari(4,i,j)
  
    tmp(1)=sqrt(tmp_u**2+tmp_v**2)
    tmp_vec(:)=tmp_u*cx(:)+tmp_v*cy(:)
    tmp_cv(:)=sqrt(cx(:)**2+cy(:)**2)
    Feq(:,i,j)=tmp_rho*wght(:)*(1d0+3d0*tmp_vec(:)+4.5d0*tmp_vec(:)**2-1.5d0*tmp(1)**2)
    Fun(:,i,j)=omegam*Feq(:,i,j)+(1d0-omegam)*Fun(:,i,j)

  end do
  
end do
end subroutine collision
