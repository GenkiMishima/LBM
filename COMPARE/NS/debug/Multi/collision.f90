subroutine collision
use prmtr
use vrble
implicit none
integer la
double precision tmp(5),tmp_rho,tmp_u,tmp_v,tmp_w,tmp_T,beta
double precision,dimension(0:prtl)::tmp_vel,force
double precision,dimension(0:prtl,10)::tmp_vec
do k=zin,zMax
  do j=yin,yMax
    !open(50,file='temp.d')
    do i=xin,xMax
      tmp_rho=Vari(1,i,j,k)
        tmp_u=Vari(2,i,j,k)
        tmp_v=Vari(3,i,j,k)
        tmp_w=Vari(4,i,j,k)
        tmp_T=Vari(5,i,j,k)
    
      beta      =Ra*alpha*nu/(yMax**3*gravity)
      force(:)  =wght(:)*gravity*beta*tmp_T*cy(:)
      tmp(1)=tmp_u**2+tmp_v**2+tmp_w**2
      tmp_vel(:)=tmp_u*cx(:)+tmp_v*cy(:)+tmp_w*cz(:)
      Feq(:,i,j,k)=tmp_rho*wght(:)*(1d0+3d0*tmp_vel(:)+4.5d0*tmp_vel(:)**2-1.5d0*tmp(1))
  
      Geq(:,i,j,k)=wght(:)*tmp_T*(1d0+(cx(:)*tmp_u+cy(:)*tmp_v)/cs(:)**2)
      Geq(0,i,j,k)=wght(0)*tmp_T!*(1d0+(cx(0)*Uvel+cy(0)*Vvel)/cs(0)**2)
  
      Fun(:,i,j,k)=omegam*Feq(:,i,j,k)+(1d0-omegam)*Fun(:,i,j,k)
      Gun(:,i,j,k)=omegas*Geq(:,i,j,k)+(1d0-omegas)*Gun(:,i,j,k)
    end do
    
  end do
end do
end subroutine collision
