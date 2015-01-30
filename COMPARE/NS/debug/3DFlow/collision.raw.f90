subroutine collision
use prmtr
use vrble
implicit none
integer la
double precision tmp(5),tmp_rho,tmp_u,tmp_v,tmp_T,beta
double precision,dimension(0:8)::tmp_vel,force,tmp_gx,tmp_gy,tmp_gz
double precision,dimension(0:8,10)::tmp_vec
do j=yin,yMax
  !open(50,file='temp.d')
  do i=xin,xMax
    tmp_rho=Vari(1,i,j)
      tmp_u=Vari(2,i,j)
      tmp_v=Vari(3,i,j)
      tmp_T=Vari(4,i,j)
  
    beta      =Ra*alpha*nu/(yMax**3*gravity)
    force(:)  =wght(:)*gravity*beta*tmp_T*cy(:)
    tmp(1)=tmp_u**2+tmp_v**2
    tmp_vel(:)=tmp_u*cx(:)+tmp_v*cy(:)
    Feq(:,i,j)=tmp_rho*wght(:)*(1d0+3d0*tmp_vel(:)+4.5d0*tmp_vel(:)**2-1.5d0*tmp(1))





    Geq(:,i,j)=wght(:)*tmp_T*(1d0+(cx(:)*tmp_u+cy(:)*tmp_v)/cs(:)**2)
    Geq(0,i,j)=wght(0)*tmp_T!*(1d0+(cx(0)*Uvel+cy(0)*Vvel)/cs(0)**2)



    dudx = Vari(2,i,j)-Vari(2,i-1,j)
    dvdx = Vari(3,i,j)-Vari(3,i-1,j)
    tmp_gx(:)=cx(:)*(


    tmp_vec(:,9)=3d0*eght(:)*1d0/tmp_rho*(tmp_gx(:)+tmp_gy(:)+tmp_gz(:))*dx

    Fun(:,i,j)=omegam*Feq(:,i,j)+(1d0-omegam)*Fun(:,i,j)
    Gun(:,i,j)=omegas*Geq(:,i,j)+(1d0-omegas)*Gun(:,i,j)+tmp_vec(:,9)
    !write(50,'(e15.7,e15.7,e15.7,e15.7,e15.7,e15.7,e15.7,e15.7)') Feq(0,i,j),Feq(1,i,j),Feq(2,i,j),Feq(3,i,j),Feq(4,i,j),Feq(5,i,j),Feq(6,i,j),Feq(7,i,j),Feq(8,i,j)
  end do
  !close(50)
  !call exit
  
end do
end subroutine collision
