subroutine collision
use prmtr
use vrble
implicit none
integer la
double precision tmp(9),tmp_rho,tmp_u,tmp_v,tmp_w,tmp_T,beta
double precision,dimension(0:prtl)::tmp_vel,force,tmp_gx,tmp_gy,tmp_gz
double precision,dimension(0:prtl,10)::tmp_vec
do k=zin,zMax
  do j=yin,yMax
    !open(50,file='temp.d')
    do i=xin,xMax
      tmp_rho=Vari(1,i,j,k)
        tmp_u=Vari(2,i,j,k)
        tmp_v=Vari(3,i,j,k)
        tmp_w=Vari(4,i,j,k)
      tmp_phi=Vari(5,i,j,k)

      dudx1  =  w(2,i,j,k) -w(2,i-1,j  ,k  )
      dvdx1  =  w(3,i,j,k) -w(3,i-1,j  ,k  )
      dwdx1  =  w(4,i,j,k) -w(4,i-1,j  ,k  )
      dudy1  =  w(2,i,j,k) -w(2,i  ,j-1,k  )
      dvdy1  =  w(3,i,j,k) -w(3,i  ,j-1,k  )
      dwdy1  =  w(4,i,j,k) -w(4,i  ,j-1,k  )
      dudz1  =  w(2,i,j,k) -w(2,i  ,j  ,k-1)
      dvdz1  =  w(3,i,j,k) -w(3,i  ,j  ,k-1)
      dwdz1  =  w(4,i,j,k) -w(4,i  ,j  ,k-1)
      dudx2  =  w(2,i+1,j  ,k  ) -w(2,i,j,k) 
      dvdx2  =  w(3,i+1,j  ,k  ) -w(3,i,j,k) 
      dwdx2  =  w(4,i+1,j  ,k  ) -w(4,i,j,k) 
      dudy2  =  w(2,i  ,j+1,k  ) -w(2,i,j,k)
      dvdy2  =  w(3,i  ,j+1,k  ) -w(3,i,j,k)
      dwdy2  =  w(4,i  ,j+1,k  ) -w(4,i,j,k)
      dudz2  =  w(2,i  ,j  ,k+1) -w(2,i,j,k) 
      dvdz2  =  w(3,i  ,j  ,k+1) -w(3,i,j,k) 
      dwdz2  =  w(4,i  ,j  ,k+1) -w(4,i,j,k) 

      tmp(1)=2d0*mu*dudx1
      tmp(2)=2d0*mu*dudx2
      tmp(3)=mu*(dvdx1+dudy1)
      tmp(4)=mu*(dvdx2+dudy2)
      tmp(5)=mu*(dwdx1+dudz1)
      tmp(6)=mu*(dwdx2+dudz2)
      tmp_gx(:)=cx(:)*(tmp(1)-tmp(2)+tmp(3)-tmp(4)+tmp(5)-tmp(6))
      tmp(1)=mu*(dudy1+dvdx1)
      tmp(2)=mu*(dudy2+dvdx2)
      tmp(3)=2d0*mu*dvdy1
      tmp(4)=2d0*mu*dvdy2
      tmp(5)=mu*(dwdy1+dvdz1)
      tmp(6)=mu*(dwdy2+dvdz2)
      tmp_gy(:)=cy(:)*(tmp(1)-tmp(2)+tmp(3)-tmp(4)+tmp(5)-tmp(6))
      tmp(1)=mu*(dudz1+dwdx1)
      tmp(2)=mu*(dudz2+dwdx2)
      tmp(3)=mu*(dvdz1+dwdy1)
      tmp(4)=mu*(dvdz2+dwdy2)
      tmp(5)=2d0*mu*dwdz1
      tmp(6)=2d0*mu*dwdz2
      tmp_gz(:)=cz(:)*(tmp(1)-tmp(2)+tmp(3)-tmp(4)+tmp(5)-tmp(6))
      tmp_vec(:,9)=3d0*wght(:)*1d0/tmp_rho*(tmp_gx(:)+tmp_gy(:)+tmp_gz(:))*dx

      tmp_vel(:)=tmp_u*cx(:)+tmp_v*cy(:)+tmp_w*cz(:)
      tmp(9)=tmp_u**2+tmp_v**2+tmp_w**2



      Geq(:,i,j,k)=wght(:)*tmp_T*(1d0+(cx(:)*tmp_u+cy(:)*tmp_v)/cs(:)**2)
      Geq(0,i,j,k)=wght(0)*tmp_T!*(1d0+(cx(0)*Uvel+cy(0)*Vvel)/cs(0)**2)

      Feq(:,i,j,k)=Hght(:)*tmp_phi+fght(:)*(po-kf*tmp_phi*
      !Feq(:,i,j,k)=tmp_rho*fght(:)*(1d0+3d0*tmp_vel(:)+4.5d0*tmp_vel(:)**2-1.5d0*tmp(9))
      Fun(:,i,j,k)=omegam*Feq(:,i,j,k)+(1d0-omegam)*Fun(:,i,j,k)
      Gun(:,i,j,k)=omegas*Geq(:,i,j,k)+(1d0-omegas)*Gun(:,i,j,k)+tmp_vec(:,9)
    end do
    
  end do
end do
end subroutine collision
