subroutine collision
use prmtr
use vrble
implicit none
integer la,ln
double precision tmp(5),tmp_rho,tmp_u,tmp_v,tmp_T
double precision,dimension(0:8)::tmp_vec,tmp_unity,tmp_A,tmp_B,tmp_D,force
do j=yin,yMax
  open(50,file='temp.d')
  do i=xin,xMax
    tmp_rho=Vari(1,i,j)
      tmp_u=Vari(2,i,j)
      tmp_v=Vari(3,i,j)
      tmp_T=Vari(4,i,j)
  
    tmp(1)=sqrt(tmp_u**2+tmp_v**2)
    tmp_vec(:)=tmp_u*cx(:)+tmp_v*cy(:)
    tmp_unity(:)=sqrt(cx(:)**2+cy(:)**2)
    tmp_A(0)=(beta-2d0)/etao**2*tmp_T
    do la=1,4
      tmp_A(la)=1d0/(4d0*(V1**2-V2**2))*(-V2**2+((beta-2d0)*V2**2/etao**2+2d0)*tmp_T+(V2/V1)**2*(tmp_u**2+tmp_v**2))
      ln=la+4
      tmp_A(ln)=1d0/(4d0*(V2**2-V1**2))*(-V1**2+((beta-2d0)*V1**2/etao**2+2d0)*tmp_T+(V1/V2)**2*(tmp_u**2+tmp_v**2))
    end do
    tmp_B(0)=0d0
    do la=1,4
      tmp_B(la)=(-V2**2+(beta+2d0)*tmp_T+tmp_u**2+tmp_v**2)/(2d0*V1**2*(V1**2-V2**2))
      ln=la+4
      tmp_B(ln)=(-V1**2+(beta+2d0)*tmp_T+tmp_u**2+tmp_v**2)/(2d0*V2**2*(V2**2-V1**2))
    end do
    tmp_D(0)=0d0
    do la=1,4
      tmp_D(la)=1d0/(2d0*V1**4)
      ln=la+4
      tmp_D(ln)=1d0/(2d0*V2**4)
    end do
    Feq(:,i,j)=tmp_rho*(tmp_A(:)+tmp_B(:)*tmp_vec(:)+tmp_D(:)*tmp_vec(:)**2)
    !write(50,'(e15.7,e15.7,e15.7,e15.7)') tmp_A(0),tmp_B(0),tmp_D(0),Feq(0,i,j)
    !Feq(:,i,j)=tmp_rho*wght(:)*(1d0+tmp_vec(:)+0.5d0*(tmp_vec(:)**2-tmp(1)+(tmp_T-1d0)*(tmp_unity(:)-2))+tmp_vec(:)/6d0*(tmp_vec(:)**2-3d0*tmp(1)+3d0*(tmp_T-1d0)*(tmp_unity(:)-2-2)))
    !Feq(:,i,j)=tmp_rho*wght(:)*(1d0+3d0*tmp_vec(:)+4.5d0*tmp_vec(:)**2-1.5d0*tmp(1))
    Fun(:,i,j)=omegam*Feq(:,i,j)+(1d0-omegam)*Fun(:,i,j)!+tmp_rho*force(:)
    tmp(5)=Fun(0,0,j)+Fun(1,0,j)+Fun(2,0,j)+Fun(3,0,j)+Fun(4,0,j)+Fun(5,0,j)+Fun(6,0,j)+Fun(7,0,j)+Fun(8,0,j)
    write(50,'(e15.7,e15.7,e15.7,e15.7,e15.7,e15.7,e15.7,e15.7,e15.7,e15.7)') Fun(0,0,j),Fun(1,0,j),Fun(2,0,j),Fun(3,0,j),Fun(4,0,j),Fun(5,0,j),Fun(6,0,j),Fun(7,0,j),Fun(8,0,j),tmp(5)
    !write(50,'(e15.7,e15.7,e15.7,e15.7,e15.7,e15.7,e15.7,e15.7,e15.7,e15.7)') cx(0),cx(1),cx(2),cx(3),cx(4),cx(5),cx(6),cx(7),cx(8)
  end do
  close(50)
  !call exit
  
end do
end subroutine collision
