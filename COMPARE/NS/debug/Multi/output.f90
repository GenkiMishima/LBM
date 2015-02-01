subroutine output
use prmtr
use vrble
implicit none
!integer,intent(in):: t

if(mod(t,out_freq).eq.0)then
  write(tmpstring,'(i3.3)') int(t/out_freq)
  open(10, file='data/density_'//trim(tmpstring)//'.d')
  open(11, file='data/U_Velocity_'//trim(tmpstring)//'.d')
  open(12, file='data/V_Velocity_'//trim(tmpstring)//'.d')
  open(13, file='data/W_Velocity_'//trim(tmpstring)//'.d')
  open(14, file='data/Energy_'//trim(tmpstring)//'.d')
  do k=zin+1,zMax-1
    do j=yin+1,yMax-1
      do i=xin+1,xMax-1
          write(10, *) Vari(1,i,j,k)
          write(11, *) Vari(2,i,j,k)
          write(12, *) Vari(3,i,j,k)
          write(13, *) Vari(4,i,j,k)
          write(14, *) Vari(5,i,j,k)
       enddo
    enddo
  enddo
  close(10)
  close(11)
  close(12)
  close(13)
  close(14)
  print *,time
end if
end subroutine output
