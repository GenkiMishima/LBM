subroutine output
use prmtr
use vrble
implicit none
!integer,intent(in):: t

if(mod(t,out_freq).eq.0)then
  print *,time
  write(tmpstring,'(i3.3)') int(t/out_freq)
  open(10, file='data/density_'//trim(tmpstring)//'.d')
  open(11, file='data/U_Velocity_'//trim(tmpstring)//'.d')
  open(12, file='data/V_Velocity_'//trim(tmpstring)//'.d')
  open(13, file='data/Energy_'//trim(tmpstring)//'.d')
  do j=yin+1,yMax-1
    do i=xin+1,xMax-1
        write(10, *) Vari(1,i,j)
        write(11, *) Vari(2,i,j)
        write(12, *) Vari(3,i,j)
        write(13, *) Vari(4,i,j)
     enddo
  enddo
  close(10)
  close(11)
  close(12)
  close(13)
end if
end subroutine output
