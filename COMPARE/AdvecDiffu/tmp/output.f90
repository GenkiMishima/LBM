subroutine output(tmpname,array)
use prmtr
use vrble
implicit none
character,intent(in):: tmpname
double precision,dimension(xin:xMax,yin:yMax),intent(in)::array


!if(mod(t,100).eq.0)then
if(time.ge.1d2)then
out_freq=1+out_freq
print*, t
write(tmpstring,'(i3.3)') out_freq
!open(55,file='data/FDM_'//trim(tmpstring)//'.d')
open(56,file='data/'//trim(tmpname)//'_'//trim(tmpstring)//'.d')
!do j=yin,yMax
!do i=xin,xMax
!  write(55,*)dble(i),dble(j),FDM(i,j)
!  write(56,*)dble(i),dble(j),LBM(i,j)
!end do
!write(55,*)
!write(56,*)
!end do
do i=xin,xMax
!  write(55,*)dble(i),FDM(i,30)
  write(56,*)dble(i),array(i,30)
end do
!close(55)
close(56)
end if

end subroutine output
