subroutine output
use prmtr
use vrble
implicit none

if(mod(t,100).eq.0)then
out_freq=1+out_freq
print*, t
write(tmpstring,'(i3.3)') out_freq
open(55,file='data/FDM_'//trim(tmpstring)//'.d')
open(56,file='data/LBM_'//trim(tmpstring)//'.d')
!do j=yin,yMax
!do i=xin,xMax
!  write(55,*)dble(i),dble(j),FDM(i,j)
!  write(56,*)dble(i),dble(j),LBM(i,j)
!end do
!write(55,*)
!write(56,*)
!end do
do j=yin,yMax
!do i=xin,xMax
  write(55,*)dble(j),FDM(30,j)
  write(56,*)dble(j),LBM(30,j)
end do
close(55)
close(56)
end if

end subroutine output
