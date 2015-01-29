subroutine output
use prmtr
use vrble
implicit none

if(mod(t,100).eq.0)then
j=1+j
write(tmpstring,'(i3.3)') j
open(55,file='data/FDM_'//trim(tmpstring)//'.d')
!open(56,file='data/LBM_'//trim(tmpstring)//'.d')
do i=xin,xMax
  write(55,*)dble(i),FDM(i)
!  write(56,*)dble(i),LBM(i)
end do
close(55)
!close(56)
end if

end subroutine output
