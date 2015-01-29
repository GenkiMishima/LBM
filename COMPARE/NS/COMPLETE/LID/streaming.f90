subroutine streaming
use prmtr
use vrble
implicit none
!streaming
do j=yin,yMax
do i=xMax,xin+1,-1
Fun(1,i,j)=Fun(1,i-1,j)
end do
do i=xin,xMax-1
Fun(3,i,j)=Fun(3,i+1,j)
end do
end do
do j=yMax,yin+1,-1
do i=xin,xMax
Fun(2,i,j)=Fun(2,i,j-1)
end do
do i=xMax,xin+1,-1
Fun(5,i,j)=Fun(5,i-1,j-1)
end do
do i=xin,xMax-1
Fun(6,i,j)=Fun(6,i+1,j-1)
end do
end do
do j=yin,yMax-1
do i=xin,xMax
Fun(4,i,j)=Fun(4,i,j+1)
end do
do i=xin,xMax-1
Fun(7,i,j)=Fun(7,i+1,j+1)
end do
do i=xMax,xin+1,-1
Fun(8,i,j)=Fun(8,i-1,j+1)
end do
end do




!do j=yMax,yin,-1
!  do i=xin,xMax
!    Fun(2,i,j)=Fun(2,i  ,j-1)
!    Fun(6,i,j)=Fun(6,i+1,j-1)
!  end do
!end do
!do j=yMax,yin,-1
!  do i=xMax,xin,-1
!    Fun(1,i,j)=Fun(1,i-1,j  )
!    Fun(5,i,j)=Fun(5,i-1,j-1)
!  end do
!end do
!do j=yin,yMax
!  do i=xMax,xin,-1
!    Fun(4,i,j)=Fun(4,i  ,j+1)
!    Fun(8,i,j)=Fun(8,i-1,j+1)
!  end do
!end do
!do j=yin,yMax
!  do i=xin,xMax
!    Fun(3,i,j)=Fun(3,i  ,j+1)
!    Fun(7,i,j)=Fun(7,i+1,j+1)
!  end do
!end do
end subroutine streaming
