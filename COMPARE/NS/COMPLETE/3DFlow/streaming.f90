subroutine streaming
use prmtr
use vrble
implicit none
!streaming
do k=zin,zMax
  do j=yin,yMax
    do i=xMax,xin+1,-1
      Fun(1,i,j,k)=Fun(1,i-1,j,k)
      Gun(1,i,j,k)=Gun(1,i-1,j,k)
    end do
  end do
end do
do k=zin,zMax
  do j=yMax,yin+1,-1
    do i=xin,xMax
      Fun(2,i,j,k)=Fun(2,i,j-1,k)
      Gun(2,i,j,k)=Gun(2,i,j-1,k)
    end do
  end do
end do
do k=zMax,zin+1,-1
  do j=yin,yMax
    do i=xin,xMax
      Fun(3,i,j,k)=Fun(3,i,j,k-1)
      Gun(3,i,j,k)=Gun(3,i,j,k-1)
    end do
  end do
end do
do k=zin,zMax
  do j=yin,yMax
    do i=xin,xMax-1
      Fun(4,i,j,k)=Fun(4,i+1,j,k)
      Gun(4,i,j,k)=Gun(4,i+1,j,k)
    end do
  end do
end do
do k=zin,zMax
  do j=yin,yMax-1
    do i=xin,xMax
      Fun(5,i,j,k)=Fun(5,i,j+1,k)
      Gun(5,i,j,k)=Gun(5,i,j+1,k)
    end do
  end do
end do
do k=zin,zMax-1
  do j=yin,yMax
    do i=xin,xMax
      Fun(6,i,j,k)=Fun(6,i,j,k+1)
      Gun(6,i,j,k)=Gun(6,i,j,k+1)
    end do
  end do
end do
do k=zMax,zin+1,-1
  do j=yMax,yin+1,-1
    do i=xMax,xin+1,-1
      Fun(7,i,j,k)=Fun(7,i-1,j-1,k-1)
      Gun(7,i,j,k)=Gun(7,i-1,j-1,k-1)
    end do
  end do
end do
do k=zMax,zin+1,-1
  do j=yMax,yin+1,-1
    do i=xin,xMax-1
      Fun(8,i,j,k)=Fun(8,i+1,j-1,k-1)
      Gun(8,i,j,k)=Gun(8,i+1,j-1,k-1)
    end do
  end do
end do
do k=zMax,zin+1,-1
  do j=yin,yMax-1
    do i=xMax,xin+1,-1
      Fun(9,i,j,k)=Fun(9,i-1,j+1,k-1)
      Gun(9,i,j,k)=Gun(9,i-1,j+1,k-1)
    end do
  end do
end do
do k=zin,zMax-1
  do j=yMax,yin+1,-1
    do i=xMax,xin+1,-1
      Fun(10,i,j,k)=Fun(10,i-1,j-1,k+1)
      Gun(10,i,j,k)=Gun(10,i-1,j-1,k+1)
    end do
  end do
end do
do k=zin,zMax-1
  do j=yin,yMax-1
    do i=xin,xMax-1
      Fun(11,i,j,k)=Fun(11,i+1,j+1,k+1)
      Gun(11,i,j,k)=Gun(11,i+1,j+1,k+1)
    end do
  end do
end do
do k=zin,zMax-1
  do j=yin,yMax-1
    do i=xMax,xin+1,-1
      Fun(12,i,j,k)=Fun(12,i-1,j+1,k+1)
      Gun(12,i,j,k)=Gun(12,i-1,j+1,k+1)
    end do
  end do
end do
do k=zin,zMax-1
  do j=yMax,yin+1,-1
    do i=xin,xMax-1
      Fun(13,i,j,k)=Fun(13,i+1,j-1,k+1)
      Gun(13,i,j,k)=Gun(13,i+1,j-1,k+1)
    end do
  end do
end do
do k=zMax,zin+1,-1
  do j=yin,yMax-1
    do i=xin,xMax-1
      Fun(14,i,j,k)=Fun(14,i+1,j+1,k-1)
      Gun(14,i,j,k)=Gun(14,i+1,j+1,k-1)
    end do
  end do
end do





!do j=yin,yMax
!  do i=xMax,xin+1,-1
!    Fun(1,i,j)=Fun(1,i-1,j)
!    Gun(1,i,j)=Gun(1,i-1,j)
!  end do
!  do i=xin,xMax-1
!    Fun(3,i,j)=Fun(3,i+1,j)
!    Gun(3,i,j)=Gun(3,i+1,j)
!  end do
!end do
!do j=yMax,yin+1,-1
!  do i=xin,xMax
!    Fun(2,i,j)=Fun(2,i,j-1)
!    Gun(2,i,j)=Gun(2,i,j-1)
!  end do
!  do i=xMax,xin+1,-1
!    Fun(5,i,j)=Fun(5,i-1,j-1)
!    Gun(5,i,j)=Gun(5,i-1,j-1)
!  end do
!  do i=xin,xMax-1
!    Fun(6,i,j)=Fun(6,i+1,j-1)
!    Gun(6,i,j)=Gun(6,i+1,j-1)
!  end do
!end do
!do j=yin,yMax-1
!  do i=xin,xMax
!    Fun(4,i,j)=Fun(4,i,j+1)
!    Gun(4,i,j)=Gun(4,i,j+1)
!  end do
!  do i=xin,xMax-1
!    Fun(7,i,j)=Fun(7,i+1,j+1)
!    Gun(7,i,j)=Gun(7,i+1,j+1)
!  end do
!  do i=xMax,xin+1,-1
!    Fun(8,i,j)=Fun(8,i-1,j+1)
!    Gun(8,i,j)=Gun(8,i-1,j+1)
!  end do
!end do
end subroutine streaming
