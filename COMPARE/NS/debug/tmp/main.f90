program main
use prmtr
use vrble
implicit none
double precision a,b,c

!!$omp parallel do 
!$omp parallel do shared(k),private(b,i,j)
do k=1,100
  do j=1,100
    do i=1,100
      b=dble(i)+dble(j)
      a=b*dble(k)
    end do
  end do
end do
!$omp end parallel do
print *, a




!call IC
!print *,'Renolds',Re,'alpha',alpha
!print *,'LBM'
!!do t=tin,1
!do t=tin,tMax
!time=dble(t)*dt
!  call collision
!  call streaming
!  call BC
!  call set_vari
!  call output
!end do
end program main
