program main
use prmtr
use post_prmtr
implicit none
   integer time_ini,i,j,time,cntr
   integer*4,external::access
   double precision,dimension( bis:bie,bjs:bje) :: x,r
   double precision, dimension(bis:bie,bjs:bje)::rho_mat,u_mat,v_mat,p_mat,M_mat,T_mat,vecx,vecr
   character  tmpstring*20
   time_ini=1
   cntr = 0
   open(50,file='../grid.d')
   do j=bjs,bje
     do i=bis,bie
        read(50,*) x(i,j),r(i,j)
     end do 
   end do 
   close(50)
   do time=time_ini,300000
     !if(mod(time,out_freq).eq.0)then
       !write(tmpstring,'(i3.3)') int(t/1)
       write(tmpstring,'(i3.3)') time
       open(66,file='../data/density_'//trim(tmpstring)//'.d')
       read(66,*) ((rho_mat(i,j),i=1,ni),j=1,nj)
       close(66)
       open(66,file='../data/U_Velocity_'//trim(tmpstring)//'.d')
       read(66,*) ((u_mat(i,j),i=1,ni),j=1,nj)
       close(66)
       open(66,file='../data/V_Velocity_'//trim(tmpstring)//'.d')
       read(66,*) ((v_mat(i,j),i=1,ni),j=1,nj)
       close(66)
       open(66,file='../data/Energy_'//trim(tmpstring)//'.d')
       read(66,*) ((T_mat(i,j),i=1,ni),j=1,nj)
       close(66)
       open(66,file='result/result_'//trim(tmpstring)//'.vtk')
       call vtk_grid(x(:,:),r(:,:))
       call vtk_scalar(rho_mat,"Density")
       call vtk_scalar(  u_mat,"UVelo")
       call vtk_scalar(  v_mat,"VVelo")
       call vtk_scalar(  T_mat,"Energ")
       call vtk_vector(u_mat,v_mat,"Velocity")
       close(66)
       open(15,file="restart.bin",form="unformatted")
          write(15) time
       close(15)
       cntr = cntr + 1
       print *,cntr
     !end if
   enddo
end program main

subroutine vtk_grid(xgrid,ygrid)
use prmtr
use post_prmtr
   implicit none
   integer i,j
   double precision,dimension(bis:bie,bjs:bje)::xgrid,ygrid
   write(66,'(a26)') '# vtk DataFile Version 2.0'
   write(66,'(a7)') '2D Data'
   write(66,'(a5)') 'ASCII'
   write(66,'(a23)') 'DATASET STRUCTURED_GRID'
   write(66,'(a11,i3,a1,i3,a1,i1)') 'DIMENSIONS ', ni, ' ', nj, ' ', 1


   write(66,'(a17)') 'FIELD FiledData 2'

   write(66,'(a15)') 'TIME 1 1 double'
   write(66,'(e14.6e3)') 0.d0
   write(66,'(a13)') 'CYCLE 1 1 int'
   write(66,'(i0)') 0


   write(66,'(a7,i7,a6)') 'POINTS ', (ni)*(nj)*1, ' float'
   do j=1,nj
      do i=1,ni
         write(66,'(e14.6e3,2(1x,e14.6e3))') xgrid(i,j), ygrid(i,j), 0.d0
      enddo
   enddo
   write(66,*)

   write(66,'(a11,i7)') 'POINT_DATA ', (ni)*(nj)*1
   write(66,'(a22)') 'VECTORS Indices float'
   do j = 1, nj
      do i = 1, ni
         write(66,'(e14.6e3,2(1x,e14.6e3))') dble(i), dble(j), 0.d0
      enddo
   enddo
   write(66,*)

end subroutine vtk_grid

subroutine vtk_scalar(arr,title)!{{{
use prmtr
use post_prmtr
   implicit none
  integer i,j
  character(*),intent(in)::title
  double precision, dimension(bis:bie,bjs:bje), intent(in)::arr

   write(66,'(a)') 'SCALARS '//trim(title)//' float 1'
   write(66, '(a20)') 'LOOKUP_TABLE default'
   do j = 1, nj
      do i = 1, ni
         write(66,'(e14.6e3)') arr(i,j)
      enddo
   enddo
   write(66,*)
end subroutine vtk_scalar
subroutine vtk_vector(arr1,arr2,title)
use prmtr
use post_prmtr
  implicit none
  integer i,j
  double precision,dimension(bis:bie,bjs:bje),intent(in)::arr1,arr2
  character(*),intent(in)::title

   write(66,'(a22)') 'VECTORS '//trim(title)//' float'
   do j = 1, nj
     do i = 1, ni
       write(66,'(e14.6e3,2(1x,e14.6e3))') arr1(i,j), arr2(i,j), 0.d0
     end do
   end do
   write(66,*)
end subroutine vtk_vector
 !}}}
