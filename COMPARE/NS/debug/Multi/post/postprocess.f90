program main!{{{
use prmtr
use post_prmtr
implicit none
   integer time_ini,i,j,k,time,cntr
   integer*4,external::access
   double precision,dimension( bis:bie,bjs:bje,bks:bke) :: x,y,z
   double precision, dimension(bis:bie,bjs:bje,bks:bke)::rho_mat,u_mat,v_mat,w_mat,p_mat,M_mat,T_mat,vecx,vecr
   character  tmpstring*20
   time_ini=1
   cntr = 0
   call vtk_object
   open(50,file='../grid.d')
   do k=bks,bke
     do j=bjs,bje
       do i=bis,bie
          read(50,*) x(i,j,k),y(i,j,k),z(i,j,k)
       end do 
     end do 
   end do 
   close(50)
   do time=time_ini,10
       write(tmpstring,'(i3.3)') time
       open(66,file='../data/density_'//trim(tmpstring)//'.d')
       read(66,*) (((rho_mat(i,j,k),i=1,ni),j=1,nj),k=1,nk)
       close(66)
       open(66,file='../data/U_Velocity_'//trim(tmpstring)//'.d')
       read(66,*) (((u_mat(i,j,k),i=1,ni),j=1,nj),k=1,nk)
       close(66)
       open(66,file='../data/V_Velocity_'//trim(tmpstring)//'.d')
       read(66,*) (((v_mat(i,j,k),i=1,ni),j=1,nj),k=1,nk)
       close(66)
       open(66,file='../data/W_Velocity_'//trim(tmpstring)//'.d')
       read(66,*) (((w_mat(i,j,k),i=1,ni),j=1,nj),k=1,nk)
       close(66)
       !open(66,file='../data/Energy_'//trim(tmpstring)//'.d')
       !read(66,*) ((T_mat(i,j),i=1,ni),j=1,nj)
       !close(66)
       open(66,file='result/result_'//trim(tmpstring)//'.vtk')
       call vtk_grid(x(:,:,:),y(:,:,:),z(:,:,:))
       call vtk_scalar(rho_mat,"Density")
       !call vtk_scalar(  u_mat,"UVelo")
       !call vtk_scalar(  v_mat,"VVelo")
       !call vtk_scalar(  w_mat,"WVelo")
       !call vtk_scalar(  T_mat,"Energ")
       call vtk_vector(u_mat,v_mat,w_mat,"Velocity")
       close(66)
   !    open(15,file="restart.bin",form="unformatted")
   !       write(15) time
   !    close(15)
   !    cntr = cntr + 1
   !    print *,cntr
   !  !end if
   print *,'test',time
   enddo
end program main!}}}
subroutine vtk_grid(xgrid,ygrid,zgrid)!{{{
use prmtr
use post_prmtr
   implicit none
   integer i,j,k
   double precision tmp(3)
   double precision,dimension(bis:bie,bjs:bje,bks:bke)::xgrid,ygrid,zgrid
   write(66,'(a26)') '# vtk DataFile Version 2.0'
   write(66,'(a5)') 'TEST '
   write(66,'(a5)') 'ASCII'
   write(66,'(a23)') 'DATASET STRUCTURED_GRID'
   write(66,'(a11,i3,a1,i3,a1,i3)') 'DIMENSIONS ', ni, ' ', nj, ' ', nk
   write(66,'(a7,i8,a6)') 'POINTS ', ni*nj*nk ,' float'
   do k=1,nk
     do j=1,nj
        do i=1,ni
           write(66,'(f8.4,f8.4,f8.4)') xgrid(i,j,k), ygrid(i,j,k), zgrid(i,j,k)
        enddo
     enddo
   enddo
   write(66,*)

   write(66,'(a11,i7)') 'POINT_DATA ', (ni)*(nj)*(nk)
   write(66,'(a22)') 'VECTORS Indices float'
   do k = 1, nk
     do j = 1, nj
        do i = 1, ni
           write(66,'(e14.6e3,2(1x,e14.6e3))') dble(i), dble(j), dble(k)
        enddo
     enddo
   enddo
   write(66,*)

end subroutine vtk_grid!}}}
subroutine vtk_scalar(arr,title)!{{{
use prmtr
use post_prmtr
   implicit none
  integer i,j,k
  character(*),intent(in)::title
  double precision, dimension(bis:bie,bjs:bje,bks:bke), intent(in)::arr

   write(66,'(a)') 'SCALARS '//trim(title)//' float 1'
   write(66, '(a20)') 'LOOKUP_TABLE default'
   do k = 1, nk
     do j = 1, nj
        do i = 1, ni
           write(66,'(e14.6e3)') arr(i,j,k)
        enddo
     enddo
   enddo
   write(66,*)
end subroutine vtk_scalar
subroutine vtk_vector(arr1,arr2,arr3,title)
use prmtr
use post_prmtr
  implicit none
  integer i,j,k
  double precision,dimension(bis:bie,bjs:bje,bks:bke),intent(in)::arr1,arr2,arr3
  character(*),intent(in)::title

   write(66,'(a22)') 'VECTORS '//trim(title)//' float'
   do k = 1, nk
     do j = 1, nj
       do i = 1, ni
         write(66,'(e15.7,e15.7,e15.7)') arr1(i,j,k), arr2(i,j,k), arr3(i,j,k)
       end do
     end do
   end do
   write(66,*)
end subroutine vtk_vector
 !}}}

subroutine vtk_object!{{{
use prmtr
use post_prmtr
   implicit none
   integer i
   double precision tmp(3)

   open(50,file='ob.vtk')
   write(50,'(a26)') '# vtk DataFile Version 3.0'
   write(50,'(a8)') 'POLYGON '
   write(50,'(a5)') 'ASCII'
   write(50,'(a16)') 'DATASET POLYDATA'
   write(50,'(a7,i2,a6)') 'POINTS ', 8 ,' float'
   open(51,file='../ob_grid.d')
   do i=1,8
     read(51,*) tmp(1),tmp(2),tmp(3)
     write(50,'(f8.3,f8.3,f8.3)') tmp(1),tmp(2),tmp(3)
   end do
   close(51)
   write(50,'(a9,i2,a1,i2)') 'POLYGONS ', 6,' ',30
   write(50,'(i2,i2,i2,i2,i2)')  4, 0, 1, 3, 2
   write(50,'(i2,i2,i2,i2,i2)')  4, 4, 5, 7, 6
   write(50,'(i2,i2,i2,i2,i2)')  4, 1, 5, 7, 3
   write(50,'(i2,i2,i2,i2,i2)')  4, 1, 5, 4, 0
   write(50,'(i2,i2,i2,i2,i2)')  4, 0, 4, 6, 2
   write(50,'(i2,i2,i2,i2,i2)')  4, 2, 3, 7, 6
   close(50)
print *,'Make Object'

end subroutine vtk_object!}}}
