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
   open(50,file='../grid.d')
   do k=bks,bke
     do j=bjs,bje
       do i=bis,bie
          read(50,*) x(i,j,k),y(i,j,k),z(i,j,k)
       end do 
     end do 
   end do 
   close(50)
   do time=time_ini,300000
   !do time=time_ini,3
     !if(mod(time,out_freq).eq.0)then
       !write(tmpstring,'(i3.3)') int(t/1)
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
       call vtk_scalar(  u_mat,"UVelo")
       call vtk_scalar(  v_mat,"VVelo")
       call vtk_scalar(  w_mat,"WVelo")
       !call vtk_scalar(  T_mat,"Energ")
       call vtk_vector(u_mat,v_mat,w_mat,"Velocity")
       close(66)
       open(15,file="restart.bin",form="unformatted")
          write(15) time
       close(15)
       cntr = cntr + 1
       print *,cntr
     !end if
   enddo
end program main!}}}

subroutine vtk_grid(xgrid,ygrid,zgrid)!{{{
use prmtr
use post_prmtr
   implicit none
   integer i,j,k
   double precision,dimension(bis:bie,bjs:bje,bks:bke)::xgrid,ygrid,zgrid
   write(66,'(a26)') '# vtk DataFile Version 2.0'
   write(66,'(a5)') 'TEST '
   write(66,'(a5)') 'ASCII'
   write(66,'(a23)') 'DATASET STRUCTURED_GRID'
   write(66,'(a11,i3,a1,i3,a1,i1)') 'DIMENSIONS ', ni, ' ', nj, ' ', nk
   write(66,'(a7,i4,a6)') 'POINTS ', ni*nj*nk ,' float'
   do k=1,nk
     do j=1,nj
        do i=1,ni
           write(66,'(e14.6e3,2(1x,e14.6e3))') xgrid(i,j,k), ygrid(i,j,k), zgrid(i,j,k)
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
