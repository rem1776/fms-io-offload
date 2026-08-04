program model 
  use fms_mod
  use mpp_mod
  use mpp_domains_mod
  use platform_mod
  use fms2_io_mod

  implicit none

  integer :: my_pe, i, layout(2)
  integer, parameter :: nx = 16, ny = 16
  real, allocatable :: data_field_2d(:,:)
  integer, parameter :: halo = 1
  type(domain2d) :: domain
  integer :: isc, iec, jsc, jec, axis_sizes(2)
  character(len=128) :: axis_names(2), cart_names(2), field_names(1)
  integer, allocatable :: global_pelist(:), model_pelist(:), offload_pelist(:)
  integer, parameter :: IO_OP_OPEN = 1, IO_OP_REGISTER_AXIS = 2, IO_OP_REGISTER_FIELD = 3, IO_OP_WRITE = 4, IO_OP_CLOSE = 5

  call fms_init()

  ! set up 3 pelists for global, model, offloaded writers
  call setup_pelists(global_pelist, model_pelist, offload_pelist)
  print *, "model pelists: global=", global_pelist, " model=", model_pelist, " offload=", offload_pelist
  ! set model pelist and define domain
  call mpp_set_current_pelist(model_pelist)
  call mpp_define_layout( (/1,nx,1,ny/), size(model_pelist), layout )
  call mpp_define_domains( (/1,nx,1,ny/), layout, domain, whalo=halo, ehalo=halo, &
                           shalo=halo, nhalo=halo, name='model_domain') 
  call mpp_define_io_domain(domain, (/1,1/))
  call mpp_get_compute_domain(domain, isc, iec, jsc, jec)
  allocate(data_field_2d(isc:iec, jsc:jec))

  ! set global pelist and send domain metadata to offload pes
  call mpp_set_current_pelist(global_pelist)
  call send_domain_metadata(domain)
  call mpp_set_current_pelist(model_pelist)

  axis_names = (/ 'x', 'y' /)
  cart_names = (/ 'x', 'y' /)
  axis_sizes = (/ nx, ny /)
  field_names = (/ 'var1' /)

  print *, 'opening offloaded file...'

  call mpp_set_current_pelist(global_pelist, no_sync=.true.)
  if( mpp_pe() .eq. mpp_root_pe() ) then 
    call open_offloaded_file('offload_test.nc')
    call register_axis_offloaded_file(axis_names(1), cart_names(1), axis_sizes(1))
  endif
  call mpp_set_current_pelist(model_pelist, no_sync=.true.)

  do i=1,10
    print *, 'model doing timestep ', i
    data_field_2d = real(i) ! just fill with timestep number for testing
    !if( mpp_pe() .eq. mpp_root_pe() ) then 
      !call write_field_offloaded_file(field_names(1), data_field_2d)
    !endif
  end do

  call mpp_set_current_pelist(global_pelist, no_sync=.true.)
  if( mpp_pe() .eq. mpp_root_pe() ) then 
    call close_offloaded_files()
  endif
  call mpp_set_current_pelist(model_pelist, no_sync=.true.)

  ! need to be on global to end correctly
  call mpp_set_current_pelist(global_pelist, no_sync=.true.)
  call fms_end()

  contains

#include "offload_shared.inc"

end program model 
