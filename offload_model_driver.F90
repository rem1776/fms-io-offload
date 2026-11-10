program offload_model_driver 
  use fms_mod, only: fms_init, fms_end, string, check_nml_error
  use platform_mod
  use mpp_mod
  use mpp_domains_mod
  use fms2_io_mod
  use memutils_mod
  use offloading_io_mod

  implicit none

  integer, parameter          :: lat_lon = 1     !< Using a lat lon domain
  integer, parameter          :: cube_sphere = 2 !< Using a cube sphere domain
  integer, allocatable        :: model_pes(:)    !< The model PEs
  integer, allocatable        :: writer_pes(:)  !< The PEs for offloading
  integer, allocatable        :: global_pes(:)       !< all of the PEs
  logical                     :: is_root_pe      !< The root PE from all of the PEs
  logical                     :: is_model_pe     !< .True. if current PE is a member of the model_pes
  logical                     :: is_offload_pe   !< .True. if current PE is a member of the offload_pes
  type(domain2D)              :: model_domain    !< Domain for the model PEs
  integer                     :: i               !< For do loops
  integer                     :: io              !< Error code when reading namelist
  integer                     :: ierr            !< Error code for namelist
  character(len=30)           :: filename        !< Filename for the test
  type(FmsNetcdfDomainFile_t) :: fileobj         !< Fileobj for the test
  real(kind=r4_kind), allocatable :: var_r4(:,:,:) !< Variable data for the "model"
  integer, parameter :: timesteps = 365 !< Number of timesteps to write

  integer :: clock_id 

  !< Namelist variables
  integer :: nx = 96                  !< Number of points in the x direction (per tile)
  integer :: ny = 96                  !< Number of points in the y direction (per tile)
  integer :: nz = 65
  integer :: nxhalo = 2               !< Number of halo points in the x direction
  integer :: nyhalo = 2               !< Number of halo points in the x direction
  integer :: io_layout(2)             !< Io layout to use for the model domain
  integer :: domain_type = lat_lon    !< The type of domain to use [lat_lon or cube_sphere]
  integer :: layout(2)                !< layout for each tile
  integer :: nwriters                 !< number of writing pes, taken from the end of the pelist
  integer :: max_pending              !< number of pending write operations

  namelist /test_io_offloading_nml/ nx, ny, nz, nxhalo, nyhalo, domain_type, io_layout, layout, nwriters, max_pending

  call fms_init

  layout = -1

  read(input_nml_file, nml=test_io_offloading_nml, iostat=io)
  ierr = check_nml_error(io, 'test_io_offloading_nml')

  call offloading_io_init(global_pes, model_pes, writer_pes, nwriters=6)

  call mpp_set_current_pelist(model_pes)

  ! Only the model pes are creating the domain and allocating the data
  select case (domain_type)
  case (lat_lon)
    model_domain = mpp_define_lat_lon_domain(nx, ny, halox=nxhalo, haloy=nyhalo)
  case (cube_sphere)
    io_layout = 1
    if( layout(1) .eq. -1) then
      call mpp_define_cubic_domain(nx, ny, 6, io_layout, nhalos=nxhalo, domain_out=model_domain)
    else
      call mpp_define_cubic_domain(nx, ny, 6, io_layout, nhalos=nxhalo, domain_out=model_domain)
    endif
  end select

  filename = "offload_output.nc"

  var_r4 = create_dummy_data(model_domain)

  call send_domain_metadata(model_domain, max_pending_writes=max_pending)

  clock_id = mpp_clock_id( 'WriteClock' )

  call print_memuse_stats('Begin')

  call open_offloaded_file(filename, fileobj)

  call register_axes_offloaded_file( (/"lon","lat"/), (/"x","y"/), (/nx,ny/), model_domain, fileobj)
  call register_axis_offloaded_file("level", nz, fileobj)
  call register_axis_offloaded_file("time", unlimited, fileobj)
  call register_field_offloaded_file("daily_3d", fileobj, "double", (/"lon", "lat", "level", "time"/))

  do i = 1, timesteps
    if(mpp_pe() .eq. mpp_root_pe()) print *, "writing timestep i=", i
    var_r4 = var_r4 + (i * 1.0e-5)
    call mpp_clock_begin(clock_id)
    call write_field_offloaded_file("daily_3d", var_r4, model_domain, fileobj, unlim_dim_level=i)
    call mpp_clock_end(clock_id)
  enddo
  print *, "model pe:", mpp_pe(), " done with calculations"
  call close_offloaded_file(fileobj)

  call offloading_io_end()

  call print_memuse_stats('End')

  call fms_end

  contains

  function create_dummy_data(domain) &
    result(dummy_data)

    type(domain2D), intent(in) :: domain
    real(kind=r4_kind), allocatable :: dummy_data(:,:,:)

    integer :: is !< Starting x index
    integer :: ie !< Ending x index
    integer :: js !< Starting y index
    integer :: je !< Ending y index

    integer :: j, k

    !Allocate the data to the size of the data domain but only fill the compute domain with data
    call mpp_get_data_domain(domain, is, ie, js, je)
    allocate(dummy_data(is:ie, js:je, nz))
    dummy_data = -999_r4_kind

    call mpp_get_compute_domain(domain, is, ie, js, je)
    do j = is, ie
      do k = js, je
        do i = 1, nz
          dummy_data(j, k, i) = real(j, kind=r4_kind)* 100_r4_kind + &
            real(k, kind=r4_kind) + real(i, kind=r4_kind)/100_r4_kind
        enddo
      enddo
    enddo
  end function
end program
