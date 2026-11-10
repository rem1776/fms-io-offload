program offload_writer_driver

  use fms_mod
  use offloading_io_mod
  use mpp_mod
  use mpp_domains_mod

  implicit none

  integer, allocatable :: global_pes(:), model_pes(:), writer_pes(:)
  integer :: global_size(2)
  type(domain2D) :: domain
  integer :: io, ierr

  ! this namelist is exactly the same as the model driver to simplify things, only nwriters is used here
  !< Namelist variables
  integer :: nx = 96                  !< Number of points in the x direction (per tile)
  integer :: ny = 96                  !< Number of points in the y direction (per tile)
  integer :: nz = 65
  integer :: nxhalo = 2               !< Number of halo points in the x direction
  integer :: nyhalo = 2               !< Number of halo points in the x direction
  integer :: io_layout(2)             !< Io layout to use for the model domain
  integer :: domain_type = 0 !lat_lon    !< The type of domain to use [lat_lon or cube_sphere]
  integer :: layout(2)                !< layout for each tile
  integer :: nwriters                 !< number of writing pes, taken from the end of the pelist
  integer :: max_pending = 0

  namelist /test_io_offloading_nml/ nx, ny, nz, nxhalo, nyhalo, domain_type, io_layout, layout, nwriters, max_pending

  read(input_nml_file, nml=test_io_offloading_nml, iostat=io)
  ierr = check_nml_error(io, 'test_io_offloading_nml')

  call fms_init

  call offloading_io_init(global_pes, model_pes, writer_pes, nwriters=6)

  call mpp_set_current_pelist(writer_pes)

  if(any(mpp_pe() .eq. model_pes)) call mpp_error(FATAL, "Model PE is running the writer driver")

  call recv_domain_metadata(domain, global_size)

  call run_offload_writer()

  call fms_end

end program
