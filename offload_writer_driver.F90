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
  integer :: nwriters                 !< number of writing pes, taken from the end of the pelist

  namelist /writer_driver_nml/ nwriters

  call fms_init

  read(input_nml_file, nml=writer_driver_nml, iostat=io)
  call offloading_io_init(global_pes, model_pes, writer_pes, nwriters=nwriters)
  ierr = check_nml_error(io, 'writer_driver_nml')


  call mpp_set_current_pelist(writer_pes)

  if(any(mpp_pe() .eq. model_pes)) call mpp_error(FATAL, "Model PE is running the writer driver")

  call recv_domain_metadata(domain, global_size)

  call run_offload_writer()

  call fms_end

end program
