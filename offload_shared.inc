subroutine recv_domain_metadata(model_domain, global_domain_size)
  type(domain2d), intent(inout) :: model_domain
  integer, intent(out) :: global_domain_size(2)

  call mpp_define_null_domain(model_domain) ! null domain to be overwritten by model domain type during bcast
  call mpp_broadcast_domain(model_domain)
  call mpp_broadcast(global_domain_size, 2, 0)
end subroutine

subroutine send_domain_metadata(model_domain)
  type(domain2d), intent(inout) :: model_domain
  integer :: global_domain_size(2)

  call mpp_broadcast_domain(model_domain)
  call mpp_get_global_domain(model_domain, xsize=global_domain_size(1), ysize=global_domain_size(2))
  call mpp_broadcast(global_domain_size, 2, 0)
end subroutine

subroutine setup_pelists(global_pelist, model_pelist, writer_pelist)
  integer, allocatable, intent(out) :: global_pelist(:), model_pelist(:), writer_pelist(:)
  integer :: npes, i

  npes = mpp_npes()
  allocate(global_pelist(npes))
  call mpp_get_current_pelist(global_pelist)
  call mpp_declare_pelist(global_pelist)

  ! Assume model runs on first npes-1 PEs, writer on last PE
  allocate(model_pelist(npes-1))
  do i = 1, npes-1
    model_pelist(i) = i-1
  end do
  call mpp_declare_pelist(model_pelist)

  allocate(writer_pelist(1))
  writer_pelist(1) = npes - 1
  call mpp_declare_pelist(writer_pelist)
end subroutine setup_pelists

subroutine open_offloaded_file(filename)
  integer :: io_op
  character(len=*), intent(in) :: filename

  if( mpp_pe() .ne. mpp_root_pe() ) return
  print *, "model sending open signal to writer pe(", mpp_npes()-1, ") for file: ", trim(filename)
  io_op = IO_OP_OPEN
  call mpp_send(io_op, mpp_npes()-1)
end subroutine open_offloaded_file

subroutine close_offloaded_file()
  integer :: io_op

  if( mpp_pe() .ne. mpp_root_pe() ) return
  io_op = IO_OP_CLOSE
  call mpp_send(io_op, mpp_npes()-1)
end subroutine close_offloaded_file

subroutine close_offloaded_files()
  call close_offloaded_file()
end subroutine close_offloaded_files

subroutine register_axis_offloaded_file(axis_name, cart_name, axis_size)
  integer :: io_op
  character(len=*), intent(in) :: axis_name, cart_name
  integer, intent(in) :: axis_size
  character(len=128) :: axis_name_buf, cart_name_buf

  if( mpp_pe() .ne. mpp_root_pe() ) return
  axis_name_buf = trim(axis_name)
  cart_name_buf = trim(cart_name)
  io_op = IO_OP_REGISTER_AXIS
  call mpp_send(io_op, mpp_npes()-1)
  call mpp_send_char(axis_name_buf, mpp_npes()-1)
  call mpp_send_char(cart_name_buf, mpp_npes()-1)
  call mpp_send(axis_size, mpp_npes()-1)
end subroutine register_axis_offloaded_file

subroutine register_axes_offloaded_file(axis_names, cart_names, axis_sizes, domain)
  character(len=*), intent(in) :: axis_names(:), cart_names(:)
  integer, intent(in) :: axis_sizes(:)
  type(domain2d), intent(in) :: domain
  integer :: i

  if( mpp_pe() .ne. mpp_root_pe() ) return
  do i = 1, size(axis_names)
    call register_axis_offloaded_file(axis_names(i), cart_names(i), axis_sizes(i))
  end do
end subroutine register_axes_offloaded_file

subroutine register_field_offloaded_file(field_name, field_dims)
  integer :: io_op
  character(len=*), intent(in) :: field_name
  integer, intent(in) :: field_dims(:)
  character(len=128) :: field_name_buf

  if( mpp_pe() .ne. mpp_root_pe() ) return
  field_name_buf = trim(field_name)
  io_op = IO_OP_REGISTER_FIELD
  call mpp_send(io_op, mpp_npes()-1)
  call mpp_send_char(field_name_buf, mpp_npes()-1)
end subroutine register_field_offloaded_file

subroutine write_field_offloaded_file(field_name, field_data)
  integer :: io_op, fld_nx, fld_ny
  character(len=*), intent(in) :: field_name
  real, intent(in) :: field_data(:,:)
  character(len=128) :: field_name_buf

  if( mpp_pe() .ne. mpp_root_pe() ) return
  field_name_buf = trim(field_name)
  io_op = IO_OP_WRITE
  call mpp_send(io_op, mpp_npes()-1)
  call mpp_send_char(field_name_buf, mpp_npes()-1)
  fld_nx = size(field_data, 1)
  fld_ny = size(field_data, 2)
  call mpp_send(fld_nx, mpp_npes()-1)
  call mpp_send(fld_ny, mpp_npes()-1)
  call mpp_send(field_data, fld_nx*fld_ny, mpp_npes()-1)
end subroutine write_field_offloaded_file

! Send a fixed-length (128) character string via direct MPI call
subroutine mpp_send_char(str, to_pe)
  use mpi
  character(len=128), intent(in) :: str
  integer, intent(in) :: to_pe
  integer :: ierr, tag

  tag = 1  ! arbitrary tag for character messages
  !call MPI_SEND(str, 128, MPI_CHARACTER, to_pe, tag, MPI_COMM_WORLD, ierr)
end subroutine mpp_send_char

! Receive a fixed-length (128) character string via direct MPI call
subroutine mpp_recv_char(str, from_pe)
  use mpi
  character(len=128), intent(out) :: str
  integer, intent(in) :: from_pe
  integer :: ierr, tag
  integer :: status(MPI_STATUS_SIZE)

  tag = 1  ! arbitrary tag for character messages
  !call MPI_RECV(str, 128, MPI_CHARACTER, from_pe, tag, MPI_COMM_WORLD, status, ierr)
  str = "placeholder_string"
end subroutine mpp_recv_char
