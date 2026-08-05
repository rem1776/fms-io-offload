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
  character(len=*), intent(in) :: filename
  if( mpp_pe() .ne. mpp_root_pe() ) return
  print *, "model sending open signal to writer pe(", mpp_npes()-1, ") for file: ", trim(filename)
  call mpp_send(IO_OP_OPEN, mpp_npes()-1)
  call mpp_send(filename, 1, mpp_npes()-1)
end subroutine open_offloaded_file

subroutine close_offloaded_file()
  if( mpp_pe() .ne. mpp_root_pe() ) return
  call mpp_send(IO_OP_CLOSE, mpp_npes()-1)
end subroutine close_offloaded_file

subroutine io_offload_end()
  if( mpp_pe() .ne. mpp_root_pe() ) return
  call mpp_send(IO_OP_EXIT, mpp_npes()-1)
end subroutine io_offload_end

subroutine register_axis_offloaded_file(axis_name, cart_name, axis_size)
  integer :: io_op
  character(len=*), intent(in) :: axis_name, cart_name
  integer, intent(in) :: axis_size
  character(len=128) :: axis_name_buf
  character(len=1) :: cart_name_buf

  if( mpp_pe() .ne. mpp_root_pe() ) return
  axis_name_buf = trim(axis_name)
  cart_name_buf = cart_name
  io_op = IO_OP_REGISTER_AXIS
  ! send op code so the writer knows what to receive next
  call mpp_send(io_op, mpp_npes()-1)
  ! send the axis name, cartesian name, and size to the writer PE
  call mpp_send(axis_name_buf, 1, mpp_npes()-1)
  call mpp_send(cart_name_buf, 1, mpp_npes()-1)
  call mpp_send(axis_size, mpp_npes()-1)
end subroutine register_axis_offloaded_file

subroutine register_field_offloaded_file(field_name, field_dims, field_type)
  integer :: io_op
  character(len=*), intent(in) :: field_name
  character(len=16), intent(in) :: field_dims(:)
  integer, intent(in) :: field_type

  if( mpp_pe() .ne. mpp_root_pe() ) return
  io_op = IO_OP_REGISTER_FIELD
  call mpp_send(io_op, mpp_npes()-1)
  call mpp_send(field_name, 1, mpp_npes()-1)
  !! TODO this is bad, but ill put off adding string arrays to mpp_send for now 
  call mpp_send(field_dims(1), 1, mpp_npes()-1)
  call mpp_send(field_dims(2), 1, mpp_npes()-1)

  call mpp_send(field_type, mpp_npes()-1)
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
  call mpp_send(field_name_buf, 1, mpp_npes()-1)
  fld_nx = size(field_data, 1)
  fld_ny = size(field_data, 2)
  call mpp_send(fld_nx, mpp_npes()-1)
  call mpp_send(fld_ny, mpp_npes()-1)
  call mpp_send(field_data, fld_nx*fld_ny, mpp_npes()-1)
end subroutine write_field_offloaded_file

elemental function get_type_string(field_type) result(type_string)
  integer, intent(in) :: field_type
  character(len=16) :: type_string

  select case(field_type)
  case(DOUBLE)
    type_string = "double"
  case(FLOAT)
    type_string = "float"
  case(INTEGER)
    type_string = "integer"
  case(LOGICAL)
    type_string = "logical"
  case(CHARACTER)
    type_string = "character"
  case default
    type_string = "unknown"
  end select
end function get_type_string

elemental function clean_string(input_string) result(cleaned_string)
  character(len=*), intent(in) :: input_string
  character(len=len(input_string)) :: cleaned_string
  integer :: i, j

  j = 1
  do i = 1, len(input_string)
    if (input_string(i:i) /= ' ' .and. input_string(i:i) /= ACHAR(10) .and. input_string(i:i) /= ACHAR(13)) then
      cleaned_string(j:j) = input_string(i:i)
      j = j + 1
    end if
  end do
  cleaned_string = cleaned_string(1:j-1)
end function clean_string