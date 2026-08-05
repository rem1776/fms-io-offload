! writer_fms.f90
program writer
    use fms_mod
    use mpp_mod
    use mpp_domains_mod
    use fms2_io_mod
    use platform_mod
    use offloading_io_mod
    implicit none

    integer :: ierr, my_pe, n_compute_ranks, i, io_op
    integer :: received_data
    integer, parameter :: MAX_FILEOBJ = 10
    type(FmsNetcdfDomainFile_t) :: fileobj !(MAX_FILEOBJ)
    integer :: model_root_pe = 0
    logical :: is_running = .true., success
    type(domain2d) :: model_domain, offload_domain
    integer :: nx, ny, nz, ntiles, npes
    integer, allocatable :: global_pelist(:), offload_pelist(:), model_pelist(:), dims(:)
    integer :: io_layout(2), global_indices(2)
    integer, parameter :: halo = 0
    character(len=128) :: filename, axis_name, cart_name, field_name, field_units
    character(len=16) :: field_dims(5)
    integer :: filename_num, axis_len, field_type, dim_count
    real, allocatable :: field_data(:,:)
    character(len=16) :: field_type_str

    #include "enums.inc"


    call fms_init()
    my_pe = mpp_pe()
    npes = mpp_npes()
      
    ! set up pelists
    call setup_pelists(global_pelist, model_pelist, offload_pelist)

    print *, "writer pelists: global=", global_pelist, " model=", model_pelist, " offload=", offload_pelist
    call mpp_set_current_pelist(global_pelist)

    ! receive domain struct and any other initialization data from the model 
    call recv_domain_metadata(model_domain, global_indices)
    nx = global_indices(1)
    ny = global_indices(2)

    ! switch to offload pelist for defining offload domain and io domain
    call mpp_set_current_pelist(offload_pelist)

    ! use global indices + tile count to define domain for offloaded pes 
    ntiles = mpp_get_ntile_count(model_domain)

    ! set up the domain to write with
    select case (ntiles)
    case (1)
      print *, 'single tile case, x=', nx, ' y=', ny
      call mpp_define_domains( (/1,nx,1,ny/), (/1,1/), offload_domain, whalo=halo, ehalo=halo, &
                               shalo=halo, nhalo=halo, name='offload_domain') 
    case (6) 
      offload_domain = create_cubic_domain(nx, ny, ntiles, (/1,1/), offload_pes=offload_pelist)
    case default
      call mpp_error(FATAL, "Unsupported number of tiles for offloading: " // trim(adjustl(string(ntiles))))
    end select
    call mpp_define_io_domain(offload_domain, (/1,1/))

    ! main loop, need to receive data for as long as model is sending
    do while ( is_running )
      print *, 'writer waiting for io_op signal from model...'

      call mpp_set_current_pelist(global_pelist, no_sync=.true.)
      call mpp_recv(io_op, from_pe=model_root_pe)

      print *, "writer received io_op code: ", io_op

      select case (io_op)

      case (IO_OP_OPEN)
        print *, 'writer recieved open signal'
        call mpp_recv(filename, 1, model_root_pe)
        call mpp_set_current_pelist(offload_pelist, no_sync=.true.)
        success = open_file(fileobj, 'offloaded_file.nc', 'write', domain=offload_domain)

      case (IO_OP_CLOSE)
        print *, 'writer recieved close signal'
        call mpp_set_current_pelist(offload_pelist, no_sync=.true.)
        call close_file(fileobj)

      case (IO_OP_REGISTER_AXIS)
        print *, 'writer recieved register axis signal'
        call mpp_recv(axis_name, 1, model_root_pe)
        call mpp_recv(cart_name, 1, model_root_pe)
        cart_name = "" // trim(adjustl(cart_name))
        cart_name = cart_name(1:1)  ! fortran strings are annoying
        call mpp_recv(axis_len, model_root_pe)
        print *, "writer recieved cart name: '" // cart_name // "'"
        call mpp_set_current_pelist(offload_pelist, no_sync=.true.)
        call register_axis(fileobj, trim(axis_name), trim(adjustl(cart_name)))

      case (IO_OP_REGISTER_FIELD)
        print *, 'writer recieved register field signal'
        call mpp_recv(field_name, 1, model_root_pe)

        ! TODO need to implement character array support 
        call mpp_recv(field_dims(1), 1, model_root_pe)
        call mpp_recv(field_dims(2), 1, model_root_pe)

        call mpp_recv(field_type, model_root_pe)
        call mpp_set_current_pelist(offload_pelist, no_sync=.true.)
        field_type_str = get_type_string(field_type)
        field_name = clean_string(field_name)
        field_dims = clean_string(field_dims)
        !field_dims(1) = clean_string(field_dims(1))
        !field_dims(2) = clean_string(field_dims(2))
        !call register_field(fileobj, trim(field_name), trim(field_type_str), dimensions=field_dims)
        call register_field(fileobj, trim(field_name), trim(field_type_str), &
                            dimensions=(/ trim(field_dims(1)), trim(field_dims(2)) /))

      case (IO_OP_WRITE)
        print *, 'writer recieved write signal'
        call mpp_recv(field_name, 1, model_root_pe)
        call mpp_recv(nx, model_root_pe)
        call mpp_recv(ny, model_root_pe)
        if (allocated(field_data)) deallocate(field_data)
        allocate(field_data(nx, ny))
        call mpp_recv(field_data, nx*ny, model_root_pe)
        call mpp_set_current_pelist(offload_pelist, no_sync=.true.)
        call write_data(fileobj, trim(field_name), field_data)

      case (IO_OP_EXIT)
        print *, 'writer recieved exit signal'
        is_running = .false.

      case default
        call mpp_error(NOTE, "Writer received unknown io_op code: " // trim(adjustl(string(io_op))))
        is_running = .false. 

      end select

    end do

    call mpp_set_current_pelist(global_pelist, no_sync=.true.)
    call fms_end()


    contains

#include "offload_shared.inc"

end program writer
