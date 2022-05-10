!> @brief Illustrate how to attach a memory region to a window.
!> @details This application consits of 2 MPI processes. They create a window
!> dynamically, then MPI process 0 attaches a region to its window and send its
!> address to MPI process 1. Finally, MPI process 1 uses that address as part of
!> an MPI_Put to write data into MPI process 0 window, which prints its value at
!> the end.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: comm_size
    INTEGER :: my_rank
    TYPE(MPI_Win) :: window
    INTEGER :: window_buffer
    INTEGER(KIND=MPI_ADDRESS_KIND) :: window_buffer_size
    INTEGER(KIND=MPI_ADDRESS_KIND) :: window_buffer_address
    INTEGER :: value

    CALL MPI_Init()

    ! Check that only 2 MPI processes are spawn
    CALL MPI_Comm_size(MPI_COMM_WORLD, comm_size)
    IF (comm_size .NE. 2) THEN
        WRITE(*, '(A,I0,A)') 'This application is meant to be run with 2 MPI processes, not ', comm_size ,'.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1)
    END IF

    ! Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)

    ! Create the window
    CALL MPI_Win_create_dynamic(MPI_INFO_NULL, MPI_COMM_WORLD, window)
    CALL MPI_Win_fence(0, window)
    WRITE(*, '(A,I0,A)') '[MPI process ', my_rank ,'] Window created dynamically.'

    IF (my_rank .EQ. 0) THEN
        ! Allocate and attach the memory region to the window
        CALL MPI_Get_address(window_buffer, window_buffer_address)
        CALL MPI_Type_size_x(MPI_INTEGER, window_buffer_size)
        CALL MPI_Win_attach(window, window_buffer, window_buffer_size)
        WRITE(*, '(A,I0,A)') '[MPI Process 0] Memory region attached.'

        ! Get the address of that window and send it to MPI process 1
        CALL MPI_Get_address(window_buffer, window_buffer_address)
        CALL MPI_Send(window_buffer_address, 1, MPI_AINT, 1, 0, MPI_COMM_WORLD)
        WRITE(*, '(A)') '[MPI process 0] I send the local address of my memory region to MPI process 1.'
    ELSE
        ! Get the local address of the memory region attached to that window on that target MPI process
        CALL MPI_Recv(window_buffer_address, 1, MPI_AINT, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE)
        WRITE(*, '(A)') '[MPI process 1] Local address of the memory region attached to the window on MPI process 1 received. &
                        I can now use that in MPI_Put.'

        ! Put the data into into that window
        value = 12345
        CALL MPI_Put(value, 1, MPI_INTEGER, 0, window_buffer_address, 1, MPI_INTEGER, window)
        WRITE(*, '(A,I0,A)') '[MPI Process 1] I put value ', value ,' in MPI Process 0 window.'
    END IF

    ! Destroy the window
    CALL MPI_Win_fence(0, window)
    IF (my_rank .EQ. 0) THEN
        WRITE(*, '(A,I0,A)') '[MPI process 0] Value in my window: ', window_buffer ,'.'
        CALL MPI_Win_detach(window, window_buffer_address)
        WRITE(*, '(A,I0,A)') '[MPI Process 0] Memory region detached.'
    END IF
    CALL MPI_Win_free(window)
    WRITE(*, '(A,I0,A)') '[MPI process ', my_rank, '] Window destroyed.'

    CALL MPI_Finalize()
END PROGRAM main
