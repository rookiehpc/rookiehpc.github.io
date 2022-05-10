!> @brief Illustrate how to get data from a target window.
!> @details This application consists of two MPI processes. MPI process 1
!> exposes a window containing an integer. MPI process 0 gets the value in it.
!> After the MPI_Get is issued, synchronisation takes place via MPI_Win_fence
!> and the MPI process 1 prints the value in its window.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: comm_size
    INTEGER :: my_rank
    INTEGER :: window_buffer = 0
    INTEGER :: integer_size
    INTEGER(KIND=MPI_ADDRESS_KIND) :: window_buffer_size
    INTEGER :: window
    INTEGER :: value_fetched
    INTEGER(KIND=MPI_ADDRESS_KIND) :: target_displacement

    CALL MPI_Init(ierror)
    CALL MPI_Type_size(MPI_INTEGER, integer_size, ierror)
    window_buffer_size = integer_size

    ! Check that only 2 MPI processes are spawn
    CALL MPI_Comm_size(MPI_COMM_WORLD, comm_size, ierror)
    IF (comm_size .NE. 2) THEN
        WRITE(*, '(A,I0,A)') 'This application is meant to be run with 2 MPI processes, not ', comm_size ,'.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    ! Create the window
    IF (my_rank .EQ. 1) THEN
        window_buffer = 12345;
    END IF
    CALL MPI_Win_create(window_buffer, window_buffer_size, integer_size, MPI_INFO_NULL, MPI_COMM_WORLD, window, ierror)
    CALL MPI_Win_fence(0, window, ierror)

    IF (my_rank == 0) THEN
        ! Fetch the value from the MPI process 1 window
        target_displacement = 0
        CALL MPI_Get(value_fetched, 1, MPI_INTEGER, 1, target_displacement, 1, MPI_INTEGER, window, ierror)
    END IF

    ! Wait for the MPI_Get issued to complete before going any further
    CALL MPI_Win_fence(0, window, ierror)

    IF (my_rank == 0) THEN
        WRITE(*, '(A,I0,A)') '[MPI process 0] Value fetched from MPI process 1 window: ', value_fetched ,'.'
    END IF

    ! Destroy the window
    CALL MPI_Win_free(window, ierror)

    CALL MPI_Finalize(ierror)
END PROGRAM main