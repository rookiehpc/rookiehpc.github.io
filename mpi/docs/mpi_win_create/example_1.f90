!> @brief Illustrate how to create a window.
!> @details This application consists of two MPI processes. MPI process 1
!> exposes a window containing 2 integers. The first one is initialised to 0 and
!> will be overwritten by MPI process 0 via MPI_Put to become 12345. The second
!> will be initialised to 67890 and will be read by MPI process 0 via MPI_Get.
!> After these two commands are issued, synchronisation takes place via
!> MPI_Win_fence and each MPI process prints the value that came from the other
!> peer.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: comm_size
    INTEGER :: my_rank
    INTEGER, PARAMETER :: ARRAY_SIZE = 2
    INTEGER :: window_buffer(0:ARRAY_SIZE-1)
    INTEGER(KIND=MPI_ADDRESS_KIND) :: window_buffer_size
    INTEGER :: window
    INTEGER(KIND=MPI_ADDRESS_KIND) :: target_displacement
    INTEGER :: remote_value
    INTEGER :: my_value = 12345
    INTEGER :: integer_size

    CALL MPI_Init(ierror)
    CALL MPI_Type_size(MPI_INTEGER, integer_size, ierror)
    window_buffer_size = integer_size * ARRAY_SIZE

    ! Check that only 2 MPI processes are spawn
    CALL MPI_Comm_size(MPI_COMM_WORLD, comm_size, ierror)
    IF (comm_size .NE. 2) THEN
        WRITE(*, '(A,I0,A)') 'This application is meant to be run with 2 MPI processes, not ', comm_size, '.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    ! Create the window
    IF (my_rank .EQ. 1) THEN
        window_buffer(1) = 67890
    END IF
    CALL MPI_Win_create(window_buffer, window_buffer_size, integer_size, MPI_INFO_NULL, MPI_COMM_WORLD, window, ierror)
    CALL MPI_Win_fence(0, window, ierror)

    IF (my_rank .EQ. 0) THEN
        ! Fetch the second integer in MPI process 1 window
        target_displacement = 1
        CALL MPI_Get(remote_value, 1, MPI_INTEGER, 1, target_displacement, 1, MPI_INTEGER, window, ierror)

        ! Push my value into the first integer in MPI process 1 window
        target_displacement = 0
        CALL MPI_Put(my_value, 1, MPI_INTEGER, 1, target_displacement, 1, MPI_INTEGER, window, ierror)
    END IF

    ! Wait for the MPI_Get and MPI_Put issued to complete before going any further
    CALL MPI_Win_fence(0, window, ierror)

    IF (my_rank .EQ. 0) THEN
        WRITE(*, '(A,I0,A)') '[MPI process 0] Value fetched from MPI process 1 window_buffer(1): ', remote_value, '.'
    ELSE
        WRITE(*, '(A,I0,A)') '[MPI process 1] Value put in my window_buffer(0) by MPI process 0: ', window_buffer(0), '.'
    END IF

    ! Destroy the window
    CALL MPI_Win_free(window, ierror)

    CALL MPI_Finalize(ierror)
END PROGRAM main