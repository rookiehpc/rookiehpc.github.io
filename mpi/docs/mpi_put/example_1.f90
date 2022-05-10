!> @brief Illustrate how to put data into a target window.
!> @details This application consists of two MPI processes. MPI process 1
!> exposes a window containing an integer. MPI process 0 puts the value 12345
!> in it via MPI_Put. After the MPI_Put is issued, synchronisation takes place
!> via MPI_Win_fence and the MPI process 1 prints the value in its window.
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
    INTEGER :: my_value = 12345
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
    CALL MPI_Win_create(window_buffer, window_buffer_size, integer_size, MPI_INFO_NULL, MPI_COMM_WORLD, window, ierror)
    IF (my_rank .EQ. 1) THEN
        WRITE(*, '(A,I0,A)') '[MPI process 1] Value in my window_buffer before MPI_Put: ', window_buffer, '.'
    END IF
    CALL MPI_Win_fence(0, window, ierror)

    IF (my_rank == 0) THEN
        ! Push my value into the first integer in MPI process 1 window
        target_displacement = 0
        CALL MPI_Put(my_value, 1, MPI_INTEGER, 1, target_displacement, 1, MPI_INTEGER, window, ierror)
        WRITE(*, '(A,I0,A)') '[MPI process 0] I put data ', my_value ,' in MPI process 1 window via MPI_Put.'
    END IF

    ! Wait for the MPI_Put issued to complete before going any further
    CALL MPI_Win_fence(0, window, ierror)

    IF (my_rank == 1) THEN
        WRITE(*, '(A,I0,A)') '[MPI process 1] Value in my window_buffer after MPI_Put: ', window_buffer ,'.'
    END IF

    ! Destroy the window
    CALL MPI_Win_free(window, ierror)

    CALL MPI_Finalize(ierror)
END PROGRAM main