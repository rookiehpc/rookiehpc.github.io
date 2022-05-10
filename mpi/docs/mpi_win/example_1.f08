!> @brief Illustrate how to use the MPI_Win data structure.
!> @details This application consists in creating a window and destroying it.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: comm_size
    INTEGER :: my_rank
    INTEGER, PARAMETER :: ARRAY_SIZE = 2
    INTEGER(KIND=MPI_ADDRESS_KIND) :: window_buffer
    INTEGER(KIND=MPI_ADDRESS_KIND) :: window_buffer_size
    INTEGER :: window
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
    CALL MPI_Win_allocate(window_buffer_size, integer_size, MPI_INFO_NULL, MPI_COMM_WORLD, window_buffer, window, ierror)
    WRITE(*, '(A,I0,A)') '[MPI process ', my_rank, '] Window created.'

    ! Issue RMA communications
    ! ...
    ! Wait for all RMA communications to complete

    ! Destroy the window
    WRITE(*, '(A,I0,A)') '[MPI process ', my_rank, '] Window destroyed.'
    CALL MPI_Win_free(window, ierror)

    CALL MPI_Finalize(ierror)
END PROGRAM main
