!> @brief Illustrate how to destroy a window.
!> @details This application consists in creating a window and destroying it.
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
    INTEGER :: integer_size

    CALL MPI_Init(ierror)
    CALL MPI_Type_size(MPI_INTEGER, integer_size, ierror)
    window_buffer_size = integer_size * ARRAY_SIZE

    ! Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    ! Create the window
    IF (my_rank .EQ. 1) THEN
        window_buffer(1) = 67890
    END IF
    CALL MPI_Win_create(window_buffer, window_buffer_size, integer_size, MPI_INFO_NULL, MPI_COMM_WORLD, window, ierror)
    WRITE(*, '(A,I0,A)') '[MPI process ', my_rank, '] Window created.'

    ! Destroy the window
    CALL MPI_Win_free(window, ierror)
    WRITE(*, '(A,I0,A)') '[MPI process ', my_rank, '] Window destroyed.'

    CALL MPI_Finalize(ierror)
END PROGRAM main