!> @brief For each process in the default communicator MPI_COMM_WORLD, show their rank.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: my_rank

    CALL MPI_Init(ierror)

    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    WRITE(*,'(A,I0,A)') 'I am MPI process ', my_rank, '.'

    CALL MPI_Finalize(ierror)
END PROGRAM main