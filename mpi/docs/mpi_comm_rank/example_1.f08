!> @brief For each process in the default communicator MPI_COMM_WORLD, show their rank.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: my_rank

    CALL MPI_Init()

    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)

    WRITE(*,'(A,I0,A)') 'I am MPI process ', my_rank, '.'

    CALL MPI_Finalize()
END PROGRAM main
