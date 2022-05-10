!> @brief Illustrates how to use an MPI barrier.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: my_rank

    CALL MPI_Init()

    ! Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)

    WRITE(*,'(A,I0,A)') '[MPI process ', my_rank, '] I start waiting on the barrier.'
    CALL MPI_Barrier(MPI_COMM_WORLD)
    WRITE(*,'(A,I0,A)') '[MPI process ', my_rank, '] I know all MPI processes have waited on the barrier.'

    CALL MPI_Finalize()
END PROGRAM main
