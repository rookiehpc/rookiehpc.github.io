!> @brief Illustrates how to use an MPI barrier.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: my_rank

    CALL MPI_Init(ierror)

    ! Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    WRITE(*,'(A,I0,A)') '[MPI process ', my_rank, '] I start waiting on the barrier.'
    CALL MPI_Barrier(MPI_COMM_WORLD, ierror)
    WRITE(*,'(A,I0,A)') '[MPI process ', my_rank, '] I know all MPI processes have waited on the barrier.'

    CALL MPI_Finalize(ierror)
END PROGRAM main