!> @brief Illustrates how to abort all processes in the MPI_COMM_WORLD communicator.
PROGRAM main    
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror

    CALL MPI_Init(ierror)

    CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)

    CALL MPI_Finalize(ierror)
END PROGRAM main