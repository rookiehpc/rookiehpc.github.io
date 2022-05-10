!> @brief Illustrate how to finalise the MPI environment.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror

    CALL MPI_Init(ierror)

    ! Tell MPI to shut down and that we no longer need it.
    CALL MPI_Finalize(ierror)
END PROGRAM main