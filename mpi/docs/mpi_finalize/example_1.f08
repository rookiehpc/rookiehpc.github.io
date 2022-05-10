!> @brief Illustrate how to finalise the MPI environment.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    CALL MPI_Init()

    ! Tell MPI to shut down and that we no longer need it.
    CALL MPI_Finalize()
END PROGRAM main
