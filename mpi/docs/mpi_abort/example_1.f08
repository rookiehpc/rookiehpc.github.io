!> @brief Illustrates how to abort all processes in the MPI_COMM_WORLD communicator.
PROGRAM main
	USE mpi_f08

	IMPLICIT NONE

	CALL MPI_Init()

	CALL MPI_Abort(MPI_COMM_WORLD, -1)

	CALL MPI_Finalize()
END PROGRAM main
