!> @brief This exercise is to find the bug in a simple MPI hello world.
PROGRAM main
	USE mpi

	IMPLICIT NONE

	INTEGER :: ierror
	INTEGER :: my_rank
	INTEGER :: comm_size

    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    CALL MPI_Comm_size(MPI_COMM_WORLD, comm_size, ierror)

    WRITE(*, '(A,I0,A,I0,A)') '"Hello World!" from MPI process ', my_rank ,'. We are ', comm_size ,' MPI processes.'

    CALL MPI_Finalize(ierror)
END PROGRAM main