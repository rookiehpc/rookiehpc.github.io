!> @brief Display the number of MPI processes in the default communicator MPI_COMM_WORLD.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: my_rank
    INTEGER :: comm_size

    CALL MPI_Init(ierror)

    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    CALL MPI_Comm_size(MPI_COMM_WORLD, comm_size, ierror)

    IF (my_rank .EQ. 0) THEN
        WRITE(*,'(A,I0,A)') 'There are ', comm_size, ' MPI processes in the default communicator MPI_COMM_WORLD.'
    END IF

    CALL MPI_Finalize(ierror)
END PROGRAM main