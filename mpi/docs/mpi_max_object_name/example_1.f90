!> @brief Allocate a buffer to get the name of the default communicator.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    CHARACTER(LEN=MPI_MAX_OBJECT_NAME) :: name
    INTEGER :: name_length

    CALL MPI_Init(ierror)

    CALL MPI_Comm_get_name(MPI_COMM_WORLD, name, name_length, ierror)
    WRITE(*,'(A,A,A)') 'The default communicator is called ', name, '.'

    CALL MPI_Finalize(ierror)
END PROGRAM main