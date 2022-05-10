!> @brief Illustrates how to get the name of a communicator.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    CHARACTER(LEN=MPI_MAX_OBJECT_NAME) :: name
    INTEGER :: name_length

    CALL MPI_Init(ierror)

    ! Get the name of the default communicator
    CALL MPI_Comm_get_name(MPI_COMM_WORLD, name, name_length, ierror)
    WRITE(*,'(A,A,A)') 'The default communicator is named "', name, '".'

    CALL MPI_Finalize(ierror)
END PROGRAM main