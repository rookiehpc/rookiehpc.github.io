!> @brief Illustrates how to get the name of a communicator.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    CHARACTER(LEN=MPI_MAX_OBJECT_NAME) :: name
    INTEGER :: name_length

    CALL MPI_Init()

    ! Get the name of the default communicator
    CALL MPI_Comm_get_name(MPI_COMM_WORLD, name, name_length)
    WRITE(*,'(A,A,A)') 'The default communicator is named "', name, '".'

    CALL MPI_Finalize()
END PROGRAM main
