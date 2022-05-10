!> @brief Illustrates how to set the name of a communicator.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    CHARACTER(:), ALLOCATABLE :: name
    CHARACTER(LEN=MPI_MAX_OBJECT_NAME) :: verification_name
    INTEGER :: verification_name_length

    CALL MPI_Init()

    ! Set the name of the default communicator
    ALLOCATE(CHARACTER(LEN('My default communicator')) :: name)
    name = 'My default communicator'
    CALL MPI_Comm_set_name(MPI_COMM_WORLD, name)
    DEALLOCATE(name)

    ! Get the name of the default communicator and print it
    CALL MPI_Comm_get_name(MPI_COMM_WORLD, verification_name, verification_name_length)
    WRITE(*,'(A,A,A)') 'The default communicator has been renamed as "', verification_name, '".'

    CALL MPI_Finalize()
END PROGRAM main
