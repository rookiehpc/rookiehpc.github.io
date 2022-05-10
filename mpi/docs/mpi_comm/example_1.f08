!> @brief Get information of a communicator.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    TYPE(MPI_Comm) :: comm
    INTEGER :: comm_size
    CHARACTER(LEN=MPI_MAX_OBJECT_NAME) :: name
    INTEGER :: name_length

    CALL MPI_Init()

    comm = MPI_COMM_WORLD
    CALL MPI_Comm_size(comm, comm_size)
    CALL MPI_Comm_get_name(comm, name, name_length)
    WRITE(*,'(A,A,A,I0,A)') 'The communicator "', name, '" contains ', comm_size, ' MPI processes.'

    CALL MPI_Finalize()
END PROGRAM main
