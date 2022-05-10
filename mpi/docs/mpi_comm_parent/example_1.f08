!> @brief Illustrates how to obtain the MPI_COMM_PARENT name.
!> @details In order to get the MPI_COMM_PARENT name, an MPI process must have
!> been spawned by another MPI process. This application therefore does so, and
!> the MPI process spawned prints the name of its parent communicator which is
!> MPI_COMM_PARENT by default.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    TYPE(MPI_Comm) :: parent
    TYPE(MPI_Comm) :: child
    INTEGER :: spawn_error(1)
    CHARACTER(LEN=MPI_MAX_OBJECT_NAME) :: name
    INTEGER :: name_length
    CHARACTER(LEN=128) :: program_name
    CHARACTER(LEN=128) :: program_arguments(1)

    CALL GET_COMMAND_ARGUMENT(0, program_name)

    CALL MPI_Init()

    CALL MPI_Comm_get_parent(parent)
    IF (parent .EQ. MPI_COMM_NULL) THEN
        ! We have no parent commuicator so we have been spawned directly by the user
        WRITE(*,'(A)') 'We are processes spawned directly by you, we now spawn a new instance of an MPI application.'
        program_arguments(1) = ''
        CALL MPI_Comm_spawn(program_name, program_arguments, 1, MPI_INFO_NULL, 0, MPI_COMM_WORLD, child, spawn_error)
    ELSE
        ! We have been spawned by another MPI process so we do have a parent communicator
        CALL MPI_Comm_get_name(parent, name, name_length)
        WRITE(*,'(A,A,A)') 'I have been spawned by MPI processes, my parent communicator is named "', name, '".'
    END IF

    CALL MPI_Finalize()
END PROGRAM main
