!> @brief Illustrates how to spawn MPI processes.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: parent
    INTEGER :: child
    INTEGER :: spawn_error(1)
    CHARACTER(LEN=128) :: command
    CHARACTER(LEN=1) :: arguments(1)

    CALL GET_COMMAND_ARGUMENT(0, command)

    CALL MPI_Init(ierror)

    CALL MPI_Comm_get_parent(parent, ierror)
    IF (parent .EQ. MPI_COMM_NULL) THEN
        ! We have no parent communicator so we have been spawned directly by the user
        WRITE(*,'(A)') 'We are processes spawned directly by you, we now spawn a new instance of an MPI application.'
        arguments(1) = ''
        CALL MPI_Comm_spawn(command, arguments, 1, MPI_INFO_NULL, 0, MPI_COMM_WORLD, child, spawn_error, ierror)
    ELSE
        ! We have been spawned by another MPI process
        WRITE(*,'(A)') 'I have been spawned by MPI processes.'
    END IF

    CALL MPI_Finalize(ierror)
END PROGRAM main