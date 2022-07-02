!> @brief This exercise is to find the bug in a simple send-receive.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: comm_size
    INTEGER :: my_rank
    INTEGER :: buffer_size_bytes
    INTEGER :: buffer_size_elements
    INTEGER, ALLOCATABLE :: buffer(:)
    INTEGER :: value

    CALL MPI_Init(ierror)

    CALL MPI_Comm_size(MPI_COMM_WORLD, comm_size, ierror)
    IF (comm_size .NE. 2) THEN
        WRITE(*, '(A)') 'This application must be run with 2 MPI processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)
    IF (my_rank == 0) THEN
        value = 12345
        buffer_size_bytes = SIZEOF(value)
        buffer_size_elements = (buffer_size_bytes - MODULO(buffer_size_bytes, SIZEOF(value))) / SIZEOF(value)
        IF (MODULO(buffer_size_bytes, SIZEOF(value)) .NE. 0) THEN
            buffer_size_elements = buffer_size_elements + 1
        END IF
        ALLOCATE(buffer(0:buffer_size_elements-1))
        CALL MPI_Buffer_attach(buffer, buffer_size_bytes, ierror)

        WRITE(*, '(A,I0,A,I0,A)') '[MPI process ', my_rank ,'] I send value ', value ,'.'
        CALL MPI_Bsend(value, 1, MPI_INTEGER, 1, 0, MPI_COMM_WORLD, ierror)

        CALL MPI_Buffer_detach(buffer, buffer_size_bytes, ierror)
        DEALLOCATE(buffer)
    ELSE
        CALL MPI_Recv(value, 1, MPI_INTEGER, 0, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
        WRITE(*, '(A,I0,A,I0,A)') '[MPI process ', my_rank ,'] I received value ', value ,'.'
    END IF

    CALL MPI_Finalize(ierror)
END PROGRAM main