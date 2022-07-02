!> @brief This exercise is to find the bug in a simple send-receive.
PROGRAM main
    USE mpi_f08
    USE, INTRINSIC ::  ISO_C_BINDING, &
         ONLY : C_PTR, C_LOC

    IMPLICIT NONE

    INTEGER :: comm_size
    INTEGER :: my_rank
    INTEGER :: buffer_size_bytes
    INTEGER :: buffer_size_elements
    INTEGER, ALLOCATABLE :: buffer(:)
    TYPE(C_PTR) :: buffer_attached_C_pointer
    INTEGER, POINTER :: buffer_attached_F_pointer(:)
    INTEGER :: value

    CALL MPI_Init()

    CALL MPI_Comm_size(MPI_COMM_WORLD, comm_size)
    IF (comm_size .NE. 2) THEN
        WRITE(*, '(A)') 'This application must be run with 2 MPI processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1)
    END IF

    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
    IF (my_rank == 0) THEN
        value = 12345
        buffer_size_bytes = SIZEOF(value)
        buffer_size_elements = (buffer_size_bytes - MODULO(buffer_size_bytes, SIZEOF(value))) / SIZEOF(value)
        IF (MODULO(buffer_size_bytes, SIZEOF(value)) .NE. 0) THEN
            buffer_size_elements = buffer_size_elements + 1
        END IF
        ALLOCATE(buffer(0:buffer_size_elements-1))
        CALL MPI_Buffer_attach(buffer, buffer_size_bytes)

        WRITE(*, '(A,I0,A,I0,A)') '[MPI process ', my_rank ,'] I send value ', value ,'.'
        CALL MPI_Bsend(value, 1, MPI_INTEGER, 1, 0, MPI_COMM_WORLD)

        CALL MPI_Buffer_detach(buffer_attached_C_pointer, buffer_size_bytes)
        CALL C_F_POINTER(buffer_attached_C_pointer, buffer_attached_F_pointer, SHAPE=[buffer_size_elements])
        DEALLOCATE(buffer_attached_F_pointer)
    ELSE
        CALL MPI_Recv(value, 1, MPI_INTEGER, 0, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE)
        WRITE(*, '(A,I0,A,I0,A)') '[MPI process ', my_rank ,'] I received value ', value ,'.'
    END IF

    CALL MPI_Finalize()
END PROGRAM main