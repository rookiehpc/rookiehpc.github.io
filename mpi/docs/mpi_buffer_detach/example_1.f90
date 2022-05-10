!> @brief Illustrate how to detach the memory buffer from MPI.
!> @details This application requires 2 processes: 1 sender and 1 receiver.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: size
    INTEGER, PARAMETER :: sender_rank = 0
    INTEGER, PARAMETER :: receiver_rank = 1
    INTEGER :: my_rank
    INTEGER :: message
    INTEGER, ALLOCATABLE :: buffer(:)
    INTEGER :: buffer_size_bytes
    INTEGER :: buffer_size_elements

    CALL MPI_Init(ierror)

    ! Check that 2 processes are used
    CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    IF (size .NE. 2) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 2 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! Get my rank and do the corresponding job
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)
    SELECT CASE (my_rank)
        CASE (sender_rank)
            ! The message to send
            message = 1234

            ! Allocate enough space to issue the buffered send
            buffer_size_bytes = (MPI_BSEND_OVERHEAD + SIZEOF(message))
            buffer_size_elements = (buffer_size_bytes - MODULO(buffer_size_bytes, SIZEOF(message))) / SIZEOF(message);
            IF (MODULO(buffer_size_bytes, SIZEOF(message)) .NE. 0) THEN
                buffer_size_elements = buffer_size_elements + 1
            END IF
            ALLOCATE(buffer(buffer_size_elements))

            ! Pass the buffer allocated to MPI so it uses it when we issue MPI_Bsend
            CALL MPI_Buffer_attach(buffer, buffer_size_bytes, ierror)

            ! Issue the buffered send
            WRITE(*,'(A,I0,A,I0,A,I0,A)') '[Process ', my_rank, '] I send value ', message, ' to process ', receiver_rank, '.'
            CALL MPI_Bsend(message, 1, MPI_INTEGER, receiver_rank, 0, MPI_COMM_WORLD, ierror)

            ! Detach the buffer no-longer used (it will wait for MPI_Bsend message to be sent first)
            CALL MPI_Buffer_detach(buffer, buffer_size_bytes, ierror)
            DEALLOCATE(buffer)
        CASE (receiver_rank)
            ALLOCATE(buffer(1))
            CALL MPI_Recv(buffer, 1, MPI_INTEGER, sender_rank, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
            WRITE(*,'(A,I0,A,I0,A)') '[Process ', my_rank, '] I received value ', buffer, '.'
            DEALLOCATE(buffer)
    END SELECT

    CALL MPI_Finalize(ierror)
END PROGRAM main