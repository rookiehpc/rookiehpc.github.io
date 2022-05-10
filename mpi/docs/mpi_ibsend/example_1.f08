!> @brief Illustrates how to send a message in a non-blocking asynchronous
!> fashion.
!> @details This program is meant to be run with 2 processes: a sender and a
!> receiver.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: size
    INTEGER, PARAMETER :: sender_rank = 0
    INTEGER, PARAMETER :: receiver_rank = 1
    INTEGER :: my_rank
    INTEGER :: buffer_attached_size_bytes
    INTEGER :: buffer_attached_size_elements
    INTEGER, ALLOCATABLE, TARGET :: buffer_attached(:)
    TYPE(C_PTR) :: buffer_attached_C_pointer
    INTEGER, POINTER :: buffer_attached_F_pointer(:)
    INTEGER :: buffer_sent
    INTEGER :: received
    TYPE(MPI_Request) :: request

    CALL MPI_Init()

    ! Get the number of processes and check 2 processes are used
    CALL MPI_Comm_size(MPI_COMM_WORLD, size)
    IF (size .NE. 2) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 2 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1)
    END IF

    ! Get my rank and do the corresponding job
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
    SELECT CASE (my_rank)
        CASE (sender_rank)
            ! Declare the buffer and attach it
            buffer_attached_size_bytes = MPI_BSEND_OVERHEAD + SIZEOF(buffer_sent)
            buffer_attached_size_elements = (buffer_attached_size_bytes - MODULO(buffer_attached_size_bytes, SIZEOF(buffer_sent))) &
                                          / SIZEOF(buffer_sent)
            ALLOCATE(buffer_attached(0:buffer_attached_size_elements-1))
            CALL MPI_Buffer_attach(buffer_attached, buffer_attached_size_bytes)

            ! Issue the MPI_Ibsend
            buffer_sent = 12345
            WRITE(*,'(A,I0,A,I0,A)') 'MPI process ', my_rank, ' sends value ', buffer_sent, '.'
            CALL MPI_Ibsend(buffer_sent, 1, MPI_INTEGER, receiver_rank, 0, MPI_COMM_WORLD, request)
            
            ! Let's wait for the MPI_Ibsend to complete before progressing further.
            CALL MPI_Wait(request, MPI_STATUS_IGNORE)

            ! Detach the buffer. It blocks until all messages stored are sent.
            CALL MPI_Buffer_detach(buffer_attached_C_pointer, buffer_attached_size_bytes)
            CALL C_F_POINTER(buffer_attached_C_pointer, buffer_attached_F_pointer, SHAPE=[buffer_attached_size_elements])
            DEALLOCATE(buffer_attached)
        CASE (receiver_rank)
            CALL MPI_Recv(received, 1, MPI_INTEGER, sender_rank, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE)
            WRITE(*,'(A,I0,A,I0,A)') 'MPI process ', my_rank, ' received value ', received, '.'
    END SELECT

    CALL MPI_Finalize()
END PROGRAM main
