!> @brief Illustrates how to send a message in a blocking asynchronous fashion.
!> @details This application is meant to be used with 2 processes; 1 sender and
!> 1 receiver. The sender will declare a buffer containing enough space for 1
!> message that will contain 1 integer. It then attaches the buffer to MPI and
!> issues the MPI_Bsend. Finally, it detaches the buffer and frees it, while the
!> receiver prints the message received.
PROGRAM main
    USE mpi_f08
    USE, INTRINSIC ::  ISO_C_BINDING, &
         ONLY : C_PTR, C_LOC

    IMPLICIT NONE

    INTEGER :: size
    INTEGER :: my_rank
    INTEGER, PARAMETER :: sender_rank = 0
    INTEGER, PARAMETER :: receiver_rank = 1
    INTEGER :: buffer_attached_size_bytes
    INTEGER :: buffer_attached_size_elements
    INTEGER, ALLOCATABLE, TARGET :: buffer_attached(:)
    TYPE(C_PTR) :: buffer_attached_C_pointer
    INTEGER, POINTER :: buffer_attached_F_pointer(:)
    INTEGER :: buffer_sent
    INTEGER :: received

    CALL MPI_Init()

    ! Get the number of processes and check only 2 are used.
    CALL MPI_Comm_size(MPI_COMM_WORLD, size)
    IF (size .NE. 2) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 2 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1)
    END IF

    ! Get my rank and do the corresponding job
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
    SELECT CASE (my_rank)
        case (sender_rank)
            ! Declare the buffer and attach it
            buffer_attached_size_bytes = MPI_BSEND_OVERHEAD + SIZEOF(buffer_sent)
            buffer_attached_size_elements = (buffer_attached_size_bytes - MODULO(buffer_attached_size_bytes, SIZEOF(buffer_sent))) &
                                            / SIZEOF(buffer_sent)
            IF (MODULO(buffer_attached_size_bytes, SIZEOF(buffer_sent)) .NE. 0) THEN
                buffer_attached_size_elements = buffer_attached_size_elements + 1
            END IF
            ALLOCATE(buffer_attached(0:buffer_attached_size_elements-1))
            CALL MPI_Buffer_attach(buffer_attached, buffer_attached_size_bytes)

            ! Issue the MPI_Bsend
            buffer_sent = 12345;
            WRITE(*,'(A,I0,A,I0,A)') '[MPI process ', my_rank, '] I send value ', buffer_sent, '.'
            CALL MPI_Bsend(buffer_sent, 1, MPI_INTEGER, receiver_rank, 0, MPI_COMM_WORLD)

            ! Detach the buffer. It blocks until all messages stored are sent.
            CALL MPI_Buffer_detach(buffer_attached_C_pointer, buffer_attached_size_bytes)
            CALL C_F_POINTER(buffer_attached_C_pointer, buffer_attached_F_pointer, SHAPE=[buffer_attached_size_elements])
            DEALLOCATE(buffer_attached_F_pointer)
        case (receiver_rank)
            ! Receive the message and print it.
            CALL MPI_Recv(received, 1, MPI_INTEGER, sender_rank, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE)
            WRITE(*,'(A,I0,A,I0,A)') '[MPI process ', my_rank, '] I received value ', received, '.'
    END SELECT

    CALL MPI_Finalize()
END PROGRAM main
