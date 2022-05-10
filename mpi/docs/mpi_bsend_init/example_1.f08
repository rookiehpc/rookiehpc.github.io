!> @brief Illustrates how to send a message in a blocking asynchronous fashion
!> using persistent communications.
!> @details This application is meant to be used with 2 processes; 1 sender and
!> 1 receiver.
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
    TYPE(MPI_Request) :: request
    INTEGER :: i

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

            ! Prepare the buffered send
            CALL MPI_Bsend_init(buffer_sent, 1, MPI_INTEGER, receiver_rank, 0, MPI_COMM_WORLD, request)

            DO i = 0, 2
                buffer_sent = 12345 + i
                WRITE(*,'(A,I0,A,I0,A,I0,A)') '[MPI process ', my_rank, '] I send value ', buffer_sent, ' for message ', i, '.'
                ! Launch the buffered send
                CALL MPI_Start(request)
                ! Wait for the buffered send to complete
                CALL MPI_Wait(request, MPI_STATUS_IGNORE)
            END DO

            ! Detach the buffer. It blocks until all messages stored are sent.
            CALL MPI_Buffer_detach(buffer_attached_C_pointer, buffer_attached_size_bytes)
            CALL C_F_POINTER(buffer_attached_C_pointer, buffer_attached_F_pointer, SHAPE=[buffer_attached_size_elements])
            DEALLOCATE(buffer_attached_F_pointer)
        case (receiver_rank)
            ! Receive the message and print it.
            DO i = 0, 2
                CALL MPI_Recv(received, 1, MPI_INTEGER, sender_rank, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE)
                WRITE(*,'(A,I0,A,I0,A,I0,A)') '[MPI process ', my_rank, '] I received value ', received, ' for message ', i, '.'
            END DO
    END SELECT

    CALL MPI_Finalize()
END PROGRAM main
