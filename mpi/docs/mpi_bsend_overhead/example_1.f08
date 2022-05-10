!> @brief Illustrate how to allocates the memory buffer in which store MPI_Bsend
!> message.
!> @details This application requires 3 processes: 1 sender and 2 receivers. It
!> shows how to allocate memory for multiple messages of different sizes:
!> - 1 MPI_INTEGER to send to receiver 1
!> - 2 MPI_INTEGER to send to receiver 2
!>
!> It can be visualised as follows:
!>
!> <------ Destined to receiver 1 -----><------ Destined to receiver 2 ------>
!> +--------------------+---------------+--------------------+---------------+
!> | MPI_Bsend overhead | 1 MPI_INTEGER | MPI_Bsend overhead | 2 MPI_INTEGER |
!> +--------------------+---------------+--------------------+---------------+
PROGRAM main
    USE mpi_f08
    USE, INTRINSIC ::  ISO_C_BINDING, &
         ONLY : C_PTR, C_LOC

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: size
    INTEGER, PARAMETER :: sender_rank = 0
    INTEGER, PARAMETER :: receiver_1_rank = 1
    INTEGER, PARAMETER :: receiver_2_rank = 2
    INTEGER :: my_rank
    INTEGER :: message_1
    INTEGER :: message_2(0:1)
    INTEGER :: buffer_size_bytes
    INTEGER :: buffer_size_elements
    INTEGER, ALLOCATABLE, TARGET :: buffer(:)
    TYPE(C_PTR) :: buffer_attached_C_pointer
    INTEGER, POINTER :: buffer_attached_F_pointer(:)

    CALL MPI_Init(ierror)

    !> Check that 3 processes are used
    CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    IF (size .NE. 3) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 3 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    !> Get my rank and do the corresponding job
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)
    SELECT CASE(my_rank)
        CASE (sender_rank)
            !> The messages to send
            message_1 = 1234
            message_2 = [567, 890]

            !> Allocate enough space to issue 2 buffered sends and their messages
            buffer_size_bytes = (MPI_BSEND_OVERHEAD + SIZEOF(message_1)) + (MPI_BSEND_OVERHEAD + SIZEOF(message_2))
            buffer_size_elements = (buffer_size_bytes - MODULO(buffer_size_bytes, SIZEOF(message_1))) / SIZEOF(message_1)
            IF (MODULO(buffer_size_bytes, SIZEOF(message_1)) .NE. 0) THEN
                buffer_size_elements = buffer_size_elements + 1
            END IF
            ALLOCATE(buffer(0:buffer_size_elements-1))
            WRITE(*,'(A,I0,A)') 'Size of an MPI_Bsend overhead: ', MPI_BSEND_OVERHEAD, ' bytes.'

            !> Pass the buffer allocated to MPI so it uses it when we issue MPI_Bsend
            CALL MPI_Buffer_attach(buffer, buffer_size_bytes, ierror)

            !> Issue the buffered sends
            WRITE(*,'(A,I0,A,I0,A,I0,A)') '[Process ', my_rank, '] I send value ', message_1, &
                                          ' to process ', receiver_1_rank, '.'
            CALL MPI_Bsend(message_1, 1, MPI_INTEGER, receiver_1_rank, 0, MPI_COMM_WORLD, ierror)
            WRITE(*,'(A,I0,A,I0,A,I0,A,I0,A)') '[Process ', my_rank, '] I send values ', message_2(0), ' and ', message_2(1), &
                                               ' to process ', receiver_2_rank, '.'
            CALL MPI_Bsend(message_2, 2, MPI_INTEGER, receiver_2_rank, 0, MPI_COMM_WORLD, ierror)

            !> Detach the buffer no-longer used (it will wait for MPI_Bsend messages to be sent first)
            CALL MPI_Buffer_detach(buffer_attached_C_pointer, buffer_size_bytes)
            CALL C_F_POINTER(buffer_attached_C_pointer, buffer_attached_F_pointer, SHAPE=[buffer_size_elements])
            DEALLOCATE(buffer_attached_F_pointer)
        CASE (receiver_1_rank)
            ALLOCATE(buffer(0:0))
            CALL MPI_Recv(buffer, 1, MPI_INTEGER, sender_rank, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
            WRITE(*,'(A,I0,A,I0,A)') '[Process ', my_rank, '] I received value ', buffer, '.'
            DEALLOCATE(buffer)
        CASE (receiver_2_rank)
            ALLOCATE(buffer(0:1))
            CALL MPI_Recv(buffer, 2, MPI_INTEGER, sender_rank, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
            WRITE(*,'(A,I0,A,I0,A,I0,A)') '[Process ', my_rank, '] I received values ', buffer(0), ' and ', buffer(1), '.'
            DEALLOCATE(buffer)
    END SELECT

    CALL MPI_Finalize(ierror)
END PROGRAM main
