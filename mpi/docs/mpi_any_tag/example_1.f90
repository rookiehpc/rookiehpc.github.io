!> @brief Illustrates how to receive a message without restricting the tag of
!> the message.
!> @details This application is meant to be run with 2 MPI processes: 1 sender
!> and 1 receiver. It consists in the sender process sending a message to the 
!> receiver process, which will receive it without restricting the message tag
!> during the reception operation. The receiver processes then concludes by
!> printing the tag of the message obtained via the MPI_Status collected.
PROGRAM main    
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: size
    INTEGER :: my_rank
    INTEGER, PARAMETER :: sender_rank = 0
    INTEGER, PARAMETER :: receiver_rank = 1
    INTEGER :: buffer_sent
    INTEGER :: tag
    INTEGER :: buffer_received
    INTEGER :: status(MPI_STATUS_SIZE)

    CALL MPI_Init(ierror)

    ! Check that 2 MPI processes are used.
    CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    IF (size .NE. 2) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 2 MPI processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! Get my rank and do the corresponding job
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)
    SELECT CASE (my_rank)
        CASE (sender_rank)
            ! Sends the message
            buffer_sent = 12345
            tag = 67890
            WRITE(*,'(A,I0,A,I0,A,I0,A)') '[MPI process ', my_rank, '] I send value ', buffer_sent, ' with tag ', tag, '.'
            CALL MPI_Ssend(buffer_sent, 1, MPI_INTEGER, receiver_rank, tag, MPI_COMM_WORLD, ierror)
        CASE (receiver_rank)
            ! Receives the message
            CALL MPI_Recv(buffer_received, 1, MPI_INTEGER, sender_rank, MPI_ANY_TAG, MPI_COMM_WORLD, status, ierror)
            WRITE(*,'(A,I0,A,I0,A,I0,A)') '[MPI process ', my_rank, '] I received value ', buffer_received, &
                                          ', with tag ', status(MPI_TAG), '.'
    END SELECT

    CALL MPI_Finalize(ierror)
END PROGRAM main