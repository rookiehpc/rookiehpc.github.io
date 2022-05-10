!> @brief Illustrates how to receive a message without restricting the rank of
!> the sender.
!> @details This application is meant to be run with 2 MPI processes: 1 sender
!> and 1 receiver. It consists in the sender process sending a message to the 
!> receiver process, which will receive it without restricting the sender rank
!> during the reception operation. The receiver processes then concludes by
!> printing the rank of the message sender obtained via the MPI_Status
!> collected.
PROGRAM main    
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: size
    INTEGER :: my_rank
    INTEGER, PARAMETER :: sender_rank = 0
    INTEGER, PARAMETER :: receiver_rank = 1
    INTEGER :: buffer_sent
    INTEGER :: buffer_received
    Type(MPI_Status) :: status

    CALL MPI_Init()

    ! Check that only 2 MPI processes are used.
    CALL MPI_Comm_size(MPI_COMM_WORLD, size)
    IF (size .NE. 2) THEN
        WRITE (*,'(A)') 'This application is meant to be run with 2 MPI processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1)
    END IF

    ! Get my rank and do the corresponding job
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
    SELECT CASE (my_rank)
        CASE(sender_rank)
            ! Sends the message.
            buffer_sent = 12345;
            WRITE(*, '(A,I0,A,I0,A)') '[MPI process ', my_rank, '] I send value ', buffer_sent, '.'
            CALL MPI_Ssend(buffer_sent, 1, MPI_INTEGER, receiver_rank, 0, MPI_COMM_WORLD)
        CASE(receiver_rank)
            ! Receives the message.
            CALL MPI_Recv(buffer_received, 1, MPI_INTEGER, MPI_ANY_SOURCE, 0, MPI_COMM_WORLD, status)
            WRITE(*, '(A,I0,A,A,I0,A,I0,A)') '[MPI process ', my_rank, ']', &
                                           ' I received value ', buffer_received, &
                                           ', from rank ', status % MPI_SOURCE, '.'
    END SELECT

    CALL MPI_Finalize()

END PROGRAM main
