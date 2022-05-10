!> @brief Illustrates how to synchronously send a message using persistent
!> communications.
!> @details This program is meant to be run with 2 processes: a sender and a
!> receiver.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: size
    INTEGER, PARAMETER :: sender_rank = 0
    INTEGER, PARAMETER :: receiver_rank = 1
    INTEGER :: my_rank
    INTEGER :: buffer
    TYPE(MPI_Request) :: request
    INTEGER :: i

    CALL MPI_Init()

    ! Get the number of processes and check only 2 processes are used
    CALL MPI_Comm_size(MPI_COMM_WORLD, size)
    IF (size .NE. 2) THEN
        WRITE(*, '(A)') 'This application is meant to be run with 2 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1)
    END IF

    ! Get my rank and do the corresponding job
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
    SELECT CASE (my_rank)
        CASE (sender_rank)
            ! Prepare the synchronous send request handle
            CALL MPI_Ssend_init(buffer, 1, MPI_INTEGER, receiver_rank, 0, MPI_COMM_WORLD, request)
            DO i = 0, 2
                buffer = 12345 + i
                ! Launch the synchronous send
                CALL MPI_Start(request)
                ! Wait for the synchronous send to complete
                CALL MPI_Wait(request, MPI_STATUS_IGNORE)
                WRITE(*, '(A,I0,A,I0,A,I0,A)') 'MPI process ', my_rank, ' sends value ', buffer, ' for message ', i, '.'
                
            END DO
        CASE (receiver_rank)
            DO i = 0, 2
                CALL MPI_Recv(buffer, 1, MPI_INTEGER, sender_rank, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE)
                WRITE(*, '(A,I0,A,I0,A,I0,A)') 'MPI process ', my_rank, ' received value ', buffer, ' for message ', i, '.'
            END DO
    END SELECT

    CALL MPI_Finalize()
END PROGRAM main
