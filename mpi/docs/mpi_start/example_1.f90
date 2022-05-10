!> @brief Illustrates how to launch the communication represented with a request
!> handle.
!> @details This program is meant to be run with 2 processes: a sender and a
!> receiver. The sender prepares an MPI_Send with MPI_Send_init, then launches
!> it with MPI_Start before waiting for its completion with MPI_Wait. The
!> receiver only issues a common MPI_Recv. This communication is repeated three
!> times to illustrate the use of the same request handle by MPI_Start, that is,
!> persistent communications.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: size
    INTEGER, PARAMETER :: sender_rank = 0
    INTEGER, PARAMETER :: receiver_rank = 1
    INTEGER :: my_rank
    INTEGER :: buffer
    INTEGER :: request
    INTEGER :: i

    CALL MPI_Init(ierror)

    ! Get the number of processes and check only 2 processes are used
    CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    IF (size .NE. 2) THEN
        WRITE(*, '(A)') 'This application is meant to be run with 2 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! Get my rank and do the corresponding job
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)
    SELECT CASE (my_rank)
        CASE (sender_rank)
            ! Prepare the send request handle
            CALL MPI_Send_init(buffer, 1, MPI_INTEGER, receiver_rank, 0, MPI_COMM_WORLD, request, ierror)
            DO i = 0, 2
                buffer = 12345 + i
                ! Launch the send
                CALL MPI_Start(request, ierror)
                ! Wait for the send to complete
                CALL MPI_Wait(request, MPI_STATUS_IGNORE, ierror)
                WRITE(*, '(A,I0,A,I0,A,I0,A)') 'MPI process ', my_rank, ' sends value ', buffer, ' for message ', i, '.'
                
            END DO
        CASE (receiver_rank)
            DO i = 0, 2
                CALL MPI_Recv(buffer, 1, MPI_INTEGER, sender_rank, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
                WRITE(*, '(A,I0,A,I0,A,I0,A)') 'MPI process ', my_rank, ' received value ', buffer, ' for message ', i, '.'
            END DO
    END SELECT

    CALL MPI_Finalize(ierror)
END PROGRAM main