!> @brief Illustrates how to test for the completion of a non-blocking
!> operation.
!> @details This application is designed to cover both cases:
!> - Issuing an MPI_Test when the operation tested is not complete
!> - Issuing an MPI_Test when the operation tested is complete
!>
!> The application execution flow can be visualised below:
!>
!>               +---------------+-----------+
!>               | Operation not | Operation |
!>               | complete  yet | complete  |
!> +-------------+---------------+-----------+
!> | MPI_Test #1 |        X      |           |
!> | MPI_Test #2 |               |      X    |
!> +-------------+---------------+-----------+
!>
!> This program is meant to be run with 2 processes: a sender and a
!> receiver.
!>
!> (Note to readers: the use of a barrier and a second message message is only
!> to guarantee that the application exposes the execution flow depicted above.)
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: size
    INTEGER, PARAMETER :: sender_rank = 0
    INTEGER, PARAMETER :: receiver_rank = 1
    INTEGER :: my_rank
    INTEGER :: first_message
    INTEGER :: second_message
    LOGICAL :: ready
    TYPE(MPI_Request) :: request

    CALL MPI_Init()

    ! Get the number of processes and check only 2 processes are used
    CALL MPI_Comm_size(MPI_COMM_WORLD, size)
    IF (size .NE. 2) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 2 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1)
    END IF

    ! Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)

    SELECT CASE (my_rank)
        CASE (sender_rank)
            first_message = 12345
            second_message = 67890

            ! Wait for the receiver to issue the MPI_Test meant to fail
            CALL MPI_Barrier(MPI_COMM_WORLD)

            WRITE(*,'(A,I0,A)') '(Process 0) Sends first message (', first_message, ').'
            CALL MPI_Isend(first_message, 1, MPI_INTEGER, receiver_rank, 0, MPI_COMM_WORLD, request)
            WRITE(*,'(A,I0,A)') '(Process 0) Sends second message (', second_message, ').'
            CALL MPI_Send(second_message, 1, MPI_INTEGER, receiver_rank, 0, MPI_COMM_WORLD)
            CALL MPI_Wait(request, MPI_STATUS_IGNORE)
        CASE (receiver_rank)
            CALL MPI_Irecv(first_message, 1, MPI_INTEGER, sender_rank, 0, MPI_COMM_WORLD, request)

            ! The corresponding send has not been issued yet, this MPI_Test will 'fail'.
            CALL MPI_Test(request, ready, MPI_STATUS_IGNORE)
            IF (ready) THEN
                WRITE(*,'(A,I0,A)') '(Process 1) MPI_Test #1: message received (', first_message, ').'
            ELSE
                WRITE(*,'(A)') '(Process 1) MPI_Test #1: message not received yet.'
            END IF

            ! Tell the sender that we issued the MPI_Test meant to fail, it can now send the message.
            CALL MPI_Barrier(MPI_COMM_WORLD)

            CALL MPI_Recv(second_message, 1, MPI_INTEGER, sender_rank, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE)
            WRITE(*,'(A,I0,A,A)') '(Process 1) Second message received (', second_message, '), ', &
                                'which implies that the first message is received too.'
            CALL MPI_Test(request, ready, MPI_STATUS_IGNORE)
            IF (ready) THEN
                WRITE(*,'(A,I0,A)') '(Process 1) MPI_Test #2: message received (', first_message, ').'
            ELSE
                WRITE(*,'(A)') '(Process 1) MPI_Test #2: message not received yet.'
            END IF
    END SELECT

    CALL MPI_Finalize()
END PROGRAM main
