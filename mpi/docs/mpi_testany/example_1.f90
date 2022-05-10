!> @brief Illustrates how to check multiple non-blocking routines for the 
!> completion of one of them.
!> @details This program is meant to be run with 3 processes: a sender and two
!> receivers. The sender emits 2 messages in a non-blocking fashion. It then
!> tests the corresponding request handlers to see which operation finished. It
!> then repeats the process to check the other operation to finish.
!> This application covers two cases:
!> - None of the underlying non-blocking operations is complete yet.
!> - One of the underlying non-blocking operations is complete.
!> 
!>                  +---------------+---------------+
!>                  | MPI_Recv from | MPI_Recv from |
!>                  | process 1 is  | process 2 is  |
!>                  |    complete   |    complete   |
!> +----------------+---------------+---------------+
!> | MPI_Testany #1 |               |               |
!> | MPI_Testany #2 |       X       |               |
!> | MPI_Testany #3 |               |       X       |
!> +----------------+---------------+---------------+
!>
!> (Note to the reader: the use of MPI_Barrier is only to control the execution
!> flow to make sure it exposes the different cases mentioned above when the
!> MPI_Testany are called.)
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: size
    INTEGER :: my_rank
    INTEGER :: buffer(0:1)
    INTEGER :: received
    INTEGER :: count
    INTEGER :: requests(0:1)
    INTEGER :: recipient_rank_of_request(0:1)
    INTEGER :: index
    LOGICAL :: ready

    CALL MPI_Init(ierror)

    ! Get the number of processes and check only 3 processes are used
    CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    IF (size .NE. 3) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 3 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    IF (my_rank .EQ. 0) THEN
        ! The 'master' MPI process sends the messages.
        buffer = (/12345, 67890/)
        count = 2

        ! Send first message to process 1
        WRITE(*,'(A,I0,A,I0,A)') '(Process ', my_rank, ') Sends ', buffer(0), ' to process 1.'
        CALL MPI_Issend(buffer(0), 1, MPI_INTEGER, 1, 0, MPI_COMM_WORLD, requests(0), ierror)
        recipient_rank_of_request(0) = 1

        ! Send second message to process 2
        WRITE(*,'(A,I0,A,I0,A)') '(Process ', my_rank, ') Sends ', buffer(1), ' to process 2.'
        CALL MPI_Issend(buffer(1), 1, MPI_INTEGER, 2, 0, MPI_COMM_WORLD, requests(1), ierror)
        recipient_rank_of_request(1) = 2

        ! Check if one of the requests finished
        CALL MPI_Testany(count, requests, index, ready, MPI_STATUS_IGNORE, ierror)
        IF (ready) THEN
            WRITE(*,'(A,I0,A)') 'MPI_Issend to process ', recipient_rank_of_request(index-1), ' completed.'
        ELSE
            WRITE(*,'(A)') 'None of the MPI_Issend completed for now.'
        END IF

        ! Tell receivers they can now issue the first MPI_Recv.
        CALL MPI_Barrier(MPI_COMM_WORLD, ierror)

        ! Receivers tell us the MPI_Recv is complete.
        CALL MPI_Barrier(MPI_COMM_WORLD, ierror)

        ! Check if one of the requests finished
        CALL MPI_Testany(count, requests, index, ready, MPI_STATUS_IGNORE, ierror)
        IF (ready) THEN
            WRITE(*,'(A,I0,A)') 'MPI_Issend to process ', recipient_rank_of_request(index-1), ' completed.'
        ELSE
            WRITE(*,'(A)') 'None of the MPI_Issend completed for now.'
        END IF

        ! Tell receivers they can now issue the second MPI_Recv.
        CALL MPI_Barrier(MPI_COMM_WORLD, ierror)

        ! Receivers tell us the second MPI_Recv is complete.
        CALL MPI_Barrier(MPI_COMM_WORLD, ierror)

        ! Check if one of the requests finished
        CALL MPI_Testany(count, requests, index, ready, MPI_STATUS_IGNORE, ierror)
        IF (ready) THEN
            WRITE(*,'(A,I0,A)') 'MPI_Issend to process ', recipient_rank_of_request(index-1), ' completed.'
        ELSE
            WRITE(*,'(A)') 'None of the MPI_Issend completed for now.'
        END IF
    ELSE
        ! The 'slave' MPI processes receive the messages.

        ! Wait until the first MPI_Testany is issued.
        CALL MPI_Barrier(MPI_COMM_WORLD, ierror)

        IF (my_rank .EQ. 1) THEN
            CALL MPI_Recv(received, 1, MPI_INTEGER, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
            WRITE(*,'(A,I0,A,I0,A)') '(Process ', my_rank, ') Received value ', received, '.'
        END IF

        ! Tell that the first MPI_Recv is issued.
        CALL MPI_Barrier(MPI_COMM_WORLD, ierror)

        ! Wait until the second MPI_Testany is issued.
        CALL MPI_Barrier(MPI_COMM_WORLD, ierror)

        IF (my_rank .EQ. 2) THEN
            CALL MPI_Recv(received, 1, MPI_INTEGER, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
            WRITE(*,'(A,I0,A,I0,A)') '(Process ', my_rank, ') Received value ', received, '.'
        END IF

        ! Wait for the second MPI_Recv to be issued.
        CALL MPI_Barrier(MPI_COMM_WORLD, ierror)
    END IF

    CALL MPI_Finalize(ierror)
END PROGRAM main