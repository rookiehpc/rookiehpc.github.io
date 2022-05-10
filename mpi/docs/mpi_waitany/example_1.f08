!> @brief Illustrates how to wait on multiple non-blocking routines until one of
!> them completes.
!> @details This program is meant to be run with 3 processes: a sender and two
!> receivers. The sender emits 2 messages in a non-blocking fashion. It then
!> waits on the corresponding request handlers to see which operation finishes
!> first. It then repeats the process to wait for the other operation to finish.
!> This application covers two cases:
!> - All request handlers passed are active
!> - Certain request handlers passed have already completed
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: size
    INTEGER :: my_rank
    INTEGER, ALLOCATABLE :: buffer(:)
    INTEGER :: buffer_length
    TYPE(MPI_Request) :: requests(0:1)
    INTEGER :: recipient_rank_of_request(0:1)
    INTEGER :: index

    CALL MPI_Init()

    ! Get the number of processes and check only 3 processes are used
    CALL MPI_Comm_size(MPI_COMM_WORLD, size)
    IF (size .NE. 3) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 3 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1)
    END IF

    ! Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)

    IF (my_rank .EQ. 0) THEN
        ! The 'master' MPI process sends the messages.
        buffer_length = 2
        ALLOCATE(buffer(0:buffer_length-1))
        buffer = [12345, 67890]

        ! Send first message to process 1
        WRITE(*,'(A,I0,A,I0,A)') '(Process ', my_rank, ') Sends ', buffer(0), ' to process 1.'
        CALL MPI_Isend(buffer(0), 1, MPI_INTEGER, 1, 0, MPI_COMM_WORLD, requests(0))
        recipient_rank_of_request(0) = 1

        ! Send second message to process 2
        WRITE(*,'(A,I0,A,I0,A)') '(Process ', my_rank, ') Sends ', buffer(1), ' to process 2.'
        CALL MPI_Isend(buffer(1), 1, MPI_INTEGER, 2, 0, MPI_COMM_WORLD, requests(1))
        recipient_rank_of_request(1) = 2

        ! Wait for one of non-blocking sends to complete
        CALL MPI_Waitany(2, requests, index, MPI_STATUS_IGNORE)
        WRITE(*,'(A,I0,A,I0,A)') '(Process ', my_rank, ') The non-blocking send to process ', &
                                 recipient_rank_of_request(index), ' is complete.'

        ! Wait for the other non-blocking send to complete
        CALL MPI_Waitany(2, requests, index, MPI_STATUS_IGNORE)
        WRITE(*,'(A,I0,A,I0,A)') '(Process ', my_rank, ') The non-blocking send to process ', &
                                 recipient_rank_of_request(index), ' is complete too.'
    ELSE
        ! The 'slave' MPI processes receive the message.
        buffer_length = 1
        ALLOCATE(buffer(0:buffer_length-1))
        CALL MPI_Recv(buffer, 1, MPI_INTEGER, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE)
        WRITE(*,'(A,I0,A,I0,A)') '(Process ', my_rank, ') Received value ', buffer, '.'
    END IF

    DEALLOCATE(buffer)

    CALL MPI_Finalize()
END PROGRAM main
