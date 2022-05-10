!> @brief Illustrates how to wait on multiple non-blocking routines until at
!> least one of them completes.
!> @details This program is meant to be run with 4 processes: 1 sender and 3
!> receivers. The sender emits 3 messages using non-blocking sends, one to each
!> receiver. It then uses MPI_Waitsome to see which non-blocking routines
!> completed. This application covers multiple cases:
!> - Multiple non-blocking routines completed
!> - Single non-blocking routines completed
!> 
!> The execution flow can be visualised below:
!>
!>                       +-----------+-----------+-----------+
!>                       | Process 1 | Process 2 | Process 3 |
!> +---------------------+-----------+-----------+-----------+
!> | First MPI_Waitsome  |     x     |     x     |           |
!> | Second MPI_Waitsome |           |           |     x     |
!> +---------------------+-----------+-----------+-----------+
!>
!> (Note to readers: the MPI barriers used in this code are present just to make
!> sure that the application always exposes the execution flow depicted above.)
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: size
    INTEGER :: my_rank
    INTEGER :: requests(0:2)
    INTEGER :: recipient_rank_of_request(0:2)
    INTEGER, ALLOCATABLE :: buffer(:)
    INTEGER :: buffer_length
    INTEGER :: index_count
    INTEGER :: indices(0:2)
    INTEGER :: i

    CALL MPI_Init(ierror)

    ! Get the number of processes and check only 4 processes are used
    CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    IF (size .NE. 4) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 4 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    SELECT CASE (my_rank)
        CASE (0)
            ! The 'master' MPI process sends the messages.
            buffer_length = 3
            ALLOCATE(buffer(0:buffer_length-1))
            buffer = (/123, 456, 789/)

            ! Send first message to process 1
            WRITE(*,'(A,I0,A,I0,A)') '(Process ', my_rank, ') Sends ', buffer(0), ' to process 1.'
            CALL MPI_Issend(buffer(0), 1, MPI_INTEGER, 1, 0, MPI_COMM_WORLD, requests(0), ierror)
            recipient_rank_of_request(0) = 1

            ! Send second message to process 2
            WRITE(*,'(A,I0,A,I0,A)') '(Process ', my_rank, ') Sends ', buffer(1), ' to process 2.'
            CALL MPI_Issend(buffer(1), 1, MPI_INTEGER, 2, 0, MPI_COMM_WORLD, requests(1), ierror)
            recipient_rank_of_request(1) = 2

            ! Send second message to process 3
            WRITE(*,'(A,I0,A,I0,A)') '(Process ', my_rank, ') Sends ', buffer(2), ' to process 3.'
            CALL MPI_Issend(buffer(2), 1, MPI_INTEGER, 3, 0, MPI_COMM_WORLD, requests(2), ierror)
            recipient_rank_of_request(2) = 3

            ! Barrier to make sure that the sends 1 and 2 are complete by the first MPI_Waitsome
            CALL MPI_Barrier(MPI_COMM_WORLD, ierror)

            ! Wait for one of non-blocking sends to complete
            CALL MPI_Waitsome(3, requests, index_count, indices, MPI_STATUSES_IGNORE, ierror)
            DO i = 0, index_count-1
                WRITE(*,'(A,I0,A,A,I0,A)') '(Process ', my_rank ,') First MPI_Waitsome: ', &
                                         'the non-blocking send to process ', recipient_rank_of_request(indices(i)) ,' is complete.'
            END DO

            ! Barrier to make sure that the send 3 is complete by the second MPI_Waitsome
            CALL MPI_Barrier(MPI_COMM_WORLD, ierror)

            ! Wait for the other non-blocking send to complete
            CALL MPI_Waitsome(3, requests, index_count, indices, MPI_STATUSES_IGNORE, ierror)
            DO i = 0, index_count-1
                WRITE(*,'(A,I0,A,A,I0,A)') '(Process ', my_rank ,') First MPI_Waitsome: ', &
                                         'the non-blocking send to process ', recipient_rank_of_request(indices(i)) ,' is complete.'
            END DO
        CASE (3)
            CALL MPI_Barrier(MPI_COMM_WORLD, ierror)
            CALL MPI_Barrier(MPI_COMM_WORLD, ierror)

            ! The last MPI process will wait on the barrier before receiving the message.
            buffer_length = 1
            ALLOCATE(buffer(0:buffer_length-1))
            CALL MPI_Recv(buffer, 1, MPI_INTEGER, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
            WRITE(*,'(A,I0,A,I0,A)') '(Process ', my_rank ,') Received value ', buffer ,'.'
        CASE DEFAULT
            ! The MPI processes 1 and 2 will receive the message, then they wait on the barrier.
            buffer_length = 1
            ALLOCATE(buffer(0:buffer_length-1))
            CALL MPI_Recv(buffer, 1, MPI_INTEGER, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
            WRITE(*,'(A,I0,A,I0,A)') '(Process ', my_rank ,') Received value ', buffer ,'.'

            CALL MPI_Barrier(MPI_COMM_WORLD, ierror)
            CALL MPI_Barrier(MPI_COMM_WORLD, ierror)
    END SELECT

    CALL MPI_Finalize(ierror)
END PROGRAM main