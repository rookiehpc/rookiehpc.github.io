!> @brief Illustrates how to wait on multiple non-blocking routines until at
!> least one of them completes.
!> @details This program is meant to be run with 4 processes: 1 sender and 3
!> receivers. The sender emits 3 messages using non-blocking sends, one to each
!> receiver. It then uses MPI_Testsome to see which non-blocking routines
!> completed. This application covers multiple cases:
!> - Multiple non-blocking routines completed
!> - Single non-blocking routines completed
!> 
!> The execution flow can be visualised below:
!>
!>                       +-----------+-----------+-----------+
!>                       | Process 1 | Process 2 | Process 3 |
!> +---------------------+-----------+-----------+-----------+
!> | First MPI_Testsome  |     x     |     x     |           |
!> | Second MPI_Testsome |           |           |     x     |
!> +---------------------+-----------+-----------+-----------+
!>
!> (Note to readers: the MPI barriers used in this code are present just to make
!> sure that the application always exposes the execution flow depicted above.)
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: size
    INTEGER :: my_rank
    INTEGER, ALLOCATABLE :: buffer(:)
    INTEGER :: buffer_length
    TYPE(MPI_Request) :: requests(0:2)
    INTEGER :: recipient_rank_of_request(0:2)
    INTEGER :: i
    INTEGER :: index_count
    INTEGER :: indices(0:2)

    CALL MPI_Init()

    ! Get the number of processes and check only 4 processes are used
    CALL MPI_Comm_size(MPI_COMM_WORLD, size)
    IF (size .NE. 4) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 4 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1)
    END IF

    ! Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)

    SELECT CASE (my_rank)
        CASE (0)
            ! The 'master' MPI process sends the messages.
            buffer_length = 3
            ALLOCATE(buffer(0:buffer_length-1))
            buffer = [123, 456, 789]

            ! Send the messages
            DO i = 0, 2
                WRITE(*,'(A,I0,A,I0,A,I0,A)') '(Process ', my_rank, ') Sends ', buffer(i), ' to process ', i + 1, '.'
                CALL MPI_Issend(buffer(i), 1, MPI_INTEGER, i + 1, 0, MPI_COMM_WORLD, requests(i))
                recipient_rank_of_request(i) = i + 1
            END DO

            ! Barrier to make sure that the sends 1 and 2 are complete by the first MPI_Testsome
            CALL MPI_Barrier(MPI_COMM_WORLD)

            ! Test which of the non-blocking sends to complete
            CALL MPI_Testsome(3, requests, index_count, indices, MPI_STATUSES_IGNORE)
            DO i = 0, index_count-1
                WRITE(*,'(A,I0,A,A,I0,A)') '(Process ', my_rank, ') First MPI_Testsome: ', &
                                           ' the non-blocking send to process ', &
                                           recipient_rank_of_request(indices(i)-1), ' is complete.'
            END DO

            ! Tell process 3 to do the receive the message
            CALL MPI_Barrier(MPI_COMM_WORLD)

            ! Wait for process 3 to tell us the message has been received
            CALL MPI_Barrier(MPI_COMM_WORLD)

            ! Test if the other non-blocking send to complete
            CALL MPI_Testsome(3, requests, index_count, indices, MPI_STATUSES_IGNORE)
            DO i = 0, index_count-1
                WRITE(*,'(A,I0,A,A,I0,A)') '(Process ', my_rank, ') Second MPI_Testsome: ', &
                                           ' the non-blocking send to process ', &
                                           recipient_rank_of_request(indices(i)-1), ' is complete.'
            END DO
        CASE (3)
            ! Process 1 and 2 saying they completed their receive
            CALL MPI_Barrier(MPI_COMM_WORLD)

            ! Signal for process 3 to receive its message
            CALL MPI_Barrier(MPI_COMM_WORLD)

            ! The last MPI process will wait on the barrier before receiving the message.
            buffer_length = 1
            ALLOCATE(buffer(0:buffer_length-1))

            CALL MPI_Recv(buffer(0), 1, MPI_INTEGER, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE)
            WRITE(*,'(A,I0,A,I0,A)') '(Process ', my_rank, ') Received value ', buffer(0), '.'

            ! Tell the master process that the MPI_Recv is complete
            CALL MPI_Barrier(MPI_COMM_WORLD)
        CASE DEFAULT
            ! The MPI processes 1 and 2 will receive the message, then they wait on the barrier.
            buffer_length = 1
            ALLOCATE(buffer(0:buffer_length-1))

            CALL MPI_Recv(buffer(0), 1, MPI_INTEGER, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE)
            WRITE(*,'(A,I0,A,I0,A)') '(Process ', my_rank, ') Received value ', buffer(0), '.'

            ! Tell master process that process 1 and 2 have received their message
            CALL MPI_Barrier(MPI_COMM_WORLD)

            ! Master process telling process 3 to start receiving
            CALL MPI_Barrier(MPI_COMM_WORLD)

            ! Process 3 saying it received its message
            CALL MPI_Barrier(MPI_COMM_WORLD)
    END SELECT
    DEALLOCATE(buffer)

    CALL MPI_Finalize()
END PROGRAM main
