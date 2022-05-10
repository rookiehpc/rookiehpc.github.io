!> @brief Illustrates how to launch the communications represented with an array
!> request handles.
!> @details This program is meant to be run with 3 processes: 1 sender and 2
!> receivers. The sender prepares 2 MPI_Send with MPI_Send_init, then launches
!> both with MPI_Startall before waiting for both with MPI_Waitall. Receivers
!> only issue a common MPI_Recv.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: size
    INTEGER :: my_rank
    INTEGER :: buffers_sent(2) = [12345, 67890]
    INTEGER :: received
    TYPE(MPI_Request) :: requests(2)

    CALL MPI_Init()

    ! Get the number of processes and check only 3 processes are used
    CALL MPI_Comm_size(MPI_COMM_WORLD, size)
    IF (size .NE. 3) THEN
        WRITE(*, '(A)') 'This application is meant to be run with 3 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1)
    END IF

    ! Get my rank and do the corresponding job
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
    IF (my_rank .EQ. 0) THEN
        ! Prepare the send request handle
        CALL MPI_Send_init(buffers_sent(1), 1, MPI_INTEGER, 1, 0, MPI_COMM_WORLD, requests(1))
        CALL MPI_Send_init(buffers_sent(2), 1, MPI_INTEGER, 2, 0, MPI_COMM_WORLD, requests(2))
        WRITE(*, '(A,I0,A,I0,A)') 'MPI process ', my_rank, ' sends value ', buffers_sent(1), ' to process 1.'
        WRITE(*, '(A,I0,A,I0,A)') 'MPI process ', my_rank, ' sends value ', buffers_sent(2), ' to process 2.'
        ! Launch the sends
        CALL MPI_Startall(2, requests)
        ! Wait for the send to complete
        CALL MPI_Waitall(2, requests, MPI_STATUSES_IGNORE)
    ELSE
        CALL MPI_Recv(received, 1, MPI_INTEGER, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE)
        WRITE(*, '(A,I0,A,I0,A)') 'MPI process ', my_rank, ' received value ', received, '.'
    END IF

    CALL MPI_Finalize()
END PROGRAM main
