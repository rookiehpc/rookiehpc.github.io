!> @brief Illustrates how to retrieve the number of elements in a message
!> received.
!> @details This application is meant to be run with 2 processes: a sender and
!> a receiver.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: size
    INTEGER, PARAMETER :: sender_rank = 0
    INTEGER, PARAMETER :: receiver_rank = 1
    INTEGER :: my_rank
    INTEGER :: buffer(0:2) = 0
    TYPE(MPI_Status) :: status
    INTEGER :: count

    CALL MPI_Init()

    ! Size of the default communicator
    CALL MPI_Comm_size(MPI_COMM_WORLD, size)
    IF (size .NE. 2) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 2 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1)
    END IF

    ! Get my rank and do the corresponding job
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
    SELECT CASE (my_rank)
        CASE (sender_rank)
            CALL MPI_Send(buffer, 3, MPI_INTEGER, receiver_rank, 0, MPI_COMM_WORLD)
        CASE (receiver_rank)
            ! Receive the buffer
            CALL MPI_Recv(buffer, 3, MPI_INTEGER, sender_rank, 0, MPI_COMM_WORLD, status)

            ! Retrieve the number of elements (should be 3)
            CALL MPI_Get_count(status, MPI_INTEGER, count)
            WRITE(*,'(A,I0,A)') 'Number of elements retrieved from the message received: ', count, '.'
    END SELECT

    CALL MPI_Finalize()
END PROGRAM main
