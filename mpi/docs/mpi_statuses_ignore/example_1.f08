!> @brief Illustrates how to use MPI_STATUSES_IGNORE.
!> @details This program is meant to be run with 3 processes: a sender and two
!> receivers.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: size
    INTEGER :: my_rank
    INTEGER, ALLOCATABLE :: buffer(:)
    INTEGER :: buffer_length
    TYPE(MPI_Request) :: requests(0:1)

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
        ! The 'master' MPI process sends the message.
        buffer_length = 2
        ALLOCATE(buffer(0:buffer_length-1))
        buffer = [12345, 67890]
        WRITE(*,'(A,I0,A,I0,A,I0,A)') 'MPI process ', my_rank, ' sends the values ', buffer(0), '  ', buffer(1), '.'
        CALL MPI_Isend(buffer(0), 1, MPI_INTEGER, 1, 0, MPI_COMM_WORLD, requests(0))
        CALL MPI_Isend(buffer(1), 1, MPI_INTEGER, 2, 0, MPI_COMM_WORLD, requests(1))

        ! Wait for both routines to complete, but tell MPI we won't need the statuses.
        CALL MPI_Waitall(2, requests, MPI_STATUSES_IGNORE)
        WRITE(*,'(A,I0,A)') 'Process ', my_rank, ': both messages have been sent.'
    ELSE
        ! The 'slave' MPI processes receive the message.
        buffer_length = 1
        ALLOCATE(buffer(0:buffer_length-1))
        CALL MPI_Recv(buffer, 1, MPI_INTEGER, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE)
        WRITE(*,'(A,I0,A,I0,A)') 'Process ', my_rank, ' received value ', buffer, '.'
    END IF
    DEALLOCATE(buffer)

    CALL MPI_Finalize()
END PROGRAM main
