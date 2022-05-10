!> @brief Illustrates how to probe a message.
!> @details This application is designed to receive a message with an unknown
!> length. The receiver issues a probe to retrieve information about the
!> incoming message to allocate a buffer with enough space before actually
!> receiving the message.
!> This application is meant to be run with 2 processes: a sender and a
!> receiver.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: size
    INTEGER, PARAMETER :: sender_rank = 0
    INTEGER, PARAMETER :: receiver_rank = 1
    INTEGER :: my_rank
    INTEGER, ALLOCATABLE :: buffer(:)
    INTEGER :: count
    INTEGER :: status(MPI_STATUS_SIZE)
    INTEGER :: i

    CALL MPI_Init(ierror)

    ! Size of the default communicator
    CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    IF (size .NE. 2) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 2 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! Get my rank and do the corresponding job
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)
    SELECT CASE (my_rank)
        CASE (sender_rank)
            count = 3
            ALLOCATE(buffer(0:count-1))
            buffer = (/123, 456, 789/)
            WRITE(*,'(A,I0,A,A,I0,A,I0,A,I0,A)') 'Process ', my_rank, ': sending a message containing 3 ints ', &
                          '(', buffer(0), ', ', buffer(1), ', ', buffer(2), '), but the receiver is not aware of the length.'
            CALL MPI_Send(buffer, 3, MPI_INTEGER, receiver_rank, 0, MPI_COMM_WORLD, ierror)
        CASE (receiver_rank)
            ! Retrieve information about the incoming message
            CALL MPI_Probe(0, 0, MPI_COMM_WORLD, status, ierror)
            WRITE(*,'(A,I0,A)') 'Process ', my_rank, ': obtained message status by probing it.'

            ! Get the number of integers in the message probed
            CALL MPI_Get_count(status, MPI_INTEGER, count, ierror)

            ! Allocate the buffer now that we know how many elements there are
            count = 3
            ALLOCATE(buffer(0:count-1))

            ! Finally receive the message
            CALL MPI_Recv(buffer, count, MPI_INTEGER, sender_rank, 0, MPI_COMM_WORLD, status, ierror)
            WRITE(*,'(A,I0,A,I0,A)',advance='no') 'Process ', my_rank, ': received message with all ', count, ' ints:'
            DO i = 0, count -1
                WRITE(*,'(A,I0)',advance='no') ' ', buffer(i)
            END DO
            WRITE(*,'(A)') ''
    END SELECT
    DEALLOCATE(buffer)

    CALL MPI_Finalize(ierror)
END PROGRAM main