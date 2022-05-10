!> @brief Illustrates how to test for the completion of an array of non-blocking
!> operations.
!> @details This application is designed to cover both cases:
!> - Issuing an MPI_Testall when not all the operations tested are complete
!> - Issuing an MPI_Testall when all the operations tested are complete
!>
!> The application execution flow can be visualised below:
!>
!>                  +--------------------+----------------+
!>                  | Not all operations | All operations |
!>                  |  are complete yet  |  are complete  |
!> +----------------+--------------------+----------------+
!> | MPI_Testall #1 |          X         |                |
!> | MPI_Testall #2 |                    |        X       |
!> +----------------+--------------------+----------------+
!>
!> This program is meant to be run with 3 processes: a sender and two
!> receivers.
!>
!> (Note to readers: the use of a barriers is only to guarantee that the
!> application exposes the execution flow depicted above.)
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: size
    INTEGER :: my_rank
    INTEGER :: message
    INTEGER, ALLOCATABLE :: buffer(:)
    INTEGER :: buffer_length
    INTEGER :: requests(0:1)
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
        buffer_length = 2
        ALLOCATE(buffer(0:buffer_length-1))

        CALL MPI_Irecv(buffer(0), 1, MPI_INTEGER, 1, 0, MPI_COMM_WORLD, requests(0), ierror)
        CALL MPI_Irecv(buffer(1), 1, MPI_INTEGER, 2, 0, MPI_COMM_WORLD, requests(1), ierror)

        ! This MPI_Testall is guaranteed to fail since the corresponding MPI_Ssends have not been issued yet.
        CALL MPI_Testall(2, requests, ready, MPI_STATUSES_IGNORE, ierror)
        IF (ready) THEN
            WRITE(*,'(A)') '(Process 0) First attempt: both receives are complete.'
        ELSE
            WRITE(*,'(A)') '(Process 0) First attempt: not both receives are complete yet.'
        END IF

        ! We can tell other processes to start sending messages
        CALL MPI_Barrier(MPI_COMM_WORLD, ierror)

        ! We wait for the other processes to tell us their MPI_Ssend is complete
        CALL MPI_Barrier(MPI_COMM_WORLD, ierror)

        ! This MPI_Testall is guaranteed to succeed since the corresponding MPI_Ssends are all complete.
        CALL MPI_Testall(2, requests, ready, MPI_STATUSES_IGNORE, ierror)
        IF (ready) THEN
            WRITE(*,'(A)') '(Process 0) Second attempt: both receives are complete.'
        ELSE
            WRITE(*,'(A)') '(Process 0) Second attempt: not both receives are complete yet.'
        END IF
    ELSE
        buffer_length = 1
        ALLOCATE(buffer(0:buffer_length-1))
        buffer(0) = MERGE(12345, 67890, my_rank .EQ. 1)

        ! Wait for the MPI_Testall #1 to be done.
        CALL MPI_Barrier(MPI_COMM_WORLD, ierror)

        CALL MPI_Ssend(message, 1, MPI_INTEGER, 0, 0, MPI_COMM_WORLD, ierror)

        ! Tell the sender it can now issue the MPI_Testall #2.
        CALL MPI_Barrier(MPI_COMM_WORLD, ierror)
    END IF
    DEALLOCATE(buffer)

    CALL MPI_Finalize(ierror)
END PROGRAM main