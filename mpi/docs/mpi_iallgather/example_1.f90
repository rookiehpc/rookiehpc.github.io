!> @brief Illustrates how to use a gather in a non-blocking way.
!> @details This application is meant to be run with 3 MPI processes. Every MPI
!> process begins with a value, then every MPI process collects the entirety of
!> the data gathered and moves on immediately to do something else while the
!> gather progresses. They then wait for the gather to complete before printing
!> the data gathered. It can be visualised as follows:
!>
!> +-----------+  +-----------+  +-----------+
!> | Process 0 |  | Process 1 |  | Process 2 |
!> +-+-------+-+  +-+-------+-+  +-+-------+-+
!>   | Value |      | Value |      | Value |
!>   |   0   |      |  100  |      |  200  |
!>   +-------+      +-------+      +-------+
!>       |________      |      ________|
!>                |     |     |
!>             +-----+-----+-----+
!>             |  0  | 100 | 200 |
!>             +-----+-----+-----+
!>             |   Each process  |
!>             +-----------------+
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: size
    INTEGER :: my_rank
    INTEGER :: my_value
    INTEGER :: buffer(0:2)
    INTEGER :: request

    CALL MPI_Init(ierror)

    ! Get number of processes and check that 3 processes are used
    CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    IF (size .NE. 3) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 3 MPI processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    ! Define my value
    my_value = my_rank * 100
    WRITE(*,'(A,I0,A,I0,A)') 'Process ', my_rank, ', my value = ', my_value, '.'

    ! Issue the gather and move on immediately after, before the MPI_Iallgather completes
    CALL MPI_Iallgather(my_value, 1, MPI_INTEGER, buffer, 1, MPI_INTEGER, MPI_COMM_WORLD, request, ierror)

    ! Do another job while the gather progresses
    ! ...

    ! Wait for the gather to complete before printing the values received
    CALL MPI_Wait(request, MPI_STATUS_IGNORE, ierror)
    WRITE(*,'(A,I0,A,I0,A,I0,A,I0,A)') 'Values collected on process ', my_rank, ': ', &
                                       buffer(0), ', ', buffer(1), ', ', buffer(2), '.'

    CALL MPI_Finalize(ierror)
END PROGRAM main