!> @brief Illustrates how to use a non-blocking all to all.
!> @details This application is meant to be run with 3 MPI processes. Every MPI
!> process begins with a buffer containing 3 integers, one for each process
!> including themselves. They also have a buffer in which receive the integer
!> that has been sent by each other process for them. It can be visualised as
!> follows:
!>
!> +-----------------------+ +-----------------------+ +-----------------------+
!> |       Process 0       | |       Process 1       | |       Process 2       |
!> +-------+-------+-------+ +-------+-------+-------+ +-------+-------+-------+
!> | Value | Value | Value | | Value | Value | Value | | Value | Value | Value |
!> |   0   |  100  |  200  | |  300  |  400  |  500  | |  600  |  700  |  800  |
!> +-------+-------+-------+ +-------+-------+-------+ +-------+-------+-------+
!>     |       |       |_________|_______|_______|_________|___    |       |
!>     |       |    _____________|_______|_______|_________|   |   |       |
!>     |       |___|_____________|_      |      _|_____________|___|       |
!>     |      _____|_____________| |     |     | |_____________|_____      |
!>     |     |     |               |     |     |               |     |     |
!>  +-----+-----+-----+         +-----+-----+-----+         +-----+-----+-----+
!>  |  0  | 300 | 600 |         | 100 | 400 | 700 |         | 200 | 500 | 800 |
!>  +-----+-----+-----+         +-----+-----+-----+         +-----+-----+-----+
!>  |    Process 0    |         |    Process 1    |         |    Process 2    |
!>  +-----------------+         +-----------------+         +-----------------+
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: size
    INTEGER :: my_rank
    INTEGER :: my_values(0:2)
    INTEGER :: buffer_recv(0:2)
    INTEGER :: i
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
    DO i = 0, 2
        my_values(i) = my_rank * 300 + i * 100
    END DO
    WRITE(*,'(A,I0,A,I0,A,I0,A,I0,A)') 'Process ', my_rank, ', my values = ', &
                                       my_values(0), ', ', my_values(1), ', ', my_values(2), '.'

    CALL MPI_Ialltoall(my_values, 1, MPI_INTEGER, buffer_recv, 1, MPI_INTEGER, MPI_COMM_WORLD, request, ierror)

    ! Do another job while the non-blocking all to all progresses
    WRITE(*,'(A,I0,A)') '[Process ', my_rank, '] The non-blocking all to all is in progress.'

    CALL MPI_Wait(request, MPI_STATUS_IGNORE, ierror)
    WRITE(*,'(A,I0,A,I0,A,I0,A,I0,A)') 'Values collected on process ', my_rank, ', my values = ', &
                                       buffer_recv(0), ', ', buffer_recv(1), ', ', buffer_recv(2), '.'

    CALL MPI_Finalize(ierror)
END PROGRAM main