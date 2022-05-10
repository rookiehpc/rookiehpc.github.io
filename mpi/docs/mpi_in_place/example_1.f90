!> @brief Illustrates how to use an in-place gather.
!> @details This application is meant to be run with 4 MPI processes. Every MPI
!> process begins with a value, then MPI process 0 is picked to gather all these
!> values and print them. It can be visualised as follows:
!>
!> +-----------+ +-----------+ +-----------+ +-----------+
!> | Process 0 | | Process 1 | | Process 2 | | Process 3 |
!> +-+-------+-+ +-+-------+-+ +-+-------+-+ +-+-------+-+ 
!>   | Value |     | Value |     | Value |     | Value |   
!>   |   0   |     |  100  |     |  200  |     |  300  |   
!>   +-------+     +-------+     +-------+     +-------+   
!>            \            |     |            /
!>             \           |     |           /
!>              \          |     |          /
!>               \         |     |         /
!>                \        |     |        /
!>                 \       |     |       /
!>                +-----+-----+-----+-----+
!>                |  0  | 100 | 200 | 300 |
!>                +-----+-----+-----+-----+
!>                |       Process 0       |
!>                +-----------------------+
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: size
    ! Determine root's rank
    INTEGER, PARAMETER :: root_rank = 0
    INTEGER :: my_rank
    INTEGER :: my_value
    INTEGER :: buffer(0:3)

    CALL MPI_Init(ierror)

    ! Get number of processes and check that 4 processes are used
    CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    IF (size .NE. 4) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 4 MPI processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    ! Define my value
    my_value = my_rank * 100;
    WRITE(*,'(A,I0,A,I0,A)') 'Process ', my_rank, ', my value = ', my_value, '.'

    IF (my_rank .EQ. root_rank) THEN
        buffer(my_rank) = my_value
        CALL MPI_Gather(MPI_IN_PLACE, 1, MPI_INTEGER, buffer, 1, MPI_INTEGER, root_rank, MPI_COMM_WORLD, ierror)
        WRITE(*,'(A,I0,A,I0,A,I0,A,I0,A,I0)') 'Values collected on process ', &
            my_rank, ': ', buffer(0), ', ', buffer(1), ', ', buffer(2), ', ', buffer(3), '.'
    ELSE
        CALL MPI_Gather(my_value, 1, MPI_INTEGER, buffer, 0, MPI_INTEGER, root_rank, MPI_COMM_WORLD, ierror)
    END IF

    CALL MPI_Finalize(ierror)
END PROGRAM main
